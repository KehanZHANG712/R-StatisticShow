
########library use
library(shiny)
library(ggplot2)
library(ggiraph)
library(leaflet)
library(dplyr)
library(tidyr)
library(colorspace)
library(sf)
library(rnaturalearth)
library(readxl)
library(sp)
library(plotly)

###########data 

#data import and fix empty(NA)
covid_data <- read_xlsx('owid-covid-data.xlsx')
covid_data[is.na(covid_data)] <- 0
covid_data <- covid_data[as.Date(covid_data$date) >= as.Date("2021-01-01") & as.Date(covid_data$date) <= as.Date("2021-12-31"), ]

#import world wide map and set its form
world <- ne_countries(scale = "medium", returnclass = "sf")
world <- st_transform(world, 4326)


########user interface

ui <- fluidPage(
  
#make sure the tooltip appear at the front of the window
  tags$script(HTML("
    $(document).on('shown.bs.modal', function() {
      setTimeout(function() {
        $('.d3-tip').css('z-index', '99999999');
      }, 50);
    });
  ")),
  
#the 'view trend' link setting
  tags$script(HTML("
  $(document).on('click', '.view-trend-link', function(e) {
    e.preventDefault();
    var country = $(this).data('country');
    Shiny.setInputValue('view_trend_clicked', country, {priority: 'event'});
  });
")),
  
#title
  titlePanel("COVID-19 Cases for 2021"),
  
#overall window(webpage) settings
  tags$div(
    style = "display: flex; flex-direction: column; height: 90vh;", 
#map container settings   
    tags$div(
      id = "map-container",
      style = "flex: 1; width: 100%; overflow: hidden;",
      leafletOutput('map_hospitals', height = "100%")
    ),
# footer options settings    
    div(
      class = "custom-footer",
      style = "height: 120px; display: flex; justify-content: space-between; align-items: center;",  # Increased height
      
      # Spacer for 1/4 positioning
      div(style = "flex-basis: 25%; flex-shrink: 1; flex-grow: 0;"),
      
      # Slider at 1/4
      div(
        style = "flex-basis: 25%; flex-shrink: 0; flex-grow: 0;", 
        sliderInput(
          inputId = "slider",
          label = "Select Date:",
          min = as.Date("2021-01-01"),
          max = as.Date("2021-12-31"),
          value = as.Date("2021-01-10"),
          timeFormat = "%Y-%m-%d"
        )
      ),
      
      # Spacer for center positioning
      div(style = "flex-basis: 25%; flex-shrink: 1; flex-grow: 0;"),
      
      # Radio buttons at 3/4
      div(
        style = "flex-basis: 25%; flex-shrink: 0; flex-grow: 0;", 
        radioButtons("metric", "Choose data:", choices = c("New Cases" = "new_cases", "New Deaths" = "new_deaths"), selected = "new_cases")
      ),
      
      # Spacer for end positioning
      div(style = "flex-basis: 25%; flex-shrink: 1; flex-grow: 0;")
    ),
    
    # Adding a container for line chart
    uiOutput("popup")
    
  )
)


#######Server 
server <- function(input, output, session) {
#map part
  output$map_hospitals <- renderLeaflet({
    #map needed data
    subset_data <- covid_data[covid_data$date == format(input$slider, "%Y-%m-%d"), ]
    world_joined <- left_join(world, subset_data, by = c("name" = "location"))
    
    chosen_metric <- input$metric
    
    # Conditionally set the color palette based on the metric
    color_palette <- ifelse(chosen_metric == "new_cases", "Reds", "YlOrBr")
    #set colour and NA number to grey
    color_pal_continuous <- colorNumeric(
      palette = color_palette,
      domain = range(world_joined[[chosen_metric]], na.rm = TRUE),
      na.color = "grey"
    )
    #set map out put
    m <- leaflet(data = world_joined, options = leafletOptions(minZoom = 2)) %>%
      addProviderTiles(providers$OpenStreetMap) %>%
      #countries
      addPolygons(
        fillColor = ~color_pal_continuous(get(chosen_metric)),
        weight = 2,
        opacity = 1,
        color = "white",
        dashArray = "3",
        fillOpacity = 0.7,
        label = ~name,
        layerId = ~name,
        #popup information
        popup = ~paste("<strong>Country:</strong>", name, 
                       "<br><strong>", 
                       ifelse(chosen_metric == "new_cases", "New Cases:", "New Deaths:"), 
                       "</strong>", get(chosen_metric), 
                       '<br><a href="#" class="view-trend-link" data-country="', name, '">View Trend</a>'),
        group = "countries"
      ) %>%
      #set click country
      addLegend(
        pal = color_pal_continuous, 
        values = ~get(chosen_metric),
        title = ifelse(chosen_metric == "new_cases", "Number of New Cases", "Number of New Deaths"),
        position = "bottomright"
      ) %>%
      #set map bounds
      setMaxBounds(
        lng1 = -180, lat1 = -90,
        lng2 = 180, lat2 = 90
      ) %>%
      #set map center
      setView(lng = 10.4515, lat = 51.1657, zoom = 3)
    
    m
  })
  
  #click action 
  observeEvent(input$view_trend_clicked, {
    #get clicked country name 
    country_name <- trimws(input$view_trend_clicked)
    country_data <- covid_data[covid_data$location == country_name, ]
    
    chosen_metric <- input$metric
    # null judgement
    if (all(is.na(country_data[[chosen_metric]]))) {
      return(NULL)
    }
    # standard data
    country_data$date <- as.character(country_data$date)
    total_data_points <- nrow(country_data)
    #get button choice   
    country_data_seg <- country_data %>%
      arrange(date) %>%
      mutate(next_date = lead(date), 
             next_value = lead(get(chosen_metric)),
             tooltip_text = paste("Date:", date, "<br>", 
                                  ifelse(chosen_metric == "new_cases", "New Cases:", "New Deaths:"), 
                                  scales::comma(get(chosen_metric))))
    #set the line chart
    p <- ggplot(country_data_seg, aes(x = as.Date(date), xend = as.Date(next_date), 
                                      y = get(chosen_metric), yend = next_value,
                                      color = get(chosen_metric), text = tooltip_text)) +
      geom_segment(aes(group = location), show.legend = FALSE) +  # Hide legend for this layer
      labs(x = "Date", y = ifelse(chosen_metric == "new_cases", "New Cases", "New Deaths")) +
      #x,y axis setting
      scale_x_date(breaks = scales::date_breaks("3 months")) +
      scale_y_continuous(labels = scales::comma) +
      #color setting
      scale_color_gradient(low = "yellow", high = "red")
    #window settings
    showModal(
      modalDialog(
        title = paste("COVID-19", ifelse(chosen_metric == "new_cases", "Cases", "Deaths"), "for", country_name, "in 2021"),
        size = "l",
        renderPlotly({
          ggplotly(p, tooltip = "text")
        })
      )
    )
  })
}


##########start
shinyApp(ui, server)