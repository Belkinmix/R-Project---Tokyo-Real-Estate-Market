library(tidyverse)
library(shiny)
library(shinydashboard)
library(DT)
library(plotly)
library(sf)  
library(leaflet)
library(tmap)

load("TokyoDatabase.RData") # Our dataset
map_data <- st_read("map.geojson") # Our geospatial data

average_price <- Tokyo %>%
  group_by(Municipality) %>%
  summarise(average_price = mean(TradePrice)) # For Map 1

average_area <- Tokyo %>%
  group_by(Municipality) %>%
  summarise(average_area = mean(Area)) # For Map 2

average_year <- Tokyo %>%
  group_by(Municipality) %>%
  summarise(average_year = mean(Year)) # For Map 3 

map_data <- map_data %>%
  left_join(average_price, by = "Municipality") # For Map 1

map_data <- map_data %>%
  left_join(average_area, by = "Municipality") # For Map 2

map_data <- map_data %>%
  left_join(average_year, by = "Municipality") # For Map 3 

ui <- dashboardPage(skin = "red",                                               
                    dashboardHeader(title = strong("Tokyo Real Estate"), titleWidth = 250),                        
                    dashboardSidebar(
                      sidebarMenu(
                        menuItem("Click for Inputs", tabName = "inputvalues", icon = icon("keyboard", lib = "font-awesome"),
                                 width = 300,
                                 sliderInput("price", h4("Price Range"),              
                                             min = 0, max = 500000000,                      
                                             value = c(0, 500000000), step = 1000000),
                                 sliderInput("year", h4("Year"),                
                                             min = 2005, max = 2019,
                                             value = c(2005, 2019), step = 1),
                                 sliderInput("area", h4("Area"),
                                             min = 0, max = 2500,
                                             value = c(0, 2500), step = 10),
                                 checkboxGroupInput("region", h4("Region"),
                                                    choices = c("Commercial Area", "Residential Area", 
                                                                "Industrial Area", "Potential Residential Area")),
                                 selectInput("municipality", h4("Municipality"),
                                             choices = Tokyo$Municipality |> unique(),
                                             selectize = TRUE, multiple = TRUE)
                        )
                      )
                    ),        
                    dashboardBody(
                      tabBox(
                        title = strong("Results"), width = 12,
                        tabPanel(strong("How to Use"),
                                 h4("Operating Instructions:"),
                                 p("1. Use the 'Click for Inputs' section in the sidebar to select your desired filters for the real estate data."),
                                 p("2. Adjust the 'Price Range' slider to filter properties by their trade price."),
                                 p("3. Use the 'Year' slider to select the range of years for the properties you're interested in."),
                                 p("4. Adjust the 'Area' slider to filter properties based on their size."),
                                 p("5. Select one or more regions from the 'Region' checkboxes to filter properties by their area type."),
                                 p("6. Use the 'Municipality' dropdown to select specific municipalities you are interested in."),
                                 p("7. Navigate through the tabs in the main panel to view different visualizations and summaries based on your selected filters."),
                                 tags$br(),
                                 h4("Interactivity:"),
                                 p("The application updates automatically based on the filters you set."),
                                 p("The 'Real Estate Statistics', 'Property List', and 'Short Summary' tabs will reflect the data that matches your criteria."),
                                 p("The data table can be sorted and searched through for specific entries.")
                        ),
                        tabPanel(strong("Tokyo Map"), 
                                 h3("Map Information:"),
                                 p("This sector is not interactive and presents an overall information of the Tokyo Real Estate Market for the past 15 years."),
                                 p("Initially, the map shows half of Japan and some islands - because Tokyo is quite big ! It is not only inside the Honshu Island - the main & biggest island of Japan, but also small islands further away."),
                                 p("You can zoom freely to observe everything you want."),
                                 p("You will see three maps - average price in Yen, average area size in square meters, and average year of listings - all per municipality. "),
                                 tags$br(),
                                 h4("Tokyo Map - Average Price in Yen"),
                                 leafletOutput("mymap", height = 800),
                                 h4("Tokyo Map - Average Size in Square Meters"),
                                 leafletOutput("mymap2", height = 800),
                                 h4("Tokyo Map - Average Year of Listings"),
                                 leafletOutput("mymap3", height = 800)
                        ),
                        tabPanel(strong("Real Estate Statistics"), 
                                 plotOutput("plot", height = 500),
                                 plotOutput("areaplot", height = 500),
                        ),
                        tabPanel(strong("Property List"), DT::dataTableOutput("table")
                        ),
                        tabPanel(strong("Short Summary"),
                                 valueBoxOutput("box1", width = 6),
                                 valueBoxOutput("box2", width = 6),
                                 valueBoxOutput("box3", width = 6),
                                 valueBoxOutput("box4", width = 6)
                        )
                      )
                    )
)

server <- function(input, output){                                     
  data <- reactive(
    Tokyo %>% filter(TradePrice >= input$price[1],
                     TradePrice <= input$price[2],
                     Year >= input$year[1],
                     Year <= input$year[2],
                     Area >= input$area[1],
                     Area <= input$area[2],
                     Region %in% input$region,
                     Municipality %in% input$municipality)
  )
  output$table <- DT::renderDataTable(data())         
  
  output$plot <- renderPlot({
    data() %>% 
      group_by(Year, Municipality) %>%
      summarize(AveragePrice = mean(TradePrice)) %>%
      ggplot(aes(x = Year, y = AveragePrice, color = Municipality)) + 
      geom_line(size = 1) + geom_point() +
      theme_minimal() +
      labs(title = "Average Real Estate Price Per Year by Municipality",
           x = "Year", y = "Average Price", color = "Municipality") +
      coord_cartesian(ylim = c(0, 450000000))
  })
  
  output$areaplot <- renderPlot({
    data() %>% 
      group_by(Year, Municipality) %>%
      summarize(AverageArea = mean(Area)) %>%
      ggplot(aes(x = Year, y = AverageArea, color = Municipality)) + 
      geom_line(size = 1) + geom_point() +
      theme_minimal() +
      labs(title = "Average Real Estate Area Per Year by Municipality",
           x = "Year", y = "Average Area", color = "Municipality") 
  })
  
  # For the map part, it was a bit tough, had to use a combination of Google / chatgpt (aka GenAI), and your slides from S8)
  output$mymap <- renderLeaflet({
    pal <- colorNumeric(palette = "YlOrRd", domain = map_data$average_price)
    leaflet(map_data) %>%
      addTiles() %>%
      addPolygons(fillColor = ~pal(average_price),
                  weight = 2,
                  opacity = 1,
                  color = 'white',
                  dashArray = '3',
                  fillOpacity = 0.7,
                  highlightOptions = highlightOptions(weight = 3,
                                                      color = "#666",
                                                      dashArray = '',
                                                      fillOpacity = 0.7,
                                                      bringToFront = TRUE),
                  label = ~paste0(Municipality, ": ", round(average_price, 2)),
                  labelOptions = labelOptions(direction = 'auto')) %>%
      addLegend("bottomright", pal = pal, values = ~average_price,
                title = "Average Price",
                opacity = 1)
  })
  
  output$mymap2 <- renderLeaflet({
    pal <- colorNumeric(palette = "YlOrRd", domain = map_data$average_area)
    leaflet(map_data) %>%
      addTiles() %>%
      addPolygons(fillColor = ~pal(average_area),
                  weight = 2,
                  opacity = 1,
                  color = 'white',
                  dashArray = '3',
                  fillOpacity = 0.7,
                  highlightOptions = highlightOptions(weight = 3,
                                                      color = "#666",
                                                      dashArray = '',
                                                      fillOpacity = 0.7,
                                                      bringToFront = TRUE),
                  label = ~paste0(Municipality, ": ", round(average_area, 2)),
                  labelOptions = labelOptions(direction = 'auto')) %>%
      addLegend("bottomright", pal = pal, values = ~average_area,
                title = "Average Area",
                opacity = 1)
  })
  
  output$mymap3 <- renderLeaflet({
    pal <- colorNumeric(palette = "YlOrRd", domain = map_data$average_year)
    leaflet(map_data) %>%
      addTiles() %>%
      addPolygons(fillColor = ~pal(average_year),
                  weight = 2,
                  opacity = 1,
                  color = 'white',
                  dashArray = '3',
                  fillOpacity = 0.7,
                  highlightOptions = highlightOptions(weight = 3,
                                                      color = "#666",
                                                      dashArray = '',
                                                      fillOpacity = 0.7,
                                                      bringToFront = TRUE),
                  label = ~paste0(Municipality, ": ", round(average_year)),
                  labelOptions = labelOptions(direction = 'auto')) %>%
      addLegend("bottomright", pal = pal, values = ~average_year,
                title = "Average Year",
                opacity = 1)
  })
  
  output$box1 <- renderValueBox({
    valueBox(
      value = nrow(data()), 
      subtitle =  "Total Properties", 
      icon = icon("building", lib = "font-awesome"),
      color = "green"
    )
  })
  
  output$box2 <- renderValueBox({
    avg_area <- round(mean(data()$Area), 1)
    valueBox(
      value = avg_area, 
      subtitle = "Average Area (sqm)", 
      icon = icon("th-large", lib = "font-awesome"),
      color = "red"
    )
  })
  
  output$box3 <- renderValueBox({
    avg_price <- round(mean(data()$TradePrice), 1)
    valueBox(
      value = avg_price, 
      subtitle = "Average Price (Yen)", 
      icon = icon("yen-sign", lib = "font-awesome"),
      color = "blue"
    )
  })
  
  output$box4 <- renderValueBox({
    avg_year <- round(mean(data()$Year))
    valueBox(
      value = avg_year, 
      subtitle = "Average Year", 
      icon = icon("calendar-alt", lib = "font-awesome"),
      color = "maroon"
    )
  })
}
shinyApp(ui = ui, server = server)