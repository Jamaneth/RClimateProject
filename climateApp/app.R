#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(googleVis)
library(tidyverse)
library(dygraphs)

tempsByCountry <- read.csv2("tempsByCountry.csv")

ui <- fluidPage(
  
  navbarPage("Navigation",
             
    tabPanel("Overview",
      mainPanel(
        dygraphOutput("overviewGraph")
      )
    ),
    
    tabPanel("Graph per country",
      titlePanel("Temperature graph per country"),       
      sidebarLayout(
        sidebarPanel(
          selectInput(
            inputId = "selectCountry",
            label = "Choose country:",
            choices = unique(as.vector(tempsByCountry$Country))
          )
        ),
      
      mainPanel(
        dygraphOutput("countryGraph")
        )
      )
    ),
             
    tabPanel("World map",
             
      titlePanel("Map temperatures (°C)"),
             
       sidebarLayout(
         sidebarPanel(
                 
         # Slider for the year selected
           sliderInput("selectYear",
                       "Year selected:",
                       min = 1900,
                       max = 2012,
                       value = 1900,
                       sep = "",
                       animate =
                         animationOptions(interval = 2000)),
                   
         # Scrolling menu to choose a type of map
           selectInput(
            inputId = "selectGraph",
             label = "Choose: ",
             choices = c("Average Temperature per Country" = 1, "Difference With Average" = 2)),
                 
         # Indicating the data source
           br(),
           h4("Data source"),
           p("Berkeley Earth,",
           a("Climate Change: Earth Surface Temperature Data",
             href = "https://www.kaggle.com/berkeleyearth/climate-change-earth-surface-temperature-data"),
           "(includes data on global temperatures since 1750)")
                 
                 
          ),
               
       # Show a plot of the generated distribution
         mainPanel(
           textOutput("graphTitle"),
           htmlOutput("distPlot")
         )
       )
    )
  )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
  
  output$countryGraph <- renderDygraph({
    x <- tempsByCountry %>% filter(Country == input$selectCountry) %>%
      select("Year", "TenYearAvg", "AverageTemperature")
    dygraph(x,
            main = paste("Temperatures in", input$selectCountry, "between 1900 and 2012"),
            xlab = "Year",
            ylab = "Temperatures (°C)") %>%
      dySeries("TenYearAvg",
               label = "Moving Average",
               color = "red",
               strokeWidth = 3) %>%
      dySeries("AverageTemperature",
               label = "Yearly Average",
               color = "gray",
               strokeWidth = 0.5)
  })  
  
  output$overviewGraph <- renderDygraph({
    x <- globalTemps
    dygraph(x,
            main = "World Temperatures Between 1750 and 2012",
            xlab = "Year",
            ylab = "Temperatures (°C)") %>%
      dySeries("TenYearAvg",
               label = "Moving Average",
               color = "red",
               strokeWidth = 3) %>%
      dySeries("LandAverageTemperature",
               label = "Yearly Average",
               color = "gray",
               strokeWidth = 0.5)
  })
  
  
  output$distPlot <- renderGvis({
    x <- tempsByCountry %>% filter(Year == input$selectYear)
    
    if(input$selectGraph == 1) {
      selectColumn = "TenYearAvg"
      graphThingy = "{
        values:[-20, 0, 30],
        colors:['blue', 'white', 'red']}"
      
    } else if(input$selectGraph == 2) {
      selectColumn = "TempDiff"
      graphThingy = "{
        values:[-1.3, 0, 1.7],
        colors:['blue', 'white', 'red']}"      
    }
    
    GeoStates <- gvisGeoChart(x, locationvar = "Country",
                              colorvar = selectColumn,
                              options = list(
                                projection = "kavrayskiy-vii",
                                height = 400,
                                width = 750,
                                keepAspectRatio = FALSE,
                                backgroundColor = "white",
                                defaultColor = "gray",
                                colorAxis = graphThingy))
    })

}

# Run the application 
shinyApp(ui = ui, server = server)

