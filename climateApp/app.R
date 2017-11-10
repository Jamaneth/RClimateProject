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
    # First panel         
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
             choices = c("Average Temp. per Country" = 1, "Difference With Average" = 2)),
                 
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
           htmlOutput("distPlot"),
           textOutput("yearTemperature")
         )
       )
    ),
    # Second panel
    tabPanel("Graph per country",
      titlePanel("Temperature graph per country"),       
      sidebarLayout(
        sidebarPanel(
          selectInput(
            inputId = "selectCountry",
            label = "Choose country:",
            choices = unique(as.vector(tempsByCountry$Country))
          ),
                 
          # Adding a comment on the methodology
          br(),
          h4("Method"),
          tags$ul(
            tags$li("For each country, the yearly average was calculated by averaging
                    the average monthly temperature."),
            tags$li("The moving average was calculated on a basis of ten years,
                    so as to reduce the variability between years.")
          ),

          # Indicating the data source
          br(),
          h4("Data source"),
          p("Berkeley Earth,",
            a("Climate Change: Earth Surface Temperature Data",
              href = "https://www.kaggle.com/berkeleyearth/climate-change-earth-surface-temperature-data"),
            "(includes data on global temperatures since 1750)")
        ),
               
        mainPanel(
          dygraphOutput("countryGraph")
        )
      )
    ),
    
    # Third panel
    tabPanel("Overview",
      titlePanel("Temperature graph per country"),       
      sidebarLayout(
        sidebarPanel(
          selectInput(
            inputId = "selectCountry",
            label = "Choose country:",
            choices = unique(as.vector(tempsByCountry$Country))
            ),
                 
          # Adding a commentary on the methodology
          br(),
          h4("Method"),
          tags$ul(
            tags$li("For each country, the yearly average was calculated by averaging
                    the average monthly temperature."),
            tags$li("The moving average was calculated on a basis of ten years,
                    so as to reduce the variability between years.")
            ),
                 
          # Indicating the data source
          br(),
          h4("Data source"),
          p("Berkeley Earth,",
            a("Climate Change: Earth Surface Temperature Data",
              href = "https://www.kaggle.com/berkeleyearth/climate-change-earth-surface-temperature-data"),
            "(includes data on global temperatures since 1750)")
            ),
               
      mainPanel(
        dygraphOutput("overviewGraph"),
        dygraphOutput("carbonGraph")
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
    x <- genOverview()
    dygraph(x %>% select(-CO2),
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
  
  output$carbonGraph <- renderDygraph({
    x <- genCarbonDioxide()
    dygraph(x,
            main = "Carbon concentration in the atmosphere between 1750 and 2012",
            xlab = "Year",
            ylab = "ppm") %>%
      dySeries("CO2",
               label = "CO2",
               color = "black",
               strokeWidth = 3)
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
                                width = 700,
                                keepAspectRatio = FALSE,
                                backgroundColor = "white",
                                defaultColor = "gray",
                                colorAxis = graphThingy))
    })
  
  output$yearTemperature <- renderText({
    globalTemps <- genGlobalTemperatures()
    index <- which(globalTemps$Year == input$selectYear)
    paste("Average temperature in the world in", input$selectYear, ":",
          round(globalTemps$LandAverageTemperature[index], 2), "°C.")

  })

}

# Run the application 
shinyApp(ui = ui, server = server)

