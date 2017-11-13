library(shiny)
library(googleVis)
library(tidyverse)
library(dygraphs)

tempsByCountry <- read.csv2("tempsByCountry.csv")

source("datagenerators.R")

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
    tabPanel("Temperatures and CO2",
      titlePanel("Temperatures and CO2 concentration"),       
      sidebarLayout(
        sidebarPanel(

          h4("Method"),
          p("The average temperature was calculated with a 10-year moving average."),
          p("As for the prediction: the CO2 concentration follows a linear progression on the period 1980-2014, so this was used as a
            basis to extrapole values for the period 2015-2030,"),
          p("During the same period, we observed a .996 correlation between the temperature and CO2 concentration,
                    so we express the temperature for the period 2015-2030 as a linear function of the CO2 concentration."),
          
          # Indicating the data source
          br(),
          h4("Data source"),
          p("Berkeley Earth,",
            a("Climate Change: Earth Surface Temperature Data",
              href = "https://www.kaggle.com/berkeleyearth/climate-change-earth-surface-temperature-data")),
          p("IAC Switzerland,",
            a("Global CO2 Yearly", href = "https://www.co2.earth/historical-co2-datasets"))
        ),
               
      mainPanel(
        checkboxInput(
          inputId = "selectPrediction",
          label = "Include prediction",
          value = TRUE),
        
        dygraphOutput("overviewGraph"),
        textOutput("correlationText")
        )
      )
    )
  )
)



server <- function(input, output) {
  
  output$countryGraph <- renderDygraph({
    x <- tempsByCountry %>% filter(Country == input$selectCountry) %>%
      select("Year", "TenYearAvg", "AverageTemperature")
    dygraph(x,
            main = paste("Temperatures in", input$selectCountry, "between 1900 and 2012"),
            xlab = "Year",
            ylab = "Temperatures (°C)") %>%
      dySeries("TenYearAvg",
               label = "Moving Average (°C)",
               color = "red",
               strokeWidth = 3) %>%
      dySeries("AverageTemperature",
               label = "Yearly Average (°C)",
               color = "gray",
               strokeWidth = 0.5) %>%
      dyLegend(width = 500)
  })  
  
  
  output$overviewGraph <- renderDygraph({
    
    if(input$selectPrediction == FALSE){
      x <- genOverview()
    } else {
      x <- rbind(genOverview(), genOverviewPrediction())
    }
    
    dygraphPlot <- dygraph(x %>% select(-LandAverageTemperature),
            main = "World Temperatures and CO2 Concentration",
            xlab = "Year",
            ylab = "Temperatures (°C)") %>%
      dyAxis("y", label = "Temperatures", valueRange = c(7, 10.3)) %>%
      dyAxis("y2", label = "CO2 Concentration (ppm)", valueRange = c(220, 451)) %>%
      dySeries("TenYearAvg",
               label = "Average Temp. (°C)",
               color = "red",
               strokeWidth = 3) %>%
      dySeries("CO2",
               label = "CO2 conc. (ppm)",
               color = "black",
               strokeWidth = 3,
               axis = ("y2")) %>%
      dyLegend(width = 460)
    
    if(input$selectPrediction == TRUE){
      dygraphPlot %>% dyShading(from = "2015", to = "2030", color = "#F3F781")
    } else {
      dygraphPlot
    }
  })
  
  
  output$correlationText <- renderText({
    x <- genOverview()
    correlation <- cor(x$TenYearAvg, x$CO2, use = "na.or.complete")
    paste("Correlation between the evolution of the temperature and the CO2 in the atmosphere:",
                  round(correlation,3))
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

