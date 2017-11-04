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

ui <- fluidPage(
   
   titlePanel("Temperatures per country"),
   
   sidebarLayout(
      sidebarPanel(
        
        # Slider for the year selected
        sliderInput("selectYear",
                    "Year selected:",
                    min = 1900,
                    max = 2012,
                    value = 1900,
                    sep = ""),
        
        # Scrolling menu to choose a type of map
        selectInput(
          inputId = "selectGraph",
          label = "Choose: ",
          choices = c("Map Temperature" = 1, "Temperature Difference" = 2)),
        
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
         htmlOutput("distPlot")
      )
   )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
  
  output$distPlot <- renderGvis({
    x <- tempsByCountry %>% filter(year == input$selectYear)
    GeoStates <- gvisGeoChart(x, locationvar = "Country",
                 colorvar = "TenYearAvg",
                 options = list(
                   height = 500,
                   width = 750,
                   keepAspectRatio = FALSE,
                   backgroundColor = "#FFFFFF",
                   defaultColor = "white",
                   colorAxis ="{
                   values:[-20, 30],
                   colors:['blue', 'red']}"))

  })

     
#   output$distPlot <- renderPlot({
#      # generate bins based on input$bins from ui.R
#      x    <- (tempsByCountry %>% filter(year == input$selectYear))[, 4]
#      
#      # draw the histogram with the specified number of bins
#      hist(x[[1]], col = 'darkgray', border = 'white',
#           xlab = "Temperatures", main = "Number of countries per temperature")
#   })
}

# Run the application 
shinyApp(ui = ui, server = server)

