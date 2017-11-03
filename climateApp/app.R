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

# Define UI for application that draws a histogram
ui <- fluidPage(
   
   # Application title
   titlePanel("Temperatures per country"),
   
   # Sidebar with a slider input for number of bins 
   sidebarLayout(
      sidebarPanel(
        sliderInput("selectYear",
                    "Year selected:",
                    min = 1900,
                    max = 2012,
                    value = 1900)
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

