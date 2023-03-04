#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#
#
# CITY OF CHICAGO VACANT AND ABANDONED BUILDINGS DASHBOARD
# Author: Jack Jacobs
# Date: 4 March 2023


# Importing libraries
library(shiny)
library(shinydashboard)
library(yaml)
library(dplyr)
library(ggplot2)
library(plotly)
library(DT)
library(leaflet)
library(sf)
library(leaflet.extras)

# Load Chicago neighborhood polygons
## Source: https://data.cityofchicago.org/Facilities-Geographic-Boundaries/Boundaries-Neighborhoods/bbvz-uum9
chi.nbrhd <- st_read("Boundaries - Neighborhoods.geojson")

# Load data from vacant and abandoned building API
## URL: https://data.cityofchicago.org/Buildings/Vacant-and-Abandoned-Buildings-Violations/kc9i-wq85
tkn <- read_yaml("auth.yaml")$app_token  # Secretly load API app token
url <- paste0(  # Insert API token into request URL
  "https://data.cityofchicago.org/resource/kc9i-wq85.geojson?$$app_token=",
  tkn, "&$limit=999999"
)
vct.abdn.load <- st_read(url)  # Load data from API URL

# Define UI for application that draws a histogram
ui <- fluidPage(

    # Application title
    titlePanel("Old Faithful Geyser Data"),

    # Sidebar with a slider input for number of bins 
    sidebarLayout(
        sidebarPanel(
            sliderInput("bins",
                        "Number of bins:",
                        min = 1,
                        max = 50,
                        value = 30)
        ),

        # Show a plot of the generated distribution
        mainPanel(
           plotOutput("distPlot")
        )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output) {

    output$distPlot <- renderPlot({
        # generate bins based on input$bins from ui.R
        x    <- faithful[, 2]
        bins <- seq(min(x), max(x), length.out = input$bins + 1)

        # draw the histogram with the specified number of bins
        hist(x, breaks = bins, col = 'darkgray', border = 'white')
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
