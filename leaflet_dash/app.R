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


#### Importing libraries ####
library(shiny)
library(shinydashboard)
library(yaml)  # Used to securely access API token
library(dplyr)
library(stringr)
library(ggplot2)
library(plotly)
library(DT)
library(leaflet)
library(sf)
library(shiny)
library(shinyWidgets)

#### Load Data ####
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

# Remove trailing characters from violation entity column
vct.abdn.load <- vct.abdn.load %>%
  mutate(entity_or_person_s_ = str_sub(
    entity_or_person_s_, end = -3
  ))

# Remove points that don't have an associated location
vct.abdn.load <- vct.abdn.load %>%
  filter(!is.na(latitude) & !is.na(longitude))

# Join neighborhood data to vct.abdn.load
vct.abdn <- st_join(chi.nbrhd, vct.abdn.load, join = st_within)

#### Define UI ####
ui <- dashboardPage(
  
  # Set title
  dashboardHeader(
    title = "Vacant and Abandoned Building Violations in Chicago",
    titleWidth=600
  ),
  
  # Define sidebar
  dashboardSidebar(
    sidebarMenu(
      
      # Set tab titles
      menuItem("Violation Map", tabName="map"),
      menuItem("Data Visualizations", tabName="viz"),
      menuItem("Data table", tabName="table"),
      
      br(),  # Visual spacer
      
      ## Set filter inputs ##
      # Select date range
      dateRangeInput(
        "date.range", "Violation Date Range",
        start = Sys.Date() - 365, end = Sys.Date(),
        min = "2001-01-01", max = Sys.Date()
      ),
      
      # Select neighborhoods
      pickerInput(
        "nbrhds", "Neighborhoods",
        choices = chi.nbrhd %>%
          distinct(pri_neigh) %>%
          arrange(pri_neigh),
        multiple = TRUE,
        selected = "Loop"
      ),
      
      # Include only violations with outstanding fines?
      materialSwitch(
        "fines", "Only include violations with outstanding fines?",
        status = "Outstanding fines only"
      )
    )
  ),
  
  # Define body
  dashboardBody(
    tabItems(
      
      # Leaflet map content
      tabItem(
        tabName="map",
              
        # Full-width leaflet box
        fluidRow(
          box(
            width = 12,
            leafletOutput("leaflet")
          )
        )
      ),
      
      # Data visualization tab content
      tabItem(
        tabName="viz",
          
        # Complaints by neighborhood over time plot
        fluidRow(
          box(
            width=12,
            title="Complaints by Neighborhood Over Time in Selected Data",
            plotlyOutput("complaint.plotly")
          )
        ),
        
        # Most fined entities bar chart
        fluidRow(
          box(
            width=12,
            title="Top 10 Most Fined Entities in Selected Data",
            plotlyOutput("fines.plotly")
          )
        )
      ),
      
      # Data table tab content
      tabItem(
        tabName="table",
        
        # Download button
        downloadButton("download", "Download"),
        
        # Data table
        fluidRow(
          box(
            width=12,
            title="Filtered Vacant/Abandoned Violation Data",
            DTOutput("data")
          )
        )
      )
    )
  )
)

#### Define server logic ####
server <- function(input, output) {
  
  # Reactive data function
  vct.abdn <- reactive({
    
    # Require non-null neighborhood selection
    req(input$nbrhds)
    
    # Filter according to selections
    vct.abdn.subset <- vct.abdn.load %>%
      filter(
        
      )
  })
}

# Run the application 
shinyApp(ui = ui, server = server)
