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
library(lubridate)
library(ggplot2)
library(plotly)
library(DT)
library(leaflet)
library(sp)
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
vct.abdn <- st_join(vct.abdn.load, chi.nbrhd, join = st_within)

#### Define UI ####
ui <- dashboardPage(
  
  # Set title
  dashboardHeader(
    title = "Vacant and Abandoned Building Violations in Chicago",
    titleWidth=550
  ),
  
  # Define sidebar
  dashboardSidebar(
    sidebarMenu(
      
      # Set tab titles
      menuItem("Violation Map", tabName="map"),
      menuItem("Data Visualizations", tabName="viz"),
      menuItem("Data table", tabName="table"),
      
      br(), br(), br(), br(),  # Visual spacer
      
      ## Set filter inputs ##
      # Select date range
      dateRangeInput(
        "date.range", "Violation Date Range",
        start = Sys.Date() - (365 * 5), end = Sys.Date(),
        min = as_date("2001-01-01"), max = Sys.Date()
      ),
      
      # Select neighborhoods
      pickerInput(
        "nbrhds", "Neighborhoods",
        choices = chi.nbrhd %>%
          distinct(pri_neigh) %>%
          arrange(pri_neigh),
        selected = c("Loop","Humboldt Park","New City","Lake View"),
        options = list(
          `actions-box` = TRUE,
          size = 5,
          `live-search` = TRUE
        ),
        multiple = TRUE
      ),
      
      # Include only violations with outstanding fines?
      switchInput(
        "fines", "Display only points with fines due?",
        onLabel = "Yes", offLabel = "No", labelWidth = "150px"
        
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
            title="Violations by Neighborhood Over Time in Selected Data",
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
        
        # Visual spacer
        br(), br(),
        
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
  vct.abdn.input <- reactive({
    
    # Filter according to selections
    vct.abdn.subset <- vct.abdn %>%
      filter(
        issued_date >= as_date(input$date.range[1]) &
        issued_date <= as_date(input$date.range[2]) &
        pri_neigh %in% input$nbrhds
      )
    
    # Filter for only violations with outstanding fines if switch is on
    if (input$fines) {
      vct.abdn.subset <- vct.abdn.subset %>%
        filter(
          current_amount_due > 0
        )
    }
    
    return(vct.abdn.subset)
  })
  
  ## Leaflet tab ##
  # Render tiles
  output$leaflet <- renderLeaflet({
    leaflet() %>%
      addTiles(
        urlTemplate = "http://mt0.google.com/vt/lyrs=m&hl=en&x={x}&y={y}&z={z}&s=Ga",
        attribution = "Google", group = "Map"
      ) %>%
      addProviderTiles(
        provider = providers$Esri.WorldImagery, group = "Satellite"
      ) %>%
      setView(-87.679365, 41.840675, 10) %>%
      addLayersControl(baseGroups = c("Map", "Satellite"))
  })
  
  # Filtered violation layer observer
  observe({
    
    # Define data as reactive input
    violations <- vct.abdn.input()
    
    # Use Leaflet Proxy to add violation locations to the map
    leafletProxy("leaflet", data = violations) %>%
      clearMarkers() %>%
      addMarkers(
        popup = ~paste0(
          "Docket Number: ", docket_number, "<br>",
          "Issue Date: ", format(issued_date, "%m/%d/%Y"), "<br>",
          "Address: ", property_address, "<br>",
          "Violation: ", violation_type
        )
      )
  })
  
  # Reactive neighborhood filter
  nbrhd.input <- reactive({
    nbrhd <- chi.nbrhd %>%
      filter(pri_neigh %in% input$nbrhds)
    return(nbrhd)
  })
  
  # Filtered neighborhood polygon observer
  observe({
    
    # Define data as reactive input
    nbrhd <- nbrhd.input()
    
    # Add polygon outlines to map
    leafletProxy("leaflet", data = nbrhd) %>%
      clearShapes() %>%
      addPolygons(fill=F, color="blue")
  })
  
  ## Visualization tab ##
  # Total complaints by neighborhood over time
  output$complaint.plotly <- renderPlotly({
    ggplotly(
      ggplot(
        vct.abdn.input() %>%
          mutate(Month = as_date(format(issued_date, "%Y-%m-01"))) %>%
          group_by(Month, pri_neigh) %>%
          summarize(Count = n()) %>%
          rename(Neighborhood = pri_neigh),
        aes(x = Month, y = Count, fill = Neighborhood)
      ) +
        geom_bar(stat="summary", fun="sum") +
        theme_classic() +
        xlab("Month") + ylab("Total violations issued"),
      tooltip = c("Month","Count","Neighborhood")
    )
  })
}

# Run the application 
shinyApp(ui = ui, server = server)
