#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#
######################################################################################
# 
# CALIFORNIA HEALTHCARE FACILITY SEARCH TOOL
# Author: Anuj Patel
# Date: 10/29/2018
# 
# Data Sources:
# - California "Licensed and Certified Healthcare Facility Listing" sourced from
#   https://data.chhs.ca.gov/dataset/healthcare-facility-locations
#
# - ZIP Codes Geocoded Database sourced from https://simplemaps.com/data/us-zips

library(shiny)
library(leaflet)
library(shinythemes)
library(dplyr)
library(fields)
library(ggplot2)

# Define function to return zoom level based on search radius
radius2zoom <- function(radius) {
    zoom <- 8
    if (radius <= 6) {
        zoom <- 12
    }
    else if (radius <= 10) {
        zoom <- 11
    }
    else if (radius <= 23) {
        zoom <- 10
    }
    else if (radius <= 47) {
        zoom <- 9
    }
    
    return(zoom)
}

# Get hospital and zip code location data frames
hospitals <- read.csv("https://data.chhs.ca.gov/dataset/3b5b80e8-6b8d-4715-b3c0-2699af6e72e5/resource/0a0476ba-442c-40ff-97dc-dc840fa7e907/download/healthcare_facility_locations.csv")
zips <- read.csv("https://simplemaps.com/static/data/us-zips/1.4/uszipsv1.4.csv")

# Clean hospitals data frame to remove entries with blank coordinates and create column to store if hospital has inpatient capacity
hospitals <- filter(hospitals, (LATITUDE != '' | LONGITUDE != ''))
hospitals <- mutate(hospitals, HAS_INPAT_CAP = CAPACITY > 0)

# Filter zips data frame to only California zip codes
zips <- filter(zips, state_id == 'CA')

# Define UI for project
ui <- fluidPage(
    
    # Change theme to lumen for aesthetic purposes
    theme = shinytheme("lumen"),
    
    # Application title
    titlePanel("California Healthcare Facility Search Tool"),
    
    # Sidebar for uploading the file and setting search filters
    sidebarLayout(
        sidebarPanel(
            # Input clarifying text
            helpText("Input a California ZIP code and search radius:"),
            
            # Create a text input for the searched ZIP code
            textInput("zipInput", label = "Insert the 5-digit ZIP code:"),
            
            # Create a slider to set the search radius
            sliderInput("searchRadius", label = "Radius of search (miles):", min = 0, max = 100, value = 20),
            
            # Input clarifying text
            helpText("Edit the search options below with the settings you wish to explore:"),
            
            # Create a check box to enable filtering by facility type
            checkboxInput("filterFacilityType", label = "Filter by type of facility"),
            
            # Create a drop-down menu to select facility type to filter by
            selectInput("facilityType", label = "Select the type of facility:", 
                        choices = levels(hospitals$FAC_FDR)),
            
            #Create a check box group to enable filtering by long-term care or birthing services availability
            checkboxGroupInput("additionalFilters", label = ("Additional search filters:"), 
                               choices = list("Long-Term Care Facilities only" = "LONGTERM",
                                              "Birthing Service Provider Facilities only" = "BIRTHING"))
        ),
        
        # Locate the sidebar on the right of the webpage
        position = "right",
        fluid = TRUE,
        
        # Create the main panel to contain the project description, map, and statistical information
        mainPanel(
            hr(),
            tabsetPanel(type="tabs",
                        #Add a tab for the problem description
                        tabPanel("Project Description", htmlOutput("projDescription")),
                        
                        #Tab for the Leaflet Map
                        tabPanel("Map", leafletOutput("map", height=630)),
                        
                        #Add a tab for summary stats and plots
                        tabPanel("Summary Statistics and Plot",
                                 #Add subtabs - PLOT SUBTAB IS COMMENTED OUT FOR NOW
                                 tabsetPanel(
                                     tabPanel("Summary",verbatimTextOutput("dataSummary"))#,
                                     #tabPanel("Inpatient Capacity vs. Distance Plot", plotOutput("plot"))
                                 )
                        )
            )
        )
    )
)

# Define server logic required to output required text and visuals
server <- function(input, output) {
    
    # Create an output variable for the tool's description
    output$projDescription <- renderText({
        
        "<p>This project employs the use of the California's \"Licensed and Certified Healthcare Facility Listing.\"
        This dataset lists operational California healthcare facilities with either a license from California's
        Department of Public Health (CDPH) or a certification from the US Department of Health and Human Services'
        (HHS) Centers for Medicare and Medicaid Services (CMS). It also contains various attributes about each
        facility, including geocoded location, facility type, facility inpatient capacity, and contact information.</p>
        
        <p>This tool allows a user to input a California ZIP code and find healthcare facilities around that location.
        Among its many uses, this tool can be used by patients looking for facilities near their residence at which
        to seek care, by hospital administrators looking to find potential locations for new facilities, or by
        policy researchers looking for gaps in medical care for underserved regions and communities.</p>
        
        If you do not know California ZIP codes, here are some notable locations and their ZIP code:<br>
        Hollywood Sign, Los Angeles: 90068<br>
        Disneyland, Anaheim: 92802<br>
        SeaWorld, San Diego: 92109<br>
        Golden Gate Bridge, San Francisco: 94129<br>
        Apple Headquarters, Cupertino: 95014<br>"
        
    })
    
    # Wrap the data processing steps and the outputs that depend on that process in an observer
    observe({
        # If valid ZIP code hasn't been input, print request to upload a valid one
        if(!(input$zipInput %in% zips$zip)){
            output$dataSummary <- renderPrint({
                print("Input valid California ZIP code for analysis!")
            })
            output$facilityList <- renderPrint({
                print("Input valid California ZIP code for analysis!")
            })
            
            # Output map to view all of California if appropriate California ZIP code isn't provided
            output$map <- renderLeaflet({

                # Create the leaflet function
                leaflet() %>%

                    # Set the default view to be centered on the center coordinates of the state of California
                    setView(lng = -119.449444, lat = 37.166111, zoom = 6) %>%

                    # Provide tiles
                    addProviderTiles("CartoDB.Positron",
                                     options = providerTileOptions(noWrap = TRUE))
            })
            
            return(NULL)
        }
        
        # Obtain the coordinates for the input ZIP code and calculate the distance between that location and all hospitals
        inputLocation <- filter(zips, zip == input$zipInput)
        distance <- rdist.earth.vec(select(hospitals, c("LONGITUDE", "LATITUDE")), select(inputLocation, c("lng", "lat")))
        hospitals <- cbind(hospitals, "DIST" = distance)
        
        # Filter to hospitals within the search radius, of the selected hospital type, and fitting the additional filters
        hospitals <- filter(hospitals, DIST <= input$searchRadius)
        if(input$filterFacilityType) {
            hospitals <- filter(hospitals, FAC_FDR == input$facilityType)
        }
        if("LONGTERM" %in% input$additionalFilters) {
            hospitals <- filter(hospitals, LTC == "LTC")
        }
        if("BIRTHING" %in% input$additionalFilters) {
            hospitals <- filter(hospitals, BIRTHING_FACILITY_FLAG == "YES")
        }
        
        # Create color palette to display whether facility has inpatient capacity
        palette <- colorFactor(c("#f92c32", "#127cd4"), hospitals$HAS_INPAT_CAP)

        # Output map to view hospitals fitting the search filters
        output$map <- renderLeaflet({
            
            # Create the leaflet function
            leaflet(hospitals) %>%
                
                # Center the map to the input ZIP code location and use the search radius to determine the zoom level
                setView(lng = inputLocation$lng, lat = inputLocation$lat, zoom = radius2zoom(input$searchRadius)) %>%
                
                # Provide tiles
                addProviderTiles("CartoDB.Positron", 
                                 options = providerTileOptions(noWrap = TRUE)) %>%
                
                # Add a circle with a black outline to denote the search radius
                addCircles(
                    lng = inputLocation$lng,
                    lat = inputLocation$lat,
                    radius = 1609.344 * input$searchRadius, #convert search radius from miles to meters
                    weight = 3,
                    color = "#000",
                    fillOpacity = 0) %>%
                
                # Add circle markers for each filtered facility and show their names, addresses, and distances as popups
                addCircleMarkers(
                    radius = 4,
                    lng = hospitals$LONGITUDE,
                    lat = hospitals$LATITUDE,
                    stroke= FALSE,
                    color = palette(hospitals$HAS_INPAT_CAP),
                    fillOpacity=0.8,
                    popup = paste(hospitals$FACNAME, "<br>", hospitals$ADDRESS, "<br>",
                                  hospitals$CITY, ", CA<br>", round(hospitals$DIST, 2), " miles")) %>%
                
                # Add legend for the colors representing whether facilities have inpatient capacity
                addLegend(
                    "bottomleft",
                    pal=palette,
                    values=hospitals$HAS_INPAT_CAP,
                    opacity=0.5,
                    title="Facility has inpatient capacity?"
                )
        })
        
        # Obtain summary statistics and display them
        output$dataSummary <- renderPrint({
            stat <- c("Number of Facilities", "Mean Distance", "Facilities with Inpatient Capacity", "Total Inpatient Capacity", "Mean Inpatient Capacity")
            values <- c(length(hospitals$FACID), mean(hospitals$DIST), sum(hospitals$HAS_INPAT_CAP), sum(hospitals$CAPACITY), mean(hospitals$CAPACITY))
            data.frame(stat, values)
        })
        
        
        # Display plot of some kind - UNCOMMENT PLOT TAB ON UI SIDE IF PLOTTING SOMETHING
        # output$plot <- renderPlot({
        #     
        # })
    })
}

# Run the application 
shinyApp(ui = ui, server = server)

