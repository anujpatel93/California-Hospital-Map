# California-Hospital-Map
R Shiny search tool of hospital facilities in California

### Data Sources:
* “Licensed and Certified Healthcare Facility Listing” (https://data.chhs.ca.gov/dataset/healthcare-facility-locations)
* “US ZIP Codes Database” (https://simplemaps.com/data/us-zips)

A user inputs a California ZIP code to map healthcare facilities around that location within a user defined search radius with additional filtering by facility type, long-term care availability, and birthing service availability available. Clicking on mapped facilities results in a popup that displays the facility’s name, address, and the distance from the ZIP code and the “Summary Statistics” tab provides the user with statistics and plots about the mapped facilities. The color of the facility on the map indicates whether the facility has inpatient capacity for their services.
