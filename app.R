library(shiny)
library(shinydashboard)
library(ggplot2)
library(lubridate)
library(DT)
library(leaflet)
library(scales)
library(shinyjs)
library(tigris)
library(mapview)
library(reshape2)
library(crimedata)
library(leaflet.extras)

# using tigris cache
options(tigris_use_cache = TRUE)

# creating lists for data table labels
ls1 = list(
  list(title = 'Neighborhood'),
  list(title = 'Census Block ID'),
  list(title = 'Month'),
  list(title = 'Electricity Usage (KWH)'))

ls2 = list(
  list(title = 'Census Tract ID'),
  list(title = 'Month'),
  list(title = 'Electricity Usage (KWH)'))

ls3 = list(
  list(title = 'Neighborhood'),
  list(title = 'Census Block ID'),
  list(title = 'Month'),
  list(title = 'Gas Usage (thm)'))

ls4 = list(
  list(title = 'Census Tract ID'),
  list(title = 'Month'),
  list(title = 'Gas Usage (thm)'))
  
# obtaining power usage data 
chicagoData <- read.csv(file = "https://data.cityofchicago.org/api/views/8yq3-m6wp/rows.csv?accessType=DOWNLOAD", sep = ",", header = TRUE) 
# using tigris to get block data from 2010
chicago <- blocks(state = "IL", county = "Cook", year = 2010)
# creating a TRACT ID with crimedata library
chicagoData$TRACT.ID <- block_geoid_to(chicagoData$CENSUS.BLOCK, to = "tract")
# cook county tracts from tigris
cook_tracts <- tracts(state = "IL", county = "Cook", year = 2010)
# combine tract and block info that specifically belongs to Chicago
cook_tracts$GEOID10 <- as.numeric(cook_tracts$GEOID10)
chicago_tracts <- subset(cook_tracts, GEOID10 %in% chicagoData$TRACT.ID)
tract_all <- merge(chicago_tracts, chicagoData, by.x = "GEOID10", by.y = "TRACT.ID")
chicago$GEOID10 <- as.numeric (chicago$GEOID10)
chicago_blocks <- subset(chicago,GEOID10 %in% chicagoData$CENSUS.BLOCK )
chicago_all <- merge(chicago_blocks, chicagoData, by.x = "GEOID10", by.y = "CENSUS.BLOCK")

# combine rows by either summing or averaging them by census block
# electricity
elec_chi <- aggregate( cbind(KWH.JANUARY.2010, KWH.FEBRUARY.2010, KWH.MARCH.2010, KWH.APRIL.2010,
KWH.MAY.2010, KWH.JUNE.2010, KWH.JULY.2010, KWH.AUGUST.2010, KWH.SEPTEMBER.2010, KWH.OCTOBER.2010, 
KWH.NOVEMBER.2010, KWH.DECEMBER.2010, TOTAL.KWH) ~ COMMUNITY.AREA.NAME + CENSUS.BLOCK + BUILDING.TYPE, chicagoData, sum)
elecChi <- merge(chicago_blocks, elec_chi, by.x = "GEOID10", by.y = "CENSUS.BLOCK")
# gas
gas_chi <- aggregate( cbind(THERM.JANUARY.2010, THERM.FEBRUARY.2010, THERM.MARCH.2010, TERM.APRIL.2010,
                             THERM.MAY.2010, THERM.JUNE.2010, THERM.JULY.2010, THERM.AUGUST.2010, THERM.SEPTEMBER.2010, THERM.OCTOBER.2010, 
                             THERM.NOVEMBER.2010, THERM.DECEMBER.2010, TOTAL.THERMS) ~ COMMUNITY.AREA.NAME + CENSUS.BLOCK + BUILDING.TYPE, chicagoData, sum)
gasChi <- merge(chicago_blocks, gas_chi, by.x = "GEOID10", by.y = "CENSUS.BLOCK")
# population
pop_chi <- aggregate(TOTAL.POPULATION ~ COMMUNITY.AREA.NAME + CENSUS.BLOCK + BUILDING.TYPE, chicagoData, sum)
popChi <- merge(chicago_blocks, pop_chi, by.x = "GEOID10", by.y = "CENSUS.BLOCK")
# age
avgage_chi <- aggregate(AVERAGE.BUILDING.AGE ~ COMMUNITY.AREA.NAME + CENSUS.BLOCK + BUILDING.TYPE, chicagoData, mean)
ageChi <- merge(chicago_blocks, avgage_chi, by.x = "GEOID10", by.y = "CENSUS.BLOCK")
# height
avgheight_chi <- aggregate(AVERAGE.STORIES ~ COMMUNITY.AREA.NAME + CENSUS.BLOCK + BUILDING.TYPE, chicagoData, mean)
heightChi <- merge(chicago_blocks, avgheight_chi, by.x = "GEOID10", by.y = "CENSUS.BLOCK")

# combine rows by either summing or averaging them by census tract
# electricity
elec_tr <- aggregate( cbind(KWH.JANUARY.2010, KWH.FEBRUARY.2010, KWH.MARCH.2010, KWH.APRIL.2010,
                             KWH.MAY.2010, KWH.JUNE.2010, KWH.JULY.2010, KWH.AUGUST.2010, KWH.SEPTEMBER.2010, KWH.OCTOBER.2010, 
                             KWH.NOVEMBER.2010, KWH.DECEMBER.2010, TOTAL.KWH) ~ COMMUNITY.AREA.NAME + TRACT.ID + BUILDING.TYPE, chicagoData, sum)
elecTr <- merge(cook_tracts, elec_tr, by.x = "GEOID10", by.y = "TRACT.ID")
# gas
gas_tr <- aggregate( cbind(THERM.JANUARY.2010, THERM.FEBRUARY.2010, THERM.MARCH.2010, TERM.APRIL.2010,
                            THERM.MAY.2010, THERM.JUNE.2010, THERM.JULY.2010, THERM.AUGUST.2010, THERM.SEPTEMBER.2010, THERM.OCTOBER.2010, 
                            THERM.NOVEMBER.2010, THERM.DECEMBER.2010, TOTAL.THERMS) ~ COMMUNITY.AREA.NAME + TRACT.ID + BUILDING.TYPE, chicagoData, sum)
gasTr <- merge(cook_tracts, gas_tr, by.x = "GEOID10", by.y = "TRACT.ID")
#population
pop_tr <- aggregate(TOTAL.POPULATION ~ COMMUNITY.AREA.NAME + TRACT.ID  + BUILDING.TYPE, chicagoData, sum)
popTr <- merge(cook_tracts, pop_tr, by.x = "GEOID10", by.y = "TRACT.ID")
# age
avgage_tr <- aggregate(AVERAGE.BUILDING.AGE ~ COMMUNITY.AREA.NAME + TRACT.ID  + BUILDING.TYPE, chicagoData, mean)
ageTr <- merge(cook_tracts, avgage_tr, by.x = "GEOID10", by.y = "TRACT.ID")
# height
avgheight_tr <- aggregate(AVERAGE.STORIES ~ COMMUNITY.AREA.NAME + TRACT.ID  + BUILDING.TYPE, chicagoData, mean)
heightTr <- merge(cook_tracts, avgheight_tr, by.x = "GEOID10", by.y = "TRACT.ID")
# occupation percentage
occupied_tr <- aggregate(OCCUPIED.UNITS.PERCENTAGE ~ COMMUNITY.AREA.NAME + TRACT.ID  + BUILDING.TYPE, chicagoData, mean)
occupiedTr <- merge(cook_tracts, occupied_tr, by.x = "GEOID10", by.y = "TRACT.ID")
# renter occupation percentage
renter_tr <- aggregate(RENTER.OCCUPIED.HOUSING.PERCENTAGE ~ COMMUNITY.AREA.NAME + TRACT.ID + BUILDING.TYPE, chicagoData, mean)
renterTr <- merge(cook_tracts, renter_tr, by.x = "GEOID10", by.y = "TRACT.ID")

# generating the top 10% of each category from above to map as layers
newBuild <- head(ageTr[order(ageTr$AVERAGE.BUILDING.AGE),], n = 172)
oldBuild <- head(ageTr[order(ageTr$AVERAGE.BUILDING.AGE, decreasing = TRUE),], n = 172)
tallBuild <- head(heightTr[order(heightTr$AVERAGE.STORIES, decreasing = TRUE), ], n = 172)
topElec <- head(elecTr[order(elecTr$TOTAL.KWH, decreasing = TRUE),], n = 172)
topGas <- head(gasTr[order(gasTr$TOTAL.THERMS, decreasing = TRUE),], n = 172)
topPop <- head(popTr[order(popTr$TOTAL.POPULATION, decreasing = TRUE),], n = 172)
topOcc <- head(occupiedTr[order(occupiedTr$OCCUPIED.UNITS.PERCENTAGE, decreasing = TRUE),], n = 172)
topRent <- head(renterTr[order(renterTr$RENTER.OCCUPIED.HOUSING.PERCENTAGE, decreasing = TRUE),], n = 172)

# months, neighborhoods, building types for UI options
months <- c(month.name, "Total")
btypes <- c("Commercial", "Industrial", "Residential", "All")
neighborhoods <- c(levels(factor(chicagoData$COMMUNITY.AREA.NAME)), "Chicago")

# create a mapview based off of UI inputs
genMap<-function(community, filter, month, building, pal){
  # create default vals 
  d <- chicago_all
  col <- "TOTAL.KWH"
  lg = "Total Electricity (KWH)"
  # change dataframe based off of filter
  if (filter == "Electricity"){
    d <- elecChi
    if (building == "Commercial"){
      d <- subset(d, BUILDING.TYPE == "Commercial")
    }
    if (building == "Industrial"){
      d <- subset(d, BUILDING.TYPE == "Industrial")
    }
    if (building == "Residential"){
      d <- subset(d, BUILDING.TYPE == "Residential")
    }
    col <- "TOTAL.KWH"
    lg <- "Total Electricity Usage (KWH)"
    if (month == "January"){ 
      col <- "KWH.JANUARY.2010" 
      lg <- "January Electricity Usage (KWH)"
    }
    
    if (month == "February"){ 
      col <- "KWH.FEBRUARY.2010" 
      lg <- "February Electricity Usage (KWH)"
    }
    
    if (month == "March"){ 
      col <- "KWH.MARCH.2010" 
      lg <- "March Electricity Usage (KWH)"
    }
    
    if (month == "April"){ 
      col <- "KWH.APRIL.2010" 
      lg <- "April Electricity Usage (KWH)"
    }
    
    if (month == "May"){ 
      col <- "KWH.MAY.2010" 
      lg <- "May Electricity Usage (KWH)"
    }
    
    if (month == "June"){ 
      col <- "KWH.JUNE.2010" 
      lg <- "June Electricity Usage (KWH)"
    }
    
    if (month == "July"){ 
      col <- "KWH.JULY.2010" 
      lg <- "July Electricity Usage (KWH)"
    }
    
    if (month == "August"){ 
      col <- "KWH.AUGUST.2010" 
      lg <- "August Electricity Usage (KWH)"
    }
    
    if (month == "September"){ 
      col <- "KWH.SEPTEMBER.2010" 
      lg <- "September Electricity Usage (KWH)"
    }
    
    if (month == "October"){ 
      col <- "KWH.OCTOBER.2010" 
      lg <- "October Electricity Usage (KWH)"
    }
    
    if (month == "November"){ 
      col <- "KWH.NOVEMBER.2010" 
      lg <- "November Electricity Usage (KWH)"
    }
    
    if (month == "December"){ 
      col <- "KWH.DECEMBER.2010"
      lg <- "December Electricity Usage (KWH)"
    }
  }
  if (filter == "Gas"){
    d <- gasChi
    if (building == "Commercial"){
      d <- subset(d, BUILDING.TYPE == "Commercial")
    }
    if (building == "Industrial"){
      d <- subset(d, BUILDING.TYPE == "Industrial")
    }
    if (building == "Residential"){
      d <- subset(d, BUILDING.TYPE == "Residential")
    }
    
    col <- "TOTAL.THERMS"
    lg <- "Total Gas Usage (thm)"
    if (month == "January"){ 
      col <- "THERM.JANUARY.2010" 
      lg <- "January Gas Usage (thm)"
    }
    
    if (month == "February"){ 
      col <- "THERM.FEBRUARY.2010" 
      lg <- "February Gas Usage (thm)"
    }
    
    if (month == "March"){ 
      col <- "THERM.MARCH.2010" 
      lg <- "March Gas Usage (thm)"
    }
    
    if (month == "April"){ 
      col <- "TERM.APRIL.2010" 
      lg <- "April Gas Usage (thm)"
    }
    
    if (month == "May"){ 
      col <- "THERM.MAY.2010" 
      lg <- "May Gas Usage (thm)"
    }
    
    if (month == "June"){ 
      col <- "THERM.JUNE.2010" 
      lg <- "June Gas Usage (thm)"
    }
    
    if (month == "July"){ 
      col <- "THERM.JULY.2010" 
      lg <- "July Gas Usage (thm)"
    }
    
    if (month == "August"){ 
      col <- "THERM.AUGUST.2010" 
      lg <- "August Gas Usage (thm)"
    }
    
    if (month == "September"){ 
      col <- "THERM.SEPTEMBER.2010" 
      lg <- "September Gas Usage (thm)"
    }
    
    if (month == "October"){ 
      col <- "THERM.OCTOBER.2010" 
      lg <- "October Gas Usage (thm)"
    }
    
    if (month == "November"){ 
      col <- "THERM.NOVEMBER.2010" 
      lg <- "November Gas Usage (thm)"
    }
    
    if (month == "December"){ 
      col <- "THERM.DECEMBER.2010"
      lg <- "December Gas Usage (thm)"
    }
  }
  if (filter == "Building Age"){
    d <- ageChi
    if (building == "Commercial"){
      d <- subset(d, BUILDING.TYPE == "Commercial")
    }
    if (building == "Industrial"){
      d <- subset(d, BUILDING.TYPE == "Industrial")
    }
    if (building == "Residential"){
      d <- subset(d, BUILDING.TYPE == "Residential")
    }
    col <- "AVERAGE.BUILDING.AGE"
    lg <- "Average Building Age (Years)"
  }
  if (filter == "Building Type"){
    d <- elecChi
    if (building == "Commercial"){
      d <- subset(d, BUILDING.TYPE == "Commercial")
    }
    if (building == "Industrial"){
      d <- subset(d, BUILDING.TYPE == "Industrial")
    }
    if (building == "Residential"){
      d <- subset(d, BUILDING.TYPE == "Residential")
    }
    col <- "BUILDING.TYPE"
    lg <- "Building Type"
  }
  if(filter == "Building Height"){
    d <- heightChi
    if (building == "Commercial"){
      d <- subset(d, BUILDING.TYPE == "Commercial")
    }
    if (building == "Industrial"){
      d <- subset(d, BUILDING.TYPE == "Industrial")
    }
    if (building == "Residential"){
      d <- subset(d, BUILDING.TYPE == "Residential")
    }
    col <- "AVERAGE.STORIES"
    lg <- "Average Building Height (Stories)"
    
  }
  if (filter == "Total Population"){
    d <- popChi
    if (building == "Commercial"){
      d <- subset(d, BUILDING.TYPE == "Commercial")
    }
    if (building == "Industrial"){
      d <- subset(d, BUILDING.TYPE == "Industrial")
    }
    if (building == "Residential"){
      d <- subset(d, BUILDING.TYPE == "Residential")
    }
    col <- "TOTAL.POPULATION"
    lg <- "Total Population"
    
  }
  
  # create map and set to bottom right
  mapviewOptions(legend.pos = "bottomright")
  map <- mapview(subset(d, COMMUNITY.AREA.NAME == community), zcol=col, layer.name = lg, col.regions = hcl.colors(n = 7 , palette = pal) ) 
  return(map)
}

# similar to above to generate map but with tract dataframes
genMap2<-function(community, filter, month, building, pal){
  # default values
  d <- tract_all
  col <- "TOTAL.KWH"
  lg = "Total Electricity (KWH)"
  # change values by filter
  if (filter == "Electricity"){
    d <- elecTr
    if (building == "Commercial"){
      d <- subset(d, BUILDING.TYPE == "Commercial")
    }
    if (building == "Industrial"){
      d <- subset(d, BUILDING.TYPE == "Industrial")
    }
    if (building == "Residential"){
      d <- subset(d, BUILDING.TYPE == "Residential")
    }
    col <- "TOTAL.KWH"
    lg <- "Total Electricity Usage (KWH)"
    if (month == "January"){ 
      col <- "KWH.JANUARY.2010" 
      lg <- "January Electricity Usage (KWH)"
    }
    
    if (month == "February"){ 
      col <- "KWH.FEBRUARY.2010" 
      lg <- "February Electricity Usage (KWH)"
    }
    
    if (month == "March"){ 
      col <- "KWH.MARCH.2010" 
      lg <- "March Electricity Usage (KWH)"
    }
    
    if (month == "April"){ 
      col <- "KWH.APRIL.2010" 
      lg <- "April Electricity Usage (KWH)"
    }
    
    if (month == "May"){ 
      col <- "KWH.MAY.2010" 
      lg <- "May Electricity Usage (KWH)"
    }
    
    if (month == "June"){ 
      col <- "KWH.JUNE.2010" 
      lg <- "June Electricity Usage (KWH)"
    }
    
    if (month == "July"){ 
      col <- "KWH.JULY.2010" 
      lg <- "July Electricity Usage (KWH)"
    }
    
    if (month == "August"){ 
      col <- "KWH.AUGUST.2010" 
      lg <- "August Electricity Usage (KWH)"
    }
    
    if (month == "September"){ 
      col <- "KWH.SEPTEMBER.2010" 
      lg <- "September Electricity Usage (KWH)"
    }
    
    if (month == "October"){ 
      col <- "KWH.OCTOBER.2010" 
      lg <- "October Electricity Usage (KWH)"
    }
    
    if (month == "November"){ 
      col <- "KWH.NOVEMBER.2010" 
      lg <- "November Electricity Usage (KWH)"
    }
    
    if (month == "December"){ 
      col <- "KWH.DECEMBER.2010"
      lg <- "December Electricity Usage (KWH)"
    }
  }
  if (filter == "Gas"){
    d <- gasTr
    if (building == "Commercial"){
      d <- subset(d, BUILDING.TYPE == "Commercial")
    }
    if (building == "Industrial"){
      d <- subset(d, BUILDING.TYPE == "Industrial")
    }
    if (building == "Residential"){
      d <- subset(d, BUILDING.TYPE == "Residential")
    }
    
    col <- "TOTAL.THERMS"
    lg <- "Total Gas Usage (thm)"
    if (month == "January"){ 
      col <- "THERM.JANUARY.2010" 
      lg <- "January Gas Usage (thm)"
    }
    
    if (month == "February"){ 
      col <- "THERM.FEBRUARY.2010" 
      lg <- "February Gas Usage (thm)"
    }
    
    if (month == "March"){ 
      col <- "THERM.MARCH.2010" 
      lg <- "March Gas Usage (thm)"
    }
    
    if (month == "April"){ 
      col <- "TERM.APRIL.2010" 
      lg <- "April Gas Usage (thm)"
    }
    
    if (month == "May"){ 
      col <- "THERM.MAY.2010" 
      lg <- "May Gas Usage (thm)"
    }
    
    if (month == "June"){ 
      col <- "THERM.JUNE.2010" 
      lg <- "June Gas Usage (thm)"
    }
    
    if (month == "July"){ 
      col <- "THERM.JULY.2010" 
      lg <- "July Gas Usage (thm)"
    }
    
    if (month == "August"){ 
      col <- "THERM.AUGUST.2010" 
      lg <- "August Gas Usage (thm)"
    }
    
    if (month == "September"){ 
      col <- "THERM.SEPTEMBER.2010" 
      lg <- "September Gas Usage (thm)"
    }
    
    if (month == "October"){ 
      col <- "THERM.OCTOBER.2010" 
      lg <- "October Gas Usage (thm)"
    }
    
    if (month == "November"){ 
      col <- "THERM.NOVEMBER.2010" 
      lg <- "November Gas Usage (thm)"
    }
    
    if (month == "December"){ 
      col <- "THERM.DECEMBER.2010"
      lg <- "December Gas Usage (thm)"
    }
  }
  if (filter == "Building Age"){
    d <- ageTr
    if (building == "Commercial"){
      d <- subset(d, BUILDING.TYPE == "Commercial")
    }
    if (building == "Industrial"){
      d <- subset(d, BUILDING.TYPE == "Industrial")
    }
    if (building == "Residential"){
      d <- subset(d, BUILDING.TYPE == "Residential")
    }
    col <- "AVERAGE.BUILDING.AGE"
    lg <- "Average Building Age (Years)"
  }
  if (filter == "Building Type"){
    d <- elecTr
    if (building == "Commercial"){
      d <- subset(d, BUILDING.TYPE == "Commercial")
    }
    if (building == "Industrial"){
      d <- subset(d, BUILDING.TYPE == "Industrial")
    }
    if (building == "Residential"){
      d <- subset(d, BUILDING.TYPE == "Residential")
    }
    col <- "BUILDING.TYPE"
    lg <- "Building Type"
  }
  if(filter == "Building Height"){
    d <- heightTr
    if (building == "Commercial"){
      d <- subset(d, BUILDING.TYPE == "Commercial")
    }
    if (building == "Industrial"){
      d <- subset(d, BUILDING.TYPE == "Industrial")
    }
    if (building == "Residential"){
      d <- subset(d, BUILDING.TYPE == "Residential")
    }
    col <- "AVERAGE.STORIES"
    lg <- "Average Building Height (Stories)"
    
  }
  if (filter == "Total Population"){
    d <- popTr
    if (building == "Commercial"){
      d <- subset(d, BUILDING.TYPE == "Commercial")
    }
    if (building == "Industrial"){
      d <- subset(d, BUILDING.TYPE == "Industrial")
    }
    if (building == "Residential"){
      d <- subset(d, BUILDING.TYPE == "Residential")
    }
    col <- "TOTAL.POPULATION"
    lg <- "Total Population"
    
  }
  
  # set legend to bottom right
  mapviewOptions(legend.pos = "bottomright")
  # add additional layers
  map <- mapview(d, zcol=col, layer.name = lg, col.regions = hcl.colors(n = 7 , palette = pal) ) + mapview(oldBuild, layer.name = "10% of Oldest Buildings") + 
    mapview(newBuild, layer.name = "10% of Newest Buildings", col.regions = "Blue") + mapview(tallBuild, layer.name = "10% of Tallest Buildings", col.regions = "Green") + mapview(topElec, layer.name = "10% of Most Electricity Used", col.regions = "Orange") + 
    mapview(topGas, layer.name = "10% of Most Gas Used", col.regions = "Yellow") + mapview(topPop, layer.name = "10% of Largest Population", col.regions = "Pink") + 
    mapview(topOcc, layer.name = "10% of Largest Occupied Percentage", col.regions = "Grey") + mapview(topRent, layer.name = "10% of Largest Renter Occupied Percentage", col.regions = "Red")
  return(map)
}


# start of UI 
ui <- dashboardPage(
  dashboardHeader(title="CS 424 Project 3"),
  dashboardSidebar(
    sidebarMenu(
      # three pages for project 3
      menuItem("Near West Side Power Usage", tabName = "page1"),
      menuItem("Power Usage Comparison", tabName = "page2"),
      menuItem("About", tabName = "about")
    )
  ),
  dashboardBody(
    useShinyjs(),
    tabItems(
      tabItem(tabName = "page1",
              fluidRow(
                box(title = "Near West Side Power Usage in 2010", solidHeader = TRUE, width=12,height="900px", 
                    column(4, selectInput("f1", "Filter Heatmap", choices=c("Electricity", "Gas", "Building Age", "Building Type", "Building Height", "Total Population"), selected = "Electricity")),
                    column(4, selectInput("mon1", "Select a Month", choices=months, selected = "Total")),
                    column(4, selectInput("bt1", "Select a Building Type", choices=btypes, selected = "All")),
                    column(7, leafletOutput("map1", height = 600)),
                    column(5, 
                           tabBox(
                             title = "Power Usage per Block Bar Plot", side = "right",
                             id = "tabset1", height = "325px", selected = "Electricity",
                             tabPanel("Gas", plotOutput("plot2", height=300)),
                             tabPanel("Electricity", plotOutput("plot1", height=300)),
                             width = NULL
                           ),
                           
                           tabBox(
                             title = "Power Usage per Block Data Table", side = "right",
                             id = "tabset2", height = "325px", selected = "Electricity",
                             tabPanel("Gas", dataTableOutput("dt2", height = 300)),
                             tabPanel("Electricity", dataTableOutput("dt1", height = 300)),
                             width = NULL
                           )
                           ),
                    
                )
              )
              
      ), # end Near West Side level tab
      tabItem(tabName = "page2",
              fluidRow( 
                box(title = "Neighborhood 1 Power Usage", solidHeader = TRUE, width=6,height="1400px", 
                    fluidRow(
                      column(7, selectInput("loc1", "Select a Neighborhood", choices=neighborhoods, selected = "Near West Side")),
                      column(5, radioButtons("col1", label ="Select Color Palette", choices=c("Heat", "SunsetDark", "BluYl"), inline=TRUE, selected="Heat")),
                      
                    ),
                    fluidRow(
                      column(4, selectInput("f2", "Filter Heatmap", choices=c("Electricity", "Gas", "Building Age", "Building Type", "Building Height", "Total Population"), selected = "Electricity")),
                      column(4, selectInput("mon2", "Select a Month", choices=months, selected = "Total")),
                      column(4, selectInput("bt2", "Select a Building Type", choices=btypes, selected = "All"))
                    ),
                    column(12, leafletOutput("map2", height = 450),
                           tabBox(
                             title = "Power Usage per Block Bar Plot", side = "right",
                             id = "tabset3", height = "325px", selected = "Electricity",
                             tabPanel("Gas", plotOutput("plot4", height=300)),
                             tabPanel("Electricity", plotOutput("plot3", height=300)),
                             width = NULL
                           ),
                           tabBox(
                             title = "Power Usage per Block Data Table", side = "right",
                             id = "tabset4", height = "325px", selected = "Electricity",
                             tabPanel("Gas", dataTableOutput("dt4", height = 300)),
                             tabPanel("Electricity", dataTableOutput("dt3", height = 300)),
                             width = NULL
                           )),
                ),
                box(title = "Neighborhood 2 Power Usage", solidHeader = TRUE, width=6,height="1400px", 
                    fluidRow(
                      column(7, selectInput("loc2", "Select a Neighborhood", choices=neighborhoods, selected = "Loop")),
                      column(5, radioButtons("col2", label ="Select Color Palette", choices=c("Heat", "SunsetDark", "BluYl"), inline=TRUE, selected="Heat")),

                    ),
                    fluidRow(
                      column(4, selectInput("f3", "Filter Heatmap", choices=c("Electricity", "Gas", "Building Age", "Building Type", "Building Height", "Total Population"), selected = "Electricity")),
                      column(4, selectInput("mon3", "Select a Month", choices=months, selected = "Total")),
                      column(4, selectInput("bt3", "Select a Building Type", choices=btypes, selected = "All"))
                    ),
                    
                    
                    column(12, leafletOutput("map3", height = 450),
                           tabBox(
                             title = "Power Usage per Block Bar Plot", side = "right",
                             id = "tabset4", height = "325px", selected = "Electricity",
                             tabPanel("Gas", plotOutput("plot6", height=300)),
                             tabPanel("Electricity", plotOutput("plot5", height=300)),
                             width = NULL
                           ),
                           tabBox(
                             title = "Power Usage per Block Data Table", side = "right",
                             id = "tabset5", height = "325px", selected = "Electricity",
                             tabPanel("Gas", dataTableOutput("dt6", height = 300)),
                             tabPanel("Electricity", dataTableOutput("dt5", height = 300)),
                             width = NULL
                           )),
                    
                )
              )
              
      ), # end of Comparison level tab
      tabItem(tabName = "about",
              box(title="About the Data", solidHeader=TRUE, HTML("The data is provided by the Chicago Data Portal. 
                  The data 'displays several units of energy consumption for households, businesses, and industries in the City of Chicago during 2010'.
                  The link to the data can be found here: https://data.cityofchicago.org/api/views/8yq3-m6wp/rows.csv?accessType=DOWNLOAD <br/><br/> 
                  In this application, we specifically look at the following: <br/>
                                                                 <b>Electricity usage (KWH) per month and total year</b><br/>
                                                                 <b>Gas usage (thm) per month and total year</b><br/>
                                                                 <b>Building information (type, height, age)</b><br/>
                                                                 <b>Occupation percentages</b><br/>
                                                                 <b>Block and tract population</b><br/><br/>
                                                                 The data was used for further exploration in R and Shiny.")),
              box(title="About the Application", solidHeader=TRUE, HTML("This application was written by Yazmin Reyes. Its objective is to visualize power usage (electricity, gas) across Chicago Census blocks and tracts in 2010. It was written in R and Shiny with the assistance of the following: <br/>
                                                                        <b>Professor Andrew Johnson's Shiny App Example:</b> https://www.evl.uic.edu/aej/424/evlWeatherForR.zip <br/>
                                                                        <b>Shiny Dashboard Library:</b> https://rstudio.github.io/shinydashboard/ <br/>
                                                                        <b>Mapview for R Documentation:</b> https://www.rdocumentation.org/packages/mapview/versions/2.9.0/topics/mapView <br/>
                                                                        References to <b>Stack Overflow</b> and <b>Piazza</b> for specific R/Shiny/data usage <br/><br/>
                                                                        Last Revised: 4/24/2021"))
      ) # end of ABOUT tab
    )
  )
)


server <- function(input, output, session) { 
  # page 1 map of Near West Side
  output$map1 <- renderLeaflet({
    # map contains reset button and defined zoom level
    genMap("Near West Side", input$f1, input$mon1, input$bt1, "Heat")@map %>% setView(lat = 41.87570, lng = -87.66343, zoom = 13) %>% addResetMapButton()
  })
  
  # page 2 map for neighborhood
  output$map2 <- renderLeaflet({
    # generate a map for neighborhood with block data
    if (input$loc1 != "Chicago"){
      genMap(input$loc1, input$f2, input$mon2, input$bt2, input$col1)@map %>% addResetMapButton()
    }
    # generate a map for Chicago with tract data
    else{
      genMap2(input$loc1, input$f2, input$mon2, input$bt2, input$col1)@map %>% addResetMapButton()
    }
  })
  
  # page 2 map for neighborhood
  output$map3 <- renderLeaflet({
    # generate a map for neighborhood with block data
    if(input$loc2 != "Chicago"){
      genMap(input$loc2, input$f3, input$mon3, input$bt3, input$col2)@map %>% addResetMapButton()
    }
    # generate a map for Chicago with tract data
    else{
      genMap2(input$loc2, input$f3, input$mon3, input$bt3, input$col2)@map %>% addResetMapButton()
    }
  })
  
  # page 1 - electricity stacked bar plot
  output$plot1 <- renderPlot({
    e <- subset(elecChi, COMMUNITY.AREA.NAME == "Near West Side")
    e <- aggregate( cbind(KWH.JANUARY.2010, KWH.FEBRUARY.2010, KWH.MARCH.2010, KWH.APRIL.2010,
                                KWH.MAY.2010, KWH.JUNE.2010, KWH.JULY.2010, KWH.AUGUST.2010, KWH.SEPTEMBER.2010, KWH.OCTOBER.2010, 
                                KWH.NOVEMBER.2010, KWH.DECEMBER.2010) ~ COMMUNITY.AREA.NAME + GEOID10, e, sum)
    e <- melt(data=e, id.vars=c("COMMUNITY.AREA.NAME", "GEOID10"))
    ggplot(e, aes(x = variable, y = value, fill = factor(GEOID10))) + geom_bar(stat="identity") +
      scale_x_discrete(labels = c(month.abb)) + theme(legend.position = "none") + scale_y_continuous(labels = comma) + labs(x = "Month", y = "Electricity Usage (KWH)")
  })
  
  # page 1 - gas stacked bar plot
  output$plot2 <- renderPlot({
    g <- subset(gasChi, COMMUNITY.AREA.NAME == "Near West Side")
    g <- aggregate( cbind(THERM.JANUARY.2010, THERM.FEBRUARY.2010, THERM.MARCH.2010, TERM.APRIL.2010,
                          THERM.MAY.2010, THERM.JUNE.2010, THERM.JULY.2010, THERM.AUGUST.2010, THERM.SEPTEMBER.2010, THERM.OCTOBER.2010, 
                          THERM.NOVEMBER.2010, THERM.DECEMBER.2010) ~ COMMUNITY.AREA.NAME + GEOID10, g, sum)
    g <- melt(data=g, id.vars=c("COMMUNITY.AREA.NAME", "GEOID10"))
    ggplot(g, aes(x = variable, y = value, fill = factor(GEOID10))) + geom_bar(stat="identity") +
      scale_x_discrete(labels = c(month.abb)) + theme(legend.position = "none") + scale_y_continuous(labels = comma) + labs(x = "Month", y = "Gas Usage (thm)")
    
  })
  
  # page 2 neighborhood 1 - electricity stacked bar plot
  output$plot3 <- renderPlot({
    # specify for neighborhood and blocks
    if (input$loc1 != "Chicago"){
      e <- subset(elecChi, COMMUNITY.AREA.NAME == input$loc1)
      e <- aggregate( cbind(KWH.JANUARY.2010, KWH.FEBRUARY.2010, KWH.MARCH.2010, KWH.APRIL.2010,
                            KWH.MAY.2010, KWH.JUNE.2010, KWH.JULY.2010, KWH.AUGUST.2010, KWH.SEPTEMBER.2010, KWH.OCTOBER.2010, 
                            KWH.NOVEMBER.2010, KWH.DECEMBER.2010) ~ COMMUNITY.AREA.NAME + GEOID10, e, sum)
      e <- melt(data=e, id.vars=c("COMMUNITY.AREA.NAME", "GEOID10"))
    
    }
    # specify for Chicago tracts
    else{
      e <- elecTr
      e <- aggregate( cbind(KWH.JANUARY.2010, KWH.FEBRUARY.2010, KWH.MARCH.2010, KWH.APRIL.2010,
                            KWH.MAY.2010, KWH.JUNE.2010, KWH.JULY.2010, KWH.AUGUST.2010, KWH.SEPTEMBER.2010, KWH.OCTOBER.2010, 
                            KWH.NOVEMBER.2010, KWH.DECEMBER.2010) ~ GEOID10, e, sum)
      e <- melt(data=e, id.vars=c("GEOID10"))
    }
    
    ggplot(e, aes(x = variable, y = value, fill = factor(GEOID10))) + geom_bar(stat="identity") +
      scale_x_discrete(labels = c(month.abb)) + theme(legend.position = "none") + scale_y_continuous(labels = comma) + labs(x = "Month", y = "Electricity Usage (KWH)")
  })
  
  # page 2 neighborhood 1 - gas stacked bar plot
  output$plot4 <- renderPlot({
    # block data
    if (input$loc1 != "Chicago"){
      g <- subset(gasChi, COMMUNITY.AREA.NAME == input$loc1)
      g <- aggregate( cbind(THERM.JANUARY.2010, THERM.FEBRUARY.2010, THERM.MARCH.2010, TERM.APRIL.2010,
                            THERM.MAY.2010, THERM.JUNE.2010, THERM.JULY.2010, THERM.AUGUST.2010, THERM.SEPTEMBER.2010, THERM.OCTOBER.2010, 
                            THERM.NOVEMBER.2010, THERM.DECEMBER.2010) ~ COMMUNITY.AREA.NAME + GEOID10, g, sum)
      g <- melt(data=g, id.vars=c("COMMUNITY.AREA.NAME", "GEOID10"))
    }
    # tract data
    else{
      g <- gasTr
      g <- aggregate( cbind(THERM.JANUARY.2010, THERM.FEBRUARY.2010, THERM.MARCH.2010, TERM.APRIL.2010,
                            THERM.MAY.2010, THERM.JUNE.2010, THERM.JULY.2010, THERM.AUGUST.2010, THERM.SEPTEMBER.2010, THERM.OCTOBER.2010, 
                            THERM.NOVEMBER.2010, THERM.DECEMBER.2010) ~ GEOID10, g, sum)
      g <- melt(data=g, id.vars=c("GEOID10"))
    }
   
    ggplot(g, aes(x = variable, y = value, fill = factor(GEOID10))) + geom_bar(stat="identity") +
      scale_x_discrete(labels = c(month.abb)) + theme(legend.position = "none") + scale_y_continuous(labels = comma) + labs(x = "Month", y = "Gas Usage (thm)")
  })
  
  # page 2 neighborhood 2 - electricity stacked bar plot
  output$plot5 <- renderPlot({
    # block data
    if (input$loc2 != "Chicago"){
      e <- subset(elecChi, COMMUNITY.AREA.NAME == input$loc2)
      e <- aggregate( cbind(KWH.JANUARY.2010, KWH.FEBRUARY.2010, KWH.MARCH.2010, KWH.APRIL.2010,
                            KWH.MAY.2010, KWH.JUNE.2010, KWH.JULY.2010, KWH.AUGUST.2010, KWH.SEPTEMBER.2010, KWH.OCTOBER.2010, 
                            KWH.NOVEMBER.2010, KWH.DECEMBER.2010) ~ COMMUNITY.AREA.NAME + GEOID10, e, sum)
      e <- melt(data=e, id.vars=c("COMMUNITY.AREA.NAME", "GEOID10"))
    }
    # tract data
    else{
      e <- elecTr
      e <- aggregate( cbind(KWH.JANUARY.2010, KWH.FEBRUARY.2010, KWH.MARCH.2010, KWH.APRIL.2010,
                            KWH.MAY.2010, KWH.JUNE.2010, KWH.JULY.2010, KWH.AUGUST.2010, KWH.SEPTEMBER.2010, KWH.OCTOBER.2010, 
                            KWH.NOVEMBER.2010, KWH.DECEMBER.2010) ~ GEOID10, e, sum)
      e <- melt(data=e, id.vars=c("GEOID10"))
    }
    
    ggplot(e, aes(x = variable, y = value, fill = factor(GEOID10))) + geom_bar(stat="identity") +
      scale_x_discrete(labels = c(month.abb)) + theme(legend.position = "none") + scale_y_continuous(labels = comma) + labs(x = "Month", y = "Electricity Usage (KWH)")
  })
  
  # page 2 neighborhood 2 - gas stacked bar plot
  output$plot6 <- renderPlot({
    # block data
    if (input$loc2 != "Chicago"){
      g <- subset(gasChi, COMMUNITY.AREA.NAME == input$loc2)
      g <- aggregate( cbind(THERM.JANUARY.2010, THERM.FEBRUARY.2010, THERM.MARCH.2010, TERM.APRIL.2010,
                            THERM.MAY.2010, THERM.JUNE.2010, THERM.JULY.2010, THERM.AUGUST.2010, THERM.SEPTEMBER.2010, THERM.OCTOBER.2010, 
                            THERM.NOVEMBER.2010, THERM.DECEMBER.2010) ~ COMMUNITY.AREA.NAME + GEOID10, g, sum)
      g <- melt(data=g, id.vars=c("COMMUNITY.AREA.NAME", "GEOID10"))
    }
    # tract data
    else{
      g <- gasTr
      g <- aggregate( cbind(THERM.JANUARY.2010, THERM.FEBRUARY.2010, THERM.MARCH.2010, TERM.APRIL.2010,
                            THERM.MAY.2010, THERM.JUNE.2010, THERM.JULY.2010, THERM.AUGUST.2010, THERM.SEPTEMBER.2010, THERM.OCTOBER.2010, 
                            THERM.NOVEMBER.2010, THERM.DECEMBER.2010) ~ GEOID10, g, sum)
      g <- melt(data=g, id.vars=c("GEOID10"))
    }
    
    ggplot(g, aes(x = variable, y = value, fill = factor(GEOID10))) + geom_bar(stat="identity") +
      scale_x_discrete(labels = c(month.abb)) + theme(legend.position = "none") + scale_y_continuous(labels = comma)  + labs(x = "Month", y = "Gas Usage (thm)")
  })
  
  # page 1 - data table of electricity values
  output$dt1 <- renderDataTable({
    DT::datatable({ 
      e <- subset(elecChi, COMMUNITY.AREA.NAME == "Near West Side")
      e <- aggregate( cbind(KWH.JANUARY.2010, KWH.FEBRUARY.2010, KWH.MARCH.2010, KWH.APRIL.2010,
                            KWH.MAY.2010, KWH.JUNE.2010, KWH.JULY.2010, KWH.AUGUST.2010, KWH.SEPTEMBER.2010, KWH.OCTOBER.2010, 
                            KWH.NOVEMBER.2010, KWH.DECEMBER.2010) ~ COMMUNITY.AREA.NAME + GEOID10, e, sum)
      e <- melt(data=e, id.vars=c("COMMUNITY.AREA.NAME", "GEOID10"))
    }, 
    options = list(searching = FALSE, pageLength = 5, lengthChange = FALSE, order = list(list(1, 'desc')),
                   columns = list(
                     list(title = 'Neighborhood'),
                     list(title = 'Census Block ID'),
                     list(title = 'Month'),
                     list(title = 'Electricity Usage (KWH)'))
    ), rownames = FALSE 
    )
  })
  
 # page 1 - data table of gas values
  output$dt2 <- renderDataTable({
    DT::datatable({ 
      g <- subset(gasChi, COMMUNITY.AREA.NAME == "Near West Side")
      g <- aggregate( cbind(THERM.JANUARY.2010, THERM.FEBRUARY.2010, THERM.MARCH.2010, TERM.APRIL.2010,
                            THERM.MAY.2010, THERM.JUNE.2010, THERM.JULY.2010, THERM.AUGUST.2010, THERM.SEPTEMBER.2010, THERM.OCTOBER.2010, 
                            THERM.NOVEMBER.2010, THERM.DECEMBER.2010) ~ COMMUNITY.AREA.NAME + GEOID10, g, sum)
      g <- melt(data=g, id.vars=c("COMMUNITY.AREA.NAME", "GEOID10"))
    }, 
    options = list(searching = FALSE, pageLength = 5, lengthChange = FALSE, order = list(list(1, 'desc')),
                   columns = list(
                     list(title = 'Neighborhood'),
                     list(title = 'Census Block ID'),
                     list(title = 'Month'),
                     list(title = 'Gas Usage (thm)'))
                   ), rownames = FALSE 
    )
  })
  
  # page 2 neighborhood 1 - electricity data table
  output$dt3 <- renderDataTable({
    DT::datatable({ 
      # block data
      if (input$loc1 != "Chicago"){
        e <- subset(elecChi, COMMUNITY.AREA.NAME == input$loc1)
        e <- aggregate( cbind(KWH.JANUARY.2010, KWH.FEBRUARY.2010, KWH.MARCH.2010, KWH.APRIL.2010,
                              KWH.MAY.2010, KWH.JUNE.2010, KWH.JULY.2010, KWH.AUGUST.2010, KWH.SEPTEMBER.2010, KWH.OCTOBER.2010, 
                              KWH.NOVEMBER.2010, KWH.DECEMBER.2010) ~ COMMUNITY.AREA.NAME + GEOID10, e, sum)
        e <- melt(data=e, id.vars=c("COMMUNITY.AREA.NAME", "GEOID10"))
      }
      # tract data
      else{
        e <- elecTr
        e <- aggregate( cbind(KWH.JANUARY.2010, KWH.FEBRUARY.2010, KWH.MARCH.2010, KWH.APRIL.2010,
                              KWH.MAY.2010, KWH.JUNE.2010, KWH.JULY.2010, KWH.AUGUST.2010, KWH.SEPTEMBER.2010, KWH.OCTOBER.2010, 
                              KWH.NOVEMBER.2010, KWH.DECEMBER.2010) ~ GEOID10, e, sum)
        e <- melt(data=e, id.vars=c("GEOID10"))
      }
    }, 
    options = list(searching = FALSE, pageLength = 5, lengthChange = FALSE, order = list(list(1, 'desc')),
                   columns = if(input$loc1 != "Chicago"){ ls1 } else{ ls2 }
    ), rownames = FALSE 
    )
  })
  
  # page 2 neighborhood 1 - gas stacked bar plot
  output$dt4 <- renderDataTable({
    DT::datatable({ 
      # block data
      if (input$loc1 != "Chicago"){
        g <- subset(gasChi, COMMUNITY.AREA.NAME == input$loc1)
        g <- aggregate( cbind(THERM.JANUARY.2010, THERM.FEBRUARY.2010, THERM.MARCH.2010, TERM.APRIL.2010,
                              THERM.MAY.2010, THERM.JUNE.2010, THERM.JULY.2010, THERM.AUGUST.2010, THERM.SEPTEMBER.2010, THERM.OCTOBER.2010, 
                              THERM.NOVEMBER.2010, THERM.DECEMBER.2010) ~ COMMUNITY.AREA.NAME + GEOID10, g, sum)
        g <- melt(data=g, id.vars=c("COMMUNITY.AREA.NAME", "GEOID10"))
      }
      # tract data
      else{
        g <- gasTr
        g <- aggregate( cbind(THERM.JANUARY.2010, THERM.FEBRUARY.2010, THERM.MARCH.2010, TERM.APRIL.2010,
                              THERM.MAY.2010, THERM.JUNE.2010, THERM.JULY.2010, THERM.AUGUST.2010, THERM.SEPTEMBER.2010, THERM.OCTOBER.2010, 
                              THERM.NOVEMBER.2010, THERM.DECEMBER.2010) ~ GEOID10, g, sum)
        g <- melt(data=g, id.vars=c("GEOID10"))
      }
      
    }, 
    options = list(searching = FALSE, pageLength = 5, lengthChange = FALSE, order = list(list(1, 'desc')),
                   columns = if(input$loc1 != "Chicago"){ ls3 } else{ ls4 }
                   ), rownames = FALSE 
    )
  })
  
  # page 2 neighborhood 2 - electricity data table
  output$dt5 <- renderDataTable({
    DT::datatable({ 
      # block data
      if (input$loc2 != "Chicago"){
        e <- subset(elecChi, COMMUNITY.AREA.NAME == input$loc2)
        e <- aggregate( cbind(KWH.JANUARY.2010, KWH.FEBRUARY.2010, KWH.MARCH.2010, KWH.APRIL.2010,
                              KWH.MAY.2010, KWH.JUNE.2010, KWH.JULY.2010, KWH.AUGUST.2010, KWH.SEPTEMBER.2010, KWH.OCTOBER.2010, 
                              KWH.NOVEMBER.2010, KWH.DECEMBER.2010) ~ COMMUNITY.AREA.NAME + GEOID10, e, sum)
        e <- melt(data=e, id.vars=c("COMMUNITY.AREA.NAME", "GEOID10"))
      }
      # tract data
      else{
        e <- elecTr
        e <- aggregate( cbind(KWH.JANUARY.2010, KWH.FEBRUARY.2010, KWH.MARCH.2010, KWH.APRIL.2010,
                              KWH.MAY.2010, KWH.JUNE.2010, KWH.JULY.2010, KWH.AUGUST.2010, KWH.SEPTEMBER.2010, KWH.OCTOBER.2010, 
                              KWH.NOVEMBER.2010, KWH.DECEMBER.2010) ~ GEOID10, e, sum)
        e <- melt(data=e, id.vars=c("GEOID10"))
      }
      
    }, 
    options = list(searching = FALSE, pageLength = 5, lengthChange = FALSE, order = list(list(1, 'desc')),
                   columns = if(input$loc2 != "Chicago"){ ls1 } else{ ls2 }
    ), rownames = FALSE 
    )
  })
  
  # page 2 neighborhood 2 - gas data table
  output$dt6 <- renderDataTable({
    DT::datatable({ 
      # block data
      if (input$loc2 != "Chicago"){
        g <- subset(gasChi, COMMUNITY.AREA.NAME == input$loc2)
        g <- aggregate( cbind(THERM.JANUARY.2010, THERM.FEBRUARY.2010, THERM.MARCH.2010, TERM.APRIL.2010,
                              THERM.MAY.2010, THERM.JUNE.2010, THERM.JULY.2010, THERM.AUGUST.2010, THERM.SEPTEMBER.2010, THERM.OCTOBER.2010, 
                              THERM.NOVEMBER.2010, THERM.DECEMBER.2010) ~ COMMUNITY.AREA.NAME + GEOID10, g, sum)
        g <- melt(data=g, id.vars=c("COMMUNITY.AREA.NAME", "GEOID10"))
      }
      # tract data
      else{
        g <- gasTr
        g <- aggregate( cbind(THERM.JANUARY.2010, THERM.FEBRUARY.2010, THERM.MARCH.2010, TERM.APRIL.2010,
                              THERM.MAY.2010, THERM.JUNE.2010, THERM.JULY.2010, THERM.AUGUST.2010, THERM.SEPTEMBER.2010, THERM.OCTOBER.2010, 
                              THERM.NOVEMBER.2010, THERM.DECEMBER.2010) ~ GEOID10, g, sum)
        g <- melt(data=g, id.vars=c("GEOID10"))
      }

    }, 
    options = list(searching = FALSE, pageLength = 5, lengthChange = FALSE, order = list(list(1, 'desc')),
                   columns = if(input$loc2 != "Chicago"){ ls3 } else{ ls4 }
                   ), rownames = FALSE 
    )
  })
 
}

shinyApp(ui, server)

