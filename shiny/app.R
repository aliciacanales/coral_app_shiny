
library(tidyverse)
library(shiny)
library(bslib)
library(lubridate)
library(here)
#install.packages("mapview")
library(mapview)
library(sf)
library(leaflet)
install.packages("rio")
library(rio)

coral <- readxl::read_excel(here('data', 'coral_data_244_akd.xls')) %>% 
  mutate(date = ymd(date)) 
location <- rio::import(here('data','coral_data_244_akd.xls'))
location_geo <- st_as_sf(location, coords = c('long', 'lat'),
                         crs = 4326) 
mapview(location_geo, map.types = "OpenStreetMap.DE") 
location_geo <- st_as_sf(location, coords = c('long', 'lat'),
                         crs = 4326) 

fp<-read_sf(here::here("data","xg569rm6446.shp")) %>% 
  filter(hasc_1=="PF.WI") %>% 
  select(name_0,varname_1,geometry)


coral_map <- ggplot(data=fp)+
  geom_sf()+
  coord_sf(xlim=c(-149.70,-149.95),ylim=c(-17.42,-17.62))+
  annotation_scale(
    location = "bl",
    width_hint = 0.2)

my_theme <- bs_theme(
  bg = '#B7D1DA',
  fg = '#465775', #color of font
  primary = 'white',
  base_font = font_google('Lexend')
)


ui <- fluidPage(theme = my_theme,
                navbarPage("Coral Across Northshore Moorea",
                           tabPanel('About',
                                    mainPanel(
                                      h1('Overview of the Study'),
                                      h5("This shiny app showcases the resilience of Moorea's outer reef communities to changing ocean conditions over the past decade, despite the acidic ocean conditions and rising ocean temperatures. The app provides data from coral surveys conducted in the Northshore lagoon in Moorea, where 5 5x5m transects were set up at 16 sites to measure branching (pocillopora and Acropora) corals' lengths and available settlement space in each plot. The app aims to help understand the spatial distribution of coral taxa and size structure to further understand community dynamics and identify areas of efficient out-planting sites and optimal habitat for restoration efforts that are in Moorea. The study suggests that the recovery of outer reef coral communities around Moorea may include an increased capacity to respond to future conditions due to the diversity of coral recruits, at least among pocillopora species."),
                                      h6('Alicia Canales, Kat Mackay, Danielle Hoekstra')
                                    )),
                           tabPanel('Map 1',
                                    sidebarLayout(
                                      sidebarPanel("Genus",
                                                   checkboxGroupInput(inputId = 'pick_species',
                                                                      label = 'Choose species',
                                                                      choices = unique(coral$genus)
                                                   )
                                      ),
                                      mainPanel("Output",
                                                plotOutput('coral_plot')
                                      )
                                    ) 
                           ),
                           tabPanel('Date'),
                           tabPanel('map 2',
                                    mainPanel('Output',
                                              leafletOutput('location_geo') 
                                    )
                           ))
)



# Server for histogram
server <- function(input, output) {
  
  
  
  #### Tab 1
  coral_reactive <- reactive({
    coral %>%
      filter(genus %in% input$pick_species)
  })
  
  output$coral_plot <- renderPlot(
    ggplot(data = coral_reactive(), aes(x = length, y = width)) +
      geom_point(aes(color = genus)) + scale_color_manual(values = c('poc' = '#4dbedf', 'acr' = '#ea7070', 'NA' = '#fdc4b6')) +
      theme_minimal()
    
    
      )
  
  
  ### Tab 2
  
  ## we don't need to create a new subset - we add them within the pink {} and then up in the tabs we reference the output that we want displayed on each tab
  
  output$coral_map <- rendersf(
    leaflet(location_geo)
    
  )
} # end of histogram server





# Run the application 
shinyApp(ui = ui, server = server)