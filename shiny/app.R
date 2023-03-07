#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)

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
        hist(x, breaks = bins, col = 'darkgray', border = 'white',
             xlab = 'Waiting time to next eruption (in mins)',
             main = 'Histogram of waiting times')
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

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
                                      h2('To visualize the spatial distribution of coral sizes across the northshore of Moorea based on genus and available settlement area'),
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
  
  output$location_geo <- renderLeaflet(
    leaflet(location_geo)
    
  )
} # end of histogram server





# Run the application 
shinyApp(ui = ui, server = server)