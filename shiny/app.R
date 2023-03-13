## Tab 1 -- 
## Tab 4 -- a table output that return info on site using a text input =- number of acr and poc, if its in the garden and the % bleached 


library(tidyverse)
library(shiny)
library(bslib)
library(lubridate)
library(here)
#install.packages("mapview")
library(mapview)
library(sf)
library(leaflet)
#install.packages("rio")
library(rio)
#install.packages("ggspatial")
library(ggspatial)
library(shinyWidgets)
library(plotly)
library(tidymodels)
library(jtools)
library(tidyr)
library(RColorBrewer)

## Reading in data
coral <- readxl::read_excel(here('data', 'coral_data_244_akd.xls')) %>% 
  mutate(date = ymd(date))

poc_acr <- coral %>%
  filter(genus %in% c('poc', 'acr')) %>% 
  mutate(as.factor(site)) %>% 
  mutate(genus = fct_drop(genus))

new_coral <- poc_acr %>% 
  select(site, lat, long, genus) %>% 
  group_by(site)

counts <- new_coral %>%
  group_by(genus) %>%
  count(site)

### Site Table Wrangling 
counts_na <- coral %>%
  group_by(genus) %>%
  count(site) %>% 
  rename("total n" = n)

garden_counts <- coral %>% 
  select(genus, site, garden) %>%
  mutate(garden = as.factor(garden)) %>%
  group_by(genus) %>%
  count(site, garden) %>% 
  filter(garden == 'Y')

gc <- garden_counts %>% 
  select(site, genus, n)

dead_counts <- coral %>%
  select(genus, site, perc_dead) %>%
  group_by(genus, site) %>% 
  summarise(sum_of_runs = sum(perc_dead),
            average_of_perc_dead = mean(perc_dead, na.rm = TRUE)) 

dc <- dead_counts %>% 
  select(site, genus, average_of_perc_dead)

site_class <- full_join(dc, gc)

sites <- full_join(site_class, counts_na) %>% 
  rename("# in garden" = n)

### Tab 2
plot_counts <- coral %>% 
  select(plot, genus, bommie_loc) %>% 
  group_by(plot) %>% 
  count(bommie_loc) %>% 
  filter(!row_number() %in% c(1, 4)) %>% 
  mutate(bommie_loc = ifelse(is.na(bommie_loc), 'Undetermined', bommie_loc))

  plot_counts$bommie_loc[plot_counts$bommie_loc == 'N/A'] <- 'Undetermined'

### Locations

comb_coral <- counts %>% 
  merge(new_coral, by = c('site', 'genus')) %>% 
  unique() %>% 
  mutate(site = as.factor(site))

location <- rio::import(here('data','coral_data_244_akd.xls'))

comb_coral2 <- st_as_sf(comb_coral, coords = c('long', 'lat'),
                          crs = 4326)
  

fp<-read_sf(here::here("data","xg569rm6446.shp")) %>%
  filter(hasc_1=="PF.WI") %>%
  select(name_0,varname_1,geometry)

coral_map <- ggplot(data=fp)+
  geom_sf()+
  coord_sf(xlim=c(-149.70,-149.95), ylim=c(-17.42,-17.62))+
  annotation_scale(
    location = "bl",
    width_hint = 0.2)

### Binary Logistic Regression model

f1 <- genus ~ length * width * site
coral_blr1 <- glm(formula = f1, data = poc_acr, 
                  family = 'binomial') 
coral_tidy <- tidy(coral_blr1)

coral_fitted <- coral_blr1 %>% 
  broom::augment(type.predict = 'response')


my_theme <- bs_theme(
  bg = 'lightblue',
  fg = 'white', #color of font
  primary = 'white',
  base_font = font_google('Lexend')
)

ui <- fluidPage(theme = my_theme,
                tags$h2('Moorea Coral App'),
                setBackgroundImage(
                  src = 'https://c4.wallpaperflare.com/wallpaper/927/873/113/corals-fishes-rays-sea-wallpaper-preview.jpg'
                ),
                navbarPage("Coral Across Northshore Moorea",
                           tabPanel('About',
                                    mainPanel(
                                      h1('Overview of the Study'),
                                      h5("This shiny app showcases the resilience of Moorea's outer reef communities to changing ocean conditions over the past decade, despite the acidic ocean conditions and rising ocean temperatures. The app provides data from coral surveys conducted in the Northshore lagoon in Moorea, where 5 5x5m transects were set up at 16 sites to measure branching (pocillopora and Acropora) corals' lengths and available settlement space in each plot. The app aims to help understand the spatial distribution of coral taxa and size structure to further understand community dynamics and identify areas of efficient out-planting sites and optimal habitat for restoration efforts that are in Moorea. The study suggests that the recovery of outer reef coral communities around Moorea may include an increased capacity to respond to future conditions due to the diversity of coral recruits, at least among pocillopora species."),
                                      h6('Alicia Canales, Danielle Hoekstra, Kat Mackay')
                                    )),
                           tabPanel('Chart',
                                    sidebarLayout(
                                      sidebarPanel("Plot",
                                                   selectInput(inputId = 'coral_plot',
                                                                      label = 'Choose plot',
                                                                      choices = c('1' = '1', '2' = '2', '3' = '3', '4' = '4', '5' = '5', '6'= '6'),
                                      # sidebarPanel("Genus",
                                      #              checkboxGroupInput(inputId = 'pick_site',
                                      #                                 label = 'Choose species',
                                      #                                 choices = c('Pocillopora (POC)' = 'poc', 'Acropora (ACR)' = 'acr', 'Undetermined (NA)' = 'NA')
                                                   )
                                      ),
                                      mainPanel("Plot information",
                                                plotOutput('coral_plot')
                                      )
                                    ) 
                           ),
                           tabPanel('Predict Coral Species!',
                                    sidebarLayout( position = 'left',
                                     sidebarPanel(
                                      textInput(inputId = "site",
                                       label = "Site Number"),
                                      textInput(inputId = "length",
                                       label = "Length"),
                                      textInput(inputId = "width",
                                        label = "Width"),
                                      submitButton("Analyze!"),
                                      '
This bar plot presents the probability that an undetermined or new Moorea coral is pocillopora or acropora. The user input values are applied to a binomial logistic regression that we have trained using the rest of the coral data set. Based on those values we can predict the likelihood of if the unknown coral is species pocillopora or acropora.'),
                           
                                    mainPanel('Prediction Results',
                           plotOutput('bar')),
                           
                )),
                           
                           tabPanel('Moorea Map',
                                  sidebarLayout(position = 'right',  
                                    sidebarPanel(
                                                 "This map shows the sites along the island of Moorea that is part of French Polynesia's Society Islands archipelago. Each point displays the site number and the dominant genus' total number of individuals. These sites are important to examine individually to pinpoint where restoration efforts are most needed."),
                                    mainPanel('Location Sites along Moorea',
                                              plotlyOutput('map')))
                           ),
                tabPanel('Table',
                        sidebarLayout(position = "left",
                         sidebarPanel(
                          radioButtons(inputId = 'site_select', 
                          label = "Choose Site", 
                          choices = c('120' = '120', '124' = '124', '131' = '131', '134' = '134', '136' = '136', '143' = '143', '147' = '147', '149' = '149', '152' = '152', '154' = '154', '157' = '157', '167' = '167', '171' = '171', '173' = '173', '183' = '183', '185' = '185', '186' = '186')
                         )),
                         mainPanel('Output',
                                   tableOutput(outputId = 'table'),
                                   'The user of this tab can filter a data table by the site to display the total number of observations of each species of coral, the number of species cataloged in the garden, and the average percent perished across each species. With this information, one can isolate individual sites and assess high-priority sites for restoration efforts as well which species of coral are at risk site specifically. Moorea and the neighborring Tahitian Islands are home to more than 1,000 species of fish, the most colorful can be found in the coral gardens and lagoons of the coral reefs surrounding the islands. Therefore it is important to protect this beautiful habitat.'))),
                tabPanel('Citations',
                         mainPanel(
                           h1("citation here")
                         ))
                
))

# Server for histogram
server <- function(input, output) {
  
  coral_reactive <- reactive({
  plot_counts %>% 
      filter(plot == input$coral_plot)
  }) # end of tab 1

#data = coral_reactive(),
  output$coral_plot <- renderPlot({
  plot_output <- ggplot(coral_reactive(), aes(x = bommie_loc, y = n)) +
      geom_bar(color = "lightblue") +
      theme_minimal()
    
}) # end of coral plot
  
   output$map <- renderPlotly({
      ggplot(data=fp) +
        geom_sf() +
        theme_minimal() +
        coord_sf(xlim=c(-149.70,-149.95),ylim=c(-17.42,-17.62))+
        annotation_scale(
          location = "bl",
          width_hint = 0.2
        ) + geom_sf(data = comb_coral2, aes(color = site,
                                            label = genus,
                                            text = paste("Total Count", n)
                                            )) +
        coord_sf(xlim=c(-149.70,-149.95),ylim=c(-17.42,-17.62)) +
       guides(col= guide_legend(title= "Location Site"))
     
      
    })  # end of map server, end of plotly

      
user_df <- reactive({
  data.frame(
    site = as.numeric(input$site),
    length = as.numeric(input$length),
    width = as.numeric(input$width))
  })

output$bar <- renderPlot({
   pred <- predict(coral_blr1, user_df(),
                   type = 'response') 
    color <- c("cyan", "coral")
    df <- tribble(
      ~ species,     ~ prob,
      'pocillopora', pred,
      'acropora',  1- pred)
    
   
ggplot(df, x = 1, aes(x = species, y = prob, fill = species)) +
  geom_col() +
  theme_minimal()
     
   })
# end of predictor server
# Tab 4 -- a table output that return info on site using a text input =- number of acr and poc, if its in the garden and the % bleached
site_select <- reactive({
  sites %>% 
    filter(site == input$site)
})

output$table <- renderTable({
site_select()
  })

}




  ### Tab 2
  
  ## we don't need to create a new subset - we add them within the pink {} and then up in the tabs we reference the output that we want displayed on each tab
  
  # output$coral_map <- renderPlot(plotOutput(coral_map))
    
   # end of histogram server





# Run the application 
shinyApp(ui = ui, server = server)
