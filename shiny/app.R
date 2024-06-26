
### Packages
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
library(knitr)
library(DT)
library(htmltools)


### Reading in data and and initial wrangling:
coral <- readxl::read_excel(here('data', 'coral_data_244_akd.xls')) %>% 
  mutate(date = ymd(date)) %>% 
  mutate(site = as.integer(site))

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

### Site table wrangling: 
counts_na <- coral %>%
  group_by(genus) %>%
  count(site) %>% 
  rename("total_n" = n)

counts_na_ng <- coral %>% 
  count(site) %>% 
  rename("total_n" = n)

gc <- counts %>% 
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
  rename("in_garden" = n) 

sites_renamed <- sites %>% 
  rename("Site" = site, 
         "Species" = genus,
         "Average_%_Dead" = average_of_perc_dead, 
         "Number_in_Garden" = in_garden,
         "Total_Observations" = total_n)

plot_counts <- coral %>% 
  select(plot, genus, bommie_loc) %>% 
  group_by(plot) %>% 
  count(bommie_loc) %>% 
  filter(!row_number() %in% c(1, 4)) %>% 
  mutate(bommie_loc = ifelse(is.na(bommie_loc), 'Undetermined', bommie_loc))

plot_counts$bommie_loc[plot_counts$bommie_loc == 'N/A'] <- 'Undetermined'

### For the reactive map :
lat_lon_cor <- coral %>% 
  select(site, lat, long) %>% 
  merge(counts_na_ng, by = ('site')) %>% 
  unique()

garden_counts <- coral %>% 
  select(genus, site, garden) %>%
  mutate(garden = as.factor(garden)) %>%
  group_by(genus) %>%
  count(site, garden) %>% 
  filter(garden == 'Y')

### Location data:
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

### Bommie location plot data wrangling:
bom_dead <- coral %>% 
  select(genus, bommie_loc, perc_dead) %>% 
  group_by(bommie_loc) %>% 
  summarise(sum_of_runs = sum(perc_dead),
            average_of_perc_dead = mean(perc_dead, na.rm = TRUE)) %>% 
  filter(bommie_loc != "acr", bommie_loc != "poc", bommie_loc != "stop", bommie_loc != "N/A", bommie_loc != "N/A") %>% 
  rename("n" = sum_of_runs)

bommie_plot <- bom_dead %>% 
  ggplot(aes(x = bommie_loc, y = avg_perc_dead)) +
  geom_col() +
  theme_minimal() +
  labs(x = 'Bommie Location', y = 'Average % Dead')

site_bom <- coral %>% 
  select(site, bommie_loc, perc_dead) %>% 
  group_by(site, bommie_loc) %>% 
  summarise(sum_of_runs = sum(perc_dead),
            average_of_perc_dead = mean(perc_dead, na.rm = TRUE)) %>% 
  filter(bommie_loc != "acr", bommie_loc != "poc", bommie_loc != "stop", bommie_loc != "N/A", bommie_loc != "N/A") %>% 
  rename("n" = sum_of_runs, "avg_perc_dead" = average_of_perc_dead) %>% 
  mutate(site = as.factor(site))

### Binary Logistic Regression model:
f1 <- genus ~ length * width * site
coral_blr1 <- glm(formula = f1, data = poc_acr, 
                  family = 'binomial') 
coral_tidy <- tidy(coral_blr1)
coral_fitted <- coral_blr1 %>% 
  broom::augment(type.predict = 'response')

### Metadata:
metadata <- read_csv(here("data", "coral_metadata.csv")) %>% 
  rename("Description" = ...2)
meta_kable <- knitr::kable(metadata)  

### Theme:
my_theme <- bs_theme(
  bg = 'lightblue',
  fg = 'white', 
  primary = 'white',
  base_font = font_google('Lexend')
)

### Begin user interface:

ui <- fluidPage(theme = my_theme,
                tags$h2('Moorea Coral App'),
                setBackgroundImage(
                  src = 'https://images.unsplash.com/photo-1507166763745-bfe008fbb831?ixlib=rb-4.0.3&ixid=MnwxMjA3fDB8MHxwaG90by1wYWdlfHx8fGVufDB8fHx8&auto=format&fit=crop&w=1770&q=80'
                ),
                navbarPage("Coral Across Northshore Moorea",
                           tabPanel('About',
                                    mainPanel(
                                      h1('Overview of the Study'),
                                      column(
                                      h5("This shiny app showcases the resilience of Moorea's outer reef communities to changing ocean conditions over the past decade, despite the acidic ocean conditions and rising ocean temperatures. The app provides data from coral surveys conducted in the Northshore lagoon in Moorea, where 5 5x5m transects were set up at 17 sites to measure branching (pocillopora and Acropora) corals' lengths and available settlement space in each plot. The app aims to help understand the spatial distribution of coral taxa and size structure to further understand community dynamics and identify areas of efficient out-planting sites and optimal habitat for restoration efforts that are in Moorea. The study suggests that the recovery of outer reef coral communities around Moorea may include an increased capacity to respond to future conditions due to the diversity of coral recruits, at least among pocillopora species."),
                                      width = 12,
                                      align = "left"),
                                      br(),
                                      br(),
                                      img(src='https://upload.wikimedia.org/wikipedia/commons/3/34/Acropora_globiceps_Maldives.jpg', height = "200px", width = "300px"),
                                      img(src = "https://media.istockphoto.com/id/1333638431/photo/pocillopora-damicornis-pink-colorful-sps-coral-in-red-sea-underwater-scene.jpg?s=612x612&w=0&k=20&c=2MPGelRPoVE92rs6smWFHFFviGl8dJ_TqkRjQSRNp20=", height = '200px', width = '300px'),
                                      tags$figcaption("Coral species in the study: Acropora (left) and Pocillopora (right)")
                                    )),
                           tabPanel('Bommie Info',
                                    sidebarLayout(
                                      sidebarPanel(
                                        checkboxGroupInput(inputId = 'site_coral',
                                                           label = 'Choose Site Number',
                                                           choices = unique(site_bom$site),
                                                           selected = unique(site_bom$site)
                                                        
                                                           
                                                           
                                        ), 
                                        br(),
                                        h5('In this tab, we have created graphics to compare the locations of the coral along bommies across all of the sites. This information will be useful to determine the likelihood of survival during restoration based on their placement. Check multiple sites to compare their locations and use the pie chart to determine probability of bleaching!'),
                                        br(), 
                                        br(), 
                                        h6('Bommie: an outcrop of coral reef, often resembling a column, that is higher than the surrounding platform of reef and which may be partially exposed at low tide.')
                                      ),
                                      mainPanel(h4("Coral Distribution on Bommies by Site"),
                                                plotOutput('coral_plot'),
                                                br(),
                                                plotOutput('coral_pie')
                                      )) 
                           ),
                           tabPanel('Predict Coral Species!',
                                    sidebarLayout(position = 'left', 
                                                  sidebarPanel(h4("Enter Values:"),
                                                                  textInput(inputId = "site", 
                                                                            label = h5("Site Number"),
                                                                            value = 120),
                                                               h6("Example: 120, 124, 131, etc."),
                                                                  textInput(inputId = "length",
                                                                            label = h5("Length (cm)"),
                                                                            value = 9.9),
                                                                  h6("Enter values between 0 and 150 cm"),
                                                                  textInput(inputId = "width", 
                                                                            label = h5("Width (cm)"),
                                                                            value = 12.1),
                                                                  h6("Enter values between 0 and 150 cm")),
                                                  mainPanel(h4('Which Species is it?'),
                                                            plotOutput('bar'), 
                                                            br(),
                                                            br(),
                                                            'This bar plot presents the probability that an undetermined or new Moorea coral is pocillopora or acropora. The user input values are applied to a binomial logistic regression that we have trained using the rest of the coral data set. Based on those values we can predict the likelihood of the unknown coral species being Pocillopora or Acropora.'))),
                           
                           tabPanel('Site Map',
                                    sidebarLayout(position = 'right',  
                                                  sidebarPanel(
                                                    checkboxGroupInput(inputId = 'location',
                                                                       label = 'Choose Site Number',
                                                                       choices = unique(lat_lon_cor$site),
                                                                       selected = unique(lat_lon_cor$site)),
                                                    br(),
                                                    "This map shows the sites along the island of Moorea that is part of French Polynesia's Society Islands archipelago. Each point displays the site number and the total number of individuals at the site. These sites are important to examine individually to pinpoint where restoration efforts are most needed."),
                                                  mainPanel(h4('Location Sites along Moorea'),
                                                            leafletOutput('coral_map')))
                           ),
                           tabPanel('Coral Health',
                                    sidebarLayout(position = "left",
                                                  sidebarPanel(
                                                    selectInput(inputId = 'site_select',
                                                                label = "Choose Site Number",
                                                                choices = unique(sites_renamed$Site))),
                                                  mainPanel(h4("How Dead are We Talkin'?"),
                                                            tableOutput(outputId = 'table'),
                                                            br(),
                                                            'The user of this tab can filter a data table by the site to display the total number of observations of each species of coral, the number of species cataloged in the garden, and the average percent perished across each species. With this information, one can isolate individual sites and assess high-priority sites for restoration efforts as well which species of coral are at risk site specifically. Moorea and the neighborring Tahitian Islands are home to more than 1,000 species of fish, the most colorful can be found in the coral gardens and lagoons of the coral reefs surrounding the islands. Therefore it is important to protect this beautiful habitat.'))),
                           tabPanel('Citations & Metadata',
                                    mainPanel(
                                      h4("PhD candidate, Olivia Isbell, collected this data from Moorea from July 1st, 2022 until August 26th, 2022."),
                                      h4('This shiny app was compiled by Alicia Canales, Danielle Hoekstra and Kat Mackay'),
                                      br(),
                                      h6("Image Citations:"),
                                      h6("Igor Malakhov. (2021). Pocillopora Damicornis pink colorful SPS coral in Red Sea underwater scene [Photograph]. Retrieved from https://media.istockphoto.com/id/1333638431/photo/pocillopora-damicornis-pink-colorful-sps-coral-in-red-sea-underwater-scene.jpg?s=612x612&w=0&k=20&c=2MPGelRPoVE92rs6smWFHFFviGl8dJ_TqkRjQSRNp20="),
                                      h6("Michael Luenen. (2017). Underwater view of a coral reef with colorful fish [Photograph]. Retrieved from https://images.unsplash.com/photo-1507166763745-bfe008fbb831?ixlib=rb-4.0.3&ixid=MnwxMjA3fDB8MHxwaG90by1wYWdlfHx8fGVufDB8fHx8&auto=format&fit=crop&w=1770&q=80"),
                                      h6("Noa Shenkar. (2014). Acropora globiceps Maldives.jpg [Photograph]. Retrieved from https://upload.wikimedia.org/wikipedia/commons/3/34/Acropora_globiceps_Maldives.jpg"),
                                      br(),
                                      h6("Data Citation: Olivia Isbell. 2022. Bren School of Environmental Science and Management. Moorea Coral Reef Data."),
                                      h6("Metadata Info:"),
                                      DTOutput('meta')
                                    ))
                           ))   

### Begin server and reactive:

server <- function(input, output) {
  
  ### Bommie location analysis chart:
  bomm_reactive <- reactive({   
    message('in bomm_reactive, input$site_coral = ', input$site_coral)  
    site_bom %>% 
      filter(site %in% input$site_coral)
  }) ### End of bommie location chart

  ###Coral plot
  
  output$coral_plot <- renderPlot({
    
    ggplot(bomm_reactive(), aes(fill = bommie_loc, y = avg_perc_dead , x = site)) +
      geom_bar(position = "stack", stat = "identity") + 
      theme_minimal() + 
      labs(x = "Site Number", y = "Counts", fill = "Location on Bommie") +
      scale_fill_manual(values = c("lightblue", "#A7D0D9", "#FDE4E0", "#E1FFFF", "#E5E6FB")) + 
      theme(text = element_text(colour = "white", family = "Tahoma", face = "bold", size = 15),
        axis.text.x = element_text(family = "Tahoma",
                                       face = "bold",
                                       colour = "white",
                                       size =15),
            axis.text.y = element_text(family = "Tahoma",
                                       face = "bold",
                                       colour = "white",
                                       size =15)
) 
    
  }, bg = "transparent")
  
  output$coral_pie <- renderPlot({
    slices <- c(0, 12.9, 26.4, 34.9, 25.8)
    lbls <- c("Bottom - 0%", "Inside - 12.9%", "Side - 26.4%", "Top - 34.9%", "Under - 25.4%")
    pie(slices, labels = lbls, main = "Average Percent of Corals Bleached at each Bommie Location")+ 
      theme_minimal()
  }) ### End of coral plot
  
  
   highlight_location <- reactive({
     lat_lon_cor %>% 
       filter(site %in% input$location)
   })

   output$coral_map <-renderLeaflet({
     content <- paste(sep = "Number of Corals")

     leaflet() %>% 
               addProviderTiles(providers$OpenStreetMap.Mapnik) %>% 
               addCircleMarkers(data = highlight_location(), ~long, ~lat, label = ~htmlEscape(site), popup = paste
                          ("<br>Site Number: ", 
                            htmlEscape(lat_lon_cor$site), 
                            "<br>Number of Corals: ", 
                            htmlEscape(lat_lon_cor$total_n)
                          )
               )
   })

  
  ### Start of predictor tab 
   
  user_df <- reactive({
    message('in user_df, input$site = ', input$site) 
    message('in user_df, input$length = ', input$length)
    message('in user_df, input$width = ', input$width) 
    data.frame(
      site = as.numeric(input$site),
      length = as.numeric(input$length),
      width = as.numeric(input$width))
  })
  
  output$bar <- renderPlot({
    pred <- predict(coral_blr1, user_df(),
                    type = 'response')
    df <- tribble(
      ~ species,     ~ prob,
      'Pocillopora', pred,
      'Acropora',  1 - pred) 
    
  ggplot(df, x=1, aes(x = species, y = prob, fill = species)) +
    geom_col() +
    scale_fill_manual(values = c("#A7D0D9", "#E5E6FB")) +
      theme_minimal() + 
      labs(x = "Species", y = "Probability") +
    theme(text = element_text(colour = "white", family = "Tahoma", face = "bold", size = 15),
          axis.text.x = element_text(family = "Tahoma",
                                     face = "bold",
                                     colour = "white",
                                     size =15),
          axis.text.y = element_text(family = "Tahoma",
                                     face = "bold",
                                     colour = "white",
                                     size =15)
          
    ) 
       
  },  bg = "transparent")
  
  ### End of predictor server
  
  ### Table Tab:
  site_reactive <- reactive({
    message('in site_reactive, input$site_select = ', input$site_select) 
    sites_renamed %>% 
      filter(Site == input$site_select)
  })
  
  output$table <- renderTable({
    site_reactive()
  })
  
  output$meta <- renderTable({
    site_reactive(metadata)
  })
  
  ### Citation Tab:
  output$meta = renderDT({
    datatable(metadata,
                options = list(dom = 'tip', searchHighlight = TRUE),
                style = 'bootstrap',
                class = 'cell-border stripe',
                filter = 'top',
                rownames = FALSE) %>% 
      formatStyle(
        searchHighlight = TRUE,
        columns = c("Metadata", "Description"),
        color = "darkgrey",
        background = "white")
  })
  
}


### Run the application: 
shinyApp(ui = ui, server = server)


