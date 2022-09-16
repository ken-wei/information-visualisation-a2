#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

install.packages(c("reshape2", "shiny", "shinyWidgets", "ggplot2", 
                   "hrbrthemes", "reshape", "reshape2",
                   "dplyr", "echarts4r","shinythemes",
                   "shinyjs", "shinyBS", "plotly", "RColorBrewer")) 
library(shiny)
library(shinythemes)
library(shinyWidgets)
library(ggplot2)
library(hrbrthemes)
library(reshape)
library(reshape2)
library(dplyr)
library(RColorBrewer) # Color scheme 
library(plotly)
library(echarts4r) # To draw radar plot
library(shinyjs) # To disable html form element
library(shinyBS) # Tooltip for the elements


######################################
# Reading data from the csv files    #
######################################

# Ranking data from csv
breed_rank_data <- read.csv("./breed_rank.csv")
# Sort the data based on breed name
breed_rank_data <- breed_rank_data[order(breed_rank_data$Breed),] 
# Traits data of each breed
breed_traits_data <- read.csv("./breed_traits.csv")
# Store the images url into a dataframe
breed_images <- breed_rank_data[c("Breed", "Image")]
# Getting only years colnames from the rank data data
breed_rank_data <- breed_rank_data[,-c(10:11)]
breeds <- breed_rank_data$Breed # all of the dog breeds
years <- c("2013", "2014", "2015", "2016",
           "2017", "2018", "2019", "2020")
names(breed_rank_data) <- c("Breed", "2013", "2014", "2015", "2016",
                            "2017", "2018", "2019", "2020")

#####################################
#        Processing section         #
#####################################
# Process the data frame of rank data to fit line plot and bar chart
breed_rank_data <- as.data.frame(t(breed_rank_data))
# Update the column names 
colnames(breed_rank_data) <- breeds
# Removes the first row of repeated breed names
breed_rank_data <- breed_rank_data[-1,]
breed_rank_data$year <- c("2013",  "2014",  "2015",  "2016",  "2017",  "2018",
                         "2019",  "2020")
breed_rank_data <- as.data.frame(melt(breed_rank_data, id=c("year")))
breed_filter <- c("Retrievers (Labrador)", "French Bulldogs",
                "German Shepherd Dogs", "Retrievers (Golden)", "Bulldogs")

###########################
# Traits Sections for data#
###########################

traits_all <- c("Affectionate.With.Family", "Good.With.Young.Children", 
                "Good.With.Other.Dogs", "Shedding.Level", "Coat.Grooming.Frequency", 
                "Drooling.Level", "Openness.To.Strangers", "Playfulness.Level", 
                "Watchdog.Protective.Nature",
                "Adaptability.Level", "Trainability.Level","Energy.Level","Barking.Level",
                "Mental.Stimulation.Needs")
traits_family <- c("Affectionate.With.Family", "Good.With.Young.Children", 
                   "Good.With.Other.Dogs")
traits_physical <- c("Shedding.Level", "Coat.Grooming.Frequency", 
                     "Drooling.Level")
traits_social <- c("Openness.To.Strangers", "Playfulness.Level", 
                   "Watchdog.Protective.Nature", "Adaptability.Level")
traits_personality <- c("Trainability.Level","Energy.Level","Barking.Level",
                        "Mental.Stimulation.Needs")

# Radar plot function for each traits of different categories
traits_radar_func <- function(dataframe, string) {
  
  print("inside traits function")
  df <- data.frame(
    x = gsub('\\.', '\n', colnames(dataframe)),
    y = as.numeric(dataframe[1,]) # Convert dataframe to single
  )
  
  df |>
    e_charts(x) |>
    e_y_axis(axisLabel = list(color = 'black', fontWeight = 'bolder')) |>
    e_radar(y, max = 5, name = "Trait Scores rated 0 to 5", 
            radar = list(axisTick = list(show = FALSE), 
            axisLabel = list(show = TRUE, showMinLabel = FALSE, 
                             showMaxLabel = FALSE), splitNumber=5),
            areaStyle = list(
              color = "SkyBlue"
            )) |>
    e_tooltip(trigger = "item") |>
    e_legend(show = FALSE)
    
}

##################
# USER INTERFACE #
##################

# Tab 1 : Breeds Comparison
breed_compare_tab <- tabPanel(
  'Breeds Comparison',
  width = 10,
  fluidPage(
    titlePanel(
      h3("Dog Breeds Comparison", align = "center",
         style = "border-top: 2px lightblue solid;
                  border-bottom: 2px lightblue solid;
                  padding: 25px 5px;
                  margin: 0")
    ),
    fluidRow(
      style = "background: white;",
      chooseSliderSkin("Flat", color = "DodgerBlue"),
      tags$head(
      # Html styling for the tab content (align center)
      tags$style(HTML("/* Change opacity of the traits scores */
                      .disabled span {
                        opacity: 0.9;
                      }
                      .tab-content{
                        padding-top: 10px;
                      }
                      .tabbable ul {
                        display: inline-block;
                      }"))
    ),
    column(
        12,
        fluidRow(
        column(class = ".flex-center",
              style = "text-align: center;
              border-right:1px solid #E2DED0;",
              width = 4, offset = 2,
              pickerInput("breed_select", h5("Breed: (First Selection)"),
                          choices = breeds,
                          width = '100%',
                          selected = "German Shepherd Dogs",
                          multiple = FALSE),
              uiOutput("breed_image" ),
              switchInput(inputId = "radar_toggle", value = TRUE, 
                          onLabel = "Radar", offLabel = "Score", 
                          width = '100%'),
              tabsetPanel(
                  tabPanel("Family Life", 
                           "How well the dog breed is affectionated with family, children and other dogs.", 
                           uiOutput("breed_traits_family_fc")),
                  tabPanel("Physical", 
                           "Characteristics based on the level of shedding, grooming frequency and drooling of the breed.",
                           uiOutput("breed_traits_physical_fc")),
                  tabPanel("Social", 
                           "The level of openness, friendliness, playfulness, protective nature of its owner and adaptability of new environments.", 
                           uiOutput('breed_traits_social_fc')),
                  tabPanel("Personality", 
                           "The mental needs, vocal of breeds, difficulty of training and how energetic they are.", 
                           uiOutput('breed_traits_personality_fc'))),
                  # tabPanel("All", echarts4rOutput('breed_traits_all')))
        ),
        column(style = "text-align: center;border-left:1px solid #E2DED0;",
               width = 4,
               pickerInput("breed_select_compare", 
                           h5("Breed: (Second Selection)"),   
               choices = breeds,
               width = '100%',
               selected = "Retrievers (Labrador)",
               multiple = FALSE,),
               uiOutput("breed_image_s" ),
               switchInput(inputId = "radar_toggle_sec", value = TRUE, 
                          onLabel = "Radar", offLabel = "Score", width = '100%'),
               tabsetPanel(
                 tabPanel("Family Life", 
                          "How well the dog breed is affectionated with family, children and other dogs.",
                          uiOutput("breed_traits_family_sc")),
                 tabPanel("Physical", 
                          "Characteristics based on the level of shedding, grooming frequency and drooling of the breed.", 
                          uiOutput("breed_traits_physical_sc")),
                 tabPanel("Social", 
                          "The level of openness, friendliness, playfulness, protective nature of its owner and adaptability of new environments.", 
                          uiOutput('breed_traits_social_sc')),
                 tabPanel("Personality", 
                          "The mental needs, vocal of breeds, difficulty of training and how energetic they are.", 
                          uiOutput('breed_traits_personality_sc')),
                # tabPanel("All", echarts4rOutput('breed_traits_all')))
                )
          )
        )
      )
    )
  )
)



# Content for Tab 2: Popularity trend (Line plot and bar chart)
main_content <- fluidPage(
  
  # Sidebar with a slider input for number of bins 
  sidebarLayout(
    fluidRow(
      column(width = 8, offset = 2,
        pickerInput("breeds_select", "Breeds:",   
                    choices = breeds, 
                    selected = c("Retrievers (Labrador)", "French Bulldogs",
                                 "German Shepherd Dogs"),
                    multiple = TRUE,
                    options =  list("max-options" = 5), width = '100%'),
      ),
      column(width = 8, offset = 2,
        pickerInput("years_select", "Years:",   
                    choices = c("2013",  "2014",  "2015",  "2016",  "2017",  "2018",
                                "2019",  "2020"), 
                    selected = c("2013",  "2014",  "2015",  "2016",  "2017",  "2018",
                                 "2019",  "2020"),
                    multiple = TRUE, width = '100%'),
      )
    ),
    fluidRow(
      column(width = 8, offset = 2,
        tabsetPanel(
          tabPanel("Trendline", plotlyOutput('breed_ranks_line')),
          tabPanel("Bar Chart", plotlyOutput('breed_ranks_bar'))
        )
      )
    )
  )
)

# Tab 2: Popularity trend
traits_tab <- tabPanel(
  'Popularity Trend',
  shinyjs::useShinyjs(),
  h3("Popularity of Dog Breeds", align = "center",
     style = "border-top: 2px lightblue solid;
                  border-bottom: 2px lightblue solid;
                  padding: 25px 5px;
                  margin: 10"),
  main_content
)

# Define UI for the whole application consists of two navbar tabs
ui <- navbarPage(
  'Dog Breed',
  breed_compare_tab,
  traits_tab,
  theme = shinytheme("paper")
)

################
# SHINY SERVER #
################

server <- function(input, output, session) {
  
  # UI output of the breed image based on reactive breed selections (First)
  output$breed_image <- renderUI({
    df <- filter(breed_images, breed_images$Breed == input$breed_select)
    image_breed <- df[2]
    div(id = "image-breed",
        tags$img(src = image_breed, width = 130, height = 130, marginBottom = 10)
    )
  })
  
  # UI output of the breed image based on reactive breed selections (second)
  output$breed_image_s <- renderUI({
    df <- filter(breed_images, breed_images$Breed == input$breed_select_compare)
    image_breed <- df[2]    
    div(id = "image-breed",
        tags$img(src = image_breed, width = 130, height = 130, marginBottom = 10)
    )
  })
  
  # Output plotly trend line object for the "Popularity Trend" tab
  output$breed_ranks_line <- renderPlotly({
    filtered_data <- filter(breed_rank_data, 
                            breed_rank_data$variable %in% input$breeds_select)
    filtered_data <- filter(filtered_data,
                            filtered_data$year %in% input$years_select)
    p <- ggplot(filtered_data, 
                aes(x=year, y=value, 
                    group=variable, 
                    color=variable,
                    text = paste("In Year", year, ",", variable,
                                 " achieved Rank", value))) +
      geom_line(size=0.6) + 
      geom_point(shape=20, size=3) +
      scale_y_discrete(limits=rev) +  # Reverse the order or rank values
      theme(legend.position = "top") +
      scale_color_brewer(palette = "Paired") +
      labs(
        title = "Popularity of Dog Breeds in the US through Year 2013-2020",
        y = "Rankings",
        x = "Years",
        color = "Selected Breeds\n"
      ) +
      theme(
        # Set background color to white
        panel.background = element_rect(fill = "white"),
        # Remove tick marks by setting their length to 0
        axis.ticks.length = unit(0, "mm"),
        # Remove the title for both axes
        axis.title.x = element_blank(),
        # Only left line of the vertical axis is painted in black
        axis.line.y.left = element_line(color = "black"),
        # But customize labels for the horizontal axis
        axis.text.x = element_text(size = 10,face="bold"),
        axis.title.y = element_text(size = 11),
        plot.title = element_text(margin = 10, hjust = 0.5, size = 16)
      )
    
    ggplotly(p)
    
    ay = list(
      zerolinecolor = '#fcfcfc',
      zerolinewidth = 1,
      gridcolor = '#f0f0f0',
      showticklabels = TRUE)
    
    ggplotly(p, tooltip = "text") %>%
      layout(
        yaxis = ay,
        xaxis = ay
      )
  })
  
  # Output plotly bar chart object for the "Popularity Trend" tab
  output$breed_ranks_bar <- renderPlotly ({
    ordered_data <- breed_rank_data[order(breed_rank_data$value),]
    p <- ggplot(data = filter(ordered_data, ordered_data$variable %in% input$breeds_select), 
           aes(x = year, 
               y = value, # Reverse the rankings from highest
               fill = variable, text = paste("In Year", year, ",", variable,
                                             " achieved Rank", value))) + 
      geom_col(position="dodge2", width = 0.5) + 
      scale_y_discrete(limits=rev) + # Reverse the order
      scale_fill_brewer(palette = "Paired") +
      theme(
        # Set background color to white
        panel.background = element_rect(fill = "white"),
        # Remove tick marks by setting their length to 0
        axis.ticks.length = unit(0, "mm"),
        # Remove the title for both axes
        axis.title.x = element_blank(),
        # Only left line of the vertical axis is painted in black
        axis.line.y.left = element_line(color = "black"),
        # But customize labels for the horizontal axis
        axis.text.x = element_text(size = 10,face="bold"),
        axis.title.y = element_text(size = 11)
      ) +
      labs(
        title = "Breed Popularity",
        y = "Rankings",
        x = "Years",
        fill = "Selected Breeds\n"
      ) + 
      theme(
        plot.title = element_text(margin = 10,
          size = 16,
          hjust = 0.5,
        ),
      ) 

    ay = list(
      zerolinecolor = '#D3D3D3',
      zerolinewidth = 1,
      gridcolor = '#f0f0f0',
      showticklabels = TRUE)
    
    ggplotly(p, tooltip = "text") %>%
      layout(
        yaxis = ay
      )
  })
  
  output$caption <- renderText({
    paste("Trait Scores rated 0 to 5")
  })
  
  # Change visualisation (Radar Plot and Slider visualisation)
  radar_or_slider <- reactiveVal(TRUE)
  radar_or_slider_sec <- reactiveVal(TRUE)
  
  # Observe the event for radar toggle button (First Choice)
  observeEvent(input$radar_toggle, {
    # Do something
    print(radar_or_slider())
    radar_or_slider(!radar_or_slider())
  })
  
  # Observe the event for radar toggle button (First Choice)
  observeEvent(input$radar_toggle_sec, {
    radar_or_slider_sec(!radar_or_slider_sec())
  })
  
  # First choice of breed for physical 
  output$breed_traits_family_fc <- renderUI({
    if (radar_or_slider()) {
      uiOutput('family')
    } else {
      echarts4rOutput('breed_traits')
    }
  })
  
  # First choice of breed for physical 
  output$breed_traits_physical_fc <- renderUI({
    if (radar_or_slider()) {
      uiOutput('physical')
    } else {
      echarts4rOutput('breed_traits_physical')
    }
  })
  
  # First choice of breed for social 
  output$breed_traits_social_fc <- renderUI({
    if (radar_or_slider()) {
      uiOutput('social')
    } else {
      echarts4rOutput('breed_traits_social')
    }
  })
  
  # First choice of breed for personality 
  output$breed_traits_personality_fc <- renderUI({
    if (radar_or_slider()) {
      uiOutput('personality')
    } else {
      echarts4rOutput('breed_traits_personality')
    }
  })
  
  # Second choice of breed for physical 
  output$breed_traits_family_sc <- renderUI({
    if (radar_or_slider_sec()) {
      uiOutput('family_s')
    } else {
      echarts4rOutput('breed_traits_s')
    }
  })
  
  # Second choice of breed for physical 
  output$breed_traits_physical_sc <- renderUI({
    if (radar_or_slider_sec()) {
      uiOutput('physical_s')
    } else {
      echarts4rOutput('breed_traits_physical_s')
    }
  })
  
  # Second choice of breed for social 
  output$breed_traits_social_sc <- renderUI({
    if (radar_or_slider_sec()) {
      uiOutput('social_s')
    } else {
      echarts4rOutput('breed_traits_social_s')
    }
  })
  
  # Second choice of breed for personality 
  output$breed_traits_personality_sc <- renderUI({
    if (radar_or_slider_sec()) {
      uiOutput('personality_s')
    } else {
      echarts4rOutput('breed_traits_personality_s')
    }
  })
  
  # Reactive expression for the first breed selection choice
  breed_selected <- reactive({
    breed_traits_data %>% filter(breed_traits_data$Breed == input$breed_select)
  })
  
  # Reactive expression for the second breed selection choice
  breed_compare_selected <- reactive({
    breed_traits_data %>% 
      filter(breed_traits_data$Breed == input$breed_select_compare)
  })
  
  output$breed_traits <- renderEcharts4r({
    df1 <- as.data.frame(breed_selected())
    df1 <- df1[, c("Affectionate.With.Family", 
                  "Good.With.Young.Children", 
                  "Good.With.Other.Dogs")]
    values <- tail(df1, 1)
    traits_radar_func(dataframe = df1, string = "Family Life Traits")
  })
  
  output$breed_traits_physical <- renderEcharts4r({
    df1 <- as.data.frame(breed_selected()) # reactive breed select input
    df1 <- df1[, c("Shedding.Level", "Coat.Grooming.Frequency", 
                   "Drooling.Level")]

    traits_radar_func(dataframe = df1, string = "Physical Traits")
  })
  
  output$breed_traits_social <- renderEcharts4r({
    df1 <- as.data.frame(breed_selected()) # reactive breed select input
    df1 <- df1[, c("Openness.To.Strangers", "Playfulness.Level", 
                   "Watchdog.Protective.Nature",
                   "Adaptability.Level")]
    traits_radar_func(dataframe = df1, string = "Social Traits")
  })
  
  output$breed_traits_personality <- renderEcharts4r({
    df1 <- as.data.frame(breed_selected()) # reactive breed select input
    df1 <- df1[, c("Trainability.Level","Energy.Level","Barking.Level",
                   "Mental.Stimulation.Needs")]
    traits_radar_func(dataframe = df1, string = "Personality Traits")
  })
  
  output$breed_traits_all <- renderEcharts4r({
    df1 <- as.data.frame(breed_selected()) # reactive breed select input
    df1 <- df1[, c("Affectionate.With.Family", 
                   "Good.With.Young.Children", 
                   "Good.With.Other.Dogs", "Shedding.Level", "Coat.Grooming.Frequency", 
                   "Drooling.Level", "Openness.To.Strangers", "Playfulness.Level", 
                   "Watchdog.Protective.Nature",
                   "Adaptability.Level", "Trainability.Level","Energy.Level","Barking.Level",
                   "Mental.Stimulation.Needs")]
    traits_radar_func(dataframe = df1, string = "All Traits")
    
  })
  
  # Helper function to create a set of slider + hover tooltip
  slider_generate <- function(traits_list, traits_id, values) {
    v <- list()
    for (i in 1:length(traits_list)){
      v[[i]] <- tagList(
        shinyjs::disabled(
          sliderInput(traits_id[i], gsub('\\.', ' ', traits_list[i]),
                      min = 0, max = 5, value = values[i], ticks = TRUE,
                      width = '100%'
          )),
        bsTooltip(traits_id[i], "HoverOnMe", placement = "bottom", trigger = "hover",
                  options = NULL)
      )
    }
    return(v)
  }
  
  # Render trait scores
  output$family <- renderUI({
    df <- as.data.frame(breed_selected())[, traits_family] 
    values <- as.numeric(df[1,])
    slider_generate(traits_family, c("fm-1","fm-2","fm-3"), values)
  })
  
  # Render trait scores
  output$physical <- renderUI({
    df <- as.data.frame(breed_selected())[, traits_physical] 
    values <- as.numeric(df[1,])
    slider_generate(traits_physical, c("phy-1","phy-2","phy-3"), values)
  })
  
  # Render trait scores
  output$social <- renderUI({
    df <- as.data.frame(breed_selected())[, traits_social] 
    values <- as.numeric(df[1,])
    slider_generate(traits_social, c("sc-1","sc-2","sc-3","sc-4"), values)
  })
  
  # Render trait scores
  output$personality <- renderUI({
    df <- as.data.frame(breed_selected())[, traits_personality] 
    values <- as.numeric(df[1,])
    slider_generate(traits_personality, c("per-1","per-2","per-3","per-4"), values)
  })
  
  # Render trait scores for all
  output$all <- renderUI({
    df <- as.data.frame(breed_selected())[, traits_all] 
    values <- as.numeric(df[1,])
    slider_generate(traits_all, 
                    c("al-1","al-2","al-3","al-4","al-5","al-6","al-7",
                "al-8","al-9","al-10","al-11","al-12","al-13","al-14"), values)
  })
  
  #####################################
  ##  For second selection comparison #
  #####################################
  
  # Output Radar plot for family traits second choice
  output$breed_traits_s <- renderEcharts4r({
    df <- as.data.frame(breed_compare_selected())[, traits_family]
    traits_radar_func(dataframe = df, string = "Family Life Traits")
  })
  
  # Output Radar plot for family traits second choice
  output$breed_traits_physical_s <- renderEcharts4r({
    df <- as.data.frame(breed_compare_selected())[, traits_physical]
    traits_radar_func(dataframe = df, string = "Physical Traits")
  })
  
  # Output Radar plot for social traits second choice
  output$breed_traits_social_s <- renderEcharts4r({
    df <- as.data.frame(breed_compare_selected())[, traits_social]
    traits_radar_func(dataframe = df, string = "Social Traits")
  })
  
  # Output Radar plot for personality traits second choice
  output$breed_traits_personality_s <- renderEcharts4r({
    df <- as.data.frame(breed_compare_selected())[, traits_personality]
    traits_radar_func(dataframe = df, string = "Personality Traits")
  })
  
  # Output Radar plot for all traits second choice
  output$breed_traits_all_s <- renderEcharts4r({
    df <- as.data.frame(breed_compare_selected())[, traits_all]
    traits_radar_func(dataframe = df, string = "All Traits")
  })
  
  # Render trait scores second choice (family life)
  output$family_s <- renderUI({
    df <- as.data.frame(breed_compare_selected())[, traits_family] 
    values <- as.numeric(df[1,])
    slider_generate(traits_family, c("fm2-1","fm2-2","fm2-3"), values)
  })
  
  # Render trait scores second choice (physical)
  output$physical_s <- renderUI({
    df <- as.data.frame(breed_compare_selected())[, traits_physical] 
    values <- as.numeric(df[1,])
    slider_generate(traits_physical, c("phy2-1","phy2-2","phy2-3"), values)
  })
  
  # Render trait scores second choice (social)
  output$social_s <- renderUI({
    df <- as.data.frame(breed_compare_selected())[, traits_social] 
    values <- as.numeric(df[1,])
    slider_generate(traits_social, c("sc2-1","sc2-2","sc2-3","sc2-4"), values)
  })
  
  # Render trait scores second choice (personality)
  output$personality_s <- renderUI({
    df <- as.data.frame(breed_compare_selected())[, traits_personality] 
    values <- as.numeric(df[1,])
    slider_generate(traits_personality, c("per2-1","per2-2","per2-3","per2-4"), values)
  })
  
  # Render trait scores for all second choice
  output$all_s <- renderUI({
    df <- as.data.frame(breed_compare_selected())[, traits_all] 
    values <- as.numeric(df[1,])
    slider_generate(traits_all, 
                    c("al2-1","al2-2","al2-3","al2-4","al2-5","al2-6","al2-7",
                      "al2-8","al2-9","al2-10","al2-11","al2-12","al2-13","al2-14"), values)
  })
  
  
  # Suspend the tabset panel when selecting other panel to prevent flickering
  outputOptions(output, "breed_traits_family_fc", suspendWhenHidden = FALSE)
  outputOptions(output, "breed_traits_physical_fc", suspendWhenHidden = FALSE)
  outputOptions(output, "breed_traits_social_fc", suspendWhenHidden = FALSE)
  outputOptions(output, "breed_traits_personality_fc", suspendWhenHidden = FALSE)
  outputOptions(output, "breed_traits_family_sc", suspendWhenHidden = FALSE)
  outputOptions(output, "breed_traits_physical_sc", suspendWhenHidden = FALSE)
  outputOptions(output, "breed_traits_social_sc", suspendWhenHidden = FALSE)
  outputOptions(output, "breed_traits_personality_sc", suspendWhenHidden = FALSE)
}

# Run the application 
shinyApp(ui, server)
