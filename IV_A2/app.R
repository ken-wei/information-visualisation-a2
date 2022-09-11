#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

install.packages(c("reshape2", "shiny", "shinyWidgets", "ggplot2",
                   "gganimate", "hrbrthemes", "reshape", "reshape2",
                   "dplyr")) 
library(shiny)
library(shinyWidgets)
library(ggplot2)
library(gganimate)
library(hrbrthemes)
library(reshape)
library(reshape2)
library(dplyr)
library(viridis)
library(plotly)


breed_rank_data <- read.csv("./breed_rank.csv")
breed_rank_data <- breed_rank_data[,-c(10:11)]
breeds <- breed_rank_data$Breed
years <- c("2013", "2014", "2015", "2016",
           "2017", "2018", "2019", "2020")
names(breed_rank_data) <- c("Breed", "2013", "2014", "2015", "2016",
                            "2017", "2018", "2019", "2020")

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

##################
# USER INTERFACE #
##################

# Tab 1 : Breed Rankings
breed_ranking_tab <- tabPanel(
  'Breed Rankings',
  textOutput("caption")
)

# Content for Tab 2: Trait Scores
main_content <- fluidPage(
  
  # Sidebar with a slider input for number of bins 
  sidebarLayout(
    sidebarPanel(
      
      pickerInput("breeds_select", "Breeds:",   
                  choices = breeds, 
                  selected = c("Retrievers (Labrador)", "French Bulldogs",
                               "German Shepherd Dogs"),
                  multiple = TRUE,
                  options =  list("max-options" = 5)),
      
      pickerInput("years_select", "Years:",   
                  choices = c("2013",  "2014",  "2015",  "2016",  "2017",  "2018",
                              "2019",  "2020"), 
                  selected = c("2013",  "2014",  "2015",  "2016",  "2017",  "2018",
                               "2019",  "2020"),
                  multiple = TRUE),
    
    ),
    
    mainPanel(
      tabsetPanel(
        tabPanel("Trendline", plotlyOutput('breed_ranks_line')),
        tabPanel("Barplot", plotlyOutput('breed_ranks_bar'))
      )
    )
  )
)

# Tab 2: Trait Scores
traits_tab <- tabPanel(
  'Traits',
  titlePanel('Trait Scores by Breeds'),
  main_content
)

# Define UI for application that draws a histogram
ui <- navbarPage(
  'Dog Breed',
  breed_ranking_tab,
  traits_tab
)

################
# SHINY SERVER #
################

server <- function(input, output, session) {
  
  output$breed_ranks_line <- renderPlotly({
    print("hello world")
    filtered_data <- filter(breed_rank_data, 
                            breed_rank_data$variable %in% input$breeds_select)
    filtered_data <- filter(filtered_data,
                            filtered_data$year %in% input$years_select)
    p <- ggplot(filtered_data, 
                aes(x=year, y=value, 
                    group=variable, 
                    color=variable)) +
      geom_line(size=1) + 
      geom_point(shape=21, size=3) +
      theme_ipsum() +
      scale_y_discrete(limits=rev) +  # Reverse the order
      # transition_reveal(as.Date(year))
      theme(legend.position = "top") +
      # scale_color_viridis() +
      labs(
        title = "Popularity of Dog Breeds in the US through Year 2013-2020",
        y = "Rankings",
        x = "Years"
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
        # Remove labels from the vertical axis
        # axis.text.y = element_blank(),
        # But customize labels for the horizontal axis
        axis.text.x = element_text(family = "Econ Sans Cnd", size = 16),
        axis.title.y = element_text(family = "Econ Sans Cnd", size = 16)
      )
    
    ggplotly(p)
  })
  
  output$breed_ranks_bar <- renderPlotly ({
    ordered_data <- breed_rank_data[order(breed_rank_data$value),]
    p <- ggplot(data = filter(ordered_data, ordered_data$variable %in% input$breeds_select), 
           aes(x = year, 
               y = value, # Reverse the rankings from highest
               fill = variable)) + 
      geom_col(position="dodge2", width = 0.5) + 
      # coord_flip() +
      scale_y_discrete(limits=rev) + # Reverse the order
      # scale_y_discrete(position = "right") +
      # scale_x_reverse() +
      labs(
        title = "Rankings through years",
        y = "Rankings",
        x = "Years"
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
        # Remove labels from the vertical axis
        # axis.text.y = element_blank(),
        # But customize labels for the horizontal axis
        axis.text.x = element_text(family = "Econ Sans Cnd", size = 16),
        axis.title.y = element_text(family = "Econ Sans Cnd", size = 16)
      ) +
      labs(
        title = "Breed Popularity",
      ) + 
      theme(
        plot.title = element_text(
          family = "Econ Sans Cnd", 
          face = "bold",
          size = 22
        ),
        plot.subtitle = element_text(
          family = "Econ Sans Cnd",
          size = 20
        )
      ) 
      # geom_text(aes(label=value),
      #           position=position_dodge(width=1),
      #           colour = "black",
      #           family = "Econ Sans Cnd",
      #           size = 3)
    ay = list(
      zerolinecolor = '#D3D3D3',
      zerolinewidth = 1,
      gridcolor = '#D3D3D3',
      showticklabels = TRUE,
      tickfont = list(size = 11))
    
    ggplotly(p) %>%
      layout(
        yaxis = ay
      )
  })
  
  output$caption <- renderText({
    paste("Dog Breeds in the US.",
          input$year)
  })
  
}

# Run the application 
shinyApp(ui, server)
