#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(shinyWidgets)
library(ggplot2)
library(gganimate)
library(hrbrthemes)
install.packages("reshape2") 
library(reshape2)


breed_rank_data <- read.csv("./breed_rank.csv")
breed_rank_data <- breed_rank_data[,-c(10:11)]
breeds <- breed_rank_data$Breed
years <- c("2013", "2014", "2015", "2016",
           "2017", "2018", "2019", "2020")
print(breeds)
print(names(breed_rank_data))
names(breed_rank_data) <- c("Breed", "2013", "2014", "2015", "2016",
                            "2017", "2018", "2019", "2020")

print(names(breed_rank_data))
breed_rank_data <- as.data.frame(t(breed_rank_data))
# Update the column names 
colnames(breed_rank_data) <- breeds
# Removes the first row of repeated breed names
breed_rank_data <- breed_rank_data[-1,]
testing <- colnames(breed_rank_data)
print("Hello1")
print(head(breed_rank_data))
breed_rank_data$year <- c("2013",  "2014",  "2015",  "2016",  "2017",  "2018",
                          "2019",  "2020")
print("Hello2")
print(breed_rank_data$year)
breed_rank_data <- melt(breed_rank_data, id.vars=c("year"))
# , variable.name = "Breed"

# , value.name = "Rank"
# print(breed_rank_data)

# Tab 1 : Breed Rankings
breed_ranking_tab <- tabPanel(
  'Breed Rankings',
  plotOutput("plot_rankings"),
  titlePanel('Dog Breeds in United States'),
  p('Australia\'s population has been growing consistently since the 1830s.'),
  p(img(src = 'https://upload.wikimedia.org/wikipedia/commons/thumb/2/25/Australia_Day_%282049745267%29.jpg/800px-Australia_Day_%282049745267%29.jpg')),
  p('Data source: ',
    a(href = 'https://www.abs.gov.au/statistics/people/population', 'Australian Bureau of Statistics'))
)

# Content for Tab 2: Trait Scores
main_content <- fluidPage(
  
  # Sidebar with a slider input for number of bins 
  sidebarLayout(
    sidebarPanel(
      
      span(tags$i(h6("Reported cases are subject to significant variation in testing policy and capacity between countries.")), style="color:#045a8d"),
      span(tags$i(h6("Occasional anomalies (e.g. spikes in daily case counts) are generally caused by changes in case definitions.")), style="color:#045a8d"),
      
      pickerInput("year", label = "Year:",   
                  choices = c('2013'='X2013.Rank',
                              '2014'='X2014.Rank',
                              '2015'='X2015.Rank',
                              '2016'='X2016.Rank',
                              '2017'='X2018.Rank',
                              '2019'='X2019.Rank',
                              '2020'='X2020.Rank'),
                  selected = 'X2020.RANK',
                  multiple = FALSE),
      
      pickerInput("breeds_select", "Breeds:",   
                  choices = breeds, 
                  selected = c("Retrievers (Labrador)", "French Bulldogs",
                               "German Shepherd Dogs", "Retrievers (Golden)", "Bulldogs"),
                  multiple = TRUE,
                  options =  list("max-options" = 3)),
      
      
      "Select outcome, regions, and plotting start date from drop-down menues to update plots. Countries with at least 1000 confirmed cases are included."
    ),
    
    mainPanel(
      tabsetPanel(
        tabPanel("Rank", plotOutput("plot_rankings")),
        tabPanel("New"),
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

# Define server logic required to draw a histogram
server <- function(input, output) {

  output$plot_rankings <- renderPlot({
    p <- ggplot(data=breed_rank_data, aes(x=as.numeric(year),
                                      y=value,
                                      color=variable)) +
      geom_line() +
      geom_point() +
      ggtitle("Top Breeds in US")
  })
}

# Run the application 
shinyApp(ui = ui, server = server)
