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


breed_rank_data <- read.csv("./breed_rank.csv")

# Tab 1 : Breed Rankings
breed_ranking_tab <- tabPanel(
  'Breed Rankings',
  
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
      
      pickerInput("level_select", "Level:",   
                  choices = c("Global", "Continent", "Country", "US state"), 
                  selected = c("Country"),
                  multiple = FALSE),
      
      pickerInput("outcome_select", "Outcome:",   
                  choices = c("Deaths per million", "Cases per million", "Cases (total)", "Deaths (total)"), 
                  selected = c("Deaths per million"),
                  multiple = FALSE),
      
      pickerInput("start_date", "Plotting start date:",   
                  choices = c("Date", "Week of 100th confirmed case", "Week of 10th death"), 
                  options = list(`actions-box` = TRUE),
                  selected = "Date",
                  multiple = FALSE), 
      
      "Select outcome, regions, and plotting start date from drop-down menues to update plots. Countries with at least 1000 confirmed cases are included."
    ),
    
    mainPanel(
      tabsetPanel(
        tabPanel("Cumulative", plotlyOutput("country_plot_cumulative")),
        tabPanel("New", plotlyOutput("country_plot")),
        tabPanel("Cumulative (log10)", plotlyOutput("country_plot_cumulative_log"))
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
