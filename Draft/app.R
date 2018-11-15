library(shiny)
library(tidyverse)


compare <- read_csv("fandango_score_comparison.csv")
scrape <- read_csv("fandango_scrape.csv")

# Define UI for application that draws a histogram
ui <- fluidPage(
  
  # Application title
  titlePanel("Movie Ratings on Fandango"),
  
  # Sidebar with a slider input for number of bins 
  sidebarLayout(
    sidebarPanel(
      selectInput(inputId = "Fandango_Stars",
                  label = "Fandango Stars:",
                  choices = c(3.0,
                              3.5,
                              4.0,
                              4.5,
                              5.0))
    ),
    
    # Show a plot of the generated distribution
    mainPanel(
      plotOutput("fandstarPlot")
    )
  )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
  
  output$fandstarPlot <- renderPlot({
    
    movie_ratings <- left_join(compare, scrape, by = "FILM")
    
    movie_ratings <- movie_ratings %>% 
      filter(Fandango_Stars == input$Fandango_Stars)
    
    
    ggplot(data = movie_ratings, aes(x = RATING)) + 
      geom_histogram(na.rm = TRUE, stat = "count") + 
      labs(x = "Fandango Absolute Rating", 
           title = "Count of Films' Fandango Stars and Absolute Ratings")
  })
}

# Run the application 
shinyApp(ui = ui, server = server)

