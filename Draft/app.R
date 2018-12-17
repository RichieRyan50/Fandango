library(shiny)
library(haven)
library(sjlabelled)
library(rsconnect)
library(shinythemes)
library(knitr)
library(scales)
library(stargazer)
library(ggrepel)
library(tidyverse)

#Reading in data
compare <- read_csv("fandango_score_comparison.csv")
scrape <- read_csv("fandango_scrape.csv")

#Determining shinytheme
ui <- fluidPage(theme = shinytheme("flatly"),

#Plugging in movie rating sites for x and y axes
                navbarPage("Movie Rating Site Comparison",
                           tabPanel("Graph",
                                    sidebarLayout(
                                      sidebarPanel(
                                        selectInput("x_axis",
                                                    "Movie Rating Sites' Scores (x-axis)",
                                                    choices = c("Rotten Tomatoes" = "RT_norm", 
                                                                "Metacritic" = "Metacritic_norm", 
                                                                "IMDB" = "IMDB_norm", 
                                                                "Fandango" = "Fandango_Ratingvalue")), 
                                        tags$h6(helpText("These film scores have been normalized to a 0 to 5 point system")), 
                                        
                                        br(),
                                        
                                        selectInput("y_axis", 
                                                    "Movie Rating Sites' Scores (y-axis)", 
                                                    choices = c("Rotten Tomatoes" = "RT_norm", 
                                                                "Metacritic" = "Metacritic_norm", 
                                                                "IMDB" = "IMDB_norm",
                                                                "Fandango" = "Fandango_Ratingvalue")), 
                                        tags$h6(helpText("These film scores have been normalized to a 0 to 5 point system")) 
                                        
                                      ),
                                      
                                      
                                      mainPanel(
                                        plotOutput("plot"),
                                        htmlOutput("summary")
                                      )
                                    )
                           )
                )
)



server <- function(input, output) {

#Determining reactives for x and y axes inputs 
  x_label <- reactive({
    req(input$x_axis) 
    if(input$x_axis == "RT_norm"){
      x_label <- "RottenTomatoes"
    } else if(input$x_axis == "Metacritic_norm"){
      x_label <- "Metacritic"
    } else if(input$x_axis == "IMDB_norm"){
      x_label <- "IMDB"
    } else if(input$x_axis == "Fandango_Ratingvalue"){
      x_label <- "Fandango"
    }})
  
  y_label <- reactive({
    req(input$y_axis) 
    if(input$y_axis == "RT_norm"){
      y_label <- "RottenTomatoes"
    } else if(input$y_axis == "Metacritic_norm"){
      y_label <- "Metacritic"
    } else if(input$y_axis == "IMDB_norm"){
      y_label <- "IMDB"
    } else if(input$y_axis == "Fandango_Ratingvalue"){
      y_label == "Fandango"
    }})
  
  
  output$plot <- renderPlot({
    
    movie_ratings <- left_join(compare, scrape, by = "FILM")
    
#Creating scatterplot of film ratings from sites
    movie_ratings %>% 
      ggplot(aes_string(x = input$x_axis, y = input$y_axis)) + 
      geom_point() + 
      geom_smooth(method = "lm") + 
      labs(x = x_label(),
           y = y_label(),
           title = "Correlation of Movie Scores Among Popular Movie Rating Sites",
           subtitle = "Fandango seems to show the weakest and most erratic correlation when compared to the other sites",
           caption = "Data taken from FiveThirtyEight")
    
  })
}

#Run the application 
shinyApp(ui = ui, server = server)

