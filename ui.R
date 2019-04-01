#ui.R
library(shiny)
library(shinyWidgets)
library(shiny)

genre_list <- c("Action", "Adventure", "Animation", "Children", 
                "Comedy", "Crime","Documentary", "Drama", "Fantasy",
                "Film.Noir", "Horror", "Musical", "Mystery","Romance",
                "Sci.Fi", "Thriller", "War", "Western")

shinyUI(fluidPage(
  setBackgroundColor(
    color = c("#F7FBFF", "#2171B4"),
    gradient = "radial",
    direction = c("top", "left")
  ),
  titlePanel("Recommender of Movies // Dieudonne Ouedraogo"),
  fluidRow(
    
    column(4, h3("Select three genres in descending preference:"),
           wellPanel(
      selectInput("input_genre", "First Genre",
                  genre_list),
      selectInput("input_genre2", "Second Genre",
                  genre_list),
      selectInput("input_genre3", "Third Genre",
                  genre_list)
      #submitButton("Update List of Movies")
    )),
    
    column(4, h3("Select movies in those genres"),
           wellPanel(
      # This outputs the dynamic UI component
      uiOutput("ui"),
      uiOutput("ui2"),
      uiOutput("ui3")
      #submitButton("Get Recommendations")
    )),
    
    column(4,
           h3("We recommend you, those movies below!"),
           tableOutput("table")
           
  ))
))