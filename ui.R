## ui.R
library(shiny)
library(shinydashboard)
library(recommenderlab)
library(data.table)
library(ShinyRatingInput)
library(shinyjs)

source('functions/helpers.R')

genre_list = c("Action", "Adventure", "Animation", 
               "Children's", "Comedy", "Crime",
               "Documentary", "Drama", "Fantasy",
               "Film-Noir", "Horror", "Musical", 
               "Mystery", "Romance", "Sci-Fi", 
               "Thriller", "War", "Western")

shinyUI(dashboardPage(
  skin = "green",
  dashboardHeader(title = "Movie Recommender"),
  
  dashboardSidebar(sidebarMenu(
    menuItem("Genres", tabName = "genre", icon = icon("film")),
    menuItem("Ratings", tabName = "rating", icon = icon("star"))
  )),
  
  dashboardBody(includeCSS("css/movie.css"),
                tabItems(
                  
                  # First tab content
                  tabItem(tabName = "genre",
                          selectInput("genre", "Genre:", 
                                      choices=genre_list),
                          selectInput("choices", "Number of Recommend:", 
                                      choices=c(5, 10, 15, 20, 25)),
                          uiOutput('genre')
                          
                  ),
                  
                  #Second tab content
                  tabItem(tabName = "rating",
                          fluidRow(
                            box(
                              width = 12,
                              title = "Step 1: Rate as many movies as possible",
                              status = "info",
                              solidHeader = TRUE,
                              collapsible = TRUE,
                              div(class = "rateitems",
                                  uiOutput('ratings'))
                            )
                          ),
                          
                          
                          fluidRow(
                            useShinyjs(),
                            box(
                              width = 12,
                              status = "info",
                              solidHeader = TRUE,
                              title = "Step 2: Discover movies you might like",
                              br(),
                              withBusyIndicatorUI(
                                actionButton("btn", "Click here to get your recommendations", class = "btn-warning")
                              ),
                              br(),
                              tableOutput("results")
                            )
                          ))
                ))
))
