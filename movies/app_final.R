rm(list = ls())

library(shiny)
library(shinydashboard)   #  framework for dashboards
library(shinyjs)          #  Visual effects, animation of pages
library(tidyverse)
library(recommenderlab)
library(reshape2)         #  work with matrices
library(shinyWidgets)     #  extra widgets

########based on the chosen movies, make predictions  about good movies using highly rated
#users and a model ---------------------

# read data ----
data("MovieLense")
# use data from the users with higher ratings
MovieLense100 <- MovieLense[rowCounts(MovieLense) >100,]

# make a model for prediction, use "POPULAR" algo as an example
train <- MovieLense100[1:50]
model <- Recommender(train, method = "POPULAR")

# UI ----
dash_header <- dashboardHeader(title = "Recommender system")
dash_sidebar <-    dashboardSidebar(
  width = 300,
  sidebarMenu(
    id = "sidebar_menu",
    menuItem("Test project", tabName = "recommender", selected = TRUE),
    uiOutput("select_movies"),
    uiOutput("select_n_recommends"),
    actionButton("go", "Find movies")
    )
  )

dash_body <-  dashboardBody(
  #
  tags$style(type="text/css",
             ".shiny-output-error { visibility: hidden; }",
             ".shiny-output-error:before { visibility: hidden; }"),
  tabItems(
    # tab ONE ----
    tabItem(
      tabName = "recommender",
      fluidRow(
        box(
          title = "Chosen movies",
          width = NULL,
          dataTableOutput("show_selected")
        )
      ),
      fluidRow(
        box(
          title = "Recommended movies",
          width = NULL,
          dataTableOutput("show_pred")
        )
      )
    )
  )


)
ui <- dashboardPage(dash_header, dash_sidebar, dash_body, skin = "purple")

server <- function(input, output, session) {

  output_pred <- eventReactive(input$go, {
    # use the model  and input to  choose best movies
    predict <- model %>%
      predict(pred_matrix(), n = input$select_n_recommends.server)

    as(predict, "list") %>%
      as_tibble()
  })

  output$select_n_recommends <- renderUI({
    sliderInput(
      "select_n_recommends.server",
      label = "Set the number of recommended movies",
      min = 1,
      max = 15,
      value = 4
    )
  })


  ###Here we choose movies----
  output$select_movies <- renderUI({
    pickerInput(
      "select_movies.server",
      label = "Movies",
      width = 280,
      choices = colnames(MovieLense100),
      #choices = colnames(MSWeb),
      options = list(`actions-box` = TRUE,
                     `live-search` = TRUE,
                     `select-all-text` = "Everything",
                     `deselect-all-text` = "Nothing"),
      inline = TRUE,
      multiple = TRUE
    )
  })

  ### make an input table with movies ----
  input_table <- reactive({
    input$select_movies.server %>%
      as_tibble()
  })

  # main function, here we prepare a matrix with test movies to feed into the model and
  # get predictions----

  pred_matrix <- reactive({
    #read th chosen movies
    test_samples <- input_table() %>% pull(value)

    #make a template using a random user from the highly rated sample of users
    OneRandomNumber <- sort(sample.int(100, 1))
    data <- MovieLense100[OneRandomNumber]
    #formatting
    test_prime <- as(data, "matrix")

    # what we basically need regarding the template is the movie names
    colnames <- colnames(data)

    # based on the input assign either highest or lowest rate to each movie from
    # the template
    for (colname in colnames){
      for (value in test_samples){
          if (colname == value){
              test_prime[,colname ] <- 5
          }else{
              if (!is.na(test_prime[,colname])){
              test_prime[,colname ] <- 1
        }
      }
      }
    }

    #formatting
    test_prime <- as.matrix(test_prime)
    row.names(test_prime) <- "value"
    as(test_prime, "realRatingMatrix")
  })

  ### settings for output ----
  output$show_selected <- renderDataTable(
    options = list(pageLength = 10),
    input_table()
  )

  ### format the output ----
  output$show_pred <- renderDataTable(
    options = list(pageLength = 10),
    output_pred()
  )

}

shinyApp(ui, server)
