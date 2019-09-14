#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)

## Supress message 
suppressPackageStartupMessages({
    library(tidyverse)
    library(stringr)
})

#' Source ngram matching function
source("ngram.R")



# Define UI for application that draws a histogram
ui <- fluidPage(

    # Application title
    titlePanel("Capstone Data Science - Final Project Submission"),

    # Sidebar with a slider input for number of bins 
    sidebarLayout(
        sidebarPanel(
            h2("Instructions:"), 
            h5("1. Enter a word or words in the text box."),
            h5("2. The predicted next word prints below it in blue."),
            h5("3. No need to hit enter of submit."),
            h5("4. A question mark means no prediction, typically due to mis-spelling"),
            h5("5. Additional tabs show plots of the top ngrams in the dataset"),
            br(),
            a("Source Code", href = "")
            
        ),

        # Show a plot of the generated distribution
        mainPanel(
            tabsetPanel(
                tabPanel("predict",
                         textInput("user_input", h3("Your Input:"), 
                                   value = "Your words"),
                         h3("Predicted Next Word:"),
                         h4(em(span(textOutput("ngram_output"), style="color:blue")))),
                
                tabPanel("top quadgrams",
                         br(),
                         img(src = "quadgrams.png", height = 500, width = 700)),
                
                tabPanel("top trigrams",
                         br(),       
                         img(src = "trigrams.png", height = 500, width = 700)),
                
                tabPanel("top bigrams",
                         br(),
                         img(src = "bigrams.png", height = 500, width = 700))
            )   
        )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output) {

    output$ngram_output <- renderText({
        ngrams(input$user_input)
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
