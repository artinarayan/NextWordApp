

library(shiny)
suppressPackageStartupMessages({
library(RSQLite)
library(data.table)
})


source('predictWord.R')


ui <- fluidPage(

    # Application title
    titlePanel("Next Word Prediction"),
    p("Next Word App takes an input phrase (multiple words) in a text box and predicts the next word."),
    

    
    sidebarLayout(
         sidebarPanel(
            h4("Instructions:"),
            h6("1. Enter words to see next possible words"),
            h6("2. Go to About Next Word tab to know how the app works"),
            hr(), 
            h4("Examples:"),
            h6("Chances to"),
            h6("Once upon a"),
            h6("Many many"),
            h6("Would mean the"),
            h6("Quite some"),
            h6("Can you")
            
            
        ),
        
        
        mainPanel(
            tabsetPanel(tabPanel("Predict", 
                                 textInput('InputText', ""),
                                 h5("Possible next words:"),
                                 verbatimTextOutput("prediction")
                                 ),
                        tabPanel("About Next Word",
                                 br(),
                                 p('Application predicts next word to enter based on the already entered words. N-grams were built using twitter, blogs and news corpora. The algorithm:
                                   ', align='left'),
                                 tags$li("takes in the last words (n-1) of the user input"),
tags$li("uses the n-grams to match the last (n-1) words of the sentence") ,
tags$li("The most likely next word is then predicted, the last word of the n-grams that has sufficient probability.")


                                 )
                        
                        )
           
        )
    )
) #termsSimple


server <- function(input, output,session) {

    output$simpleSentence <- renderPrint({input$InputText})
    output$prediction = reactive({paste0(phrases()[1]$V1, "\n",phrases()[2]$V1,"\n",phrases()[3]$V1) })
    
    
    phrases <- reactive({
        nextWords =  getPossibleWords(paste(input$InputText, collapse = " "), 40)
    })
    
    
}

# Run the application 
shinyApp(ui = ui, server = server)
