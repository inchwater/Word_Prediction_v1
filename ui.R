library(shiny)
library(shinythemes)
library(dplyr)
library(xlsx)

#define ui
ui <- fluidPage(theme = shinytheme("cyborg"),
        
        #Title of the app
        titlePanel("Word Predictor v1"),
        
        #sidebar layout with input and output definitions
        sidebarLayout(
                
                #sidebar panel for inputs
                sidebarPanel(
                        
                        #text input for entering words
                        textInput ("words", "Enter words here:", "ice")
                       
                        
                        
                ),
                #main panel for displaying the output
        mainPanel(
                span(textOutput ("predicted"), style = "color:red")
                
                 )        
        )
        
)



    
 
