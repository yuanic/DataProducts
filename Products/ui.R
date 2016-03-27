library(shiny)

# ui.R

shinyUI( fluidPage(
  titlePanel("Validating Credit Card Numbers"),
  br(),
  br(),
  
  textInput("cc_number15","Enter the 1st 15 Digits of the Credit Card No. :",value=""),   
  
  actionButton("checkButton","Find the 16th Digit!"),
  
  br(),
  br(),
  
  div(textOutput(outputId="len15"),style="color:red"),
  
  br(),
  
  p(strong("This is your 16th digit of your credit card number:")),
   textOutput(outputId="d16_result")
   ))
      
