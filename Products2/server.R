library(shiny)

shinyServer(function(input, output) {
  
  library(stringr)
  library(shinyBS)
  library(dplyr)
  library(plyr)
  
  ##Evaluating 15 digits inputed by user. 
  ##Users are required to enter 15 digits into the input field.
  ##The app will calculate and return the 16th digit on the Credit Card.
  ##This is one of the security functions behind credit card numbers.
  
    output$len15<-renderText({ if (str_length(input$cc_number15)!=15) 
      {"Please enter 15 digits ONLY!"
        } 
      else  
      {return("You have entered 15 digits.")
      }
  })
    
    
  ##calculating the 16th digit
    cc15<-renderText({input$cc_number15})
    output$d16_result<-eventReactive(input$checkButton ,{
      
      d1<-if(str_length(input$cc_number15)==15) {if((as.integer(substr(input$cc_number15,1,1))*2)==0) {0} else {if (((as.integer(substr(input$cc_number15,1,1))*2)%%9)==0) {9} else {((as.integer(substr(input$cc_number15,1,1))*2)%%9)}}} else {0}
      d2<-if(str_length(input$cc_number15)==15) {if((as.integer(substr(input$cc_number15,2,2))*1)==0) {0} else {if (((as.integer(substr(input$cc_number15,2,2))*1)%%9)==0) {9} else {((as.integer(substr(input$cc_number15,2,2))*1)%%9)}}} else {0}
      d3<-if(str_length(input$cc_number15)==15) {if((as.integer(substr(input$cc_number15,3,3))*2)==0) {0} else {if (((as.integer(substr(input$cc_number15,3,3))*2)%%9)==0) {9} else {((as.integer(substr(input$cc_number15,3,3))*2)%%9)}}} else {0}
      d4<-if(str_length(input$cc_number15)==15) {if((as.integer(substr(input$cc_number15,4,4))*1)==0) {0} else {if (((as.integer(substr(input$cc_number15,4,4))*1)%%9)==0) {9} else {((as.integer(substr(input$cc_number15,4,4))*1)%%9)}}} else {0}
      d5<-if(str_length(input$cc_number15)==15) {if((as.integer(substr(input$cc_number15,5,5))*2)==0) {0} else {if (((as.integer(substr(input$cc_number15,5,5))*2)%%9)==0) {9} else {((as.integer(substr(input$cc_number15,5,5))*2)%%9)}}} else {0}
      d6<-if(str_length(input$cc_number15)==15) {if((as.integer(substr(input$cc_number15,6,6))*1)==0) {0} else {if (((as.integer(substr(input$cc_number15,6,6))*1)%%9)==0) {9} else {((as.integer(substr(input$cc_number15,6,6))*1)%%9)}}} else {0}
      d7<-if(str_length(input$cc_number15)==15) {if((as.integer(substr(input$cc_number15,7,7))*2)==0) {0} else {if (((as.integer(substr(input$cc_number15,7,7))*2)%%9)==0) {9} else {((as.integer(substr(input$cc_number15,7,7))*2)%%9)}}} else {0}
      d8<-if(str_length(input$cc_number15)==15) {if((as.integer(substr(input$cc_number15,8,8))*1)==0) {0} else {if (((as.integer(substr(input$cc_number15,8,8))*1)%%9)==0) {9} else {((as.integer(substr(input$cc_number15,8,8))*1)%%9)}}} else {0}
      d9<-if(str_length(input$cc_number15)==15) {if((as.integer(substr(input$cc_number15,9,9))*2)==0) {0} else {if (((as.integer(substr(input$cc_number15,9,9))*2)%%9)==0) {9} else {((as.integer(substr(input$cc_number15,9,9))*2)%%9)}}} else {0}
      d10<-if(str_length(input$cc_number15)==15) {if((as.integer(substr(input$cc_number15,10,10))*1)==0) {0} else {if (((as.integer(substr(input$cc_number15,10,10))*1)%%9)==0) {9} else {((as.integer(substr(input$cc_number15,10,10))*1)%%9)}}} else {0}
      d11<-if(str_length(input$cc_number15)==15) {if((as.integer(substr(input$cc_number15,11,11))*2)==0) {0} else {if (((as.integer(substr(input$cc_number15,11,11))*2)%%9)==0) {9} else {((as.integer(substr(input$cc_number15,11,11))*2)%%9)}}} else {0}
      d12<-if(str_length(input$cc_number15)==15) {if((as.integer(substr(input$cc_number15,12,12))*1)==0) {0} else {if (((as.integer(substr(input$cc_number15,12,12))*1)%%9)==0) {9} else {((as.integer(substr(input$cc_number15,12,12))*1)%%9)}}} else {0}
      d13<-if(str_length(input$cc_number15)==15) {if((as.integer(substr(input$cc_number15,13,13))*2)==0) {0} else {if (((as.integer(substr(input$cc_number15,13,13))*2)%%9)==0) {9} else {((as.integer(substr(input$cc_number15,13,13))*2)%%9)}}} else {0}
      d14<-if(str_length(input$cc_number15)==15) {if((as.integer(substr(input$cc_number15,14,14))*1)==0) {0} else {if (((as.integer(substr(input$cc_number15,14,14))*1)%%9)==0) {9} else {((as.integer(substr(input$cc_number15,14,14))*1)%%9)}}} else {0}
      d15<-if(str_length(input$cc_number15)==15) {if((as.integer(substr(input$cc_number15,15,15))*2)==0) {0} else {if (((as.integer(substr(input$cc_number15,15,15))*2)%%9)==0) {9} else {((as.integer(substr(input$cc_number15,15,15))*2)%%9)}}} else {0}
      
      d16_pre<-(d1+d2+d3+d4+d5+d6+d7+d8+d9+d10+d11+d12+d13+d14+d15)
      
      d16 <- if(str_length(input$cc_number15)==15)
      
      {
        if ((round_any(d16_pre,10)-d16_pre)>=0)
      {round_any(d16_pre,10)-d16_pre} else
      {round_any(d16_pre,10)+10-d16_pre}
      }
      else
      {return("")}
      
      d16
      
      
      
 })
})