library(shiny)
library(png)
library(shinyBS)
library(shinyjs)
library(V8)
library(ggplot2)
library(dplyr)
library(shinydashboard)


#Use jscode to for reset button to reload the app
jsResetCode <- "shinyjs.reset = function() {history.go(0)}"

#Define the function to disable all the button
disableActionButton <- function(id,session) {
  session$sendCustomMessage(type="jsCode",
                            list(code= paste("$('#",id,"').prop('disabled',true)"
                                             ,sep="")))
}


####read in dataset###
seaotters <- read.csv("C:\\Users\\llfsh\\Desktop\\otter.csv",header=T)




shinyServer(function(input, output,session) {
  
  
  #Text on the instruction page
  output$background1<-renderUI(
    print('Background')
  )
  output$background2<-renderUI(
    h4('1')
  )
  output$background3<-renderUI(
    h4('2')
  )
  output$about1 <- renderUI(
    print("About")
  )
  output$about2<-renderUI(
    h3('This app introduces the concept of ANCOVA ')
  )
  output$instruction1<-renderUI(
    print('Instruction')
  )
  output$instruction2<-renderUI(
    h2('1')
  )
  output$instruction3<-renderUI(
    h2('2')
  )
  output$instruction44<-renderUI(
    h2('3')
  )
  output$ack1<-renderUI(
    print('Acknowledgement')
  )
  output$ack2<-renderUI((
    print('This app is developed and coded by Luxin Wang')
  ))

  
  ####button###

  observeEvent(input$go,{
    updateTabItems(session,"tabs","exploring")
  })
  
  observeEvent(input$start,{
    updateTabItems(session,"tabs","instruction")
  })
  
  ###############################  Exploring  ##############################
  
  
  ###prep the otter data###
  otters.model <- lm(Otters ~ Location + Year + Location:Year, data = seaotters)
  pred.data <- expand.grid(Year = 1992:2003, Location = c("Lagoon", "Bay"))
  pred.data <- mutate(pred.data, Otters = predict(otters.model, pred.data))
  
  
  

  
  output$plot1<-renderPlot(if (input$menu1=='Otter') {ggplot(pred.data, aes(x = Year, y = Otters, colour = Location)) + 
                             geom_line() + geom_point(data = seaotters) + 
                             xlab("Year") + ylab("Otters")})
  
  output$analysis1<-renderPrint(if (input$menu1=='Otter') {anova(otters.model)})
  
  
})





