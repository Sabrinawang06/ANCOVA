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
diet <- read.csv("C:\\Users\\llfsh\\Desktop\\Diet.csv",header=T)
diet$Diet<-as.character(diet$Diet)



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
  
  
  ###prep the data###
  otters.model <- lm(Otters ~ Location + Year + Location:Year, data = seaotters)
  pred.data <- expand.grid(Year = 1992:2003, Location = c("Lagoon", "Bay"))
  pred.data <- mutate(pred.data, Otters = predict(otters.model, pred.data))
  
  diet.model<-lm(ab_change~gender+Diet+Age+Height+pre.weight+gender:Age+gender:Height+gender:pre.weight+
                 Diet:Age+Diet:Height+Diet:pre.weight,data=diet)
  
  diet.model2<-lm(ab_change~Age+gender+Age:gender,data=diet)
  pred.data2 <- expand.grid(Age = 16:60, gender = c("M", "F"))
  pred.data2 <- mutate(pred.data2, ab_change = predict(diet.model2, pred.data2))
  
  diet.model3<-lm(ab_change~Height+gender+Height:gender,data=diet)
  pred.data3 <- expand.grid(Height = 141:201, gender = c("M", "F"))
  pred.data3 <- mutate(pred.data3, ab_change = predict(diet.model3, pred.data3))
  
  
  diet.model4<-lm(ab_change~pre.weight+gender+pre.weight:gender,data=diet)
  pred.data4 <- expand.grid(pre.weight=58:103, gender = c("M", "F"))
  pred.data4 <- mutate(pred.data4, ab_change = predict(diet.model4, pred.data4))
  
  
  diet.model5<-lm(ab_change~Age+Diet+Age:Diet,data=diet)
  pred.data5 <- expand.grid(Age = 16:60, Diet = c('1','2','3'))
  pred.data5 <- mutate(pred.data5, ab_change = predict(diet.model5, pred.data5))
  
  diet.model6<-lm(ab_change~Height+Diet+Height:Diet,data=diet)
  pred.data6<- expand.grid(Height = 141:201, Diet = c('1','2','3'))
  pred.data6 <- mutate(pred.data6, ab_change = predict(diet.model6, pred.data6))
  
  
  diet.model7<-lm(ab_change~pre.weight+Diet+pre.weight:Diet,data=diet)
  pred.data7 <- expand.grid(pre.weight=58:103, Diet = c('1','2','3'))
  pred.data7 <- mutate(pred.data7, ab_change = predict(diet.model7, pred.data7))

  
  ###Graph the plot of interaction###
  output$plot1<-renderPlot(if (input$menu1=='Otter') {ggplot(pred.data, aes(x = Year, y = Otters, colour = Location)) + 
                             geom_line() + geom_point(data = seaotters) + 
                             xlab("Year") + ylab("Otters")}
                           else if (input$select_conti=='Age' & input$select_covar=='Gender'){ggplot(pred.data2, aes(x = Age, y = ab_change, colour = gender)) + 
                               geom_line() + geom_point(data = diet) + 
                               xlab("Age") + ylab("Change in Weight")}
                           else if (input$select_conti=='Height' & input$select_covar=='Gender'){ggplot(pred.data3, aes(x = Height, y = ab_change, colour = gender)) + 
                               geom_line() + geom_point(data = diet) + 
                               xlab("Height") + ylab("Change in Weight")}
                           else if (input$select_conti=='Pre-diet Weight' & input$select_covar=='Gender'){ggplot(pred.data4, aes(x = pre.weight, y = ab_change, colour = gender)) + 
                               geom_line() + geom_point(data = diet) + 
                               xlab("Pre-diet Weight") + ylab("Change in Weight")}
                           else if (input$select_conti=='Age' & input$select_covar=='Diet'){ggplot(pred.data5, aes(x = Age, y = ab_change, colour =Diet)) + 
                               geom_line() + geom_point(data = diet) + 
                               xlab("Age") + ylab("Change in Weight")}
                           else if (input$select_conti=='Height' & input$select_covar=='Diet'){ggplot(pred.data6, aes(x = Height, y = ab_change, colour = Diet)) + 
                               geom_line() + geom_point(data = diet) + 
                               xlab("Height") + ylab("Change in Weight")}
                           else if (input$select_conti=='Pre-diet Weight' & input$select_covar=='Diet'){ggplot(pred.data7, aes(x = pre.weight, y = ab_change, colour = Diet)) + 
                               geom_line() + geom_point(data = diet) + 
                               xlab("Pre-diet Weight") + ylab("Change in Weight")}
                           )
  
  
  ###ANCOVA analysis table###
  output$analysis1<-renderPrint(if (input$menu1=='Otter') {anova(otters.model)}
                                else if (input$select_conti=='Age' & input$select_covar=='Gender'){anova(diet.model2)}
                                else if (input$select_conti=='Height' & input$select_covar=='Gender'){anova(diet.model3)}
                                else if (input$select_conti=='Pre-diet Weight' & input$select_covar=='Gender'){anova(diet.model4)}
                                else if (input$select_conti=='Age' & input$select_covar=='Diet'){anova(diet.model5)}
                                else if (input$select_conti=='Height' & input$select_covar=='Diet'){anova(diet.model6)}
                                else if (input$select_conti=='Pre-diet Weight' & input$select_covar=='Diet'){anova(diet.model7)}
                                )

  
  
    
##closing for ui DON'T DELET####  
})





