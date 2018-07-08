library(shiny)
library(png)
library(shinyBS)
library(shinyjs)
library(shinyDND)
library(ggplot2)
library(dplyr)  ###NEW PACKAGE 
library(shinydashboard)
library(simstudy) ### NEW PACKAGE 
library(lubridate)###NEW PACKGE
library(shinyalert)##NEW PACKAGE




####read in dataset###
seaotters <- read.csv("otter.csv",header=T)

diet <- read.csv("Diet.csv",header=T)
diet$Diet<-as.character(diet$Diet)

bank = read.csv("questionbank.csv")
bank = data.frame(lapply(bank, as.character), stringsAsFactors = FALSE)




shinyServer(function(input, output,session) {
  
  
  #Text on the instruction page
  output$background1<-renderUI(
    h3(strong('Background'))
  )
  output$background2<-renderUI(
    h4('What is ANCOVA')
  )
  output$background3<-renderUI(
    h5('A ‘classic’ ANOVA tests for differences in mean responses to categorical factor 
       (treatment) levels. When we have heterogeneity in experimental units sometimes restrictions on the randomization (blocking) can improve the test for treatment effects. In some cases, we don’t have the opportunity to construct blocks, but can recognize and measure a continuous variable as contributing to the heterogeneity in the experimental units.
        These sources of extraneous variability historically have been referred to as ‘nuisance’ or ‘concomitant’ variables. More recently, these variables are referred to as ‘covariates’.
        When a continuous covariate is included in an ANOVA we have the analysis of covariance (ANCOVA). (PSU STAT 502-Lesson 10: Analysis of Covariance (ANCOVA))  ')
  )
  
  output$background4<-renderUI(
    h4('Diagnostic Plot')
  )
  output$background5<-renderUI(
    h5('Model check is critical before analysis: you need to understanding the four diagnostic plot.',br(),
       '1. Residuals vs Fitted plot checks linear pattern of residuals. You should expect a horizontal line spreading the dots equally.', br(),
       '2. Normal Q-Q plot checks normality. You should expect the dots follow a straight line.',br(),
       '3. Scale-Location plot checks equal spreads of residual. You should expect a horizontal line with spreading the dots equally also.',br(),
       '4. Residual vs Leverage plot checks influential outliers. You should expect all dots place with in the dash line range. ')
  )
  
  output$about1 <- renderUI(
    h3(strong("About"))
  )
  output$about2<-renderUI(
    h4('This app introduces the concept of ANCOVA focusing on interpret interaction plot.')
  )
  output$instruction1<-renderUI(
    h3(strong('Instruction'))
  )
  output$instruction2<-renderUI(
    h4('Click Go button to enter the explore page. Use the dropdown menu to select different dataset.')
  )
  output$instruction3<-renderUI(
    h4('Use the radio button to select different varibles and see the change in interaction plot. Or use slider bars to change the parameters. ')
  )
  output$instruction44<-renderUI(
    h4('After the explore section, you can start the matching game to test your understand on this concept.')
  )
  output$ack1<-renderUI(
    h3(strong('Acknowledgement'))
  )
  output$ack2<-renderUI((
  h4('This app is developed and coded by Luxin Wang. Thanks for the data set and code provided by The University of Sheffield (https://www.sheffield.ac.uk/mash/data) and Dylan Childs.(https://github.com/dzchilds)')
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
  
  
 ###save random model
  rand<-reactiveValues(rand_mod=NULL)

  
 
  ###Graph the plot of interaction###
  output$plot_gg<-renderPlot(if (input$menu1=='Otter') {ggplot(pred.data, aes(x = Year, y = Otters, colour = Location)) + 
                             geom_line() + geom_point(data = seaotters) + 
                             xlab("Year") + ylab("Otters")+theme(text = element_text(size=20),panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
                                                                 panel.background = element_blank(), axis.line = element_line(colour = "black"))}
                           
                          else if (input$menu1=='Diet'){
                           if (input$select_conti=='Age' & input$select_covar=='Gender'){ggplot(pred.data2, aes(x = Age, y = ab_change, colour = gender)) + 
                               geom_line() + geom_point(data = diet) + 
                               xlab("Age") + ylab("Decrease in Weight")+theme(text = element_text(size=20),panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
                                                                              panel.background = element_blank(), axis.line = element_line(colour = "black"))}
                           else if (input$select_conti=='Height' & input$select_covar=='Gender'){ggplot(pred.data3, aes(x = Height, y = ab_change, colour = gender)) + 
                               geom_line() + geom_point(data = diet) + 
                               xlab("Height") + ylab("Decrease in Weight")+theme(text = element_text(size=20),panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
                                                                                 panel.background = element_blank(), axis.line = element_line(colour = "black"))}
                           else if (input$select_conti=='Pre-diet Weight' & input$select_covar=='Gender'){ggplot(pred.data4, aes(x = pre.weight, y = ab_change, colour = gender)) + 
                               geom_line() + geom_point(data = diet) + 
                               xlab("Pre-diet Weight") + ylab("Decrease in Weight")+theme(text = element_text(size=20),panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
                                                                                          panel.background = element_blank(), axis.line = element_line(colour = "black"))}
                           else if (input$select_conti=='Age' & input$select_covar=='Diet'){ggplot(pred.data5, aes(x = Age, y = ab_change, colour =Diet)) + 
                               geom_line() + geom_point(data = diet) + 
                               xlab("Age") + ylab("Decrease in Weight")+theme(text = element_text(size=20),panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
                                                                              panel.background = element_blank(), axis.line = element_line(colour = "black"))}
                           else if (input$select_conti=='Height' & input$select_covar=='Diet'){ggplot(pred.data6, aes(x = Height, y = ab_change, colour = Diet)) + 
                               geom_line() + geom_point(data = diet) + 
                               xlab("Height") + ylab("Decrease in Weight")+theme(text = element_text(size=20),panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
                                                                                 panel.background = element_blank(), axis.line = element_line(colour = "black"))}
                           else if (input$select_conti=='Pre-diet Weight' & input$select_covar=='Diet'){ggplot(pred.data7, aes(x = pre.weight, y = ab_change, colour = Diet)) + 
                               geom_line() + geom_point(data = diet) + 
                               xlab("Pre-diet Weight") + ylab("Decrease in Weight")+theme(text = element_text(size=20),panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
                                                                                          panel.background = element_blank(), axis.line = element_line(colour = "black"))}
                          }
                           else if (input$menu1=='Random'){
                             ###create data with label A and B with different slope and intersection
                             A<-'A'
                             B<-'B'
                             
                             a<-input$inter1
                             b<-input$inter2
                             
                             slope1<-input$slope1
                             slope2<-input$slope2
                             
                             def <- defData(varname = "inter", dist = "nonrandom", formula = a, id = "id")
                             
                             def<- defData(def,varname = "slope", dist = "nonrandom", formula = slope1, id = "slope")
                             def <- defData(def, varname = "X", dist = "uniform", formula = "10;20")
                             def <- defData(def, varname = "Y", formula = "inter + X * slope", variance = 11)
                             
                             def2<- defData(varname = "inter", dist = "nonrandom", formula = b, id = "id")
                             
                             def2 <- defData(def2,varname = "slope", dist = "nonrandom", formula = slope2, id = "slope")
                             def2<- defDataAdd(def2, varname = "X", dist = "uniform", formula = "10;20")
                             def2 <- defDataAdd(def2, varname = "Y", formula = "inter + X * slope", variance =11)
                             
                             
                             dt <- genData(input$sample, def)
                             dt2<-genData(input$sample,def2)
                             
                             names(dt2)[1]<-'id'
                             
                             dt$cov<-'A'
                             dt2$cov<-'B'
                             
                             comb<-rbind(dt,dt2)
                             
                             
                             aov.model<-lm(Y~X+cov+cov:X,data=comb)
                             
                            
                             
                             pred.aov <- expand.grid(X =10:20, cov = c("A","B"))
                             pred.aov <- mutate(pred.aov, Y = predict(aov.model, pred.aov))
                             
                             
                             ggplot(pred.aov, aes(x = X, y = Y, colour = cov)) + 
                               geom_line() + geom_point(data = comb) + 
                               xlab("X") + ylab("Y")+theme(text = element_text(size=20),panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
                                                           panel.background = element_blank(), axis.line = element_line(colour = "black"))}
                           )
  
  
  
  
  ###ANCOVA analysis table###
  output$analysis1<-renderPrint(if (input$menu1=='Otter') {anova(otters.model)}
                                else if (input$menu1=='Diet'){
                                if (input$select_conti=='Age' & input$select_covar=='Gender'){anova(diet.model2)}
                                else if (input$select_conti=='Height' & input$select_covar=='Gender'){anova(diet.model3)}
                                else if (input$select_conti=='Pre-diet Weight' & input$select_covar=='Gender'){anova(diet.model4)}
                                else if (input$select_conti=='Age' & input$select_covar=='Diet'){anova(diet.model5)}
                                else if (input$select_conti=='Height' & input$select_covar=='Diet'){anova(diet.model6)}
                                else if (input$select_conti=='Pre-diet Weight' & input$select_covar=='Diet'){anova(diet.model7)}
                                 }
                                else if (input$menu1=='Random'){
                                  A<-'A'
                                  B<-'B'
                                  
                                  a<-input$inter1
                                  b<-input$inter2
                                  
                                  slope1<-input$slope1
                                  slope2<-input$slope2
                                  
                                  def <- defData(varname = "inter", dist = "nonrandom", formula = a, id = "id")
                                  
                                  def<- defData(def,varname = "slope", dist = "nonrandom", formula = slope1, id = "slope")
                                  def <- defData(def, varname = "X", dist = "uniform", formula = "10;20")
                                  def <- defData(def, varname = "Y", formula = "inter + X * slope", variance = 11)
                                  
                                  def2<- defData(varname = "inter", dist = "nonrandom", formula = b, id = "id")
                                  
                                  def2 <- defData(def2,varname = "slope", dist = "nonrandom", formula = slope2, id = "slope")
                                  def2<- defDataAdd(def2, varname = "X", dist = "uniform", formula = "10;20")
                                  def2 <- defDataAdd(def2, varname = "Y", formula = "inter + X * slope", variance = 11)
                                  
                                  
                                  dt <- genData(input$sample, def)
                                  dt2<-genData(input$sample,def2)
                                  
                                  names(dt2)[1]<-'id'
                                  
                                  dt$cov<-'A'
                                  dt2$cov<-'B'
                                  
                                  comb<-rbind(dt,dt2)
                                  
                                  
                                  aov.model<-lm(Y~X+cov+cov:X,data=comb)
                                  
                                  ##testing passing the model
                                  rand$rand_mod<-anova(aov.model)[3,"Pr(>F)"]
                                  
                                  anova(aov.model)
                                 
                                  
                                  }
                                )
  
  
  #####get p values for each interaction 
  
  var<-reactiveValues(p=NULL)
  observe({

    # if (is.null(input$menu1)){
    #   return()
    # }
    # 
    # isolate({
    #   var$p<-as.numeric(anova(otters.model)[3,"Pr(>F)"])
    #  
    # })
    
    
    if (input$menu1=='Otter') {var$p<-as.numeric(anova(otters.model)[3,"Pr(>F)"])}
    else if (input$menu1=='Diet'){
      if (input$select_conti=='Age' & input$select_covar=='Gender'){var$p<-as.numeric(anova(diet.model2)[3,"Pr(>F)"])}
      else if (input$select_conti=='Height' & input$select_covar=='Gender'){var$p<-as.numeric(anova(diet.model3)[3,"Pr(>F)"])}
      else if (input$select_conti=='Pre-diet Weight' & input$select_covar=='Gender'){var$p<-as.numeric(anova(diet.model4)[3,"Pr(>F)"])}
      else if (input$select_conti=='Age' & input$select_covar=='Diet'){var$p<-as.numeric(anova(diet.model5)[3,"Pr(>F)"])}
      else if (input$select_conti=='Height' & input$select_covar=='Diet'){var$p<-as.numeric(anova(diet.model6)[3,"Pr(>F)"])}
      else if (input$select_conti=='Pre-diet Weight' & input$select_covar=='Diet'){var$p<-as.numeric(anova(diet.model7)[3,"Pr(>F)"])}
    }
    else if (input$menu1=='Random'){var$p<-as.numeric(rand$rand_mod)}
  })
  
  output$p<-renderUI(
    if (var$p<=0.05){
    h4(strong('P-value for this interaction is',signif(var$p,4),'.' ,br(),'Since the p-value is smaller than 0.05
    (α=0.05), there is a statistically significant interaction between these two variables.'))}
    else {h4(strong('P-value for this interaction is',signif(var$p,4),'.' ,br(),'Since the p-value is greater than 0.05
    (α=0.05), there is NOT a statistically significant interaction between these two variables.'))}
)

 
  
  #####game pictures#####
  
  ######################################  Bank B #############################################################
  numbers <- reactiveValues(strong = c(), moderate = c(), insig = c(), index = c(), question = data.frame())

  observeEvent(input$go,{
    numbers$strong = sample(1:12,1)
    numbers$moderate = sample(13:24,1)
    numbers$insig= sample(25:36,1)


    numbers$index =c("A","B","C")
    numbers$question = cbind(bank[c(numbers$strong,numbers$moderate,numbers$insig),],numbers$index)

  })
  
  observeEvent(input$new,{
    numbers$strong = sample(1:12,1)
    numbers$moderate = sample(13:24,1)
    numbers$insig= sample(25:36,1)
    
    
    numbers$index = c("A","B","C")
    numbers$question = cbind(bank[c(numbers$strong,numbers$moderate,numbers$insig),],numbers$index)
    
  })
  
  output$plot1 <- renderUI({
    img(src = numbers$question[numbers$question[5] == "A",4], width = "100%", height = "107%", style = "text-align: center")
  })

  # output$table1 <- renderUI({
  #   img(src = numbers$question[numbers$question[5] == "A",3], width = "100%", height = "100%", style = "text-align: center")
  # })

  output$plot2 <- renderUI({
    img(src = numbers$question[numbers$question[5] == "B",4], width = "100%", height = "107%", style = "text-align: center")
  })

  # output$table2 <- renderUI({
  #   img(src = numbers$question[numbers$question[5] == "B",3], width = "100%", height = "100%", style = "text-align: center")
  # })

  output$plot3 <- renderUI({
    img(src = numbers$question[numbers$question[5] == "C",4], width = "100%", height = "107%", style = "text-align: center")
  })

  # output$table3 <- renderUI({
  #   img(src = numbers$question[numbers$question[5] == "C",3], width = "100%", height = "100%", style = "text-align: center")
  # })

 #######randomize the table######
  
  index2 <- reactiveValues(index2 = 3)
  
  observeEvent(input$new,{index2$index2 <- sample(1:4,1, replace=TRUE, prob=NULL)
  })
  
  output$table1<-renderUI({
    if (index2$index2==1){img(src = numbers$question[numbers$question[5] == "A",3], width = "105%", height = "105%", style = "text-align: center")}
    else if (index2$index2==2){img(src = numbers$question[numbers$question[5] == "B",3], width = "105%", height = "105%", style = "text-align: center")}
    else if (index2$index2==3){img(src = numbers$question[numbers$question[5] == "C",3], width = "105%", height = "105%", style = "text-align: center")}
    else if (index2$index2==4){img(src = numbers$question[numbers$question[5] == "A",3], width = "105%", height = "105%", style = "text-align: center")}
    })
  
  output$table2<-renderUI({
    if (index2$index2==1){img(src = numbers$question[numbers$question[5] == "B",3], width = "105%", height = "105%", style = "text-align: center")}
    else if (index2$index2==2){img(src = numbers$question[numbers$question[5] == "C",3], width = "105%", height = "105%", style = "text-align: center")}
    else if (index2$index2==3){img(src = numbers$question[numbers$question[5] == "A",3], width = "105%", height = "105%", style = "text-align: center")}
    else if (index2$index2==4){img(src = numbers$question[numbers$question[5] == "C",3], width = "105%", height = "105%", style = "text-align: center")}
  })
  
  output$table3<-renderUI({
    if (index2$index2==1){img(src = numbers$question[numbers$question[5] == "C",3], width = "105%", height = "105%", style = "text-align: center")}
    else if (index2$index2==2){img(src = numbers$question[numbers$question[5] == "A",3], width = "105%", height = "105%", style = "text-align: center")}
    else if (index2$index2==3){img(src = numbers$question[numbers$question[5] == "B",3], width = "105%", height = "105%", style = "text-align: center")}
    else if (index2$index2==4){img(src = numbers$question[numbers$question[5] == "B",3], width = "105%", height = "105%", style = "text-align: center")}
  })
   
  
  
  ####letter for the plot
  output$a<-renderUI(h3('A'))
  output$b<-renderUI(h3('B'))
  output$c<-renderUI(h3('C'))
  #####buttons####

  observeEvent(input$submitA,{
    updateButton(session,"submitA",disabled = TRUE)
  })
  observeEvent(input$new,{
    updateButton(session,"submitA",disabled = FALSE)
  })
  
  observeEvent(input$submitA,{
    updateButton(session,"new",disabled = FALSE)
  })
  
  observeEvent(input$new,{
    updateButton(session,"new",disabled = TRUE)
  })
  
  
  observeEvent(input$new, {
    reset("radio1")
  })
  
  
  ###################check answers#####
 
  summationC<-reactiveValues(correct1 = c(0), started=FALSE)
  
  observeEvent(input$submitA,{
    observeEvent(input$new,{
      output$answer1 <- renderUI({
        img(src = NULL,width = 30)
      })
    })
    observe({
      output$answer1 <- renderUI({
        if (!is.null(input$radio1)){
          if (index2$index2==1 &input$radio1 == 'A'){
            img(src = "check.png",width = 30)
          }
          else if (index2$index2==2 &input$radio1 == 'B') {img(src = "check.png",width = 30)}
          else if (index2$index2==3 &input$radio1 == 'C'){img(src = "check.png",width = 30)}
          else if (index2$index2==4 &input$radio1 == 'A'){img(src = "check.png",width = 30)}
          else{
            img(src = "cross.png",width = 30)
          }
        }
      })
    })
  })
  
  
  observeEvent(input$submitA,{
    observeEvent(input$new,{
      output$answer2 <- renderUI({
        img(src = NULL,width = 30)
      })
    })
    observe({
      output$answer2 <- renderUI({
        if (!is.null(input$radio2)){
          if (index2$index2==1 &input$radio2 == 'B'){
            img(src = "check.png",width = 30)
            
          }
          else if (index2$index2==2 &input$radio2 == 'C') {img(src = "check.png",width = 30)}
          else if (index2$index2==3 &input$radio2 == 'A'){img(src = "check.png",width = 30)}
          else if (index2$index2==4 &input$radio2 == 'C'){img(src = "check.png",width = 30)}
          else{
            img(src = "cross.png",width = 30)
          }
        }
      })
    })
  })
  
  observeEvent(input$submitA,{
    observeEvent(input$new,{
      output$answer3 <- renderUI({
        img(src = NULL,width = 30)
      })
    })
    observe({
      output$answer3 <- renderUI({
        if (!is.null(input$radio3)){
          if (index2$index2==1 &input$radio3 == 'C'){
            img(src = "check.png",width = 30)
          }
          else if (index2$index2==2 &input$radio3 == 'A') {img(src = "check.png",width = 30)}
          else if (index2$index2==3 &input$radio3 == 'B'){img(src = "check.png",width = 30)}
          else if (index2$index2==4 &input$radio3 == 'B'){img(src = "check.png",width = 30)}
          else{
            img(src = "cross.png",width = 30);
            
          }
        }
      })
    })
  })
  
  
  #####count correct answer ########
  summationC<-reactiveValues(correct1 = c(0), started=FALSE)
  
  observeEvent(input$submitA,{
         for (i in input$radio1){
          if (index2$index2==1 &input$radio1 == 'A'){
            summationC$correct1 = c(summationC$correct1,1)
          }
          else if (index2$index2==2 &input$radio1 == 'B') { summationC$correct1 = c(summationC$correct1,1)}
          else if (index2$index2==3 &input$radio1 == 'C'){summationC$correct1 = c(summationC$correct1,1)}
          else if (index2$index2==4 &input$radio1 == 'A'){summationC$correct1 = c(summationC$correct1,1)}
          else{
            summationC$correct1 = c(summationC$correct1,0)}
         
          }
  
          for (i in input$radio2){
          if (index2$index2==1 &input$radio2 == 'B'){
      
            summationC$correct1 = c(summationC$correct1,1)
            
          }
          else if (index2$index2==2 &input$radio2 == 'C') {summationC$correct1 = c(summationC$correct1,1)}
          else if (index2$index2==3 &input$radio2 == 'A'){summationC$correct1 = c(summationC$correct1,1)}
          else if (index2$index2==4 &input$radio2 == 'C'){summationC$correct1 = c(summationC$correct1,1)}
          else{
   
            summationC$correct1 = c(summationC$correct1,0)}
          }
       
  
        for (i in input$radio3){
          if (index2$index2==1 &input$radio3 == 'C'){
            i
            summationC$correct1 = c(summationC$correct1,1)
          }
          else if (index2$index2==2 &input$radio3 == 'A') {summationC$correct1 = c(summationC$correct1,1)}
          else if (index2$index2==3 &input$radio3 == 'B'){summationC$correct1 = c(summationC$correct1,1)}
          else if (index2$index2==4 &input$radio3 == 'B'){ summationC$correct1 = c(summationC$correct1,1)}
          else{
            
            summationC$correct1 = c(summationC$correct1,0)}
          }
        })
  
 
  
  output$correctC <- renderPrint({
    if (sum(c(summationC$correct1))==0) {cat("You have earned 0 points")}
    else{
      cat("You have earned",sum(c(summationC$correct1)),'points')}
  })
  
  
  ###########timer####################
  
  # Initialize the timer, 60 seconds, not active.
  timer <- reactiveVal(5)
  active <- reactiveVal(FALSE)
  
  # Output the time left.
  output$timeleft <- renderText({
    paste("Time left: ", seconds_to_period(timer()))
  })
  
  
 ########show up the scoreing panel and popup####
  
  
  # observer that invalidates every second. If timer is active, decrease by one.
  observe({
    invalidateLater(1000, session)
    isolate({
      if(active())
      {
        timer(timer()-1)
        if(timer()<1)
        {
          active(FALSE)
          shinyalert('Count Down Complete','Click to see your score',
                      type = "success")
          
          output$scoreBox <- renderValueBox({
            valueBox(
              paste0(sum(c(summationC$correct1))), "Totel Score", icon = icon("list"),
              color = "purple"
            )
          })
          
          
        }
      }
    })
  })
  
  
  
  observe({
        if(timer()<1)
        {
          output$time<-renderText({paste('0')})
        }
    
  })
  
  


  
  
  # observers for actionbuttons
  observeEvent(input$start_timer, {active(TRUE)})
  #observeEvent(input$stop, {active(FALSE)})
  observeEvent(input$set, {timer(input$seconds)})


##closing for ui DON'T DELET####  
})





