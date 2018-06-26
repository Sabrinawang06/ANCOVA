library(shiny)
library(png)
library(shinyBS)
library(shinyjs)
library(shinyDND)
library(V8)
library(ggplot2)
library(dplyr)
library(shinydashboard)
library(simstudy)
library(lubridate)

# #Use jscode to for reset button to reload the app
# jsResetCode <- "shinyjs.reset = function() {history.go(0)}"

ui <- dashboardPage(skin = "black",
                    dashboardHeader(title = "ANCOVA",
                                    titleWidth = 200),
                    #adding prereq pages
                    dashboardSidebar(
                      width = 220,
                      
                      sidebarMenu(id='tabs',
                                  menuItem("Pre-requisites", tabName= "prereq", icon=icon("dashboard")),
                                  menuItem("Overview",tabName = "instruction", icon = icon("dashboard")),
                                  menuItem("Exploring",tabName = "exploring", icon = icon("th")),
                                  menuItem("Game",tabName = "game", icon = icon("th"))
                      )
                    ),
                    dashboardBody(
                      tags$head(
                        tags$link(rel = "stylesheet", type = "text/css", href = "navcolor.css") #customised style sheet
                      ),
                      tags$head(
                        tags$style(HTML('#start{color:white;background-color: #D35400}')),
                        tags$style(HTML('#go{color:white;background-color: #D35400}')),
                        tags$style(HTML('#submitA{color:white;background-color: #D35400}')),
                        tags$style(HTML('#new{color:white;background-color: #D35400}')),
                        tags$style(HTML('#start_timer{color:white;background-color: #D35400}')),
                        tags$style(HTML('#set{color:white;background-color: #D35400}')),
                        tags$head(tags$style(HTML("
                            #analysis1 {
                              font-size: 16px;
                              background-color: #FDF2E9  
                            }
                            "))),
                        tags$head(tags$style(HTML("
                            #p {font-size: 18px;
                                padding-left: 5px"))),
                        tags$style(type='text/css', '#timeleft {background-color:#D35400; font-size: 30px; 
                                   color:white;font-weight: bold;font family:Sans-serif;text-align: center; border-radius: 100px}')
                        
                        
                      ),
                      
                      
                      
                      tabItems(
                        
                        tabItem(tabName="prereq",
                                fluidRow(
                                  column(8,offset = 1, uiOutput("background1"))
                                ),
                                fluidRow(
                                  column(1,img(src = "right.png", width = 20)),
                                  column(8,uiOutput("background2"))
                                ),
                                fluidRow(
                                  column(1,img(src = "right.png", width = 20)),
                                  column(8,uiOutput("background3"))
                                  ), hr(),
                                
                                fluidRow(
                                  column(1,img(src = "right.png", width = 20)),
                                  column(8,uiOutput("background4"))
                                ),
                                fluidRow(column(11,offset=2, img(src='plot.png',width=550))),
                                fluidRow(
                                  column(1,img(src = "right.png", width = 20)),
                                  column(8,uiOutput("background5"))
                                ),
                                  fluidRow(
                                    column(3,offset=1,actionButton("start","Go to the overview",icon("bolt"),style='padding:10px; font-size:120%',class="circle grow"))
                                  )
                                  
                                
                        ),
                        
                        
                        tabItem(tabName = "instruction",
                                tags$a(href='http://stat.psu.edu/',tags$img(src='logo.png', align = "left", width = 180)),
                                br(),br(),br(),br(),
                                fluidRow(
                                  column(8,offset = 1, uiOutput("about1"))
                                ),
                                fluidRow(
                                  column(1,img(src = "right.png", width = 20)),
                                  column(8,uiOutput("about2"))
                                ),br(),
                                
                                br(),
                                fluidRow(
                                  column(8,offset = 1, uiOutput("instruction1"))
                                ),
                                fluidRow(
                                  column(1,img(src = "right.png", width = 20)),
                                  column(8,uiOutput("instruction2"))
                                ),
                                fluidRow(
                                  column(1,img(src = "right.png", width = 20)),
                                  column(8,uiOutput("instruction3"))
                                ),
                                fluidRow(
                                  column(1,img(src = "right.png", width = 20)),
                                  column(8,uiOutput("instruction4")),
                                  column(5,actionButton("go","Go",icon("bolt"),style='padding:10px; font-size:120%',class="circle grow"))
                                ),br(),br(),
                                fluidRow(
                                  column(8,offset = 1, uiOutput("ack1"))
                                ),
                                fluidRow(
                                  column(1,img(src = "right.png", width = 20)),
                                  column(8, uiOutput("ack2"))
                                )
                                
                        ),
                       
                   
                        
                        tabItem(tabName ="exploring",
                                h2('ANCOVA Interaction Plot'),
                                sidebarLayout(
                                  sidebarPanel(
                                    
                                    selectInput('menu1','Select the Data',c('Otter','Diet','Random')),
                                    conditionalPanel("input.menu1=='Diet'",
                                                     radioButtons('select_conti', 'Select Continous Variable',inline=TRUE, choices =c('Age','Height','Pre-diet Weight'), selected = 'Age'),
                                                     radioButtons('select_covar', 'Select Covariance',inline=TRUE, choices =c('Gender','Diet'), selected = 'Gender')
                                    ),
                                    
                                    conditionalPanel("input.menu1=='Random'",
                                                     sliderInput('slope1','Change the slope of Line A',-5,5,0,step=1),
                                                     sliderInput('slope2','Change the slope of Line B',-5,5,0,step=1),
                                                     sliderInput('inter1','Change the intersection of Line A',-5,5,0,step=1),
                                                     sliderInput('inter2','Change the intersection of Line B',-5,5,0,step=1),
                                                     sliderInput('sample','Change the sample size',100,800,100,step=50)
                                              
                                                     
                                      
                                    ),
                                    fluidRow(tags$b(uiOutput('p')),align = "left")
                                  ),
                                  
                                  mainPanel(
                                    plotOutput('plot_gg'),
                                    tags$b(verbatimTextOutput('analysis1'))
                                   
                                  )
                                )
                                
                                
                        ),
                        
                        
                        
                        tabItem(tabName='game',
                                fluidRow(column(5,numericInput('seconds','Select the time you need for this game',value=30,min=30,max=120,step=10),
                                         
                                fluidRow(column(2,actionButton('start_timer','Start',style='padding:10px; font-size:90%')),
                                         column(2,offset=1,actionButton('set','Set Timer',style='padding:10px; font-size:90%')))
                                         ),
                                column(3,offset=4,textOutput("timeleft"))),br(),
                           
                                fluidRow(wellPanel(
                                  br(),
                                  dragUI("plotA","A", style = "width: 160px; height: 23px;")
                                  ,class = "col-lg-3 col-md-6 wellBorder",
                                  div(style = "text-align:center",

                                      uiOutput('plot1')),


                                  br(),
                                  br(),
                                  br()
                                ),

                                wellPanel(
                                  br(),
                                  dragUI("plotB","B", style = "width: 140px; height: 50px;")
                                  ,class = "col-lg-3 col-md-3 wellBorder",


                                  div(style = "text-align:center", uiOutput('plot2')),

                                  br(),br(),br()

                                ),
                                wellPanel(
                                  br(),
                                  dragUI("plotC","C", style = "width: 140px; height: 50px;")
                                  ,class = "col-lg-3 col-md-3 wellBorder",

                                  div(style = "text-align:center", uiOutput('plot3')),

                                  br()

                                )
                               ),
                                fluidRow(
                                  
                                  

                                    wellPanel(dropUI("drp1", class = "dropelement"),
                                              div(style = "position:absolute;top: 10%;right:2%;",htmlOutput("answer1")), class = "wellTransparent col-sm-12 col-md-6 col-lg-3",
                                              br(),
                                              uiOutput("table1")

                                    ),
                                    wellPanel(dropUI("drp2", class = "dropelement"),
                                              div(style = "position:absolute;top: 10%;right:2%;",htmlOutput("answer2")), class = "wellTransparent col-sm-12 col-md-6 col-lg-3",
                                              br(),

                                              uiOutput("table2")
                                    ),
                                    wellPanel(dropUI("drp3", class = "dropelement"),
                                              div(style = "position:absolute;top: 10%;right:2%;",htmlOutput("answer3")), class = "wellTransparent col-sm-12 col-md-6 col-lg-3",
                                              br(),

                                              uiOutput("table3")
                                    )



                                  ),



                               fluidRow(

                                 column(1,offset = 4, conditionalPanel("(input.drp1!='') & (input.drp2!='') & (input.drp3!='') & (input.drp4!='') "
                                                                       ,actionButton("submitA", "Submit Answer",style='padding:10px; font-size:120%',class="circle grow"))),
                                 column(1,offset = 5,bsButton("new","New>>",style='padding:10px; font-size:120%',class="circle grow", disabled = FALSE))
                               )

                               
                               
                                )#closing for game section
                                
                                
                                
                        )
                      
                    
                    )                  
)

