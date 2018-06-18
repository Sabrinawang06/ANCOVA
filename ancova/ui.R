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
                        tags$style(HTML('#start{background-color: #D35400}')),
                        tags$style(HTML('#go{background-color: #D35400}'))
                      ),
                      
                      
                      
                      tabItems(
                        
                        tabItem(tabName="prereq",
                                fluidRow(
                                  column(11,offset = 1, uiOutput("background1"))
                                ),
                                fluidRow(
                                  column(1,img(src = "right.png", width = 30)),
                                  column(11,uiOutput("background2"))
                                ),
                                fluidRow(
                                  column(1,img(src = "right.png", width = 30)),
                                  column(11,uiOutput("background3")), 
                                  fluidRow(
                                    column(3,offset=1,actionButton("start","Go to the overview",icon("bolt"),style='padding:10px; font-size:120%',class="circle grow"))
                                  )
                                  
                                )
                        ),
                        
                        
                        tabItem(tabName = "instruction",
                                tags$a(href='http://stat.psu.edu/',tags$img(src='logo.png', align = "left", width = 180)),
                                
                                fluidRow(
                                  column(11,offset = 1, uiOutput("about1"))
                                ),
                                fluidRow(
                                  column(1,img(src = "right.png", width = 30)),
                                  column(11,uiOutput("about2"))
                                ),br(),
                                
                                br(),
                                fluidRow(
                                  column(11,offset = 1, uiOutput("instruction1"))
                                ),
                                fluidRow(
                                  column(1,img(src = "right.png", width = 30)),
                                  column(11,uiOutput("instruction2"))
                                ),
                                fluidRow(
                                  column(1,img(src = "right.png", width = 30)),
                                  column(11,uiOutput("instruction3"))
                                ),
                                fluidRow(
                                  column(1,img(src = "right.png", width = 30)),
                                  column(8,uiOutput("instruction4")),
                                  column(5,actionButton("go","Go",icon("bolt"),style='padding:10px; font-size:120%',class="circle grow"))
                                ),br(),
                                fluidRow(
                                  column(11,offset = 1, uiOutput("ack1"))
                                ),
                                fluidRow(
                                  column(1,img(src = "right.png", width = 30)),
                                  column(11, uiOutput("ack2"))
                                )
                                
                        ),
                       
                   
                        
                        tabItem(tabName ="exploring",
                                h2('ANCOVA Interaction Plot'),
                                sidebarLayout(
                                  sidebarPanel(
                                    selectInput('menu1','Select the Data',c('Otter','Diet','Customized')),
                                    conditionalPanel("input.menu1=='Diet'",
                                                     radioButtons('select_conti', 'Select Continous Variable',inline=TRUE, choices =c('Age','Height','Pre-diet Weight'), selected = 'Age'),
                                                     radioButtons('select_covar', 'Select Covariance',inline=TRUE, choices =c('Gender','Diet'), selected = 'Gender')
                                    )
                                  ),
                                  
                                  mainPanel(
                                    plotOutput('plot1'),
                                    verbatimTextOutput('analysis1')
                                   
                                  )
                                )
                                
                                
                        ),
                        
                        
                        
                        tabItem(tabName='game',
                                fluidRow(h2('blank2'))
                                
                                
                        )
                      )
                    )
                    
)

