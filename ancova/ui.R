library(shiny)
library(png)
library(shinyBS)
library(shinyjs)
library(V8)
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
                                    column(3,offset=1,bsButton("start","Go to the overview",icon("ravelry"),style = "danger",size = "large",class="circle grow"))
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
                                  column(5,bsButton("go","Go",icon("ravelry"),style = "danger",size = "large",class="circle grow"))
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
                                fluidRow(h2('blank'))
                                
                        ),
                        
                        
                        
                        tabItem(tabName='game',
                                fluidRow(h2('blank2'))
                                
                                
                        )
                      )
                    )
                    
)

