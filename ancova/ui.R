
library(shiny)

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
                                  menuItem("Game",tabName = "game", icon = icon("th"))
                      )
                    ),
                    dashboardBody(
                      tags$head(
                        tags$link(rel = "stylesheet", type = "text/css", href = "navcolor.css") #customised style sheet
                      ),
                      
                      tabItems(
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
                        #Adding pre-requisites page to remove background from instructions page
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
                        
                        tabItem(tabName ="game",
                                wellPanel(
                                  fluidRow(uiOutput("question"))
                                ),hr(),
                                fluidRow(
                                  column(4,plotOutput("target", click = 'Click'), style = "height: 320px;"),
                                  column(8, 
                                         conditionalPanel(
                                           condition = 'input.submit != 0',
                                           fluidRow(
                                             #change scroll over for bias and reliability
                                             column(6,plotOutput("plota"), style = "height: 320px;",
                                                    bsPopover("plota", "Bias", "How far the points from the center.",placement = "top")),
                                             column(6,plotOutput("plotb"), style = "height: 320px;",
                                                    bsPopover("plotb", "Reliability", "How far points are from each other",placement = "top")))
                                         ))),
                                fluidRow(
                                  column(4, offset = 4, 
                                         bsButton("submit",label = "Submit",type = "toggle", size = "large", value = FALSE, disabled = TRUE),
                                         bsButton("new",label = "Next>>", style = "danger", size = "large"),
                                         bsButton('clear', label = "Clear", style = 'danger', size = 'large')
                                  ),
                                  column(3, offset = 1,
                                         conditionalPanel("input.submit != 0", 
                                                          wellPanel(
                                                            div(style = "position: relative; top:0",print("Feedback")),
                                                            img(src = "arrow.gif", width = 40), class = "arrow")))
                                ),
                                fluidRow(
                                  hr(),
                                  conditionalPanel("input.submit != 0",
                                                   h1(uiOutput("answer")),
                                                   wellPanel(uiOutput("feedback1"),
                                                             uiOutput("feedback2"),
                                                             uiOutput("feedback3"), class = "wellfeedback")
                                  ))
                                
                        )
                      )
                    )
                    
)

