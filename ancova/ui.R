library(shiny)
library(png)
library(shinyBS)
library(shinyDND)
library(shinyjs)
library(ggplot2)
library(dplyr)
library(shinydashboard)
library(simstudy)
library(lubridate)
library(shinyalert)


convertMenuItem <- function(mi,tabName) {
  mi$children[[1]]$attribs['data-toggle']="tab"
  mi$children[[1]]$attribs['data-value'] = tabName
  if(length(mi$attribs$class)>0 && mi$attribs$class=="treeview"){
    mi$attribs$class=NULL
  }
  mi
}

ui <- dashboardPage(skin = "black",
                    dashboardHeader(title = "ANCOVA",
                                    titleWidth = 180
                                    # Set height of dashboardHeader
                                    # tags$li(class = "dropdown",
                                    #         tags$style(".main-header {max-height: 45px}"),
                                    #         tags$style(".main-header .logo {height: 45px;}"),
                                    #         tags$style(".sidebar-toggle {height: 45px; padding-top: 1px !important;}"),
                                    #         tags$style(".navbar {min-height:45px !important}")
                                    # ) 
                                    
                                    ),
                    #adding prereq pages
                    dashboardSidebar(
                      width = 180,
                      
                      sidebarMenu(id='tabs',style='font-size:13px;',
                                  convertMenuItem(menuItem("Pre-requisites", tabName= "prereq", icon=icon("book"),
                                           menuSubItem('What is ANCOVA',tabName = 'box',icon=icon('calendar'))),'prereq'),
                                  menuItem("Overview",tabName = "instruction", icon = icon("dashboard")),
                                  menuItem("Exploring",tabName = "exploring", icon = icon("wpexplorer")),
                                  menuItem("Game",tabName = "game", icon = icon("gamepad"))
                      )
                    ),
                    dashboardBody(
                      tags$head(
                        tags$link(rel = "stylesheet", type = "text/css", href = "navcolor.css") #customised style sheet
                      ),
                      tags$head(
                        tags$style(HTML('#start{color:white;background-color: #BB8FCE}')),
                        tags$style(HTML('#go{color:white;background-color: #BB8FCE}')),
                        tags$style(HTML('#go2{color:white;background-color: #BB8FCE}')),
                        tags$style(HTML('#pre2{color:white;background-color: #BB8FCE}')),
                        tags$style(HTML('#submitA{color:white;background-color: #BB8FCE}')),
                        tags$style(HTML('#new{color:white;background-color: #BB8FCE}')),
                        tags$style(HTML('#start_timer{color:white;background-color: #BB8FCE}')),
                        tags$style(HTML('#set{color:white;background-color: #BB8FCE}')),
                        tags$style(HTML('#reset{color:white;background-color: #BB8FCE}')),
                        tags$head(tags$style(HTML("
                            #analysis1 {
                              font-size: 14px;
                              background-color: #F5EEF8   
                            }
                            "))),
                        # tags$head(tags$style(HTML("
                        #     #p {font-size: 8px;
                        #         padding-left: 5px"))),
                       
                        tags$style(type='text/css', '#timeleft {background-color:#BB8FCE; font-size: 18px; 
                                   color:white;font-weight: bold;font family:Sans-serif;text-align: center; border-radius: 100px}'),
                        tags$style(type='text/css', '#a {background-color:#C39BD3; font-size: 12px; 
                                   color:white;font-weight: bold;font family:Sans-serif;text-align: center; border-radius: 80px}'),
                        tags$style(type='text/css', '#b {background-color:#C39BD3; font-size: 12px; 
                                   color:white;font-weight: bold;font family:Sans-serif;text-align: center; border-radius: 80px}'),
                        tags$style(type='text/css', '#c {background-color:#C39BD3; font-size: 12px; 
                                   color:white;font-weight: bold;font family:Sans-serif;text-align: center; border-radius: 80px}'),
                        
                        
                        ###format for the pre-req
                        tags$style(type='text/css', '#ano {background-color:#900C3F; font-size: 24px; padding:10px;height:180px; width:370px;
                                   color:white;font family:Sans-serif;text-align: center; border-radius: 80px}'),
                        
                        tags$style(type='text/css', '#regression {background-color:#C70039; font-size: 24px; padding:10px;height:180px; width:370px;
                                   color:white;font family:Sans-serif;text-align: center; border-radius: 80px}'),
                        
                        tags$style(type='text/css', '#anc {background-color:#FF5733; font-size: 24px;padding:10px;height:180px; width:370px;
                                   color:white;font family:Sans-serif;text-align: center; border-radius: 80px}'),
                        
                        tags$style(type='text/css', '#box1 {background-color:#F36DA1; font-size: 26px; padding:20px;height:180px; width:520px;
                                   color:white;font-weight: bold;font family:Sans-serif;text-align: center; border-radius: 80px}'),
                        
                        tags$style(type='text/css', '#box2 {background-color:#FFB3C9; font-size: 26px; padding:20px;height:180px; width:520px;
                                   color:white;font-weight: bold;font family:Sans-serif;text-align: center; border-radius: 80px}'),
                        
                        tags$style(type='text/css', '#box3 {background-color:#FF9881; font-size: 26px;padding:20px;height:180px; width:520px;
                                   color:white;font-weight: bold;font family:Sans-serif;text-align: center; border-radius: 80px}'),
                        tags$style(
                          HTML(".shiny-notification {
                               height: 100px;
                               width: 800px;
                               position:fixed;
                               top: calc(50% - 50px);;
                               left: calc(50% - 400px);;
                               }
                               "
                          )
                          )

                      ),
                      
                      
                      
                      tabItems(
                        
                        tabItem(tabName="prereq",
                                fluidRow(
                                  column(8,offset = 1, uiOutput("background1"))
                                ),
                                fluidRow(
                                  column(1,img(src = "right.png", width = 20)),
                                  column(7,uiOutput("background2"),style='margin-top:-1em')

                                ),
                               
                                fluidRow(
                                  column(1,img(src = "right.png", width = 20)),
                                  column(8,uiOutput("background3"),style='margin-top:-1em'),
                                  fluidRow( 
                                    column(2, offset=3,
                                           actionButton('pre2','Comparision of different analysis',style='padding:10px; font-size:100%;margin-top:-1.5em;margin-left:14em',class="circle grow")))
                                  ), hr(),
                                
                                fluidRow(
                                  column(1,img(src = "right.png", width = 20)),
                                  column(8,uiOutput("background4"),style='margin-top:-1em')
                                ),
                                fluidRow(column(11,offset=2, img(src='plot.png',width=550),style='margin-top:-1em')),
                                fluidRow(
                                  column(1,img(src = "right.png", width = 20)),
                                  column(8,uiOutput("background5"),style='margin-top:-1em')
                                ),
                                  fluidRow(
                                    column(3,offset=1,actionButton("start","Go to the overview",icon("bolt"),style='padding:10px; font-size:100%',class="circle grow"))
                                  )
                                  
                                
                        ),
                        
                        tabItem(tabName='box',
                                br(),
                                fluidRow(
                                  column(3, bsButton('ano',HTML('<b> X </b> :Categorical <br/>  Y: Continuous'),type = 'toggle')),
                                  column(1, 
                                         conditionalPanel("input.ano != 0",
                                                          img(src='line1.png',width=250,style='margin-top:6em;margin-left:6.5em'))
                                  ),
                                  column(5, offset = 2,
                                      conditionalPanel("input.ano != 0",
                                                       tags$a(uiOutput('box1'),href='http://shiny.science.psu.edu/auc39/ANOVA/',target="_blank"))
                                  )
                                  ),br(),br(),
                                fluidRow(
                                  column(3, bsButton('regression' ,HTML('<b> X </b>:continuous <br/>  Y: Continuous'),type = 'toggle')),
                                  column(1, 
                                         conditionalPanel("input.regression != 0",
                                                          img(src='line2.png',width=250,style='margin-top:6em;margin-left:6.5em'))
                                  ),
                                  column(5,offset=2,
                                      conditionalPanel("input.regression != 0",
                                                      tags$a(uiOutput('box2'),href='http://tjmcintyre.shinyapps.io/AssumptionsApp/',target="_blank"))
                                  )
                                ),br(),br(),
                                fluidRow(
                                  column(3, bsButton('anc',HTML('<b> X </b>:Categorical & continuous <br/>  Y: Continuous'),type = 'toggle')),
                                  column(1, 
                                         conditionalPanel("input.anc != 0",
                                                          img(src='line3.png',width=250,style='margin-top:6em;margin-left:6.5em'))
                                  ),
                                  column(5,offset=2,
                                      conditionalPanel("input.anc != 0",
                                                      uiOutput('box3'))
                                  )
                                ),br(),
                                fluidRow(
                                  column(3,offset=4,actionButton("go2","Go to the overview",icon("bolt"),style='padding:10px; font-size:100%',class="circle grow"))
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
                                  column(5, offset=1,actionButton("go","Go",icon("bolt"),style='padding:10px; font-size:100%',class="circle grow"))
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
                                
                                h3('ANCOVA Interaction Plot'),
                                sidebarLayout(
                                  sidebarPanel(
                                    
                                    selectInput('menu1','Select the Data',c('Otter','Diet','Random')),
                                    conditionalPanel("input.menu1=='Diet'",style='font-size:14px;',
                                                     tags$style(HTML(".radio-inline {margin-right: 8%;}")),
                                                     radioButtons('select_conti', 'Select Continous Variable',inline=TRUE, choices =c('Age','Height','Pre-diet Weight'), selected = 'Age'),
                                                     radioButtons('select_covar', 'Select Covariance',inline=TRUE,choices =c('Gender','Diet'), selected = 'Gender')
                                    ),
                                    
                                    conditionalPanel("input.menu1=='Random'",
                                                     sliderInput('slope1','Change the slope of Line A',-5,5,0,step=1),
                                                     sliderInput('slope2','Change the slope of Line B',-5,5,0,step=1),
                                                     sliderInput('inter1','Change the intersection of Line A',-5,5,0,step=1),
                                                     sliderInput('inter2','Change the intersection of Line B',-5,5,0,step=1),
                                                     sliderInput('sample','Change the sample size',100,800,100,step=50)
                                              
                                                     
                                      
                                    ),
                                    fluidRow(uiOutput('p'),align = "left")
                                  ),
                                  
                                  mainPanel(
                                    plotOutput('plot_gg'),
                                    bsPopover('plot_gg', 'Notice', 'Different lines represent different values of covariate. Remember intersection does not imply significant interaction.', placement = "bottom", trigger = "hover", options = NULL),
                                    tags$b(verbatimTextOutput('analysis1')),
                                    bsPopover('analysis1', 'ANOVA Table', 'Pay attention to the last column. Small p-value indicates siginificant influence or interaction.', placement = "top", trigger = "hover", options = NULL)
                                   
                                  )
                                )
                                
                                
                        ),
                        
                        
                        
                        tabItem(tabName='game',
                                useShinyalert(),
                                
                                
                                fluidRow(column(5,numericInput('seconds','Select the time limit (second)',value=60,min=60,max=300,step=120),
                                                bsPopover('seconds', 'Timer', 'Type in the time limit you want for this game. Click "Start" to start the timer and play the game', placement = "right", trigger = "hover", options = NULL),
                                fluidRow(column(2,actionButton('start_timer','Start',style='padding:5px; font-size:90%')),
                                         #column(2,actionButton('set','Set Timer',style='padding:5px; font-size:90%')),
                                         column(2,actionButton('reset','Reset',style='padding:5px; font-size:90%')),
                                         
                                         column(1, bsButton('bq1', '',icon = icon('info',class = "iconq fa-fw"),type = 'toggle', class = 'butt',style='padding:20px'),
                                                div(id = "plot-container1",
                                                    conditionalPanel("input.bq1 != 0",
                                                                     tags$img(src = "INS.png",
                                                                              id = "ins"))
                                                )
                                                ),
                                         
                                         column(1, bsButton('bq2', '',icon = icon('question',class = "iconq fa-fw"),type = 'toggle', class = 'butt',style='padding:20px'),
                                                div(id = "plot-container2",
                                                    conditionalPanel("input.bq2 != 0",
                                                                     tags$img(src = "STAT.png",
                                                                              id = "hint"))
                                                )
                                         )), br(),
                              
                                         fluidRow(
                                           valueBoxOutput("scoreBox"),
                                           valueBoxOutput('percentBox'),
                                           valueBoxOutput('timeBox')
                                         
                                         
                                         
                                         )
                                         ),
                                column(3,offset=4,textOutput("timeleft"))),
                                
                                fluidRow(column(2, offset=1,uiOutput('a'),align='right',style='margin-top:-1em;'),
                                         column(2,offset=2,uiOutput('b'),align='right',style='margin-top:-1em;'),
                                         column(2,offset=2,uiOutput('c'),align='left',style='margin-top:-1em;')),
                          
                                fluidRow(column(4,uiOutput('plot1')),
                                         column(4,uiOutput('plot2')),
                                         column(4,uiOutput('plot3'))
                                ),
                                
                                hr(),
                                fluidRow(column(4,uiOutput('table1'),style='margin-top:-2em;'),
                                         column(4,uiOutput('table2'),style='margin-top:-2em;'),
                                         column(4,uiOutput('table3'),style='margin-top:-2em;')
                                ),
                                
                                
                                fluidRow(column(3,offset=1,style='padding:20px;margin-top:-2em;font-size: 15px',fluidRow(radioButtons('radio1','',c('A','B','C'),selected='A',inline=TRUE),fluidRow(uiOutput('answer1'),style='margin-top:-1em;'))),
                                         column(3,offset=1,style='padding:20px;margin-top:-2em;font-size: 15px',fluidRow(radioButtons('radio2','',c('A','B','C'),selected='A',inline=TRUE),fluidRow(uiOutput('answer2'),style='margin-top:-1em;'))),
                                         column(3,offset=1,style='padding:20px;margin-top:-2em;font-size: 15px',fluidRow(radioButtons('radio3','',c('A','B','C'),selected='A',inline=TRUE),fluidRow(uiOutput('answer3'),style='margin-top:-1em;')))
                                         ),
                                  
                                 fluidRow(
                                 #column(4, uiOutput('correctC')),
                                 column(1,offset = 3,actionButton("submitA", "Submit Answer",style='padding:5px; font-size:110%;margin-top:-2em;')),
                                 column(1,offset = 3,actionButton("new","New>>",style='padding:5px; font-size:110%;margin-top:-2em;', disabled = FALSE))
                               )

                               
                               
                                )#closing for game section
                                
                                
                                
                        )
                      
                    
                    )                  
)

