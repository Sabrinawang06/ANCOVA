library(shiny)
library(png)
library(shinyBS)
library(shinyjs)
library(V8)
library(ggplot2)
library(dplyr)
library(shinydashboard)
library(simstudy)

aovdata <- read.csv("C:\\Users\\llfsh\\Desktop\\model2.csv",header=T)


aovdata$Y[aovdata$Z=='A']<-aovdata$Y[aovdata$Z=='A']*1/3+3


aov.model<-lm(Y~X+Z+Z:X,data=aovdata)
pred.aov <- expand.grid(X = 4.79:17.29, Z = c("A","B"))
pred.aov <- mutate(pred.aov, Y = predict(aov.model, pred.aov))



ggplot(pred.aov, aes(x = X, y = Y, colour = Z)) + 
  geom_line() + geom_point(data = aovdata) + 
  xlab("X") + ylab("Y")


anova(aov.model)


############################
a<-7
b<-5

slope1<-2
slope2<-4

def <- defData(varname = "inter1", dist = "nonrandom", formula = a, id = "idnum1")
def <- defData(def,varname = "slope1", dist = "nonrandom", formula = slope1, id = "slope1")
def <- defData(def, varname = "x1", dist = "uniform", formula = "10;20")
def <- defData(def, varname = "y", formula = "inter1 + x1 * slope1", variance = 8)

def <- defData(def,varname = "inter2", dist = "nonrandom", formula = b, id = "idnum2")
def <- defData(def,varname = "slope2", dist = "nonrandom", formula = slope2, id = "slope2")
def<- defData(def, varname = "x2", dist = "uniform", formula = "10;20")
def <- defData(def, varname = "y", formula = "inter2 + x2 * slope2", variance = 8)


dt <- genData(1000, def)

head(dt)

aov.model<-lm(Y~X+Z+Z:X,data=aovdata)
pred.aov <- expand.grid(X = 4.79:17.29, Z = c("A","B"))
pred.aov <- mutate(pred.aov, Y = predict(aov.model, pred.aov))



ggplot(pred.aov, aes(x = X, y = Y, colour = Z)) + 
  geom_line() + geom_point(data = aovdata) + 
  xlab("X") + ylab("Y")


#############################
library(MBESS)
library(MASS)

random.data <- ancova.random.data(mu.y=c(3,5), mu.x=10, sigma.y=1, 
                                  sigma.x=2, rho=.8, J=2, n=20)

ancova.random.data <- function(mu.y, mu.x, sigma.y, sigma.x, rho, J, n, randomized=TRUE)

  
head(random.data)  
    