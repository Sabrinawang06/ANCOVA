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

class(aov.model)
############################
a<-7
b<-5
slope1<-2
slope2<-4


A<-'A'
B<-'B'

def <- defData(varname = "inter", dist = "nonrandom", formula = a, id = "id")

def<- defData(def,varname = "slope", dist = "nonrandom", formula = slope1, id = "slope")
def <- defData(def, varname = "X", dist = "uniform", formula = "10;20")
def <- defData(def, varname = "Y", formula = "inter + X * slope", variance = 8)

def2<- defData(varname = "inter", dist = "nonrandom", formula = b, id = "id")

def2 <- defData(def2,varname = "slope", dist = "nonrandom", formula = slope2, id = "slope")
def2<- defDataAdd(def2, varname = "X", dist = "uniform", formula = "10;20")
def2 <- defDataAdd(def2, varname = "Y", formula = "inter + X * slope", variance = 8)


dt <- genData(1000, def)
dt2<-genData(1000,def2)

names(dt2)[1]<-'id'

dt$cov<-'A'
dt2$cov<-'B'

comb<-rbind(dt,dt2)


aov.model<-lm(Y~X+cov+cov:X,data=comb)
pred.aov <- expand.grid(X =10:20, cov = c("A","B"))
pred.aov <- mutate(pred.aov, Y = predict(aov.model, pred.aov))


ggplot(pred.aov, aes(x = X, y = Y, colour = cov)) + 
  geom_line() + geom_point(data = comb) + 
  xlab("X") + ylab("Y")

anova(aov.model)
#############################
library(MBESS)
library(MASS)

random.data <- ancova.random.data(mu.y=c(3,5), mu.x=10, sigma.y=1, 
                                  sigma.x=2, rho=.8, J=2, n=20)

ancova.random.data <- function(mu.y, mu.x, sigma.y, sigma.x, rho, J, n, randomized=TRUE)

  
head(random.data)  
random.data    
