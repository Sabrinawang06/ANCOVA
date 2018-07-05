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


aovdata$Y[aovdata$Z=='A']<-aovdata$Y[aovdata$Z=='A']*0.2+3



aov.model<-lm(Y~X+Z+Z:X,data=aovdata)
pred.aov <- expand.grid(X = 4.79:17.29, Z = c("A","B"))
pred.aov <- mutate(pred.aov, Y = predict(aov.model, pred.aov))

png('C:\\Users\\llfsh\\Desktop\\bank\\plot1.png')


ggplot(pred.aov, aes(x = X, y = Y, colour = Z)) + 
  geom_line(size=1) + geom_point(data = aovdata,size=3) + 
  xlab("X") + ylab("Y")+theme(text = element_text(size=15))

dev.off()

anova(aov.model)


capture.output(anova(aov.model),file="C:\\Users\\llfsh\\Desktop\\bank\\table1.txt")


############################
a<-1
b<---20
slope1<--1
slope2<--0.9

A<-'A'
B<-'B'

def <- defData(varname = "inter", dist = "nonrandom", formula = a, id = "id")

def<- defData(def,varname = "slope", dist = "nonrandom", formula = slope1, id = "slope")
def <- defData(def, varname = "X", dist = "uniform", formula = "0;50")
def <- defData(def, varname = "Y", formula = "inter + X * slope", variance = 100)

def2<- defData(varname = "inter", dist = "nonrandom", formula = b, id = "id")

def2 <- defData(def2,varname = "slope", dist = "nonrandom", formula = slope2, id = "slope")
def2<- defDataAdd(def2, varname = "X", dist = "uniform", formula = "0;50")
def2 <- defDataAdd(def2, varname = "Y", formula = "inter + X * slope", variance = 100)


dt <- genData(20, def)
dt2<-genData(20,def2)

names(dt2)[1]<-'id'

dt$cov<-'A'
dt2$cov<-'B'

comb<-rbind(dt,dt2)


aov.model<-lm(Y~X+cov+cov:X,data=comb)
pred.aov <- expand.grid(X =0:50, cov = c("A","B"))
pred.aov <- mutate(pred.aov, Y = predict(aov.model, pred.aov))


ggplot(pred.aov, aes(x = X, y = Y, colour = cov)) +
  geom_line() + geom_point(data = comb) +
  xlab("X") + ylab("Y")

anova(aov.model)




png('C:\\Users\\llfsh\\Desktop\\bank\\plot36.png')


ggplot(pred.aov, aes(x = X, y = Y, colour = cov)) + 
  geom_line(size=1) + geom_point(data = comb,size=3) + 
  xlab("X") + ylab("Y")+theme(text = element_text(size=15))

dev.off()

anova(aov.model)


capture.output(anova(aov.model),file="C:\\Users\\llfsh\\Desktop\\bank\\table36.txt")

#############################
library(MBESS)
library(MASS)

random.data <- ancova.random.data(mu.y=c(3,5), mu.x=10, sigma.y=1, 
                                  sigma.x=2, rho=.8, J=2, n=20)

ancova.random.data <- function(mu.y, mu.x, sigma.y, sigma.x, rho, J, n, randomized=TRUE)

  
head(random.data)  
random.data    


#################
data(women) # Load a built-in data called ‘women’
fit = lm(weight ~ height, women) # Run a regression analysis
plot(fit)
par(mfrow=c(2,2)) # Change the panel layout to 2 x 2
plot(fit)

summary(women)

par(mfrow=c(1,1)) # Change back to 1 x 1

data("mtcars")
summary(mtcars)
fit = lm(hp ~ cyl, mtcars) # Run a regression analysis
plot(fit)
par(mfrow=c(2,2)) # Change the panel layout to 2 x 2


plot(aov.model)

aov.model<-lm(Y~X+cov+cov:X,data=comb)
pred.aov <- expand.grid(X =10:20, cov = c("A","B"))
pred.aov <- mutate(pred.aov, Y = predict(aov.model, pred.aov))


ggplot(pred.aov, aes(x = X, y = Y, colour = cov)) + 
  geom_line() + geom_point(data = comb) + 
  xlab("X") + ylab("Y")


