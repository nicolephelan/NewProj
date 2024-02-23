# Homework 6 My Data

z <- read.table(file="Burnham_field_data_bombus_seasonal_variation_Dataset.csv",header=TRUE,sep=",")
str(z)
summary(z)

z <- na.omit(z)
z$myvar <- z$pathogen_load

library(ggplot2) # for graphics
library(MASS) # for maximum likelihood estimation
library(tidyverse)

z <-z%>%filter(pathogen_load>0)%>%
  mutate(myvar=log(pathogen_load))

sum(z$pathogen_load>0)

p1 <- ggplot(data=z, aes(x=myvar, y=after_stat(density))) +
  geom_histogram(color="grey60",fill="cornsilk",size=0.2) 
print(p1)

p1 <- p1 + geom_density(linetype="dotted",size=0.75)
print(p1)

normPars <- fitdistr(z$myvar,"normal")
print(normPars)
str(normPars)
normPars$estimate["mean"] # note structure of getting a named attribute

meanML <- normPars$estimate["mean"]
sdML <- normPars$estimate["sd"]

xval <- seq(0,max(z$myvar),len=length(z$myvar))

stat <- stat_function(aes(x = xval, y = ..y..), fun = dnorm, colour="red", n = length(z$myvar), args = list(mean = meanML, sd = sdML))
p1 + stat

expoPars <- fitdistr(z$myvar,"exponential")
rateML <- expoPars$estimate["rate"]

stat2 <- stat_function(aes(x = xval, y = ..y..), fun = dexp, colour="blue", n = length(z$myvar), args = list(rate=rateML))
p1 + stat + stat2

stat3 <- stat_function(aes(x = xval, y = ..y..), fun = dunif, colour="darkgreen", n = length(z$myvar), args = list(min=min(z$myvar), max=max(z$myvar)))
p1 + stat + stat2 + stat3

gammaPars <- fitdistr(z$myvar,"gamma")
shapeML <- gammaPars$estimate["shape"]
rateML <- gammaPars$estimate["rate"]

stat4 <- stat_function(aes(x = xval, y = ..y..), fun = dgamma, colour="brown", n = length(z$myvar), args = list(shape=shapeML, rate=rateML))
p1 + stat + stat2 + stat3 + stat4

pSpecial <- ggplot(data=z, aes(x=myvar/(max(myvar + 0.1)), y=..density..)) +
  geom_histogram(color="grey60",fill="cornsilk",size=0.2) + 
  xlim(c(0,1)) +
  geom_density(size=0.75,linetype="dotted")

betaPars <- fitdistr(x=z$myvar/max(z$myvar + 0.1),start=list(shape1=1,shape2=2),"beta")
shape1ML <- betaPars$estimate["shape1"]
shape2ML <- betaPars$estimate["shape2"]

statSpecial <- stat_function(aes(x = xval, y = ..y..), fun = dbeta, colour="orchid", n = length(z$myvar), args = list(shape1=shape1ML,shape2=shape2ML))
pSpecial + statSpecial

simData <- rgamma(n=length(z$myvar),shape=shapeML,rate=rateML)
id <- 1:length(z$myvar)
sim_frame <- data.frame(simData,id)

str(simData)
summary(simData)


p1 <- ggplot(data=sim_frame, aes(x=simData, y=after_stat(density))) +
  geom_histogram(color="grey60",fill="cornsilk",size=0.2) 
print(p1)
