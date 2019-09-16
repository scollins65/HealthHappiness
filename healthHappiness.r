#Happiness and Health
#Stanton Collins scollins65@gatech.edu
#Created 9-15-2019
#World Happiness Report https://www.kaggle.com/unsdsn/world-happiness




install.packages("tydyverse")
install.packages("dplyr")
install.packages("RColorBrewer")
library(dplyr)
library(tidyverse)
library(RColorBrewer)

#Load the data.
setwd(choose.dir())
hapReport17 <- read.csv('2017.csv', header = TRUE)

#Cleaning up the data table and take a quick summary look.
hapScore<- select(hapReport17, Country, Happiness.Score, Economy..GDP.per.Capita., Health..Life.Expectancy.)
View(hapScore)
summary(hapScore)

#I am plotting health and happiness here.
plot(hapScore$Health..Life.Expectancy., hapScore$Happiness.Score, main = 'Happiness and Health', xlab = 'Health Score', ylab = 'Happiness Score', pch = 16)

#You can see on the plot that there is some relationship but let's put a regression
#line to see the connection better.
abline(lm(hapScore$Happiness.Score~hapScore$Health..Life.Expectancy), col='red')

#The regression line has a pretty steep angle, let's look at a cor.test.
cor.test(hapScore$Health..Life.Expectancy., hapScore$Happiness.Score)

#The cor.test indicates a pretty strong correlation.
#Now we can build a better plot with a smoothed line.
ggplot(data=hapScore, aes(x=Health..Life.Expectancy., y=Happiness.Score, size=Economy..GDP.per.Capita., alpha=0.1)) +
  geom_point(aes(col="Red")) + 
  geom_smooth(method=loess, se=FALSE, color="darkorchid4")

#Now let's see a linear regression data model
linearModel <- lm(hapScore$Happiness.Score~hapScore$Health..Life.Expectancy.)
summary(linearModel)
#this linear model shows an increadibly high coefficient. I will add GDP per Capita to help explain the variable.
linearModel2 <- lm(hapScore$Happiness.Score~hapScore$Health..Life.Expectancy. + hapScore$Economy..GDP.per.Capita.)
summary(linearModel2)
#Adding GDP reduces the coefficient and t value of Health indicating that not everything is being explained.
