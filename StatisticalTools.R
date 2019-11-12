#Measures of Centre-------
#mean
#median
#mode

#what is a typical value?

n <- rnorm(100, mean = 10, sd = 36)
hist(n)
mean(n)

x<-rnorm(10,5,2)
x
mean(x)

x = c(4,7,8,9,2000,89,45,8,8,9)
mean(x)
median(x)
#in this case median makes more sense of data representation

#comparing mean and median

t <- c(19.09, 19.55, 17.89, 17.73, 25.15, 27.27, 25.24, 21.05, 21.65, 20.92, 22.61, 15.71, 22.04, 22.60, 24.25)
hist(t)
par("mar")
par(mar=c(1,1,1,1))

#calculating left skew (asymetry in data) when mean<median
#https://www.statisticshowto.datasciencecentral.com/probability-and-statistics/skewed-distribution/
library(moments)
skewness(t)

#calculating right skew (when mean>median)
N <- 100
x <- rnbinom(N,10,.5)
hist(x)
skewness(x)

#eg
head(iris)
hist(iris$Petal.Length)
par("mar")
par(mar=c(5.1, 4.1, 4.1, 2.1)) #to adjust window size
skewness(iris$Petal.Length)
mean(iris$Petal.Length)
median(iris$Petal.Length)

range(iris$Petal.Length)#min and max value
#either mean or median can be used


#Measures of variation------
#standard deviation
#standard error
#interquartile range(IQR) - for median
sd(iris$Petal.Length)

library(sciplot)
se(iris$Petal.Length)


#charting and graphing continuous data-----
#boxplot[5 point data summary]
data("ToothGrowth")
head(ToothGrowth)

#comparing column data(y axis~X axis)
boxplot(len~supp,data=ToothGrowth)
#or (x axis,y axis)
library(ggplot2)
qplot(ToothGrowth$supp,ToothGrowth$len,geom = "boxplot")
#or(x axis,y axis)
ggplot(ToothGrowth,aes(x=supp,y=len))+geom_boxplot()

#outliers are plotted as point
#use coord_flip() for horizontal boxplot

#grouping data
ggplot(ChickWeight,aes(x=Time,y=weight))+geom_boxplot(aes(group=Time))+facet_grid(.~Diet)
#coloring boxplots
ggplot(diamonds,aes(factor(cut),price,fill=cut))+geom_boxplot()

#Representing discrete data using bar graphs-----

head(mtcars)
c<- table(mtcars$gear)
c
barplot(c,main="Car distribution",xlab="No of gears")

t = tapply(iris$Sepal.Length,iris$Species,mean)
t
barplot(t,main="Avg sepal length",xlab="Species",ylab="Mean")

library(ggplot2)
data(diamonds)
head(diamonds)

table(diamonds$color,diamonds$clarity)
summary(diamonds$color)
barplot(table(diamonds$color, diamonds$clarity),
        legend = levels(diamonds$color),           
        beside = TRUE)    


barplot( table(diamonds$color, diamonds$clarity),
         legend = levels(diamonds$color),           
         beside = TRUE,
         xlab = "Diamond Clarity",                      # Add a label to the X-axis
         ylab = "Diamond Count",                        # Add a label to the Y-axis
         main = "Diamond Clarity, Grouped by Color",    # Add a plot title
         col = c("#FFFFFF","#F5FCC2","#E0ED87","#CCDE57",     # Add color*
                 "#B3C732","#94A813","#718200") )

d=table(diamonds$color, diamonds$clarity)
######## USe GGPLOT for better graphs

# Very basic bar graph
qplot(factor(cyl), data=mtcars) #plot factor variables
#or
ggplot(mtcars, aes(x=factor(cyl))) + geom_bar()

qplot(color, data=diamonds, geom="bar") #specify bar

#stacked bars
head(diamonds)
ggplot(diamonds, aes(clarity, fill=cut)) + geom_bar(position="dodge")

ggplot(diamonds, aes(cut, fill=cut)) + geom_bar() + 
  facet_grid(. ~ clarity) #seperate panels on the basis of clarity

ship=as.data.frame(Titanic)
head(ship)
ggplot(aes(x=Age, weight=Freq), data=ship) +
  geom_bar()

ggplot(aes(x=Age, weight=Freq), data=ship) +
  geom_bar()+
  facet_grid(Sex~Class)


## error bar: error or uncertainty in a reported measurement (mean)
##one standard deviation of uncertainty, one standard error

library(dplyr)

isum= iris %>% # the names of the new data frame and the data frame to be summarised
  group_by(Species) %>%   # the grouping variable
  summarise(avg = mean(Petal.Length),  # calculates the mean of each group
            sdpl = sd(Petal.Length))

ggplot(isum, aes(Species, avg)) + geom_bar(stat="identity") +  geom_errorbar(aes(ymin=avg-sdpl, ymax=avg+sdpl),width=0.2)

#Deriving insights from qualitative/Nominal data(chi-square test)
############ H0: Two nominal variables (row and columns)
#have no association between them

#whether or not there is an association 
#between gender and food

# Entering the data into vectors
men = c(150, 120, 45)
women = c(320, 270, 100)

# combining the row vectors in matrices, then converting the matrix into a data frame
food.survey = as.data.frame(rbind(men, women))

# assigning column names to this data frame
names(food.survey) = c('Chicken', 'Salad', 'Cake')

food.survey
chisq.test(food.survey)
#Two nominal variables (row and columns) have no association between them cz p value is greater thn 0.05
#frequencies
library(MASS)   
levels(survey$Smoke) 
sfreq = table(survey$Smoke) 
sfreq


library(gmodels)
#2 way cross-tabulation- multivariate frequency table
#
#frequencies and relative frequencies
head(mtcars)
table(mtcars$carb, mtcars$cyl) 
CrossTable(mtcars$carb, mtcars$cyl, prop.t=TRUE, prop.r=TRUE, prop.c=TRUE)

#marginal totals: total of individual rows and columns
#grandtotal: 32 (total no of individuals in table)
#proportion of carb==1 (0.219) and cyl==4 (0.34)

#see which group is different. Needs an n*n matrix
library(fifer) 
# Makes a table of observations -- similar to first example in chisq.test
M <- as.table(rbind(c(76, 32, 46), c(48,23,47), c(45,34,78)))
M
dimnames(M) <- list(sex=c("Male","Female","Juv"),loc=c("Lower","Middle","Upper"))
M
chisq.test(M)

# Shows post-hoc pairwise comparisons using fdr method
chisq.post.hoc(M)

