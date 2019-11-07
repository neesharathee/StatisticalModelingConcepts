#vector-----

x = c(1,2,3,4,5,6)
length(x)

x = c(1,2,3,4,5,6)
print(length(x))
typeof(x)
x[2]


x=c(x,7,8)
x

x=c(x,"cat","dog",9)
x
typeof(x)
length(x)
x[1:3]

#series-----

series <- 1:10
series
seq(10)
 y = seq(1,10, by = 3)
y 
typeof(y)
typeof(as.integer(y))

#matrix------

m <- matrix(1:8, nrow =2, ncol = 4)
m
m2 <- matrix(1:6, nrow =3, ncol = 2)
m2

x<-1:5
y<-10:14
r<-c("cat","dog","rat","spider","gorilla")
cbind(x,y)
z <- cbind(x,y,r)
nrow(z)
ncol(z)
rbind(x,y)

#dataFrame-----

df1 = as.data.frame(z)
df1
str(df1)
length(df1)

data()
data(package = "datasets")
data("ChickWeight")
str(ChickWeight)
head(ChickWeight)

#reading csv -------
#set working directory
setwd("D:\\ML\\stats\\Sentimental-Analysis_R-master")
resp1 = read.csv("Resp1.csv")
head(resp1)
#column name will be readed as 1st row of data
resp1 = read.csv("Resp1.csv",header = F)
head(resp1,34)
str(resp1)

#reading txt----

resp2 = read.table("Resp2.txt",header = T)
head(resp2)


#reading xls----

library(readxl)
dfb<-read_excel("boston1.xls")
head(dfb)
summary(dfb)

#reading url-----

library(RCurl)
data = read.csv(text = getURL("https://raw.githubusercontent.com/sciruela/Happiness-Salaries/master/data.csv"))
head(data)

#indexing and subsetting data----------
data(iris)
??iris
str(iris)
summary(iris)
summary(iris$Species)
summary(iris$Sepal.Length)

#isolate first 2 columns
subset1 = iris[,1:2]
head(subset1)

#isolate cols by name
subset2 = iris[,c("Sepal.Length","Species","Petal.Length")]
head(subset2)

#isolate 1 col (will be turned in vector)
subset3 = iris[,"Sepal.Length"]
head(subset3)
#use drop to retain data frame
subset4 = iris[,"Sepal.Length", drop = FALSE]
head(subset4)

#subset by selecting variables
v <- c("Sepal.Length","Species","Petal.Length")
d <- iris[v]
head(d)
str(d)

#excluding columns by name
v <- names(iris)%in% c("Sepal.Length","Species")
d <- iris[!v]
head(d)
str(d)

#excluding columns by col index(starting from 1)
d <- iris[c(-3,-4,-5)]
head(d)

#subsetting based on col value
sub <- subset(iris,iris$Species =="setosa")
str(sub)
summary(sub)

#Dealing with NAs and missing data --------
data("airquality")
head(airquality)
#remove all rows containing "NA"
aq = na.omit(airquality)
head(aq)
#or
aq = airquality[complete.cases(airquality),]
head(aq)

#replace NA with 0
aq = airquality
aq[is.na(aq)]<-0
head(aq)

#replace missing values with average values--
#1st remove na
meanOzone = mean(airquality$Ozone,na.rm = T)
aq.fix = ifelse(is.na(airquality$Ozone),meanOzone,airquality$Ozone)
head(aq.fix) # doubt

#visualize the patterns of NAs
library(mice)
aqty2 = airquality
md.pattern(aq)
#or
library(VIM)
mp <- aggr(aqty2, col=c('navyblue','yellow'),
           numbers=TRUE, sortVars=TRUE,
           labels=names(aqty2), cex.axis=.7,
           gap=3, ylab=c("Missing data","Pattern"))

#imputations using predictive mean mapping
im <- mice(aqty2,m=5,maxit = 50,method = 'pmm',seed = 500)
summary(im)
im$imp$Ozone
completedData <-complete(im,1)
head(completedData)


#exploratory data analysis---------
#quantitative data--
names(iris)
hist(iris$Sepal.Length)

#descriptive stats
boxplot(iris$Sepal.Length,main="summary of iris data",xlab ="Sepal Length")

#relation bw 2 variables
plot(iris$Sepal.Length,iris$Sepal.Width)

#count variables
data(mtcars)
counts <- table(mtcars$gear)
counts

barplot(counts,main = "Car Data",xlab = "No of gears",horiz = TRUE,col = "pink")

#or
library(ggplot2)
qplot(Sepal.Length, Petal.Length, data = iris,color= Species, size = Petal.Length )
qplot(Sepal.Length, Petal.Length, data = iris, color = Species,
      xlab = "Sepal Length", ylab = "Petal Length",
      main = "Sepal vs. Petal Length in Iris data")
qplot(Sepal.Length, Petal.Length, data = iris, geom = "line", color = Species) #line plot

########################GGPLOT#####################

##use ggplot for viz
#Format: ggplot(data = , aes(x =, y =, ...)) + geom_xxx()
#aes-> we specify x,y 
#geom-> Plot type: wether its a histogram, boxplot
ggplot(data = iris, aes(Sepal.Length, Sepal.Width)) + geom_point()

#distinguish between species using colour scheme
ggplot(data = iris, aes(Sepal.Length, Sepal.Width)) + geom_point(aes(colour = (Species)))
#or
ggplot(iris, aes(x = Sepal.Length, y = Sepal.Width, shape = Species)) + geom_point()

## (1) we can only specify colour and shapes on factor variables
##(2)  Factors:numbers representing categorical values
##(3)function "factor" turns any number into a qualitative representation

str(mtcars)

#use mtcars as a factor in visualization
ggplot(mtcars, aes(x = mpg, y = wt, colour = factor(gear))) + geom_point()

#histogram
ggplot(iris, aes(x = Sepal.Length)) + geom_histogram()

ggplot(iris, aes(x = Sepal.Length, fill = Species)) + geom_histogram()

#boxplot
ggplot(iris, aes(x = Species, y = Sepal.Length)) +geom_boxplot()

#visualize relationship bw the different variables for the 3 species
ggplot(data = iris, aes(Sepal.Length, Sepal.Width)) + geom_point() + facet_grid(. ~ Species) + geom_smooth(method = "lm")
