#remove all objects from R
rm(list=ls())

#set current working directory
setwd("E:/R/Major Project")

#get current working directory
getwd()
list.files()

#load data1 into R
data1= read.csv("Holdout for Testing.csv", header=T)

# Load packages
library('ggplot2')
library('ggthemes')
library('scales') 
library('dplyr') 
library('randomForest')
library("outliers")
library("caret")

#Exploratory data1 Analysis
#understand the data1 type
str(data1)



#Look at the block of data1
head(data1, 10)
tail(data1, 10)

summary(data1)

list(colnames(data1))

# Remove Duplicated Records
nrow(data1) - nrow(unique(data1))

# Drop cases with no member id

data1 = subset(data1,!is.na(data1$X3))

ncol=rep(nrow(data1) ,each=ncol(data1))
missingdata1=as.data.frame(cbind(colnames=names(data1),ncol,nmsg=as.integer(as.character(as.vector(apply(data1, 2, function(x) length(which(is.na(x)))))))))
missingdata1$nmsg=as.numeric(levels(missingdata1$nmsg))[missingdata1$nmsg]

missingdata1=cbind(missingdata1,percmissing=as.integer(missingdata1$nmsg/ncol*100))
drops=as.character(subset(missingdata1,missingdata1$percmissing>0)[,1])
drops
length(drops)
data1$X25= NULL
data1$X26= NULL


# Change Class

data1$X30=sub("%","",data1$X30) # Remove %
data1$X30=as.numeric(data1$X30) # Convert to numeric

# remove "$" & "," from data1 and convert them to numeric
data1$X4 = as.numeric(gsub("[[:punct:]]", "", data1$X4))
data1$X5 = as.numeric(gsub("[[:punct:]]", "", data1$X5))
data1$X6 = as.numeric(gsub("[[:punct:]]", "", data1$X6))

# data1 Cleansing

data1$X19=strtrim(data1$X19,3)


# package to handle strings
library(stringr)

head(data1[,c("X15","X23")])

data1$month_iss=str_split_fixed(data1$X15, "-", 2)[,1] # Extract Month Ch
data1$date_iss=str_split_fixed(data1$X15, "-", 2)[,2] # Extract date

data1$month_earl_cr=str_split_fixed(data1$X23, "-", 2)[,1] # Extract Month Ch
data1$year_earl_cr=str_split_fixed(data1$X23, "-", 2)[,2] # Extract Year

data1$X15=NULL
data1$X23=NULL


#Missing Analysis
sum(is.na(data1))
#Analyze missing values by varaible. You can omit variabl;e which have morethan 50% missing values
apply(data1,2, function(x)sum(is.na(x)))##coloumn level 2


library(VIM)
aggr_plot = aggr(data1, col=c('navyblue','red'), numbers=TRUE, sortVars=TRUE, labels=names(data1), 
                 cex.axis=.7, gap=3, ylab=c("Histogram of missing data1","Pattern"))

#conversion of variables
class=sapply(data1, class)
table(class)

class.character=names(class)[class=="character"]
class.numeric=names(class)[class=="numeric"]
class.integer=names(class)[class=="integer"]
data1[, c(class.integer)] <- as.numeric(unlist(data1[, c(class.integer)]))
class=sapply(data1, class)
table(class)

# Convert Character to Factor Variables

as.data.frame(rapply(data1[,c(class.character)],function(x)length(unique(x))))

data1_character= c("X19")
class.character=setdiff(class.character,data1_character)
as.data.frame(rapply(data1[,c(class.character)],function(x)length(unique(x))))

data1[,class.character] = lapply(data1[,class.character] , factor)
str(data1[,c(class.character)])


# Normalise the data1 by min-max method
nums1 = sapply(data1, is.numeric)

library(clusterSim)
for(i in c(2,4:6))(
  data1[,i] = data.Normalization(data1[,i], type = "n4", normalization = "column"))
for(i in c(13))(
  data1[,i] = data.Normalization(data1[,i], type = "n4", normalization = "column"))
for(i in c(20:27))(
  data1[,i] = data.Normalization(data1[,i], type = "n4", normalization = "column"))


##outlier
boxplot(data[,c(1,13,21,22,24,27,28,29,30,31)])
##removing outliers
for(i in 1:ncol(data1))
{ if(class(data1[,i]) == "numeric"){ outlier_tf = outlier(data1[,i],logical=TRUE) 
find_outlier = which(outlier_tf==TRUE, arr.ind=TRUE) 
data1 = data1[-find_outlier,] } 
  else {data1 = data1} }

#correlation matrix
library(corrplot)
numericColumns = !colnames(data1) %in% c('quality', 'color')
correlationMatrix = cor(data1[,nums1 ])
highlyCorrelated = findCorrelation(correlationMatrix, cutoff = 0.6)
colnames(correlationMatrix)[highlyCorrelated]
corrplot(correlationMatrix, method = 'number', tl.cex = 0.5)

# Remove highly correlated data and check again for correlation
data1 = data1[,c(-2,-4,-6)]
data1$X31=NULL

#subset X3 variable
predicted_rate_interest_cart = subset(data1, select=c(X3))
predicted_rate_interest_Regression = subset(data1, select=c(X3))

##removing vaibles of no use
data1 = subset(data1, , -c(X3,X5,X8,X10,X12,X16,X18,X19,month_iss,date_iss,month_earl_cr,year_earl_cr))



# Predict using the holdout data set

#Regression Tree Model,R.part
predictions = predict(fit, data1[,-1])
b=data.frame(predictions)
#predicted interest rate
predicted_rate_interest_Regression = cbind(predicted_rate_interest_Regression,b)
colnames(predicted_rate_interest_Regression)[2] = "Predicted_Interest_Rate"

write.csv(predicted_rate_interest_Regression, file = "predicted_rate_interest_Regression.csv")

#apply CART model
test_pred_cart = predict(fit_cart, data1[,-1], type="class")
a=data.frame(test_pred_cart)
#predicted interest rate
predicted_rate_interest_cart = cbind(predicted_rate_interest_cart,a)
colnames(predicted_rate_interest_cart)[2] = "Predicted_Interest_Rate"

write.csv(predicted_rate_interest_cart, file = "predicted_rate_interest_cart.csv")









