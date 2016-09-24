#remove all objects from R
rm(list=ls())

#set current working directory
setwd("E:/R/Major Project")

#get current working directory
getwd()
list.files()

#load data into R
data= read.csv("Data for Cleaning & Modeling.csv", header=T)

# Load packages
library('ggplot2')
library('ggthemes')
library('scales') 
library('dplyr') 
library('randomForest')
library("outliers")
library("caret")

#Exploratory data Analysis
#understand the data type
str(data)

#Look at the block of data
head(data, 10)
tail(data, 10)

summary(data)

list(colnames(data))

# Remove Duplicated Records
nrow(data) - nrow(unique(data))

# Drop cases with no member id

data = subset(data,!is.na(data$X3))

ncol=rep(nrow(data) ,each=ncol(data))
missingdata=as.data.frame(cbind(colnames=names(data),ncol,nmsg=as.integer(as.character(as.vector(apply(data, 2, function(x) length(which(is.na(x)))))))))
missingdata$nmsg=as.numeric(levels(missingdata$nmsg))[missingdata$nmsg]

missingdata=cbind(missingdata,percmissing=as.integer(missingdata$nmsg/ncol*100))
drops=as.character(subset(missingdata,missingdata$percmissing>0)[,1])
drops
length(drops)
##drop varibles
data$X25= NULL
data$X26= NULL


# Change Class

data$X1=sub("%","",data$X1) # Remove %
data$X1=as.numeric(data$X1) # Convert to numeric
data$X30=sub("%","",data$X30) # Remove %
data$X30=as.numeric(data$X30) # Convert to numeric

# remove "$" & "," from data and convert them to numeric
data$X4 = as.numeric(gsub("[[:punct:]]", "", data$X4))
data$X5 = as.numeric(gsub("[[:punct:]]", "", data$X5))
data$X6 = as.numeric(gsub("[[:punct:]]", "", data$X6))

# Data Cleansing

data$X19=strtrim(data$X19,3)


# package to handle strings
library(stringr)

head(data[,c("X15","X23")])

data$month_iss=str_split_fixed(data$X15, "-", 2)[,1] # Extract Month Ch
data$date_iss=str_split_fixed(data$X15, "-", 2)[,2] # Extract date

data$month_earl_cr=str_split_fixed(data$X23, "-", 2)[,1] # Extract Month Ch
data$year_earl_cr=str_split_fixed(data$X23, "-", 2)[,2] # Extract Year

##no use of these varibles 
data$X15=NULL
data$X23=NULL


# Remove NA
data = na.omit(data)


#Missing Analysis
sum(is.na(data))
#Analyze missing values by varaible. You can omit variabl;e which have morethan 50% missing values
apply(data,2, function(x)sum(is.na(x)))##coloumn level 2

#Store Values in data frame
MissingData = data.frame(varaibles = colnames(data), MissingInfo = apply(data,2,function(data)sum(is.na(data))))
row.names(MissingData)=NULL

##missing data plot
library(VIM)
aggr_plot = aggr(data, col=c('navyblue','red'), numbers=TRUE, sortVars=TRUE, labels=names(data), 
                 cex.axis=.7, gap=3, ylab=c("Histogram of missing data","Pattern"))

#conversion of variables
class=sapply(data, class)
table(class)

class.character=names(class)[class=="character"]
class.numeric=names(class)[class=="numeric"]
class.integer=names(class)[class=="integer"]
data[, c(class.integer)] <- as.numeric(unlist(data[, c(class.integer)]))
class=sapply(data, class)
table(class)

# Convert Character to Factor Variables

as.data.frame(rapply(data[,c(class.character)],function(x)length(unique(x))))

data_character= c("X19")
class.character=setdiff(class.character,data_character)
as.data.frame(rapply(data[,c(class.character)],function(x)length(unique(x))))


data[,class.character] = lapply(data[,class.character] , factor)
str(data[,c(class.character)])


# Normalise the data by min-max method
nums = sapply(data, is.numeric)

#Normalized Data
library(clusterSim)
for(i in c(2:6))(
  data[,i] = data.Normalization(data[,i], type = "n4", normalization = "column"))
for(i in c(13))(
  data[,i] = data.Normalization(data[,i], type = "n4", normalization = "column"))
for(i in c(20:27))(
  data[,i] = data.Normalization(data[,i], type = "n4", normalization = "column"))



##outlier
boxplot(data[,c(1,13,21,22,24,27,28,29,30,31)])
##removing outliers
for(i in 1:ncol(data))
{ if(class(data[,i]) == "numeric"){ outlier_tf = outlier(data[,i],logical=TRUE) 
find_outlier = which(outlier_tf==TRUE, arr.ind=TRUE) 
data = data[-find_outlier,] } 
  else {data = data} }


#divide data into train and test
train = data[sample(nrow(data), 190000, replace = F), ]
test = data[!(1:nrow(data)) %in% as.numeric(row.names(train)), ]


#correlation matrix
library(corrplot)
numericColumns = !colnames(train) %in% c('quality', 'color')
correlationMatrix = cor(train[,nums ])
highlyCorrelated = findCorrelation(correlationMatrix, cutoff = 0.6)
colnames(correlationMatrix)[highlyCorrelated]
corrplot(correlationMatrix, method = 'number', tl.cex = 0.5)

# Remove highly correlated data and check again for correlation
data = data[,c(-2,-4,-6)]
train= train[,c(-2,-4,-6)]
test= test[,c(-2,-4,-6)]
train$X31=NULL
test$X31=NULL

##removing vaibles of no use
data = subset(data, , -c(X3,X5,X8,X10,X12,X16,X18,X19,month_iss,date_iss,month_earl_cr,year_earl_cr))
train= subset(train, , -c(X3,X5,X6,X8,X10,X12,X16,X18,X19,month_iss,date_iss,month_earl_cr,year_earl_cr))
test= subset(test, , -c(X3,X5,X6,X8,X10,X12,X16,X18,X19,month_iss,date_iss,month_earl_cr,year_earl_cr))



##model building
#Regression Tree Model,R.part

library(rpart)
library(Metrics)
fit = rpart(X1 ~ ., data = train, method = "anova")
predictions = predict(fit, test[,-1])
mape = function(y, yhat)
  mean(abs((y - yhat)/y))
mape(test[,1], predictions)

library(DMwR)
regr.eval(test[,1], predictions, stats = c('mae','rmse','mape'))

cor(predictions,test$X1)

# Create a rank variable based on importance

importance_fit= varImp(fit)
varImportance_fit = data.frame(importance_fit)
varImportance_fit$variable= row.names(varImportance_fit)                            
row.names(varImportance_fit)=NULL
varImportance_fit=varImportance_fit[,c(2,1)]

# Create a rank variable based on importance
rankImportance_fit = varImportance_fit %>% 
  mutate(Rank = paste0('#',dense_rank(desc(importance_fit))))

write(capture.output(rankImportance_fit), "rankImportance_fit_Regression Tree.csv")
