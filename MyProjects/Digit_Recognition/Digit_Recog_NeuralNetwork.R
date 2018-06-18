########################################
## 1. Load Data
########################################
# Set Working Directory
setwd("/Users/oindrilasen/WORK_AREA/Data Science/My Coding/DigitRecognition/")
datafile<-read.table("ex4data1.csv",stringsAsFactors = FALSE, sep = ',')
outputs<-read.table("output.csv",stringsAsFactors = FALSE, sep = ',')
# Check the structure
str(datafile)
dim(datafile)
# Merge datafile and output file
digits<-cbind(outputs,datafile)
dim(digits)
colnames(digits)
# change the first column as  output
colnames(digits)[1] <- "output"
# create output matrix
library(nnet)
outputM <- class.ind(digits$output)
# Create column names for the new output matrix
colnames(outputM)<-paste0('out.',colnames(outputM))
outputM.names<-colnames(outputM)
# Check all the input variable names
input.names<-colnames(datafile)
# Merge the existing digit dataframe  and output Matrix
digits<-cbind(digits,outputM)
dim(digits)
str(digits)
# Check the columns of the new Modified dataframe
colnames(digits)
########################################
## 2. Divide Data into training and Test Set
########################################
set.seed(170)
#Sample Indexes
indexes = sample(1:nrow(digits), size = 0.3 * nrow(digits))
# Split dataset into training and test set
test_data = digits[indexes, ]
train_data = digits[-indexes, ]

dim(train_data)
dim(test_data)
#View(train_data)
########################################
## 3.Exploratory Analysis
########################################
# Check for unique values in Output
unique(train_data$output)
digitTable <- table(train_data$output)
digitTable
class(train_data$output)
library(ggplot2)
ggplot(train_data,aes(x = factor(output), fill = output ))+
  geom_bar()+
  xlab("Digits")+
  ylab("Digit Count")+
  ggtitle("Total Number of Digits in Training Set")

# Display a few digits from training set
plotResults <- function(images, preds){
  op <- par(no.readonly=TRUE)
  x <- ceiling(sqrt(length(images)))
  par(mfrow=c(x,x), mar=c(.1,.1,.1,.1))
  
  for (i in images){
    m <- matrix(as.numeric(unlist(train_data[i,-1])),nrow=20)
    m <- apply(m, 2, rev)
    image(t(m), col=grey.colors(255), axes=FALSE)
    text(0.05,0.1,col="red", cex=1.2, preds[i])
  }
  par(op)
}

plotResults(1050:1100, train_data$output)
########################################
## 4. Create a Neural Network Model 
########################################
library(neuralnet)
# Set up the formula
set.seed((1676))
nnModel <- neuralnet(formula = paste(paste(outputM.names,collapse='+'),'~',
                                     paste(input.names,collapse='+')),
                     data = train_data[,],
                     hidden = 10,
                     linear.output = FALSE,
                     algorithm='rprop+',
                     learningrate=0.01,
                     rep=1)
summary(nnModel) 
plot(nnModel)
nnModel$result.matrix
########################################
## 5. Prediction
########################################
# Test Data
resTest<- neuralnet::compute(nnModel,test_data[,input.names])
round(resTest$net.result)
picksTest<-apply(resTest$net.result,1,which.max)
prop.table(table(test_data$output==picksTest))

test_data$nnPredict <- apply(resTest$net.result,1,which.max)

nnconfMat <-table(`Actual Output` = test_data$output, `Predicted Output` =test_data$nnPredict)
nnaccuracy <- sum(diag(nnconfMat))/sum(nnconfMat)
nnaccuracy

test_data$output[182]
test_data$nnPredict[182]

library(dplyr)
test_data%>%
  select(output,nnPredict)%>%
  arrange(test_data,output)

# Generate a Random Number
dim(test_data)
randomNum <- sample(1:1500, 1)
test_data$output[randomNum]
test_data$nnPredict[randomNum]