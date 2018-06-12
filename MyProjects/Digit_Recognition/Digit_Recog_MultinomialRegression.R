########################################
## 1. Load Data
########################################
# Set Working Directory
setwd("/Users/oindrilasen/WORK_AREA/Data Science/My Coding/DigitRecognition/")
datafile<-read.table("ex4data1.csv",stringsAsFactors = FALSE, sep = ',')
outputs<-read.table("output.csv",stringsAsFactors = FALSE, sep = ',')
dim(datafile)
dim(outputs)
# merge the datafile and output file
digits <- cbind(datafile,outputs)
dim(digits)

#Check the column names for merged file
colnames(digits)
# change the last column as  output
colnames(digits)[401] <- "output"

# Convert The outputs to a Factor Column
class(digits$output)
digits$output = as.factor(digits$output)
str(digits$output)

#Find number of missing values/check ranges
sum(is.na(digits))
########################################
## 2. Divide Data into training and Test Set
########################################
set.seed(150)
#Sample Indexes
indexes = sample(1:nrow(digits), size = 0.3 * nrow(digits))
# Split dataset into training and test set
test_data = digits[indexes, ]
train_data = digits[-indexes, ]

dim(train_data)
dim(test_data)
########################################
## 3.Exploratory Analysis
########################################
# Check for unique values in Output
unique(train_data$output)
digitTable <- table(train_data$output)
digitTable

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
## 4. Create a Multinomial Logistic Regression Model 
########################################
library(nnet)
lModel <- multinom(output ~ ., data = train_data,MaxNWts=5000,maxit=1000)
lModel$AIC
lModel$convergence

########################################
## 5. Prediction
########################################
test_data$lpredicted_val <- predict(lModel, test_data,type = "class")
# Check the Actual Count
table(test_data$output)
# Check the Predicted Count
table(test_data$lpredicted_val)

# Check Accuracy of the Model
lconfMat <- table(`Actual Class` = test_data$output,`Multinomial Predicted Class` =test_data$lpredicted_val) 
laccuracy <- sum(diag(lconfMat))/sum(lconfMat)
laccuracy

test_data$output[182]
test_data$lpredicted_val[182]

# Generate a Random Number
dim(test_data)
randomNum <- sample(1:1500, 1)
test_data$output[randomNum]
test_data$lpredicted_val[randomNum]
