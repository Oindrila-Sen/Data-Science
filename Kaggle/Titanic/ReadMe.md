---
title: "Titanic - What would have been my probability of survival if I had boarded that ship?"
author: "Oindrila Sen"
date: "12/6/2017"
output: 
  html_document: 
    highlight: tango
    theme: journal
    self_contained: false
    df_print: paged
    lib_dir: libs
    number_sections: yes
    toc: yes
    toc_float: yes
---
#Introduction

I was in my teens when I first came to know about Titanic.I sat in between my parents in the Movie Theatre, for the first time, to watch a Hollywood Blockbuster. I was mesmerized by the grandeur, the emotions, and the sadness. For the first time, I realized a ship named "Titanic" actually started sailing in 1912 and never returned. I imagined myself in that ship on my way back home from the Theatre and even for the following days. 

After 20 years of that unforgettable experience, I got a chance to re-live that feeling. My first Kaggle entry is the Titanic Dataset where I would explore if I had boarded the ship, what would have been my chances of survival?

Let's Load the data and start digging!

# Load Data

```{r}
library(dplyr)
library(ggplot2)
######################################
# 1. Load Data
######################################
# Set Working Directory
setwd("/Users/oindrilasen/WORK_AREA/Data Science/kaggle/Titanic")
# Read train.csv data file
titanic_train<-read.csv("train.csv",
                  header = TRUE,
                  na.strings = "",
                  stringsAsFactors = FALSE)

# Read test.csv data file
titanic_test<-read.csv("test.csv",
                        header = TRUE,
                        na.strings = "",
                        stringsAsFactors = FALSE)

# Add column Survived in Test DataSet
titanic_test$Survived <- NA
titanic_clean <- rbind(titanic_train,titanic_test)

# Take a look at the data
dim(titanic_train) # 891,12
dim(titanic_test) # 418, 11
glimpse(titanic_clean) # 1309, 12
```

# Data Wrangling & Cleaning

1. Convert to Factors:
First let's see which variables are more likely to be a Factor and then convert those variable to a Factor Data Type.
```{r}
# Check which variables are Factors
sapply(titanic_clean, function(x) length(unique(x)))
# Transforming categorical Variables to factors:
to_factor <- c(
  'Survived',
  'Pclass',
  'Sex',
  'Embarked'
)
for (col in to_factor) {
  titanic_clean[[col]] <- factor(titanic_clean[[col]])
}
```
2. Check for NA records:
Before proceeding further, let's find out which variables have NA values.
```{r}
# check for NA values
sapply(titanic_clean, function(x) sum(is.na(x)))
```
Now, we will replace the NA value for each variable with some meaningful value.
* Age: First, let's convert the Age variable to Integer and replace the NA values with the Mean of the Age data.
```{r}
# Convert Age column to Numeric
titanic_clean$Age <- as.integer((titanic_clean$Age))
# Relace NA values for Age with the mean
titanic_clean$Age[is.na(titanic_clean$Age)] <- mean(titanic_clean$Age,na.rm = TRUE)
```
* Cabin#:There are a couple of records where the Cabin# is not populated. Let's replace those records with "None"
```{r}
# Replace Cabin# with None for NA records
titanic_clean$Cabin[is.na(titanic_clean$Cabin)] <- "None"
```
* Embarked: There are three current values for Embarkation. Let's replace the NA values with the most common value as per our current Dataset.
```{r}
# Check for Embarked variable
table(titanic_clean$Embarked)
# Relace the Embarked value with the most common value i.e S
titanic_clean$Embarked[is.na(titanic_clean$Embarked)] <- "S"
```
* Fare: Fare has some NA values and we will replace those with the MEAN value of the Fare data.
```{r}
# Convert Fare column to Integer
titanic_clean$Fare <- as.integer((titanic_clean$Fare))
# Relace NA values for Fare with the Mean
titanic_clean$Fare[is.na(titanic_clean$Fare)] <- mean(titanic_clean$Fare,na.rm = TRUE)
```
Let's check again for NA records.
```{r}
# Again check for NA values
sapply(titanic_clean, function(x) sum(is.na(x)))
```
3. Let's change the value for a few variables with some meaningful data.
```{r}
# Change the levels to meaningful values
# 1. Pclass
levels(titanic_clean$Pclass)[levels(titanic_clean$Pclass)== "1"] <- "1st Class"
levels(titanic_clean$Pclass)[levels(titanic_clean$Pclass)== "2"] <- "2nd Class"
levels(titanic_clean$Pclass)[levels(titanic_clean$Pclass)== "3"] <- "3rd Class"

# 2. Embarked
levels(titanic_clean$Embarked)[levels(titanic_clean$Embarked)== "C"] <- "Cherbourg"
levels(titanic_clean$Embarked)[levels(titanic_clean$Embarked)== "Q"] <- "Queenstown"
levels(titanic_clean$Embarked)[levels(titanic_clean$Embarked)== "S"] <- "Southampton"
```
# Add New Features

* Fare Group: It looks like the Fare for the ship ranges from 0 to $512. Let's create a new field Fare_Group with values "Low", "Medium" and "High" as per the Fare value.
```{r}
summary(titanic_clean$Fare)
titanic_clean$Fare_Group <-factor(ifelse(titanic_clean$Fare >= 0 & titanic_clean$Fare <= 15, "Low",
                           ifelse(titanic_clean$Fare > 15 & titanic_clean$Fare <=100, "Medium",
                           ifelse(titanic_clean$Fare >100 ,"High",NA
                                               ))))

```
* Age_Group: Again, Age varies from an infant less than a year to an old person of around 80 years of age. So, add a new Age_Group column with values like "Baby","Kid","Teen" and "Adult".
```{r}
# Add new feature Age_Group
summary(titanic_clean$Age)

titanic_clean$Age_Group <-factor(ifelse(titanic_clean$Age<= 3, "Baby",
                          ifelse(titanic_clean$Age> 3 & titanic_clean$Age<=12, "Kid",
                          ifelse(titanic_clean$Age> 12 & titanic_clean$Age<=18, "Teen",
                          ifelse(titanic_clean$Age> 18, "Adult",NA
                                 ))))
)
```
* With_Family: As per the Dataset, if there is a value other than zero(0) in the fields "SibSp" and "Parch", the passenger is with a Family.
```{r}
# Add new feature with_family
titanic_clean$with_family <-factor(ifelse(titanic_clean$Parch == 0 & titanic_clean$SibSp ==0, "no","yes"))
```

# Exploratory Data Analysis

1.Class vs Total Passengers
```{r}
# Understand the Variables Individually
ggplot(titanic_clean, aes(x = Pclass)) +
  geom_bar(fill= "light blue")+
  scale_y_continuous(limits = c(0,1400), breaks = seq(0,1400,100))
  ggtitle("Class vs Total Passengers") 
```
It looks like most of the passengers were travelling in the 3rd Class. 1st class passengers count was a little more than that of the 2nd Class.

2. Sex vs Total Passengers
```{r}
prop.table((table(titanic_clean$Sex)))
ggplot(titanic_clean, aes(x = Sex)) +
    geom_bar(fill= "light blue")+
    scale_y_continuous(limits = c(0,1400), breaks = seq(0,1400,100))
  ggtitle("Sex vs Total Passengers") 
```
Around 65% of the passengers are Male and 35% are Female. 

3. Embarked vs Total Passengers:
```{r}
ggplot(titanic_clean, aes(x = Embarked)) +
    geom_bar(fill= "light blue")+
    scale_y_continuous(limits = c(0,1400), breaks = seq(0,1400,100))
  ggtitle("Embarked vs Total Passengers") 
```
Most of the Passengers boarded the ship at South Hampton. Around 250 passenegrs boarded at Cherbourg and around 120 boarded at Queenstown.

4.Age_Group vs Total Passengers
```{r}
ggplot(titanic_clean, aes(x = Age_Group)) +
    geom_bar(fill= "light blue")+
    scale_y_continuous(limits = c(0,1400), breaks = seq(0,1400,100))
  ggtitle("Age_Group vs Total Passengers") 
```
Most of the passenegrs were Adult which is obvious. But, there were a few babies and kids.

5. Fare_Group vs Total Passengers
```{r}
ggplot(titanic_clean, aes(x = Fare_Group)) +
    geom_bar(fill= "light blue")+
    scale_y_continuous(limits = c(0,1000), breaks = seq(0,1000,100))
  ggtitle("Fare_Group vs Total Passengers") 
```
Most of the passengers are in the "Low" Fare group. It tallys with the Data where we have seen that most of the passengers are in 3rd Class.

6. with_family vs Total Passengers
```{r}
prop.table(table(titanic_clean$with_family))

ggplot(titanic_clean, aes(x = with_family)) +
    geom_bar(fill= "light blue")+
    scale_y_continuous(limits = c(0,1000), breaks = seq(0,1000,100))
  ggtitle("with_family vs Total Passengers")   
```
It looks like 60% of the passengers were travelling alone and 30% were with a Family.

```{r}
  table(titanic_clean$with_family, titanic_clean$Age_Group)
   kids_without_family<-
    titanic_clean%>%
    filter(with_family=="no",Age_Group=="Kid")
  
  kids_without_family
```
