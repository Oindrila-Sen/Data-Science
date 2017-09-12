# Data Wrangling Exercise -2
#----------------------------------------
# 0. Load Titanic_Original.csv data set
#----------------------------------------
titanic_clean <- read.csv("~/WORK_AREA/Data Science/Springboard/Exercise/Exercise-2/titanic_original.csv", header = TRUE)
#----------------------------------------
# 1. Port of embarkation
# The embarked column has some missing values, 
# which are known to correspond to passengers who actually embarked at Southampton. 
# Find the missing values and replace them with S. 
#----------------------------------------
titanic_clean$embarked <- gsub("^$", "S", titanic_clean$embarked)
#----------------------------------------
# 2. Age
# Calculate the mean of the Age column and use that value to populate the missing values
#----------------------------------------
print(class(titanic_clean$age))
titanic_clean$age[is.na(titanic_clean$age)] <- mean(titanic_clean$age,na.rm = TRUE)
#----------------------------------------
# 3. Lifeboat
# There are a lot of missing values in the boat column. 
# Fill these empty slots with a dummy value e.g. the string 'None' or 'NA'
#----------------------------------------                                                                                                                                                     
print(class(titanic_clean$boat)) 
levels <- levels(titanic_original$boat)
print(levels)
levels[length(levels) + 1] <- "None"
titanic_clean$boat <- gsub("^$","None", titanic_clean$boat)
#---------------------------------------- 
# 4. Cabin
# Create a new column has_cabin_number which has 1 if there is a cabin number, and 0 otherwise.
#---------------------------------------- 
titanic_clean$has_cabin_number <- ifelse(gsub("^$","0",titanic_clean$cabin) == 0,0,
                                         1)
View(titanic_clean)

# Write and Export the clean data to a CSV file
write.csv(titanic_clean, file = "~/WORK_AREA/Data Science/Springboard/Exercise/Exercise-2/titanic_clean.csv")
