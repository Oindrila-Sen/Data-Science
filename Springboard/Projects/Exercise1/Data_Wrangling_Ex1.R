#Data Wrangling Exercise1
#--------------------------
# Install Packages
#-------------------------
#install.packages("tidyverse")
#------------------------
# Load Refine_original.csv
#--------------------------
refine_original <- read.csv("~/WORK_AREA/Data Science/Springboard/Exercise/Exercise-1/refine_original.csv",header=TRUE, sep=",")
str(refine_original)
head(refine_original)
View(refine_original)
#--------------------------
# Load tidyr
#--------------------------
library(tidyverse)
#--------------------------
#1. Cleaning up brand names
# Clean up the 'company' column so that all of the 
# misspellings of the brand names are standardized and all in lowercase.
#--------------------------
refine_original$company <- gsub("^.*ps.*$","philips", refine_original$company, ignore.case = TRUE)
refine_original$company <- gsub("^.*ak.*$","akzo", refine_original$company, ignore.case = TRUE)
refine_original$company <- gsub("^.*uni.*$","unilever", refine_original$company, ignore.case = TRUE)
refine_original$company <- gsub("^.*van.*$","van houten", refine_original$company, ignore.case = TRUE)
#View(refine_original)
#--------------------------
#2. Separating product code and number into separate columns
#--------------------------
refine_clean <-separate(refine_original,Product.code...number, c("product_code","product_number"), sep = "-")
#--------------------------
# 3. Adding product categories as per the product code value
#--------------------------
refine_clean <- mutate(refine_clean, 
                              product_category = ifelse(product_code == "p","Smartphone",
                                                 ifelse(product_code == "v","TV",
                                                 ifelse(product_code == "x","Laptop", 
                                                 ifelse(product_code == "q","Tablet",
                                                        "NA"
                                                        )))))
#--------------------------
# 4. Adding full address for geocoding
# Adding a new Column cobcatenating the data in three other columns
#--------------------------
refine_clean <- mutate(refine_clean,
                          full_address = paste(address,city,country, sep = ","))

#--------------------------
# 4. Creating dummy variables for company and product category
#--------------------------
refine_clean$company_philips <- ifelse(refine_clean$company=="philips", 1, 0)
refine_clean$company_akzo <- ifelse(refine_clean$company=="akzo", 1, 0)
refine_clean$company_van_houten <- ifelse(refine_clean$company=="van houten", 1, 0)
refine_clean$company_unilever <- ifelse(refine_clean$company=="unilever", 1, 0)

# product category
refine_clean$product_smartphone <- ifelse(refine_clean$product_category=="Smartphone", 1, 0)
refine_clean$product_tv <- ifelse(refine_clean$product_category=="TV", 1, 0)
refine_clean$product_laptop <- ifelse(refine_clean$product_category=="Laptop", 1, 0)
refine_clean$product_tablet <- ifelse(refine_clean$product_category=="Tablet", 1, 0)

View(refine_clean)

# Write and Export the clean data to a CSV file
write.csv(refine_clean, file = "~/WORK_AREA/Data Science/Springboard/Exercise/Exercise-1/refine_clean.csv")
