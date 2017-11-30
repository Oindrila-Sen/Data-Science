library(tidyr)
library(dplyr)
library(ggplot2)
########################################
## 1. Loading Data
########################################
setwd(
  "/Users/oindrilasen/WORK_AREA/Data Science/Springboard/Exercise/Capstone Project/Data Sets"
)
Detail_listing <- read.csv(
  "Detail_listings.csv",
  header = TRUE,
  na.strings = "",
  stringsAsFactors = FALSE
)
glimpse(Detail_listing)
dim(Detail_listing)
########################################
## 2. Data Wrangling
########################################
# Eliminate useless columns:
# Remove some of the columns that won't be useful based on the goals
clean_listing <- select(
  Detail_listing,
  id,
  accommodates,
  host_response_rate,
  host_is_superhost,
  host_has_profile_pic,
  host_identity_verified,
  host_listings_count,
  instant_bookable,
  property_type,
  room_type,
  bathrooms,
  bedrooms,
  beds,
  bed_type,
  cancellation_policy,
  availability_365,
  price,
  weekly_price,
  monthly_price,
  cleaning_fee,
  security_deposit,
  number_of_reviews,
  review_scores_rating
)
glimpse(clean_listing)
# Remove "$" and "," from the price columns
# Transforming prices to number
clean_listing$price <-
  gsub("\\$", replacement = "", clean_listing$price)
clean_listing$price <-
  gsub(",", replacement = "", clean_listing$price)
clean_listing$price <- as.numeric(clean_listing$price)

clean_listing$security_deposit <-
  gsub("\\$", replacement = "", clean_listing$security_deposit)
clean_listing$security_deposit <-
  gsub(",", replacement = "", clean_listing$security_deposit)
clean_listing$security_deposit <-
  as.numeric(clean_listing$security_deposit)

clean_listing$cleaning_fee <-
  gsub("\\$", replacement = "", clean_listing$cleaning_fee)
clean_listing$cleaning_fee <-
  gsub(",", replacement = "", clean_listing$cleaning_fee)
clean_listing$cleaning_fee <- as.numeric(clean_listing$cleaning_fee)

clean_listing$weekly_price <-
  gsub("\\$", replacement = "", clean_listing$weekly_price)
clean_listing$weekly_price <-
  gsub(",", replacement = "", clean_listing$weekly_price)
clean_listing$weekly_price <- as.numeric(clean_listing$weekly_price)

clean_listing$monthly_price <-
  gsub("\\$", replacement = "", clean_listing$monthly_price)
clean_listing$monthly_price <-
  gsub(",", replacement = "", clean_listing$monthly_price)
clean_listing$monthly_price <-
  as.numeric(clean_listing$monthly_price)

# Transforming categorical Variables to factors:
to_factor <- c(
  'host_has_profile_pic',
  'host_response_rate',
  'host_is_superhost',
  'instant_bookable',
  'host_identity_verified',
  'property_type',
  'room_type',
  'bed_type',
  'cancellation_policy'
)
for (col in to_factor) {
  clean_listing[[col]] <- factor(clean_listing[[col]])
}
# before continuing let's deal with NA's
sapply(clean_listing, function (x)
  sum(is.na(x)))

## Assuming that NAs in the host_is_superhost columns are False
clean_listing$host_is_superhost[is.na(clean_listing$host_is_superhost)] <-
  "f"
## Replacing the NAs with the MEAN value of that column
clean_listing$bedrooms[is.na(clean_listing$bedrooms)] <-
  mean(clean_listing$bedrooms, na.rm = TRUE)
clean_listing$bathrooms[is.na(clean_listing$bathrooms)] <-
  mean(clean_listing$bathrooms, na.rm = TRUE)
clean_listing$beds[is.na(clean_listing$beds)] <-
  mean(clean_listing$beds, na.rm = TRUE)
## Assuming that NAs in the price columns are 0
clean_listing$cleaning_fee[is.na(clean_listing$cleaning_fee)] <- 0
clean_listing$security_deposit[is.na(clean_listing$security_deposit)] <-
  0
clean_listing$weekly_price[is.na(clean_listing$weekly_price)] <- 0
clean_listing$monthly_price[is.na(clean_listing$monthly_price)] <- 0
## Replacing the NAs with the MEAN value of that column
clean_listing$review_scores_value[is.na(clean_listing$review_scores_value)] <-
  mean(clean_listing$review_scores_value, na.rm = TRUE)
## Replacing the NAs 0 % rating
clean_listing$review_scores_rating[is.na(clean_listing$review_scores_rating)] <-
  0
# convert the records with "N/A" values to "NA"
levels(clean_listing$host_response_rate)[levels(clean_listing$host_response_rate) ==
                                           "N/A"] <- NA
# convert the NA records to "0%"
clean_listing$host_response_rate <-
  ifelse(is.na(clean_listing$host_response_rate),
         "0%",
         clean_listing$host_response_rate)
## Assuming that NAs in the host_identity_verified columns are False
clean_listing$host_identity_verified[is.na(clean_listing$host_identity_verified)] <-
  "f"
## Assuming that NAs in the host_has_profile_pic columns are False
clean_listing$host_has_profile_pic[is.na(clean_listing$host_has_profile_pic)] <-
  "f"
## Replacing the NAs with 1 for host_listings_count
clean_listing$host_listings_count[is.na(clean_listing$host_listings_count)] <-
  1
# Check again for NA values
sapply(clean_listing, function (x)
  sum(is.na(x)))
dim(clean_listing)
########################################
## 3. Exploratory Data Analysis:
########################################
## 3.1. Understand the variables individually
# 1. Price
summary(clean_listing$price)
ggplot(clean_listing, aes(x = price)) +
  geom_histogram(color = 'black',
                 fill = 'light blue',
                 na.rm = TRUE) +
  scale_x_continuous(limits = c(0, 1000), breaks = seq(0, 1000, 100)) +
  scale_y_continuous(limits = c(0, 10000), breaks = seq(0, 10000, 2000))
# 2. Property_type & Room_type
table(clean_listing$property_type, clean_listing$room_type)
ggplot(clean_listing, aes(x = property_type, fill = room_type)) +
  geom_bar(na.rm = TRUE) +
  theme(axis.text.x = element_text(angle = 65, vjust = 0.6))
# 3. Bed Type
ggplot(clean_listing, aes(x = bed_type)) +
  geom_bar(fill = 'light green', na.rm = TRUE)
# 4. Cancellation_Policy
ggplot(clean_listing, aes(x = cancellation_policy)) +
  geom_bar(fill = 'blue', na.rm = TRUE)
# 5. host_is_superhost
prop.table(table(clean_listing$host_is_superhost))
ggplot(clean_listing, aes(x = host_is_superhost)) +
  geom_bar(fill = 'orange')
# 6. host_identity_verified
prop.table(table(clean_listing$host_identity_verified))
ggplot(clean_listing, aes(x = host_identity_verified)) +
  geom_bar(fill = 'light blue')
# 7. instant_bookable
prop.table(table(clean_listing$instant_bookable))
ggplot(clean_listing, aes(x = instant_bookable)) +
  geom_bar(fill = 'yellow')
## 3.2. check how the individual feature is influencing the price of a listing
# 1.plot price vs number_of_reviews
ggplot(clean_listing, aes(x = number_of_reviews, y = price)) +
  geom_point() +
  scale_y_continuous(limits = c(0, 10000), breaks = seq(0, 10000, 1000)) +
  scale_x_continuous(limits = c(0, 600), breaks = seq(0, 600, 100))
# 2.plot price vs bedrooms
ggplot(clean_listing, aes(x = factor(bedrooms), y = price)) +
  geom_boxplot(na.rm = TRUE) +
  ylim(50, 1000)
# 3.plot price vs bathrooms
ggplot(clean_listing, aes(x = factor(bathrooms), y = price)) +
  geom_boxplot(na.rm = TRUE) +
  ylim(50, 1000)
# 4.plot price vs cancellation_policy
ggplot(clean_listing, aes(x = cancellation_policy, y = price)) +
  geom_bar(
    stat = "identity",
    width = .5,
    fill = 'light blue',
    na.rm = TRUE
  ) +
  scale_y_continuous(limits = c(0, 1000), breaks = seq(0, 1000, 100))
# 5.plot price vs property_type
ggplot(clean_listing, aes(x = property_type, y = price)) +
  geom_boxplot(na.rm = TRUE) +
  ylim(00, 1000) +
  theme(axis.text.x = element_text(angle = 65, vjust = 0.6))
# 6.plot price vs room_type
ggplot(clean_listing, aes(x = room_type, y = price)) +
  geom_boxplot(na.rm = TRUE) +
  ylim(00, 1000)
# 6.plot price vs bed_type
ggplot(clean_listing, aes(x = bed_type, y = price)) +
  geom_boxplot(na.rm = TRUE) +
  ylim(00, 1000)
########################################
## 4. Add New Features
########################################
# Check the Price Column
summary(clean_listing$price)
quantile(clean_listing$price, probs = seq(0.1, 0.9, 0.1))
# Add Price_Range
clean_listing$Price_Range <-
  ifelse(
    clean_listing$price <= 50,
    "Cheap",
    ifelse(
      clean_listing$price > 50 &
        clean_listing$price <= 150,
      "Budget Friendly",
      ifelse(
        clean_listing$price > 150 & clean_listing$price <= 250,
        "Mid Range",
        ifelse(
          clean_listing$price > 250 & clean_listing$price <= 500,
          "High Range",
          ifelse(clean_listing$price > 500, "Premium",
                 NA)
        )
      )
    )
  )
table(clean_listing$Price_Range)

# Divide the dataset into Budget and Premium Listings based o the price range
Budget_listing <-
  clean_listing %>%
  filter(Price_Range %in% c("Budget Friendly", "Cheap", "Mid Range"))

Premium_listing <-
  clean_listing %>%
  filter(Price_Range %in% c("High Range", "Premium"))

dim(Budget_listing)
########################################
## 5. Create a Model
########################################
set.seed(150)
set.seed(150)
#Sample Indexes
indexes = sample(1:nrow(Budget_listing), size = 0.2 * nrow(Budget_listing))
# Split dataset into training and test set
test_data = Budget_listing[indexes, ]
train_data = Budget_listing[-indexes, ]

dim(train_data)
dim(test_data)

# Create a Linear Regression Model
lm_model <-
  lm(
    price ~  host_identity_verified + instant_bookable + room_type + bedrooms +
      beds + availability_365 + host_listings_count + accommodates + security_deposit +
      cleaning_fee + weekly_price + Price_Range,
    data = train_data
  )
summary(lm_model)
plot(lm_model)
summary(lm_model)
plot(lm_model)

# Analyze the Model
budget_pred <- predict(lm_model, test_data)
head(budget_pred)
head(test_data$price)
SSE <- sum((test_data$price - budget_pred) ^ 2)
SST <- sum((test_data$price - mean(test_data$price)) ^ 2)
R_Squared_Value_budget <- 1 - SSE / SST
R_Squared_Value_budget

# Premium Model
dim(Premium_listing)
#Sample Indexes
prem_indexes = sample(1:nrow(Premium_listing), size = 0.2 * nrow(Premium_listing))
# Split dataset into training and test set
prem_train_data = Premium_listing[-prem_indexes, ]
prem_test_data = Premium_listing[prem_indexes, ]

dim(prem_test_data)
dim(prem_train_data)

set.seed(75)
#Sample Indexes
prem_indexes = sample(1:nrow(Premium_listing), size = 0.2 * nrow(Premium_listing))

# Split dataset into training and test set
prem_train_data = Premium_listing[-prem_indexes, ]
prem_test_data = Premium_listing[prem_indexes, ]
dim(prem_test_data)  # 94, 32
dim(prem_train_data) # 378, 32

lm_prem_model <-
  lm(
    price ~  host_identity_verified + bathrooms + beds + availability_365 + security_deposit +
      cleaning_fee + weekly_price + monthly_price + Price_Range++host_is_superhost,
    data = prem_train_data
  )

summary(lm_prem_model)
# Analyze the Premium Model
SSE <- sum((test_data$price - budget_pred) ^ 2)
SST <- sum((test_data$price - mean(test_data$price)) ^ 2)
R_Squared_Value_budget <- 1 - SSE / SST
R_Squared_Value_budget

########################################
## 6. Check the Relation Between Price and Review Score
########################################

tapply(clean_listing$review_scores_rating,
       clean_listing$Price_Range,
       mean,
       na.rm = TRUE)
ggplot(clean_listing, aes(x = Price_Range, y = review_scores_rating)) +
  stat_summary(fun.data = "mean_cl_boot")
