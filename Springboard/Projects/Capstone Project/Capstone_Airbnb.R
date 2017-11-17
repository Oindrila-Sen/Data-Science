#install.packages("gridExtra")
#############################################
# Load library
#############################################
library(tidyr)
library(dplyr)
library(ggplot2)
library(ggmap)
library(cluster)
#library(grid)
#library(gridExtra)
#############################################
# Load Data File
#############################################
Detail_listing <- read.csv("~/WORK_AREA/Data Science/Springboard/Exercise/Capstone Project/Data Sets/Detail_listings.csv", header = TRUE)
# Check the Structure of the DataFrame
str(Detail_listing) # 31,253 Obs, 95 Variables
View(Detail_listing)
#############################################
# Part 1:  Setup and initial data wrangling #
#############################################
# Check for unique values and decide on the importance of the variable
levels(Detail_listing$neighbourhood_group)
levels(Detail_listing$neighbourhood_cleansed)
unique(Detail_listing$host_acceptance_rate)
unique(Detail_listing$square_feet)
unique(Detail_listing$has_availability)
unique(Detail_listing$market)
unique(Detail_listing$host_identity_verified)
unique(Detail_listing$review_scores_value)
unique(Detail_listing$experiences_offered)
# -----------------------
# Remove not-so-important columns
# -----------------------
Detail_listing$scrape_id <- NULL
Detail_listing$last_scraped <- NULL
Detail_listing$thumbnail_url <- NULL
Detail_listing$medium_url <- NULL
Detail_listing$picture_url <- NULL
Detail_listing$xl_picture_url <- NULL
Detail_listing$host_url <- NULL
Detail_listing$host_thumbnail_url <- NULL
Detail_listing$host_picture_url <- NULL
Detail_listing$host_has_profile_pic <- NULL
Detail_listing$neighbourhood <- NULL
Detail_listing$neighbourhood_group <- NULL
Detail_listing$neighbourhood_group_cleansed <- NULL
Detail_listing$host_acceptance_rate <- NULL
Detail_listing$has_availability <- NULL
Detail_listing$license <- NULL
Detail_listing$country <- NULL
Detail_listing$city <- NULL
Detail_listing$state <- NULL
Detail_listing$zipcode <- NULL
Detail_listing$country_code <- NULL
Detail_listing$host_neighbourhood <- NULL
Detail_listing$market <- NULL
Detail_listing$calculated_host_listings_count <- NULL
Detail_listing$require_guest_phone_verification <- NULL
Detail_listing$require_guest_profile_picture <- NULL
Detail_listing$host_listings_count <- NULL
Detail_listing$host_total_listings_count <- NULL
Detail_listing$experiences_offered <- NULL
# -----------------------
# Adding proper meaning to existing column values
# -----------------------
Detail_listing$host_is_superhost <- ifelse(Detail_listing$host_is_superhost == "t", "Yes","No")
Detail_listing$host_identity_verified <- ifelse(Detail_listing$host_identity_verified == "t", "Yes","No")
Detail_listing$is_location_exact <- ifelse(Detail_listing$is_location_exact == "t", "Yes","No")
Detail_listing$requires_license <- ifelse(Detail_listing$requires_license == "t", "Yes","No")
Detail_listing$instant_bookable <- ifelse(Detail_listing$instant_bookable == "t", "Yes","No")
# -----------------------
# Replace the NULL Levels with 0 for below columns:
# Cleaning_fee 
# Scecurity_Deposit 
# weekly_price
# monthly_price
# -----------------------
levels(Detail_listing$cleaning_fee)[levels(Detail_listing$cleaning_fee)==""] <- "$0.00"
levels(Detail_listing$security_deposit)[levels(Detail_listing$security_deposit)==""] <- "$0.00"
levels(Detail_listing$weekly_price)[levels(Detail_listing$weekly_price)==""] <- NA
levels(Detail_listing$monthly_price)[levels(Detail_listing$monthly_price)==""] <- NA
#############################################
# Part 2:  Adding new Features #
#############################################
# -----------------------
# Add Total Price as the sum of price, cleaning_fee 
# -----------------------
price_df <- dplyr::select(Detail_listing, price,cleaning_fee, weekly_price, monthly_price)
str(price_df)
# Convert the Factor column into Number
indx <- sapply(price_df, is.factor) 
price_df[indx] <- lapply(price_df[indx], function(x) as.numeric(gsub("[,$]", "", x)))
# Add a new feature as total_price
price_df$total_price <- price_df$price + price_df$cleaning_fee 
# -----------------------
# Check if weekly or monthly booking is beneficial
# -----------------------
price_df$weekly_discount_available <- ifelse(((price_df$price *7) - price_df$weekly_price) > 0, "Yes", "No" )
price_df$monthly_discount_available <- ifelse(((price_df$price * 30) - price_df$monthly_price) > 0, "Yes", "No" )

Detail_listing$total_price <- price_df$total_price
Detail_listing$weekly_discount_available <- price_df$weekly_discount_available
Detail_listing$monthly_discount_available <- price_df$monthly_discount_available
# -----------------------
# Add a new Feature "Review_rating" based on Review_Scores_Value
# -----------------------
Detail_listing$review_scores_value <- ifelse(is.na(Detail_listing$review_scores_value),0,Detail_listing$review_scores_value)

Detail_listing$Review_Rating <- ifelse(Detail_listing$review_scores_value==0, "No-Rating",
                                ifelse(Detail_listing$review_scores_value <= 3, "Not-Good",
                                ifelse(Detail_listing$review_scores_value >3 & Detail_listing$review_scores_value <=5, "Average",
                                ifelse(Detail_listing$review_scores_value >5 & Detail_listing$review_scores_value <=8, "Good",         
                                ifelse(Detail_listing$review_scores_value >=9 , "Excellent", "NA"
                                       )))))

table(Detail_listing$Review_Rating)
# -----------------------
# Add a new Feature "Price_Range" based on total_price
# -----------------------                                     
# set.seed(10)
# kmc <- kmeans(Detail_listing$total_price, centers = 5, iter.max = 1000)
# str(kmc)
# plot(kmc$centers, kmc$size)
# Detail_listing$Price_group <- as.factor(kmc$cluster)
# Detail_listing$Price_group <- NULL
mean(Detail_listing$total_price)
quantile(Detail_listing$total_price,probs = seq(0.1,0.9,0.20))
Detail_listing$Price_Range <- ifelse(Detail_listing$total_price<=100, "Low Price",
                              ifelse(Detail_listing$total_price>100 & Detail_listing$total_price <=250, "Budget Friendly",
                              ifelse(Detail_listing$total_price>250 & Detail_listing$total_price <=500, "Mid Range",
                              ifelse(Detail_listing$total_price>500 & Detail_listing$total_price <=1000, "High Range",
                              ifelse(Detail_listing$total_price >1000, "Premium",
                                   NA )))))

table(Detail_listing$Price_Range)
# -----------------------
# Add new features:
# wifi
# free_parking
# kids_friendly
# Ac
# Washer/Dryer
# Swimming Pool
# TV
# -----------------------
Detail_listing$wifi <- grepl("Wireless Internet", Detail_listing$amenities) == TRUE
Detail_listing$free_parking <- grepl("Free parking on premises", Detail_listing$amenities) == TRUE
Detail_listing$kids_friendly <- grepl("Family/kid friendly", Detail_listing$amenities) == TRUE
Detail_listing$Ac <- grepl("Air conditioning", Detail_listing$amenities) == TRUE
Detail_listing$washer_dryer <- grepl("Washer,Dryer", Detail_listing$amenities) == TRUE
Detail_listing$pool <- grepl("pool", Detail_listing$amenities) == TRUE
Detail_listing$tv <- grepl("TV", Detail_listing$amenities) == TRUE
#############################################
# Part 3:  Analyzing the Data
#############################################
table(Detail_listing$Price_Range,Detail_listing$Review_Rating)
# -----------------------
# Add Map
# ----------------------
# getting the map
my_map <- get_map(location = c(lon = mean(Detail_listing$longitude), lat = mean(Detail_listing$latitude)), 
                  zoom = 10,
                  maptype = "roadmap",
                  scale="auto"
                  )
# plotting the map with some points on it
ggmap(my_map) +
  geom_point(data = Detail_listing, 
             aes(x = longitude, y = latitude, color = Price_Range),
             size =1,
             shape = 16,
             alpha = 0.5,
             position = "jitter")
# -----------------------
# Plot Reiew vs Price
# ----------------------
ggplot(Detail_listing, aes(x=total_price, y=review_scores_value)) + 
  geom_point(aes(color = Price_Range,shape = Price_Range), na.rm = TRUE, position = "jitter") + 
  scale_x_continuous(limits = c(0,2000),breaks = seq(0,2000,100))+
  scale_y_continuous(limits = c(0,10),breaks = seq(0,10,1))+
  geom_smooth(method = "loess") + 
  labs(subtitle="Price Vs Review", 
       y="Review Score", 
       x="Price Range", 
       title="AirBnb Scatterplot-1") +
    theme(axis.text.x = element_text(angle=65, vjust=0.6))
  
# ggplot(Detail_listing, aes(x=Price_group, y=review_scores_value)) + 
#   stat_summary(fun.y = "mean") + 
#   #geom_smooth(method="loess", se=F) + 
#   labs(subtitle="Price Vs Review", 
#        y="Review Score", 
#        x="Price Range", 
#        title="AirBnb Scatterplot-1")
# -----------------------
# Check Relation between Reviews and No. Of Bedrooms
# -----------------------
table(Detail_listing$bedrooms, Detail_listing$bed_type)
ggplot(Detail_listing, aes(x= review_scores_value, y = bedrooms, color = factor(bed_type)))+
  geom_point(size = 0.5, position = "jitter")+
  scale_x_continuous(limits = c(0,10),breaks = seq(0,10,1)) +
  scale_y_continuous(limits = c(0,10), breaks = seq(0,10,1))+
  labs(subtitle="No Of Bedrooms Vs Review", 
       x="Review Score", 
       y="No Of Bedrooms", 
       title="AirBnb Scatterplot-2")
# -----------------------
# Check Relation between Reviews and property_type
# -----------------------
table(Detail_listing$property_type, Detail_listing$room_type)
# Average review score for each property
property_vs_review<-
Detail_listing%>%
  group_by(property_type)%>%
  summarise(review_value = mean(review_scores_value),
            review_count = n())%>%
  arrange(desc(review_count), desc(review_value))

property_vs_review

# Plot review vs property_type
ggplot(Detail_listing, aes(x=property_type, y=review_scores_value,fill=room_type )) + 
  geom_bar(stat="identity", width=.5) +
  scale_y_continuous(limits = c(0,10), breaks = c(1:10)) +
labs(title="AirBnb BarPlot-3", 
     subtitle="review_score Vs Proper_Type")+
  theme(axis.text.x = element_text(angle=65, vjust=0.6))
# -----------------------
# Check Relation between Reviews and amneties
# -----------------------
listings_reviews<- 
  Detail_listing%>%
  filter(review_scores_value!=0) %>%
  select(id,review_scores_value,total_price,wifi,free_parking,kids_friendly,tv )

str(listings_reviews)

listings_no_reviews<- 
  Detail_listing%>%
  filter(review_scores_value==0) %>%
  select(id,review_scores_value,total_price,wifi,free_parking,kids_friendly,tv )

head(listings_no_reviews)

plot(review_scores_value ~ total_price, data = listings_reviews)

Review_model <- lm(review_scores_value ~ total_price + 
                                         wifi + 
                                         free_parking + 
                                         kids_friendly + 
                                         tv
                   ,data = listings_reviews)
summary(Review_model)
coefficients(Review_model)
confint(Review_model)

predict(Review_model,listings_no_reviews,se.fit = TRUE)
# -----------------------
# Check Popular Neighbourhoods
# -----------------------
popular_neighbourhoods <-
  Detail_listing %>%
  group_by(neighbourhood = neighbourhood_cleansed)%>%
  summarize(no_of_listings = n(), 
            avg_review_rating = mean(review_scores_value, na.rm = T)
            ) %>%
  arrange(desc(no_of_listings),desc(avg_review_rating))

View(popular_neighbourhoods)
##----------------------------------------
#Error in .Call.graphics(C_palette2, .Call(C_palette2, NULL)) : 
# invalid graphics state
##----------------------------------------
dev.cur()
dev.off()
# -----------------------
# My Favorite Listings
# -----------------------
my_fav_listings <-
Detail_listing %>%
  filter(Review_Rating == "Excellent", 
         Price_Range == "Budget Friendly",
         wifi == TRUE,
         free_parking == TRUE,
         kids_friendly == TRUE,
         tv == TRUE
  )%>%
    select(id,summary,review_scores_value,number_of_reviews,neighbourhood_cleansed,total_price,
           property_type,room_type,bed_type,accommodates,bathrooms, bedrooms, beds,amenities,square_feet,
           host_response_rate,
           weekly_discount_available,monthly_discount_available) %>%
    arrange(desc(review_scores_value),desc(number_of_reviews))

View(my_fav_listings)

