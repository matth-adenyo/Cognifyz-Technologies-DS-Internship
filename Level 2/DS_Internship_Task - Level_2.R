# Importing Libraries
library(dplyr)
library(tidyr)
library(ggplot2)
library(readr)





## Task 1: TABLE BOOKING AND ONLINE DELIVERY


# Load Dataset
data <- read.csv("D:/Projects/Cognifyz Technologies - DS Internship/Dataset.csv")

# View top 10 rows of the dataset
head(data, 10)



# Percentage of restaurants that offer table booking and online delivery

# Total number of restaurants
total_num_restaurants <- nrow(data)

# Percentage calculation
table_booking_percentage <- sum(data$Has.Table.booking == 'Yes') / total_num_restaurants * 100
online_delivery_percentage <- sum(data$Has.Online.delivery == 'Yes') / total_num_restaurants * 100

# Display results
cat(sprintf("Percentage of restaurants that offer Table Booking: %.2f%%\n", table_booking_percentage))
cat(sprintf("Percentage of restaurants that offer Online Delivery: %.2f%%\n", online_delivery_percentage))



# Compare the average ratings of restaurants with table booking and those without


avg_rating_with_table <- mean(data[data$Has.Table.booking == 'Yes', "Aggregate.rating"], na.rm = TRUE)
avg_rating_without_table <- mean(data[data$Has.Table.booking == 'No', "Aggregate.rating"], na.rm = TRUE)

# Display results
cat(sprintf("Average rating with Table Booking: %.2f\n", avg_rating_with_table))
cat(sprintf("Average rating without Table Booking: %.2f\n", avg_rating_without_table))



# Analyze the availability of online delivery among restaurants with different price ranges

# Select price ranges
price_ranges <- ifelse(data$Average.Cost.for.two < 500, 'Low',
                       ifelse(data$Average.Cost.for.two >= 500 & data$Average.Cost.for.two <= 1000, 'Medium', 'High'))

# Group by price range and online delivery availability
online_delivery_by_price_range <- table(price_ranges, data$Has.Online.delivery)
online_delivery_by_price_range <- prop.table(online_delivery_by_price_range, margin = 1)

# Display results
cat("Online Delivery Availability by Price Range:\n")
print(online_delivery_by_price_range)

# Create a bar plot to visualize online delivery availability by price range
ggplot(data, aes(price_ranges, fill = Has.Online.delivery)) +
  geom_bar() +
  labs(title = "Online Delivery Availability by Price Range",
       x = "Price Range",
       y = "Count")





## Task 2: PRICE RANGE ANALYSIS


# Determine the most common price range among all the restaurants

# Most common price range among all the restaurants
most_common_price_range <- names(sort(table(data$Price.range), decreasing = TRUE))[1]

# Display result
cat("Most Common Price Range: ", most_common_price_range, "\n")


# Calculate the average rating for each price range

# Group by 'Price range' and calculate the average rating
avg_rating_by_price_range <- aggregate(data$Aggregate.rating, by = list(data$Price.range), FUN = mean, na.rm = TRUE)

# Rename the columns
colnames(avg_rating_by_price_range) <- c("Price range", "Average rating")

# Display result
cat("Average rating for each price range:\n")
print(round(avg_rating_by_price_range, 2))


# Identify the color that represents the highest average rating among different price ranges

# Find the price range with the highest average rating
highest_avg_rating_color <- which.max(avg_rating_by_price_range$`Average rating`)

# Create a bar plot
barplot(avg_rating_by_price_range$`Average rating`, 
        names.arg = avg_rating_by_price_range$`Price range`,
        col = ifelse(1:length(avg_rating_by_price_range$`Price range`) == highest_avg_rating_color,
                     "red", "blue"),
        xlab = "Price Range",
        ylab = "Average Rating",
        main = "Average Rating by Price Range")





## Task 3: FEATURE ENGINEERING


# Extract additional features from the existing columns, such as the length of the restaurant name or address

# Create a new column for the length of restaurant names
data$`Restaurant Name Length` <- nchar(data$Restaurant.Name)

# Create a new column for the length of restaurant addresses
data$`Address Length` <- nchar(data$Address)

# Display the updated DataFrame
head(data)


# Create new features like "Has Table Booking" or "Has Online Delivery" by encoding categorical variables

# Create new columns
data$`Has Table Booking` <- ifelse(data$Has.Table.booking == "Yes", 1, 0)
data$`Has Online Delivery` <- ifelse(data$Has.Online.delivery == "Yes", 1, 0)

# Display the updated DataFrame
head(data)



