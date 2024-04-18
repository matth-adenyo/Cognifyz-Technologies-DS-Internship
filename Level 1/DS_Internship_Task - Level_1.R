# Importing Libraries
library(readr)
library(ggplot2)
library(dplyr)
library(tidyr)
library(maps)
library(mapdata)
library(corrplot)





## Task 1: DATA EXPLORATION AND PREPROCESSING


# Load Dataset
data <- read.csv("D:/Projects/Cognifyz Technologies - DS Internship/Dataset.csv")

# View top 10 rows of the dataset
head(data, 10)


# Explore the dataset and identify the number of rows and columns

# Checking number of rows and columns of the dataset
cat("Number of rows:", nrow(data), "\n")
cat("Number of columns:", ncol(data), "\n")


# Dataset Duplicate Value Count
dup <- sum(duplicated(data))
cat("Number of duplicate rows:", dup)


# Check for missing values in each column and handle them accordingly

# Check for missing values
missing_values <- sum(is.na(data))

# Check for empty values
empty_values <- sum(data == "")

cat("Missing values count:", missing_values, "\n")
cat("Empty values count:", empty_values, "\n")

# There are 9 empty values, let's find out which column/columns has it
empty_values_count <- colSums(data == "")
cat("Empty Values Count:\n")
print(empty_values_count)


# The Cuisines column has 9 empty values. Since it's not many, let's remove these rows
data <- data[!(data$Cuisines == ""), , drop = FALSE]


# Check for empty values after Removing
empty_values <- sum(data == "")
cat("Empty values count:", empty_values, "\n")


# Display basic information about the dataset to check various data types
str(data)


# Analyze the distribution of the target variable ("Aggregate rating") and identify any class imbalances

# Distribution of the target variable ("Aggregate rating")
target_counts <- table(data$'Aggregate rating')

# Print the distribution
print("Distribution of target variable:")
print(target_counts)

# Check if the distribution is balanced
is_balanced <- all(target_counts >= mean(target_counts))
if (is_balanced) {
  print("The distribution of the target variable is balanced.")
} else {
  print("The distribution of the target variable is imbalanced.")
}





## Task 2: DESCRIPTIVE ANALYSIS


# Basic statistical measures (mean, median, standard deviation, etc.) for numerical columns

# Select Numerical Columns
numeric_columns <- data[, sapply(data, is.numeric)]

# Calculate basic statistical measures using summary()
summary_stats <- summary(numeric_columns)
print(summary_stats)

# Calculate standard deviation for numerical columns
sds <- sapply(data[, sapply(data, is.numeric)], sd, na.rm = TRUE)


print("Standard deviation for numerical columns:")
print(sds)




# The Distribution of Categorical Variables like 'Country Code', 'City', and 'Cuisines'

# Create count plot for Country Code
ggplot(data, aes(x = factor(Country.Code))) +
  geom_bar(fill = "steelblue") +
  labs(title = "Distribution of Restaurants by Country Codes",
       x = "Country Codes", y = "Number of Restaurants")


# Create a subset of the data containing only the top 10 cities
top_10_cities <- head(names(sort(table(data$City), decreasing = TRUE)), 10)
data_top_10_cities <- data[data$City %in% top_10_cities, ]

# Create count plot for top 10 cities (horizontal bar plot)
ggplot(data = data_top_10_cities, aes(y = factor(City, levels = rev(top_10_cities)))) +
  geom_bar(fill = "steelblue", width = 0.5, stat = "count") +
  labs(title = "Top 10 Cities with Highest Number of Restaurants",
       x = "Number of Restaurants", y = "Name of Cities") +
  theme(axis.text.y = element_text(size = 8))


# Subset the data to include only the top 10 cuisines
top_10_cuisines <- head(names(sort(table(data$Cuisines), decreasing = TRUE)), 10)
data_top_10 <- data[data$Cuisines %in% top_10_cuisines, ]

# Create count plot for top 10 cuisines (horizontal bar plot)
ggplot(data = data_top_10, aes(y = factor(Cuisines, levels = rev(top_10_cuisines)))) +
  geom_bar(fill = "steelblue", width = 0.5, stat = "count") +
  labs(title = "Top 10 Cuisines with Highest Number of Restaurants",
       x = "Number of Restaurants", y = "Name of Cuisines") +
  theme(axis.text.y = element_text(size = 8))



# The top cuisines and cities with the highest number of restaurants

# Identify the top 10 cuisines and their counts
top_cuisines <- head(sort(table(data$Cuisines), decreasing = TRUE), 10)

# Create a dataframe with cuisine names and counts
top_cuisines_df <- data.frame(Cuisine = names(top_cuisines), Count = as.numeric(top_cuisines))

# Display the dataframe
print("Top 10 Cuisines with the Highest Number of Restaurants:")
print(top_cuisines_df)


# Identify the top 10 city and their counts
top_city <- head(sort(table(data$City), decreasing = TRUE), 10)

# Create a dataframe with city names and counts
top_city_df <- data.frame(City = names(top_city), Count = as.numeric(top_city))

# Display the dataframe
print("Top 10 Cities with the Highest Number of Restaurants:")
print(top_city_df)





## Task 3: GEOSPATIAL ANALYSIS


# Visualize the locations of restaurants on a map

# Create a map of the world
world_map <- map_data("world")

# Plot restaurant locations on the map
ggplot() +
  geom_polygon(data = world_map, aes(x = long, y = lat, group = group), fill = "lightgrey", color = "black") +
  geom_point(data = data, aes(x = Longitude, y = Latitude, color = "Restaurants"), size = 2) +
  scale_color_manual(name = "Legend", values = c(Restaurants = "red")) +
  labs(title = "Restaurant Locations on World Map", x = "Longitude", y = "Latitude") +
  theme_minimal()


# Analyze the distribution of restaurants across different cities or countries

# Create a subset of the data containing only the top 10 cities
top_10_cities <- head(names(sort(table(data$City), decreasing = TRUE)), 10)
data_top_10_cities <- data[data$City %in% top_10_cities, ]

# Create a plot of the distribution of restaurants across cities
ggplot(data = data_top_10_cities, aes(y = factor(City, levels = rev(top_10_cities)))) +
  geom_bar(fill = "steelblue", width = 0.5, stat = "count") +
  labs(title = "Distribution of Restaurants Across Cities",
       x = "Number of Restaurants", y = "Name of Cities") +
  theme(axis.text.y = element_text(size = 8))


# Determine if there is any correlation between the restaurant's location and its rating

# Calculate the correlation matrix
correlation_matrix <- cor(data[c("Latitude", "Longitude", "Aggregate.rating")])

# Create a heatmap to visualize the correlation
corrplot(correlation_matrix, method="color", col=colorRampPalette(c("blue", "white", "red"))(20), type="upper", 
         order="hclust", tl.col="black", tl.srt=45, title="Correlation Between Restaurant's Location and Rating",
         mar=c(0, 0, 3, 1))


