# Importing Libraries
library(readr)
library(dplyr)
library(tidyr)
library(ggplot2)
library(caret)
library(lattice)
library(randomForest)
library(rpart)
library(e1071)
library(glmnet)




## Task 1: PREDICTIVE MODELLING


# Load Dataset
data <- read.csv("D:/Projects/Cognifyz Technologies - DS Internship/Dataset.csv")

# View top 10 rows of the dataset
head(data, 10)



# Build a regression model to predict the aggregate rating of a restaurant based on available features
# Split the dataset into training and testing sets and evaluate the model's performance using appropriate metrics.
# Experiment with different algorithms (e.g., linear regression, decision trees, random forest) and compare their performance

# Create new numerical columns
data$Has.Table.Booking_Num <- ifelse(data$Has.Table.booking == "Yes", 1, 0)
data$Has.Online.Delivery_Num <- ifelse(data$Has.Online.delivery == "Yes", 1, 0)


# Split data into training and testing sets
set.seed(123) # for reproducibility
trainIndex <- createDataPartition(data$Aggregate.rating, p = 0.8, list = FALSE, times = 1)
data_train <- data[trainIndex, ]
data_test  <- data[-trainIndex, ]


# Define predictor variables and target variable
predictors <- c("Average.Cost.for.two", "Votes", "Price.range", "Has.Table.Booking_Num", "Has.Online.Delivery_Num")
target_variable <- "Aggregate.rating"


# Linear Regression
lm_model <- train(as.formula(paste(target_variable, "~", paste(predictors, collapse = " + "))), data = data_train, 
                  method = "lm", 
                  trControl = trainControl(method = "cv"))

# Decision Tree Regression
tree_model <- train(as.formula(paste(target_variable, "~", paste(predictors, collapse = " + "))), data = data_train, 
                    method = "rpart", 
                    trControl = trainControl(method = "cv"))

# Random Forest Regression
rf_model <- train(as.formula(paste(target_variable, "~", paste(predictors, collapse = " + "))), data = data_train, 
                  method = "rf", 
                  trControl = trainControl(method = "cv"))

# Support Vector Machine 
svm_model <- train(as.formula(paste(target_variable, "~", paste(predictors, collapse = " + "))), data = data_train, 
                   method = "svmLinear", 
                   trControl = trainControl(method = "cv"))


# Predictions on test set
lm_pred <- predict(lm_model, newdata = data_test)
tree_pred <- predict(tree_model, newdata = data_test)
rf_pred <- predict(rf_model, newdata = data_test)
svm_pred <- predict(svm_model, newdata = data_test)


# Evaluation
lm_rmse <- sqrt(mean((lm_pred - data_test$Aggregate.rating)^2))
lm_r2 <- cor(lm_pred, data_test$Aggregate.rating)^2

tree_rmse <- sqrt(mean((tree_pred - data_test$Aggregate.rating)^2))
tree_r2 <- cor(tree_pred, data_test$Aggregate.rating)^2

rf_rmse <- sqrt(mean((rf_pred - data_test$Aggregate.rating)^2))
rf_r2 <- cor(rf_pred, data_test$Aggregate.rating)^2

svm_rmse <- sqrt(mean((svm_pred - data_test$Aggregate.rating)^2))
svm_r2 <- cor(svm_pred, data_test$Aggregate.rating)^2


# Print results
cat("Linear Regression RMSE:", lm_rmse, "\n")
cat("Linear Regression R-squared:", lm_r2, "\n\n")

cat("Decision Tree RMSE:", tree_rmse, "\n")
cat("Decision Tree R-squared:", tree_r2, "\n\n")

cat("Random Forest RMSE:", rf_rmse, "\n")
cat("Random Forest R-squared:", rf_r2, "\n")

cat("Support Vector Machine RMSE:", svm_rmse, "\n")
cat("Support Vector Machine R-squared:", svm_r2, "\n")





## Task 2: CUSTOMER PREFERENCE ANALYSIS


# Analyze the relationship between the type of cuisine and the restaurant's rating

# Identify the top 10 cuisines
top_cuisines <- head(sort(table(data$Cuisines), decreasing = TRUE), 10)
top_cuisines <- names(top_cuisines)

# Subset the data for only the top 10 cuisines
data_top_cuisines <- data[data$Cuisines %in% top_cuisines, ]

# Create a box plot to visualize the distribution of ratings for each cuisine type (top 10)
ggplot(data_top_cuisines, aes(x = Cuisines, y = Aggregate.rating)) +
  geom_boxplot() +
  labs(x = "Cuisine Type", y = "Rating", title = "Distribution of Ratings by Cuisine Type") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))


# Identify the most popular cuisines among customers based on the number of votes

# Group the data by cuisine and calculate the total number of votes for each cuisine
popular_cuisines <- data %>%
  group_by(Cuisines) %>%
  summarise(TotalVotes = sum(Votes, na.rm = TRUE)) %>%
  arrange(desc(TotalVotes))

# Print the top 10 most popular cuisines
head(popular_cuisines, 10)


# Determine if there are any specific cuisines that tend to receive higher ratings

# Calculate the average rating for each cuisine
average_ratings <- data %>%
  group_by(Cuisines) %>%
  summarise(AvgRating = mean(`Aggregate.rating`, na.rm = TRUE)) %>%
  arrange(desc(AvgRating))

# Print the cuisines with the highest average ratings
head(average_ratings, 10)





## Task 3: DATA VISUALIZATION


# Create visualizations to represent the distribution of ratings using different charts (histogram, bar plot, etc.)

# Create a histogram of ratings
ggplot(data, aes(x = `Aggregate.rating`)) +
  geom_histogram(binwidth = 0.3, fill = "skyblue", color = "black") +
  labs(x = "Rating", y = "Frequency", title = "Distribution of Ratings using Histogram")

# Create a density plot of ratings
ggplot(data, aes(x = `Aggregate.rating`)) +
  geom_density(fill = "skyblue", color = "black") +
  labs(x = "Rating", y = "Density", title = "Distribution of Ratings using Density Plot")


# Compare the average ratings of different cuisines or cities using appropriate visualizations

# Calculate average ratings for each cuisine
average_ratings_cuisine <- data %>%
  group_by(Cuisines) %>%
  summarise(AvgRating = mean(`Aggregate.rating`, na.rm = TRUE)) %>%
  arrange(desc(AvgRating)) %>%
  slice(1:10)

# Create bar plot for average ratings by cuisine
ggplot(average_ratings_cuisine, aes(x = AvgRating, y = reorder(Cuisines, AvgRating))) +
  geom_bar(stat = "identity", fill = "skyblue", color = "black") +
  labs(x = "Average Rating", y = "Cuisine", title = "Average Ratings by Cuisines (Top 10)")

# Calculate average ratings for each city
average_ratings_city <- data %>%
  group_by(City) %>%
  summarise(AvgRating = mean(`Aggregate.rating`, na.rm = TRUE)) %>%
  arrange(desc(AvgRating)) %>%
  slice(1:10)

# Create bar plot for average ratings by city
ggplot(average_ratings_city, aes(x = reorder(City, AvgRating), y = AvgRating)) +
  geom_bar(stat = "identity", fill = "skyblue", color = "black") +
  labs(x = "City", y = "Average Rating", title = "Average Ratings by City (Top 10)") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))


# Visualize the relationship between various features and the target variable to gain insights

# Calculate the correlation matrix
correlation_matrix <- cor(data[, c("Aggregate.rating", "Average.Cost.for.two", "Votes", "Price.range", "Has.Table.Booking_Num", "Has.Online.Delivery_Num")])

# Convert correlation matrix to dataframe
correlation_df <- as.data.frame(as.table(correlation_matrix))
names(correlation_df) <- c("Var1", "Var2", "Correlation")

# Visualize the correlation matrix as a heatmap
ggplot(data = correlation_df, aes(x = Var1, y = Var2, fill = Correlation)) +
  geom_tile() +
  scale_fill_gradient(low = "lightblue", high = "darkblue") +
  labs(title = "Correlation Matrix", x = "", y = "") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1),
        axis.text.y = element_text(angle = 0, hjust = 1))




