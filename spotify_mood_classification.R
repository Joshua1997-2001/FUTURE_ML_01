# Install necessary libraries
install.packages(c("tidyverse", "caret", "randomForest", "e1071", "ggplot2"))

# Load the libraries
library(tidyverse)
library(caret)
library(randomForest)
library(e1071)
library(ggplot2)

# Step 1: Load and preprocess the dataset
Spf <- read.csv("dataset.csv")  # Load the dataset (use the correct path)

# Inspect the first few rows
head(Spf)

# Check for missing values
sum(is.na(Spf))

# Handle missing values (drop rows with NA values)
Spf <- na.omit(Spf)

# Step 2: Split the dataset into training and testing sets
set.seed(42)  # Set seed for reproducibility
trainIndex <- createDataPartition(Spf$mood, p = 0.8, list = FALSE)
train_data <- Spf[trainIndex, ]
test_data <- Spf[-trainIndex, ]

# Step 3: Train a Random Forest model to classify moods
rf_model <- randomForest(mood ~ ., data = train_data, ntree = 100)

# Print the model summary
print(rf_model)

# Step 4: Make predictions on the test data and evaluate the model
predictions <- predict(rf_model, newdata = test_data)

# Evaluate the model performance
confusionMatrix(predictions, test_data$mood)

# Step 5: Visualize the distribution of moods in the dataset
ggplot(Spf, aes(x = mood)) +
  geom_bar(fill = "lightblue", color = "black") +
  labs(title = "Distribution of Moods in Songs", x = "Mood", y = "Count")

# Step 6: Visualize mood distribution across playlists (if applicable)
# Assuming the dataset has a 'playlist' column
df %>%
  group_by(playlist, mood) %>%
  summarise(count = n()) %>%
  ggplot(aes(x = playlist, y = count, fill = mood)) +
  geom_bar(stat = "identity", position = "stack") +
  labs(title = "Mood Distribution Across Playlists", x = "Playlist", y = "Count")

# Step 7: Save the trained model for future use
saveRDS(rf_model, "spotify_mood_classifier.rds")

# Load the saved model (example usage)
loaded_model <- readRDS("spotify_mood_classifier.rds")

# You can now use the loaded_model to predict new data
