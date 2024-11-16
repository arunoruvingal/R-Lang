# Load necessary libraries
library(ggplot2)

# Load the dataset
auto_mpg <- read.csv("auto-mpg.csv")

# Convert 'horsepower' to numeric (handle non-numeric values if any)
auto_mpg$horsepower <- as.numeric(as.character(auto_mpg$horsepower))

# Split the data
train_data <- auto_mpg[1:300, ]  # First 300 rows for training
test_data <- auto_mpg[301:398, ] # Remaining 98 rows for testing

# Best-performing model from Step 2 (example: multiple regression model)
best_model <- lm(mpg ~ horsepower + weight + displacement, data = train_data)

# Use the model to predict mpg for the test data
test_data$predicted_mpg <- predict(best_model, newdata = test_data)

# Calculate residuals
test_data$residuals <- test_data$mpg - test_data$predicted_mpg

# Compare predicted vs actual mpg
comparison <- data.frame(
  Actual_MPG = test_data$mpg,
  Predicted_MPG = test_data$predicted_mpg,
  Residuals = test_data$residuals
)

# Print comparison
print(head(comparison, 10))

# Residual Plot
ggplot(data = test_data, aes(x = predicted_mpg, y = residuals)) +
  geom_point(alpha = 0.7, color = "blue") +
  geom_hline(yintercept = 0, linetype = "dashed", color = "red") +
  labs(title = "Residual Plot",
       x = "Predicted MPG",
       y = "Residuals") +
  theme_minimal()

# Histogram of Residuals
ggplot(data = test_data, aes(x = residuals)) +
  geom_histogram(binwidth = 1, fill = "lightblue", color = "black") +
  labs(title = "Histogram of Residuals",
       x = "Residuals",
       y = "Frequency") +
  theme_minimal()
