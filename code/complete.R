# Load necessary libraries
library(ggplot2)

# Load the dataset
auto_mpg <- read.csv("auto-mpg.csv")

# Convert 'horsepower' to numeric (handle non-numeric values if any)
auto_mpg$horsepower <- as.numeric(as.character(auto_mpg$horsepower))

# Split the data
train_data <- auto_mpg[1:300, ]  # First 300 rows for training
test_data <- auto_mpg[301:398, ] # Remaining 98 rows for testing

# Set 1: Simple Linear Regression

# Perform Simple Linear Regression
simple_model <- lm(mpg ~ horsepower, data = train_data)

# Summary of the regression model
summary(simple_model)

# Extract regression metrics
multiple_r_squared <- summary(simple_model)$r.squared
adjusted_r_squared <- summary(simple_model)$adj.r.squared

# Regression Equation
coefficients <- coef(simple_model)
regression_equation <- paste0("mpg = ", round(coefficients[1], 2), 
                              " + ", round(coefficients[2], 2), " * horsepower")

# Display the results
cat("Simple Regression Equation: ", regression_equation, "\n")
cat("Multiple R-squared: ", multiple_r_squared, "\n")
cat("Adjusted R-squared: ", adjusted_r_squared, "\n")

# Plot the data with the regression line
ggplot(data = train_data, aes(x = horsepower, y = mpg)) +
  geom_point(alpha = 0.7) +
  geom_smooth(method = "lm", se = FALSE, color = "blue") +
  labs(title = "Simple Linear Regression: MPG vs Horsepower",
       x = "Horsepower",
       y = "Miles Per Gallon (MPG)") +
  theme_minimal()

# Set 2: Multiple Linear Regression

# Perform Multiple Linear Regression
multiple_model <- lm(mpg ~ horsepower + weight + displacement, data = train_data)

# Summary of the regression model
summary_output <- summary(multiple_model)

# Extract regression metrics
multiple_r_squared <- summary_output$r.squared
adjusted_r_squared <- summary_output$adj.r.squared

# Regression Equation
coefficients <- coef(multiple_model)
regression_equation <- paste0("mpg = ", round(coefficients[1], 2), 
                              " + ", round(coefficients[2], 2), " * horsepower",
                              " + ", round(coefficients[3], 2), " * weight",
                              " + ", round(coefficients[4], 2), " * displacement")

# Display the results
cat("Multiple Regression Equation: ", regression_equation, "\n")
cat("Multiple R-squared: ", multiple_r_squared, "\n")
cat("Adjusted R-squared: ", adjusted_r_squared, "\n")

# Diagnostic Plots
# Residuals vs Fitted Values
plot(multiple_model, which = 1, main = "Residuals vs Fitted")

# Q-Q Plot to check normality
plot(multiple_model, which = 2, main = "Q-Q Plot")

# Histogram of residuals
residuals <- residuals(multiple_model)
hist(residuals, breaks = 20, main = "Histogram of Residuals", xlab = "Residuals", col = "lightblue", border = "black")

# Set 3: Model Evaluation and Testing

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
