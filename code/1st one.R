# Load necessary libraries

#install.packages("ggplot2")

library(ggplot2)

# Load the dataset
auto_mpg <- read.csv("auto-mpg.csv")

# Convert 'horsepower' to numeric (handle non-numeric values if any)
auto_mpg$horsepower <- as.numeric(as.character(auto_mpg$horsepower))

# Split the data into the first 300 rows for training
train_data <- auto_mpg[1:300, ]

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
cat("Regression Equation: ", regression_equation, "\n")
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
