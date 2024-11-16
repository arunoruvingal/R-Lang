# Load necessary libraries
library(ggplot2)

# Load the dataset
auto_mpg <- read.csv("auto-mpg.csv")

# Convert 'horsepower' to numeric (handle non-numeric values if any)
auto_mpg$horsepower <- as.numeric(as.character(auto_mpg$horsepower))

# Split the data into the first 300 rows for training
train_data <- auto_mpg[1:300, ]

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
cat("Regression Equation: ", regression_equation, "\n")
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
