
data <- read.csv("listings_cleaned.csv")

hist(data$price, 
     main="Histogram of Airbnb Prices", 
     xlab="Price ($)", 
     ylab="Frequency", 
     col="blue", 
     border="black")

install.packages("ggplot2")
library(ggplot2)
library(lmtest)

install.packages("ggmap")

library(dplyr)
library(ggmap)


# Create a scatter plot using ggplot2
ggplot(data, aes(x = minimum_nights, y = price)) +
    geom_point(alpha = 0.5, color = "blue") +  # Transparency set with alpha
    labs(title = "Scatter Plot of Price vs. Minimum Nights",
         x = "Minimum Nights",
         y = "Price ($)") +
    theme_minimal() 

# Regression model
# Convert Categorical Variables to Factors
data$neighbourhood_group <- as.factor(data$neighbourhood_group)
data$room_type <- as.factor(data$room_type)

# Initial Model

model <- lm(price ~ neighbourhood_group + room_type + minimum_nights + number_of_reviews +
                calculated_host_listings_count + availability_365, data = data)

summary(model)

# Second model

model2 <- lm(price ~ neighbourhood_group + room_type + minimum_nights + number_of_reviews 
             + availability_365, data = data)

summary(model2)

# Test of linearity

plot(model2$residuals ~ model2$fitted.values,
     main = "Residuals vs. Fitted Values Plot")
abline(h = 0, col = "red")



# Test DW

dw_test_result <- dwtest(model2)

# Display the result
print(dw_test_result)

# Run the Breusch-Pagan test
bp_test_result <- bptest(model2)

# Display the result
print(bp_test_result)

# Test of multicollinearity
install.packages("car")

# Load the car library
library(car)

vif_values <- vif(model2)

# Display the VIF values
print(vif_values)


