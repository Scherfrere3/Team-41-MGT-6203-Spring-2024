# Data Cleaning

## Load the necessary library
library(dplyr)

## Load the dataset
listings <- read.csv("Data/listings.csv")

## Drop the specified columns
listings_cleaned <- listings %>%
    select(-c(name, last_review, reviews_per_month, license))

## View the first few rows of the cleaned dataset
head(listings_cleaned)

# Calculate the number of missing values in each column
missing_values_per_column <- sapply(listings_cleaned, function(x) sum(is.na(x)))

# Total number of missing values in the dataset
total_missing_values <- sum(missing_values_per_column)

# Print the number of missing values per column
print(missing_values_per_column)

# Print the total number of missing values
print(total_missing_values)


# Calculate the median price for each room type
median_prices_by_room_type <- listings_cleaned %>%
    group_by(room_type) %>%
    summarise(median_price = median(price, na.rm = TRUE))

# Replace missing values in the price column based on room_type
listings_cleaned <- listings_cleaned %>%
    rowwise() %>%
    mutate(price = ifelse(is.na(price),
                          median_prices_by_room_type$median_price[median_prices_by_room_type$room_type == room_type],
                          price))


# Calculate the number of missing values in each column
missing_values_per_column_after_cleaning <- sapply(listings_cleaned, function(x) sum(is.na(x)))

# Total number of missing values in the dataset after cleaning
total_missing_values_after_cleaning <- sum(missing_values_per_column_after_cleaning)

# Print the number of missing values per column after cleaning
print(missing_values_per_column_after_cleaning)

# Print the total number of missing values after cleaning
print(total_missing_values_after_cleaning)

# Save the cleaned dataset as a CSV file
write.csv(listings_cleaned, "Data/listings_cleaned.csv", row.names = FALSE)



