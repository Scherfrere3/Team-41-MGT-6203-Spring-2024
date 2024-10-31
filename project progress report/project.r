getwd()
setwd("~/R/mgt6203")

library(dplyr)
library(ggplot2)

listings = read.csv("listings.csv", header = T, as.is = T)
nhoods = read.csv("neighbourhoods.csv", header = T, as.is = T)
reviews = read.csv("reviews.csv", header = T, as.is = T)

head(listings)

listing = read.csv("listings_cleaned.csv", header = T, as.is = T)

airbnb <- select(listing, neighbourhood_group, neighbourhood, latitude, longitude, room_type, price, minimum_nights, number_of_reviews, availability_365)

head(listing)

summary(airbnb)

c(unique(airbnb["neighbourhood_group"]))

freq_location <- data.frame(cbind(Frequency = table(airbnb$neighbourhood_group), Percent = prop.table(table(airbnb$neighbourhood_group)) * 100))
freq_location <- freq_location[order(freq_location$Frequency),]
freq_location

freq_area <- data.frame(cbind(Frequency = table(airbnb$neighbourhood), Percent = prop.table(table(airbnb$neighbourhood)) * 100))
freq_area <- freq_area[order(freq_area$Frequency),]
freq_area

means <- data.frame(Mean = c(mean(airbnb$price), mean(airbnb$minimum_nights)))
row.names(means) <- c("Price", "Minimum nights")
means

boxplot(airbnb$price)

mean_room_type <- aggregate(list(average_price = airbnb$price), list(room_type = airbnb$room_type), mean)
mean_room_type$Percent <- prop.table(mean_room_type$average_price) * 100
mean_room_type

top_10_neighbourhood <- aggregate(list(airbnb$price), list(airbnb$neighbourhood), mean)
colnames(top_10_neighbourhood) <- c("neighbourhood", "Average_price_per_neighborhood")
top_10_neighbourhood <- top_10_neighbourhood[order(top_10_neighbourhood$Average_price_per_neighborhood),]
top_10_neighbourhood <- tail(top_10_neighbourhood, 12)
top_10_neighbourhood <- head(top_10_neighbourhood, 10)
r <- c()
for(i in 10:1){r <- c(r, i)}
row.names(top_10_neighbourhood) <- r
top_10_neighbourhood

top_10_neighbourhood_b <- aggregate(list(airbnb$price), list(airbnb$neighbourhood), mean)
colnames(top_10_neighbourhood_b) <- c("neighbourhood", "Average_price_per_neighborhood")
top_10_neighbourhood_b <- top_10_neighbourhood_b[order(top_10_neighbourhood_b$Average_price_per_neighborhood),]
top_10_neighbourhood_b <- head(top_10_neighbourhood_b, 10)
r <- c()
for(i in 1:10){r <- c(r, i)}
row.names(top_10_neighbourhood_b) <- r
top_10_neighbourhood_b
