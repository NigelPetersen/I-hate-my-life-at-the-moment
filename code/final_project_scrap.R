library(readr)
library(dplyr)
library(ggplot2)
url = 'https://raw.githubusercontent.com/NigelPetersen/STA2101_data/main/cleaned_cars.csv'
raw_data <- read.csv(url)
options(scipen=999)

# Data cleaning and preprocessing

missing_data <- numeric(length(names(raw_data)))
for(i in 1:length(names(raw_data))){
  missing_data[i] = sum(is.na(raw_data[i]))
}

missing_data <- matrix(missing_data, ncol=1)

colnames(missing_data) <- c("Missing entries")
rownames(missing_data) <- c(names(raw_data))
missing_data_table <- as.table(missing_data)


# drop observations with missing entries

cars_data <- na.omit(raw_data)

# remove variables that are too similar

cars_data <- subset(cars_data, select = -c(engine_type))

# remove variables whose values are not numeric, nor are in English

cars_data <- subset(cars_data, select = -c(location_region))

cars_data <- subset(cars_data, select = -c(model_name))

features <- names(cars_data)[17:26]

features_df <- cars_data[features]
features_df[features_df == "True"] <- 1
features_df[features_df == "False"] <- 0
count_features <- numeric(nrow(cars_data))
for(i in 1:nrow(cars_data)){
  count_features[i] = sum(as.numeric(features_df[i,]))
}

cars_data$additional_features <- as.character(count_features)

cars_data <- cars_data[!colnames(cars_data) %in% features]

cars_data[cars_data == "gas"] <- "gasoline"
cars_data[cars_data == "hybrid-diesel"]  <- "Other"
cars_data[cars_data == "hybrid-petrol"] <- "Other"

cars_data <- subset(cars_data, price_usd >= 100)







