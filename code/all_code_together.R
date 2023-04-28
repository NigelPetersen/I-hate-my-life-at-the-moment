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



attach(cars_data)

nrow(subset(cars_data, price_usd < 100))

summary(cars_data)

ggplot(as.data.frame(table(manufacturer_name)),
       aes(x=manufacturer_name, y=Freq))+
  geom_bar(stat="identity", fill="blue")+
  theme(axis.text.x = element_text(angle = 90))+
  labs(x="Manfacturers", y="Frequency", 
       title="Frequency of Manufacturers")+
  theme(plot.title = element_text(hjust = 0.5))

ggplot(cars_data, aes(x=manufacturer_name, y=price_usd))+
  geom_boxplot(fill='blue') +
  theme(axis.text.x = element_text(angle = 90))+
  labs(x="Manfacturers", y="Price", title="Manufacturers vs. Price")+
  theme(plot.title = element_text(hjust = 0.5))

ggplot(as.data.frame(table(body_type)),
       aes(x=body_type, y=Freq))+
  geom_bar(stat="identity", fill="blue")+
  theme(axis.text.x = element_text(angle = 90))+
  labs(x="Body Types", y="Frequency", 
       title="Frequency of Body Types")+
  theme(plot.title = element_text(hjust = 0.5))

ggplot(as.data.frame(table(is_exchangeable)),
       aes(x=is_exchangeable, y=Freq))+
  geom_bar(stat="identity", fill="blue")+
  theme(axis.text.x = element_text(angle = 90))+
  labs(x="Exchangable", y="Frequency", 
       title="Frequency of Exchangable Offers")+
  theme(plot.title = element_text(hjust = 0.5))

ggplot(as.data.frame(table(engine_fuel)),
       aes(x=engine_fuel, y=Freq))+
  geom_bar(stat="identity", fill="blue")+
  theme(axis.text.x = element_text(angle = 90))+
  labs(x="Engine Fuel", y="Frequency", 
       title="Frequency of Engine Fuels")+
  theme(plot.title = element_text(hjust = 0.5))

ggplot(as.data.frame(table(color)),
       aes(x=color, y=Freq))+
  geom_bar(stat="identity", fill="blue")+
  theme(axis.text.x = element_text(angle = 90))+
  labs(x="Colors", y="Frequency", 
       title="Frequency of Colors")+
  theme(plot.title = element_text(hjust = 0.5))

ggplot(cars_data, aes(x=color, y=price_usd))+
  geom_boxplot(fill='blue') +
  theme(axis.text.x = element_text(angle = 90))+
  labs(x="Color", y="Price", title="Color vs. Price")+
  theme(plot.title = element_text(hjust = 0.5))

ggplot(cars_data, aes(x=additional_features, y=price_usd))+
  geom_boxplot(fill='blue') +
  labs(x="Additional Features", y="Price", 
       title="Additional Features vs. Price")+
  theme(plot.title = element_text(hjust = 0.5))

ggplot(as.data.frame(table(additional_features)),
       aes(x=additional_features, y=Freq))+
  geom_bar(stat="identity", fill="blue")+
  labs(x="Additional Features", y="Frequency", 
       title="Frequency of Additional Features")+
  theme(plot.title = element_text(hjust = 0.5))


# propostions of observations with an odometervalue exceeding 
# 800000km make up ~ 0.6% of the data, so they are ommitted. Similarly
# 0.09% of the total observations were produced before 1980.

par(mfrow=c(1,3))
plot(odometer_value[odometer_value < 800000], 
     price_usd[odometer_value<800000], 
     xlab="Odometer value (km)",ylab="Price (USD)", 
     main="Odometer vs. Price")
plot(year_produced[year_produced>1980], price_usd[year_produced>1980], 
     xlab="Year Produced",ylab="Price (USD)", 
     main="Year Produced vs. Price")
plot(duration_listed[duration_listed<1000], 
     price_usd[duration_listed<1000], 
     xlab="Duration Listed (Days<1000)", ylab = "Price (USD)", 
     main="Duration listed vs. Price")

par(mfrow=c(1,3))
plot(duration_listed, price_usd, xlab="Duration Listed (Days)",
     ylab = "Price (USD)", main="Duration listed vs. Price")

plot(duration_listed[duration_listed<1000], 
     price_usd[duration_listed<1000], 
     xlab="Duration Listed (Days<1000)", ylab = "Price (USD)", 
     main="Duration listed vs. Price")

plot(duration_listed[duration_listed<500], 
     price_usd[duration_listed<500], 
     xlab="Duration Listed (Days < 500)", ylab = "Price (USD)", 
     main="Duration listed vs. Price")


ggplot(cars_data, aes(x=additional_features, y=log(price_usd)))+
  geom_boxplot(fill='blue') +
  labs(x="Additional Features", y="Log-Price", 
       title="Additional Features vs. Log-Price")+
  theme(plot.title = element_text(hjust = 0.5))


##################################################################

# Train-test split to improve run time and test prediction accuracy

set.seed(666)

indices = sample(c(T,F), nrow(cars_data), replace = T, prob = c(0.7,0.3))
training_data <- cars_data[indices,]
remaining <- cars_data[!indices,]
y_test = remaining[,"price_usd"]
X_test = subset(remaining, select = -c(price_usd))

# use log(price) as the response 

full_model = lm(log(price_usd) ~., data=training_data)
#summary(full_model)
#par(mfrow=c(1,2))
#plot(full_model)

reduced_model = step(full_model, trace=0)
#par(mfrow=c(2,2))
#plot(reduced_model)

####### MODEL WITH INTERACTION EFFECTS INCLUDED ###########

interaction_model = lm(log(price_usd) ~. -odometer_value - year_produced - 
                         duration_listed - additional_features + (odometer_value + year_produced +
                                                                    duration_listed + additional_features)^2, data=training_data )

#summary(interaction_model)
#par(mfrow=c(2,2))
#plot(interaction_model)

reduced_interaction_model = step(interaction_model, trace=0)
# summary(interaction_model)
# par(mfrow=c(2,2))
# plot(interaction_model)

# compute predicted values of reduced model from backwards selection

full_predictions = exp(predict.lm(full_model, X_test))
reduced_predictions = exp(predict.lm(reduced_model, X_test))
interaction_predictions = exp(predict.lm(interaction_model, X_test))
reduced_interaction_predictions = exp(predict.lm(reduced_interaction_model,
                                                 X_test))
full_RMSE = sqrt(mean((full_predictions - y_test)^2))
reduced_RMSE = sqrt(mean((reduced_predictions - y_test)^2))
interaction_RMSE = sqrt(mean((interaction_predictions - y_test))^2)
reduced_interaction_RMSE = sqrt(mean((reduced_interaction_predictions - y_test))^2)

Models = c("Full Model", "Reduced Model", "Interactions Model", 
           "Reduced Interactions Model")

R2 = c(summary(full_model)$r.squared, summary(reduced_model)$r.squared, 
       summary(interaction_model)$r.squared, 
       summary(reduced_interaction_model)$r.squared) 
RMSE = c(full_RMSE, reduced_RMSE, interaction_RMSE,
         reduced_interaction_RMSE)

metrics_df = data.frame(Models, R2, RMSE)
metrics_df

predicted_df = data.frame(X_test, interaction_predictions)

ggplot(predicted_df, aes(x=additional_features, y=interaction_predictions))+
  geom_boxplot(fill='blue') +
  labs(x="Additional Features", y="Estimated price", 
       title="Additional Features vs. Estimated Price")+
  theme(plot.title = element_text(hjust = 0.5))







############### Testing ###################

# change in price from change in additional features

means = numeric(length(unique(additional_features)))
for(i in 1:length(means)){
  means[i] = mean(exp(predict.lm(interaction_model, subset(X_test, 
          additional_features==i))))
}

unit_diffs = numeric(length(means)-1)
for(i in 1:length(unit_diffs)){
  unit_diffs[i] = means[i+1]-means[i]
}
mean(unit_diffs)

# change in price from change in odometer

nrow(subset(X_test, odometer_value > 400000))/nrow(X_test)

odom_means = numeric(8)
for(i in 1:8){
  odom_means[i]= mean(exp(predict.lm(interaction_model, subset(X_test, 
          odometer_value >= 50000*(i-1) & odometer_value < 50000*i ))))
}

odom_diff = numeric(7)
for(i in 1:7){
  odom_diff[i] = odom_means[i+1]-odom_means[i]
}
mean(odom_diff)

# change in price from change in year_produced

nrow(subset(X_test, year_produced >= 1980))

year_means = numeric(8)
for(i in 1:8){
  year_means[i] = mean(exp(predict.lm(interaction_model, subset(X_test, 
        year_produced >= 1980 + 5*(i-1) & year_produced < 1980 + 5*i ))))
}
year_diff = numeric(7)
for(i in 1:7){
  year_diff[i] = year_means[i+1] - year_means[i]
}
mean(year_means)
