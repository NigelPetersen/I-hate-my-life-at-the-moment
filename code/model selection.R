source("final_project_scrap.R")
options(digits=4)


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







## qqplot of full model is right-skewed
# interaction effects to include in separate model 
# odometer, year produced, duration listed, additional features