source("final_project_scrap.R")
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


##################################################################

ggplot(cars_data, aes(x=additional_features, y=log(price_usd)))+
  geom_boxplot(fill='blue') +
  labs(x="Additional Features", y="Log-Price", 
       title="Additional Features vs. Log-Price")+
  theme(plot.title = element_text(hjust = 0.5))



