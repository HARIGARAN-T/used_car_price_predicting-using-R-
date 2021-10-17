#Importing the Dataset
car_data <- read.csv(choose.files())
head(car_data)
str(car_data)

#price of car
summary(car_data$Price)
sum(is.na(car_data$Price))


#age of car
colnames(car_data)[4] <- "Age"
str(car_data$Age)
unique(car_data$Age)
sum(is.na(car_data$Age))


# km 
sum(is.na(car_data$KM))

#fuel type
unique(car_data$Fuel_Type)
car_data$Fuel_Type <- as.numeric(as.factor(car_data$Fuel_Type))


# hp
unique(car_data$HP)

#door
unique(car_data$Doors)


summary(car_data)# summary of data frame
boxplot(car_data$Price)
hist(car_data$Price)
car_data_2<-car_data[,-c(1,2,5,6,11,15,17,18)]

index <- sample(2,nrow(car_data_2),replace = TRUE,prob = c(0.7,0.3))
#training data
training <- car_data_2[index==1,]
#testing data
testing <- car_data_2[index==2,]


multi_linear_reg<- lm(Price ~ .,data = training)
multi_linear_reg
predicted_value<-predict(multi_linear_reg, training[,-1])
predicted_value
plot(training$Price,predicted_value,xlab = "actual value", ylab = "predicted value")
predicted_value_test<-predict(multi_linear_reg, testing[,-1])
predicted_value_test
plot(testing$Price,predicted_value_test,xlab = "actual value", ylab = "predicted value")
summary(multi_linear_reg)


#save model to RDS file
saveRDS(multi_linear_reg,"multi_linear_reg.rds")

write.csv(car_data_2,"car_data_2.csv")

