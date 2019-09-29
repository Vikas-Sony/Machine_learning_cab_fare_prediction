##Loading our train data as csv

getwd()
setwd("C:/Users/Vikas/Desktop/Assignments/Project - 2")
data = read.csv('train_cab.csv')
library("dplyr")
## So, data is simple with attributes like pickup and drop coordinates and fare amount and number of people.Pickup time and no. of people travelling.
str(data)
data <- data[-c(1120:1130),]
data$fare_amount
str(data)
data$fare_amount = as.numeric(as.character(data$fare_amount))
data$fare_amount
summary(data)
library(ggplot2)
boxplot(data$fare_amount)

boxplot(data$passenger_count)
x <- data$fare_amount
qnt <- quantile(x, probs=c(.25, .75), na.rm = T)
caps <- quantile(x, probs=c(.05, .95), na.rm = T)
H <- 1.5 * IQR(x, na.rm = T)
x[x < (qnt[1] - H)] <- caps[1]
x[x > (qnt[2] + H)] <- caps[2]
data$fare_amount <- x
boxplot(data$fare_amount)

data$pickup_datetime[1310:1320]
data <- data[-c(1310: 1329),]
data$pickup_datetime[1320:1330]
data$pickup_datetime = as.POSIXct(data$pickup_datetime)

data1  <- data %>% 
  mutate(pickup_datetime = as.POSIXct(pickup_datetime)) %>%
  mutate(hour = as.numeric(format(pickup_datetime, "%H"))) %>%
  mutate(min = as.numeric(format(pickup_datetime, "%M"))) %>%   
  mutate(year = as.factor(format(pickup_datetime, "%Y"))) %>%
  mutate(day = as.factor(format(pickup_datetime, "%d"))) %>%
  mutate(month = as.factor(format(pickup_datetime, "%m"))) %>%
  mutate(Wday = as.factor(weekdays(pickup_datetime))) %>%
  mutate(hour_class = as.factor(ifelse(hour < 7, "Overnight", 
                                       ifelse(hour < 11, "Morning", 
                                              ifelse(hour < 16, "Noon", 
                                                     ifelse(hour < 20, "Evening",
                                                            ifelse(hour < 23, "night", "overnight") ) ))))) %>%
  filter(fare_amount > 0 & fare_amount <= 500) %>%
  filter(pickup_longitude > -80 && pickup_longitude < -70) %>%
  filter(pickup_latitude > 35 && pickup_latitude < 45) %>%
  filter(dropoff_longitude > -80 && dropoff_longitude < -70) %>%
  filter(dropoff_latitude > 35 && dropoff_latitude < 45) %>%
  filter(passenger_count > 0 && passenger_count < 10) %>%
  mutate(pickup_latitude = (pickup_latitude * pi)/180) %>%
  mutate(dropoff_latitude = (dropoff_latitude * pi)/180) %>%
  mutate(dropoff_longitude = (dropoff_longitude * pi)/180) %>%
  mutate(pickup_longitude = (pickup_longitude * pi)/180 ) %>%
  mutate(dropoff_longitude = ifelse(is.na(dropoff_longitude) == TRUE, 0,dropoff_longitude)) %>%
  mutate(pickup_longitude = ifelse(is.na(pickup_longitude) == TRUE, 0,pickup_longitude)) %>%
  mutate(pickup_latitude = ifelse(is.na(pickup_latitude) == TRUE, 0,pickup_latitude)) %>%
  mutate(dropoff_latitude = ifelse(is.na(dropoff_latitude) == TRUE, 0,dropoff_latitude)) %>%
  select(-pickup_datetime,-hour_class,-min) 
summary(data1)
summary(data)
str(data1)


##getting rid of missing values

row.has.na <- apply(data1, 1, function(x){any(is.na(x))})
sum(row.has.na)
data.filtered <- data1[!row.has.na,]
unique(data$passenger_count)

## Passenger values are very poorly recorded. So, we can just try and improve it a bit and keep only values which are int and less than 6

data.filtered <- data.filtered %>% filter(passenger_count < 7)
data4 <- data.filtered %>% filter( passenger_count == 1 | passenger_count == 2|passenger_count ==3 |passenger_count ==4 | passenger_count == 5 | passenger_count ==6)
str(data4)

dlat <- data4$dropoff_latitude - data4$pickup_latitude
dlon <- data4$dropoff_longitude - data4$pickup_longitude 
R_earth <- 6371
#Compute haversine distance
hav = sin(dlat/2.0)**2 + cos(data4$pickup_latitude) * cos(data4$dropoff_latitude) * sin(dlon/2.0)**2
data4$haversine <- 2 * R_earth * asin(sqrt(hav))
str(data4)

## Now, its time to strasample <- sample.int(n = nrow(data3), size = floor(.75*nrow(data)), replace = F)

## we can see that this is a regression problem and multiple regression, Random forest regressor and sgd are some of the models that can be applied.

set.seed(101)# Set Seed so that same sample can be reproduced in future also
sample <- sample.int(n = nrow(data4), size = floor(.75*nrow(data)), replace = F)
train <- data4[sample, ]
test  <- data4[-sample, ]
# Now Selecting 75% of data as sample from total 'n' rows of the data  


library('e1071')
regressor = lm(formula = fare_amount ~ . , data = data4)
regressor = step(regressor)
y_pred = predict(regressor, test)

## Now, trying SVR

#regressor2 = svm(formula = fare_amount ~ .,
 #               data = train,
  #              type = 'eps-regression')
#y_pred2 = predict(regressor2, test)


## Now, moving to random forest regressor
library('randomForest')

regressor3 = randomForest(x = train[,-which(names(train)=="fare_amount")],
                          y = train$fare_amount)
y_pred3 = predict(regressor3, test)
# Error metrics calculations


library('Metrics')
MAE = mean(abs(y_pred- test$fare_amount))
#MAE2 = mean(abs(y_pred2- test$fare_amount))
MAE3 = mean(abs(y_pred3 - test$fare_amount))

## WE can aslo use metrics like rmse and others to get a better estimate of our data

RMSE = rmse(test$fare_amount, y_pred)
#RMSE2 = rmse(test$fare_amount, y_pred2)
RMSE3 = rmse(test$fare_amount, y_pred3)

## So, we can see that ensemble techniques gives us better results .



## Now, we can use various models to test which work best on our real test set.

test.act = read.csv('test.csv')
test1.act  <- test.act%>% 
  mutate(pickup_datetime = as.POSIXct(pickup_datetime)) %>%
  mutate(hour = as.numeric(format(pickup_datetime, "%H"))) %>%
  mutate(min = as.numeric(format(pickup_datetime, "%M"))) %>%   
  mutate(year = as.factor(format(pickup_datetime, "%Y"))) %>%
  mutate(day = as.factor(format(pickup_datetime, "%d"))) %>%
  mutate(month = as.factor(format(pickup_datetime, "%m"))) %>%
  mutate(Wday = as.factor(weekdays(pickup_datetime))) %>%
  mutate(hour_class = as.factor(ifelse(hour < 7, "Overnight", 
                                       ifelse(hour < 11, "Morning", 
                                              ifelse(hour < 16, "Noon", 
                                                     ifelse(hour < 20, "Evening",
                                                            ifelse(hour < 23, "night", "overnight") ) ))))) %>%
  
  filter(pickup_longitude > -80 && pickup_longitude < -70) %>%
  filter(pickup_latitude > 35 && pickup_latitude < 45) %>%
  filter(dropoff_longitude > -80 && dropoff_longitude < -70) %>%
  filter(dropoff_latitude > 35 && dropoff_latitude < 45) %>%
  filter(passenger_count > 0 && passenger_count < 10) %>%
  mutate(pickup_latitude = (pickup_latitude * pi)/180) %>%
  mutate(dropoff_latitude = (dropoff_latitude * pi)/180) %>%
  mutate(dropoff_longitude = (dropoff_longitude * pi)/180) %>%
  mutate(pickup_longitude = (pickup_longitude * pi)/180 ) %>%
  mutate(dropoff_longitude = ifelse(is.na(dropoff_longitude) == TRUE, 0,dropoff_longitude)) %>%
  mutate(pickup_longitude = ifelse(is.na(pickup_longitude) == TRUE, 0,pickup_longitude)) %>%
  mutate(pickup_latitude = ifelse(is.na(pickup_latitude) == TRUE, 0,pickup_latitude)) %>%
  mutate(dropoff_latitude = ifelse(is.na(dropoff_latitude) == TRUE, 0,dropoff_latitude)) %>%
  select(-pickup_datetime,-hour_class,-min) 
dlat <- test1.act$dropoff_latitude - test1.act$pickup_latitude
dlon <- test1.act$dropoff_longitude - test1.act$pickup_longitude 
R_earth <- 6371
#Compute haversine distance
hav = sin(dlat/2.0)**2 + cos(test1.act$pickup_latitude) * cos(test1.act$dropoff_latitude) * sin(dlon/2.0)**2
test1.act$haversine <- 2 * R_earth * asin(sqrt(hav))
str(test1.act)
y_pred_final = predict(regressor3, test1.act)
str(data4)
str(y_pred_final)
test1.act$fare_prediction = y_pred_final
str(test1.act)
write.csv(test1.act, file = 'cabfare_RF.csv', row.names = FALSE, quote = FALSE)

## data fare amount values

data$fare_amount
data4$fare_amount

