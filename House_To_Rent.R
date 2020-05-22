#Set the working directory
setwd("C:/Drive/Bath School of Management/Semester-2/Data Mining/Kaggle Datasets/brasilian-houses-to-rent")

#Reading all the libraries
library(readr)
library(tidyr)
library(dplyr)
library(xray)
library(skimr)

data <- read_csv("houses_to_rent_v2.csv")

#Checking what is there in the data
glimpse(data)
View(data)

#Checking the anomalies in the data
anomalies(data)
skim(data)
#The data shows thatt there are 10692 enteries and 13 columns
# 4 are characters and 9 are numeric values in the columns
#The data has zero missing values which ensure the data quality and 
#all the data is complete

#Cleaning the data
colnames(data) <- c("City", "Area", "Rooms", "Bathrooms", "Parking_Space", "Floor", "Animal_Allowed", "Furniture", "HOA", "Rent_Amount", "Property_Tax", "Fire_Insurance", "Total")

#Useful for finding the correlation 
data$Animal_Allowed <- factor(data$Animal_Allowed, levels = c("acept", "not acept"), labels = c("Accept", "Not Accepted"))
data$Furniture <- factor(data$Furniture, levels = c("furnished", "not furnished"), labels = c("Furnished", "Unfurnished"))

#Checking the structure of the data
str(data)

#Data Exploration
#Checking the most popularity of city to rent
library(ggplot2)
library(ggthemes)
#Cheking all the outliers in a single graph
data %>% select(City, Area, Rooms, Bathrooms, Parking_Space, Animal_Allowed, Furniture, HOA, Rent_Amount, Property_Tax, Fire_Insurance, Total) %>%
ggpairs()

ggplot(data) + geom_boxplot(aes(x = Area)) + theme_bw() #Outliers
ggplot(data) + geom_boxplot(aes(x = Rooms)) + theme_bw() #Restrict with 10 rooms
ggplot(data) + geom_boxplot(aes(x = Bathrooms)) + theme_bw()
ggplot(data) + geom_boxplot(aes(x = Parking_Space)) + theme_bw()
ggplot(data) + geom_boxplot(aes(x = HOA)) + theme_bw() #Outliers
ggplot(data) + geom_boxplot(aes(x = Rent_Amount)) + theme_bw() #Outliers
ggplot(data) + geom_boxplot(aes(x = Property_Tax)) + theme_bw() #Outlier
ggplot(data) + geom_boxplot(aes(x = Fire_Insurance)) + theme_bw() #Outlier
ggplot(data) + geom_boxplot(aes(x = Total)) + theme_bw()#Outlier
summary(data$Fire_Insurance)
#Removing the outliers 
data <- data %>% filter(Area <= 1000, Rooms <= 10, HOA<= 5000, Rent_Amount <= 15000, Property_Tax <= 1000, Fire_Insurance <=  500,  Total <= 10000)

#Checking the na values in the data
sum(is.na(data))

data %>% group_by(City) %>% summarize(Frequency = n()) %>% arrange(desc(Frequency)) %>%
ggplot(aes(x = City, y= Frequency,City)) + geom_bar(stat = "identity", fill = "lightblue", color = 1) +
coord_flip() + theme_bw() + labs(title = "Number of house to rent listings in each city")


#Histogram <- Checking the distribution of price 
ggplot(data, aes(Total)) + geom_histogram(aes(y = ..density..), stat = "bin", bins = 50, binwidth = 1000, fill = "lightblue", color = 1) + theme_bw() +
geom_density(alpha = 0.3) + labs(title = "Price distribution of the properties") + xlab("Price Range") + ylab("Frequency")

#It can be observed that the distribution is Right skewed and as the
#price range increases the frequency of the listings decreases.

#Checking the properties that allows pets
data %>% group_by(Animal_Allowed) %>% summarize(Frequency = n())

#Plotting the histogram of Area
ggplot(data) + geom_histogram(aes(x = Area) , binwidth = 50, breaks = seq(0,1000, by = 50) ,color = "black", fill = "lightblue") +
ylab("Frequency") + labs(title = "Area Distribution") + theme_bw()
  
#According to the area distribution the freq range is high in the 
#range of 50 -100
#Checking the price range in this range
data %>% filter(Area %in% 50:100)  %>%
  ggplot() + geom_point(aes(x = Area, y = Total, color = Furniture), size = 1) + scale_color_viridis_d() +
  theme_bw() + facet_wrap(~City , scales = "free", shrink = TRUE, nrow = 2) + ylab("Price")

data %>% filter(Area %in% 50:100, Property_Tax <= 1000)  %>%
  ggplot() + geom_point(aes(x = Area, y = Property_Tax, color = Furniture), size = 1) + scale_color_viridis_d() +
  theme_bw()

data %>% filter(Area %in% 50:100)  %>%
  ggplot() + geom_point(aes(x = Area, y = Rooms, color = Furniture), size = 1) + scale_color_viridis_d() +
  theme_bw() + facet_wrap(~City , scales = "free", shrink = TRUE, nrow = 2)

data %>% group_by(City,Rooms) %>% summarize(Freq = n()) %>% 
  ggplot(aes(x = Rooms, y = Freq)) + geom_bar(stat = "identity" , fill = "lightblue", color = 1) + 
  facet_grid(~City , scales = "free", shrink = TRUE) + theme_bw(base_size = 10) + xlab("Number of Rooms") + ylab("Number of Listings") + 
  xlim(0,10)

data %>% filter(Area %in% 50:100)  %>%
  ggplot() + geom_point(aes(x = Area, y = Total, color = Animal_Allowed), size = 1) + scale_color_viridis_d() +
  theme_bw() + facet_wrap(~City , scales = "free", shrink = TRUE, nrow = 2) + ylab("Price")

data %>% filter(Area %in% 50:100)  %>%
  ggplot() + geom_point(aes(x = Area, y = Rooms, color = Furniture), size = 1) + scale_color_viridis_d() +
  theme_bw()

data %>% filter(Area %in% 50:100)  %>%
  ggplot() + geom_point(aes(x = Rooms, y = Total, color = Furniture), size = 1) + scale_color_viridis_d() +
  theme_bw()


#Checking the data structure
str(data)
data$Floor <- as.integer(data$Floor)
data$Animal_Allowed <- as.numeric(data$Animal_Allowed)
data$Furniture <- as.numeric(data$Furniture)


#Checking the correlation between area and price range
library(ggcorrplot)
corr_data <-  round(cor(data[,2:13]), 2)
ggcorrplot(corr_data, type = "lower",
           lab = TRUE, lab_size = 3, method="square", 
           colors = c("blue", "white", "red"), 
           title="Correlogram Plot", 
           ggtheme=theme_bw)

#Developing ML Algrorithm

#To chech the flat is furnished or unfurnished
library(caret)
str(data)

#Converting city to factor
data$City <- as.factor(data$City)

#Splitting the data in to training and testing data
set.seed(123)
in_train <- createDataPartition(data$Furniture, p = 0.75, list = FALSE)
train <- data[in_train,]
test <- data[-in_train,]

#GBM
#Using 10 fold CV with random hyperparameter search.
set.seed(123)
fitControl <- trainControl(method = "repeatedcv",   
                            search = "random",  # hyper-parameters random search 
                            number = 10,     # number of folds
                            repeats = 5,     # repeated ten times
                            allowParallel = TRUE)

library(gbm)
set.seed(1234)
gb_model <- train(Furniture ~.,
                data = train,
                method = "gbm",
                trControl = fitControl,
                preProcess = c('scale', 'center'))
confusionMatrix(gb_model)  
plot(gb_model)
plot(varImp(object=gb_model))

getTrainPerf(gb_model)

gbm_predict <- predict(gb_model, test)
confusionMatrix(gbm_predict, test$Furniture)



#Random Forest
library(randomForest)
set.seed(1234)
rf_model <- train(Furniture ~ .,
                     data=train,
                     method="rf",
                     trControl=fitControl,
                     preProcess = c('center','scale'),
                     importance = TRUE)

rf_model
plot(rf_model)
predict <- predict(rf_model, test)
confusionMatrix(predict, test$Furniture)

