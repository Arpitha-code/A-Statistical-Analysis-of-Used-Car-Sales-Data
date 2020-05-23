# Final Project-R Codes
# Group Number: 302
# Name: Arpitha Jagadish
# CWID: A20453142


library(ggplot2)
library(dplyr)
library(lubridate)
library(car)
install.packages("caret")
library(caret)
install.packages("leaps")
library(leaps)
install.packages("PASWR2")

#set the current working directory
setwd("/Users/arpitha/Documents/Data Analytics")

#------------------------------------------------------------------
#Load the Dataset to R Environment
car_price <- read.csv ("cars-price.csv", header = T)

#Display first 6 records
head(car_price)

#view the complete dataset
View(car_price)

#dimension of the Dataset(number of rows and columns)
dim(car_price)

#-------------Hypothesis Testing------------------------------
#-------------Research Problem-1--------------------
#Whether there is a price difference between the automatic transmission cars
#and mechanical transmission cars.

#Two-sided Two tail Hypothesis testing ñ
# Null Hypothesis: H0: For the null hypothesis we consider the average car price for       
# both types of transmission are equal.  
#  
#Alternate Hypothesis: Ha: So, the alternate hypothesis we consider the average car      
#price for both types of transmission are not equal.  

#Loading library ggplot2 to exceute ggplot
library(ggplot2)

ggplot(car_price,aes(x=car_price$transmission, y=car_price$price_usd)) +
  geom_boxplot(aes(fill = car_price$transmission)) +
  stat_summary(fun.y = mean, geom="point", size=2) +
  xlab('Transmission') +
  ylab('Price') +
  ggtitle('Price vs.Transmission Type')

# Assigning Mechanical Transmission Rows to mechanical_cars
mechanical_cars <- car_price[ which(car_price$transmission=="mechanical"),]


# Assigning Automatic Transmission Rows to automatic_cars
automatic_cars <- car_price[ which(car_price$transmission=="automatic"),]


# Accessing Price of Mechanical Cars 
mechanical_car_price <- mechanical_cars$price_usd

# Accessing Price of Automatic Cars 
automatic_car_price <- automatic_cars$price_usd

library(PASWR2) 
# Conducting Z Test
z.test(mechanical_car_price,automatic_car_price,alternative="two.sided",mu=0,sigma.x=sd(mechanical_car_price),
       sigma.y=sd(automatic_car_price),conf.level=0.95,paired=F)


#-------------Research Problem-2---------------------
#The average price of the cars produced between 2001-2010 are greater than cars produced between 1991-2000. 

# One-sided Two tail Hypothesis testing ñ
# Null Hypothesis: H0: For the null hypothesis we consider, the average price of the 
#cars produced in the year 2000-2010 is same as the cars produced in the year 1990-2000

# 
# Alternate Hypothesis: Ha: For the alternate hypothesis we consider, 
#the average price of the cars produced in the year 2000-2010 are greater than cars produced in the year 1990-2000


# Assigning 2000-2010 manufactured Rows to Y2K_cars
Y2K_cars <- car_price[ which(car_price$year_produced %in% c("2001","2002","2003","2004","2005","2006","2007","2008","2009","2010")),]

# Assigning 1990-2000 manufactured Rows to Ninties_cars
Ninties_cars <- car_price[ which(car_price$year_produced %in% c("1991","1992","1993","1994","1995","1996","1997","1998","1999","2000")),]

#Accessing price of Y2K cars
Y2k_cars_price <- Y2K_cars$price_usd

#Accessing price of Ninties cars
Ninties_cars_price <- Ninties_cars$price_usd

#displaying both the plots at once
par(mfrow=c(1,2))

#Boxplot to compare cars between 1990-2000 and 2000-2010
boxplot(Y2k_cars_price,main="Year Produced vs Car Price", 
        xlab="Year Produced(Y2k_cars_price)", ylab="Price")
boxplot(Ninties_cars_price,main="Year Produced vs Car Price", 
        xlab="Year Produced(Ninties_cars)", ylab="Price")

#Conducting Z test 
z.test(Y2k_cars_price,Ninties_cars_price,alternative="greater",mu=0,sigma.x=sd(Y2k_cars_price),
       sigma.y=sd(Ninties_cars_price),conf.level=0.95,paired=F)



#-------------Research Problem-3---------

#Analyze how the average price of the car differs among different types of Drive Train.

#Creating Boxplot
ggplot(car_price,aes(x=car_price$drivetrain, y=car_price$price_usd)) +
  geom_boxplot(aes(fill = car_price$drivetrain)) +
  xlab('Drive Train') +
  ylab('Price') +
  ggtitle('Price vs.Drive Train')

# ANOVA F-test Hypothesis 
# Null Hypothesis: H0: For the null hypothesis we consider the average car price for        
# all types of Drive Trains are equal.  

# Alternate Hypothesis: Ha: So, the alternate hypothesis would be the average car              
# price for all the Drive Trains are not equal	

# I have also conduct the individual T-test for the ANOVA regression model to determine       
# the significant variables.

#Linear Regression Model
anov=lm(car_price$price_usd~car_price$drivetrain)
summary(anov)

#Residual Analysis
#validating Constant variance 
plot(fitted(anov),rstandard(anov),main="predicted vs residuals")
abline(a=0,b=0,col='red')

#Normality test
qqnorm(rstandard(anov))
qqline(rstandard(anov),col=2)


#Transforming Y variable

#Log Transformation
logtrans<-log10(car_price$price_usd)
anov_log=lm(logtrans~car_price$drivetrain)
summary(anov_log)

#Residual Analysis
plot(fitted(anov_log),rstandard(anov_log),main="predicted vs residuals")
abline(a=0,b=0,col='red')

#Checking for Normality
qqnorm(rstandard(anov_log))
qqline(rstandard(anov_log),col=2)


#Square Root Transformation
sqtrans<-sqrt(car_price$price_usd)
anov_sqr=lm(sqtrans~car_price$drivetrain)
summary(anov_sqr)

#Residual Analysis
plot(fitted(anov_sqr),rstandard(anov_sqr),main="predicted vs residuals")
abline(a=0,b=0,col='red')

#Checking for Normality
qqnorm(rstandard(anov_sqr))
qqline(rstandard(anov_sqr),col=2)


#Inverse Transformation
inver<-1/(car_price$price_usd)
anov_inv=lm(inver~car_price$drivetrain)
summary(anov_inv)

#Residual Analysis
plot(fitted(anov_inv),rstandard(anov_inv),main="predicted vs residuals")
abline(a=0,b=0,col='red')

#Checking for Normality
qqnorm(rstandard(anov_inv))
qqline(rstandard(anov_inv),col=2)

#----------------------------------------------------------Pre-Processing(Assigning the manufacturers to corresponding continents)-------------------------------------------------


#Listing unique Manfacurer Names
unique(car_price$manufacturer_name)

#Replacing manufacturer name yA3 with YA3 as they both are same, but displayed as duplicate
levels(car_price$manufacturer_name)[levels(car_price$manufacturer_name)=="yA3"]<-"YA3"

#Lisitng unique manufacturer name, yA3 has been changed to YA3
unique(car_price$manufacturer_name)

#Sorting and assigning different manufacturers to their corresponding continents-Asia,Europe and America based on country of origin
car_price$Manufacturing_Continent<-ifelse(car_price$manufacturer_name %in% c("Subaru","Kia","Acura","Lexus","Mitsubishi","SsangYong","Daewoo","Geely","Lifan","Toyota",
                                                                             "Great Wall","Hyundai","Nissan","Suzuki","Mazda","Infiniti","Chery","Honda"),"Asia", 
                                          ifelse(car_price$manufacturer_name %in% c("LADA", "YA3","Opel","Alfa Romeo","Dacia","Lancia"," Rover","Muscovite","GAZ","Citroen",
                                                                                    "Mini","Jaguar","Porscche","BA3","Fiat", "Renault","Seat","Volkswagen", "Audi","3A3",
                                                                                    "rA3",  "Volvo", "BMW" , "Land Rover", "Iveco", "Skoda","Saab",
                                                                                    "Mercedes-Benz","Peugeot"),"Europe","America"))

#Deleting manufacturer_name and model_name columns as we are seperating the cars with respect to continent
car_price$manufacturer_name <- NULL
car_price$model_name <- NULL

#Check the data set to see if manufacturer_name and model_name columns are deleted
head(car_price)
#Check the dataset to check if the manufacturing continent column has been added
head(car_price)

#----------------------------------------------------------Pre-Processing(Optimizing the model by removing the useless data for prediction)-----------------------------------------------

#Dimension of the car_price dataset(number of rows and columns)
dim(car_price)

#Deleting the columns feature_0 to feature_9, as the type of feature is not assigned to these columns in the dataset.So this wouldn't help to make the prediction.  
car_price<-car_price[,-c(18:27)]

#Verifying the changes and dimension of the car_price(number of rows and columns) after the changes
head(car_price)

#Dimension of the car_price dataset(number of rows and columns)
dim(car_price)

#----------------------------------------------------------Pre-Processing(Translating the Region Names to English Language)--------------------------------------------------------------

#Translating the location information to English to better understand the Names
levels(car_price$location_region)[levels(car_price$location_region)=="??????? ???."]<-"Minsk_Region"
levels(car_price$location_region)[levels(car_price$location_region)=="?????????? ???."]<-"Gomel_Region"
levels(car_price$location_region)[levels(car_price$location_region)=="????????? ???."]<-"Brest_Region"
levels(car_price$location_region)[levels(car_price$location_region)=="??????????? ???."]<-"Mogilev_Region"
levels(car_price$location_region)[levels(car_price$location_region)=="??????????? ???."]<-"Grodno_Region"
levels(car_price$location_region)[levels(car_price$location_region)=="????????? ???."]<-"Vitebsk_Region"

#Renaming the engine_fuel names
levels(car_price$engine_fuel)[levels(car_price$engine_fuel)=="hybrid-diesel"]<-"hybrid_diesel"
levels(car_price$engine_fuel)[levels(car_price$engine_fuel)=="hybrid-petrol"]<-"hybrid_petrol"

#Viewing the dataset to check if the location informaton is updated with English Names
head(car_price)

#----------------------------------------------------------Pre-Processing(Assigning different colors based on contrast)-------------------------------

#listing unique colors of the car
unique(car_price$color)

#Based on the contrast, assigning the colors as Dark, Light and Other 
car_price$color<-ifelse(car_price$color %in% c("silver","yellow","white"),"Light", 
                        ifelse(car_price$color %in% c("red","black","grey","brown","voilet","orange","green","blue"),"Dark", "Other"))

#Viewing the dataset to check if the color information is updated
head(car_price)

#----------------------------------------------------------Pre-Processing(Filling the Missing Values)------------------------------------------------------------------------------

# Checking for missing values
na_count <- sapply(car_price, function(y) sum(length(which(is.na(y)))))

#column with the missing values
na_count

#Though there is no fuel capacity for the electric cars, trying to fill it with the appropiate value by taking average value 
car_price$engine_capacity = ifelse(is.na(car_price$engine_capacity), ave(car_price$engine_capacity, FUN= function(x) mean(x, na.rm=T)), car_price$engine_capacity)

#Checking for missing values
sum(is.na(car_price$engine_capacity))

#Verifying the columns after the changes
na_count2 <- sapply(car_price, function(y) sum(length(which(is.na(y)))))
na_count2

#----------------------------------------------------------Pre-Processing(Creating dummy variables)---------------------------------------------

library(dummies)

#Checking number of rows and columns befor creating dummy variables
dim(car_price)

## Creating Dummy variables
car_price <- dummy.data.frame(car_price, names=c("transmission"))
#Removing one dummy variable(N-1)
car_price <- car_price [, -c (2)]


## Creating Dummy variables
car_price <- dummy.data.frame(car_price, names<-c("color"))
#Removing one dummy variable(N-1)
car_price <- car_price [, -c (2)]


## Creating Dummy variables
car_price <- dummy.data.frame(car_price, names<-c("engine_fuel"))
#Removing one dummy variable(N-1)
car_price <- car_price [, -c (7)]


## Creating Dummy variables
car_price <- dummy.data.frame(car_price, names<-c("engine_has_gas"))
#Removing one dummy variable(N-1)
car_price <- car_price [, -c (12)]


## Creating Dummy variables
car_price <- dummy.data.frame(car_price, names<-c("engine_type"))
#Removing one dummy variable(N-1)
car_price <- car_price [, -c (13)]


## Creating Dummy variables
car_price <- dummy.data.frame(car_price, names=c("body_type"))
# Removing one dummy variable(N-1)
car_price <- car_price [, -c (15)]


## Creating Dummy variables
car_price <- dummy.data.frame(car_price, names<-c("has_warranty"))
#Removing one dummy variable(N-1)
car_price <- car_price [, -c (26)]


## Creating Dummy variables
car_price <- dummy.data.frame(car_price, names<-c("state"))
#Removing one dummy variable(N-1)
car_price <- car_price [, -c (27)]


## Creating Dummy variables
car_price <- dummy.data.frame(car_price, names<-c("drivetrain"))
#Removing one dummy variable(N-1)
car_price <- car_price [, -c (31)]

## Creating Dummy variables
car_price <- dummy.data.frame(car_price, names<-c("is_exchangeable"))
#Removing one dummy variable(N-1)
car_price <- car_price [, -c (32)]


## Creating Dummy variables
car_price <- dummy.data.frame(car_price, names=c("location_region"))
#Removing one dummy variable(N-1)
car_price <- car_price [, -c (33)]


## Creating Dummy variables
car_price <- dummy.data.frame(car_price, names=c("Manufacturing_Continent"))
# Removing one dummy variable(N-1)
car_price <- car_price [, -c (41)]


#Verifying the data set after creating dummy variables
head(car_price)

#Checking number of rows and columns afer creating dummy variables
dim(car_price)


#----------------------------------------------------------Grouping the columns---------------------------------------------------------------

#Though the varibles year,odometer,number of photos and up counter are numerical varibles,
#it is treated as categorical and converted it to dummy varibles in the regeression model 

##Divinding the years into 4 groups 
#1940-1960 is 40_60
#1960-1980 is 60_80
#1980-2000 is 80_00
#2000-2020 is 00_20
car_price$year_produced<-cut(car_price$year_produced,breaks=c(1940,1960,1980,2000,2020),labels = c('40_60','60_80','80_00','00_20'))

#Display the first 10 values
car_price$year_produced[1:10]

#Creating dummy variables and removing one dummy variable(N-1)
car_price <- dummy.data.frame(car_price, names=c("year_produced"))
car_price <- car_price [, -c (5)]

##Dividing the odometer value into 4 groups 

# Creating Dummy variables
car_price$odometer_value<-cut(car_price$odometer_value, 
                              quantile(car_price$odometer_value, probs = c(0, .25, .50,.75, 1)), 
                              labels = c('grp1','grp2','grp3', 'grp4'), 
                              include.lowest = TRUE)

car_price$odometer_value[1:10]

#Creating dummy variables and removing one dummy variable(N-1)
car_price <- dummy.data.frame(car_price, names=c("odometer_value"))
car_price <- car_price [, -c (4)]


##Dividing the number of photos into 2 groups
car_price$number_of_photos<-cut(car_price$number_of_photos,breaks=c(0,43,86))

car_price$number_of_photos[1:10]

#Creating dummy variables and removing one dummy variable(N-1)
car_price <- dummy.data.frame(car_price, names=c("number_of_photos"))
car_price <- car_price [, -c (43)]


##Dividing the up counter into 4 groups
car_price$up_counter<-cut(car_price$up_counter,breaks=c(0,465,931,1396,1861))
car_price$up_counter[1:10]

#Creating dummy variables and removing one dummy variable(N-1)
car_price <- dummy.data.frame(car_price, names=c("up_counter"))
car_price <- car_price [, -c (46)]

##Dividing the up counter into 4 groups
car_price$duration_listed<-cut(car_price$duration_listed, quantile(car_price$duration_listed, probs = c(0, .25, .50,.75, 1)), 
                               labels = c('grp1','grp2','grp3', 'grp4'), 
                               include.lowest = TRUE)

car_price$duration_listed[1:10]

#Creating dummy variables and removing one dummy variable(N-1)
car_price <- dummy.data.frame(car_price, names=c("duration_listed"))
car_price <- car_price [, -c (48)]

#Verifying the data set after creating groups
head(car_price)

#Checking number of rows and columns afer creating groups
dim(car_price)

#Checking structure of data before creating regrssion model

str(car_price)
names(car_price)

#checking for correlation
cor(car_price)

#---------Multilinear Regression Model------------------

library(caret)
set.seed(10001) 
train.control <- trainControl(method = "cv", number = 10)

#Backvar<-car_price$price_usdward Selection
# Train the model backward selection
model_backward <- train(price_usd~., data = car_price, method = "leapBackward",
                        trControl = train.control)
# Summarize the results
print(model_backward)



#Forward selection
# Train the model backward selection
model_forward <- train(price_usd~., data = car_price, method = "leapForward",
                       trControl = train.control)
# Summarize the results
print(model_forward)


#Stepwise
# Train the model stepwise selection
model_step <- train(price_usd~., data = car_price, method = "leapSeq",
                    trControl = train.control)
#Summarize the results
print(model_step)

coef(model_step$finalModel,5)

par(mfrow=c(1,1))
#checking for constant variance
plot(predict(model_step),residuals(model_step),main="Predicted vs Residuals")
abline(a=0,b=0,col='red')

#linear plot for each x variable
plot(car_price$year_produced00_20,y=residuals(model_step),main="residual Vs year produced")
abline(a=0,b=0,col='red')

plot(car_price$transmissionautomatic,y=residuals(model_step),main="residual Vs transmission automatic")
abline(a=0,b=0,col='red')

plot(car_price$drivetrainfront,y=residuals(model_step),main="residual Vs drive train front")
abline(a=0,b=0,col='red')

plot(car_price$body_typesedan,y=residuals(model_step),main="residual Vs body type sedan")
abline(a=0,b=0,col='red')

plot(car_price$location_regionVitebsk_Region,y=residuals(model_step),main="residual Vs location region Vitebsk Region")
abline(a=0,b=0,col='red')

#normality test
qqnorm(residuals(model_step))
qqline(residuals(model_step),col=2)

------------------------------------------------------------------------------------------------------------------------
  
  #Performing Y transrformation
  #After Y transformation(log Transformation)
  price<-car_price$price_usd
#Train the model stepwise selection
model_step2_log<- train(log(price_usd)~., data =car_price, method = "leapSeq",
                        trControl = train.control)

# Summarize the results

print(model_step2_log)

#price_usd prediction after the log transformation
prelog<-predict(model_step2_log,car_price)
head(cbind(actual=car_price$price_usd,prelog))
#Back Transformed
head(cbind(actual=car_price$price_usd,pred=exp(prelog)))

#Calculating the RMSE value
RMSE(car_price$price_usd,exp(prelog))



#Square Root Transformation
#After Y transformation(squareroot Transformation)

#Train the model stepwise selection

model_step2_sqrt<- train(sqrt(price_usd)~.,data =car_price, method = "leapSeq",
                         trControl = train.control)

# Summarize the results
print(model_step2_sqrt)

#price_usd prediction after the Square root transformation
presqt<-predict(model_step2_sqrt,car_price)

head(cbind(actual=car_price$price_usd,presqt))
#Back Transformed
head(cbind(actual=car_price$price_usd,pred=(presqt*presqt)))

#Calculating the RMSE value
RMSE(car_price$price_usd,(presqt*presqt))

#Inverse Transformation
#After Y transformation(Inverse Transformation)

#Train the model stepwise selection
model_step2_inverse<- train(1/(price_usd)~., data =car_price, method = "leapSeq",
                            trControl = train.control)

# Summarize the results
print(model_step2_inverse)

#price_usd prediction after the Inverse transformationn
preinv<-predict(model_step2_inverse,car_price)

head(cbind(actual=car_price$price_usd,preinv))
#Back Transformed
head(cbind(actual=car_price$price_usd,pred=(1/preinv)))

#Calculating the RMSE value
RMSE(car_price$price_usd,(1/preinv))



#lesser value of RMSE for Square Root transformation 
car_price[,'price_usd']<-sqrt(car_price$price_usd)

#building the model using the transformed y values
model_step2_sqrt<- train(price_usd~.,data =car_price, method = "leapSeq",
                         trControl = train.control)

# Summarize the results
print(model_step2_sqrt)

#Verifying constant variance
plot(predict(model_step2_sqrt),residuals(model_step2_sqrt),main="Predicted Vs Residuals")
abline(a=0,b=0,col='red')

#Linearity check for each X variable
plot(car_price$year_produced00_20,y=residuals(model_step2_sqrt),main="residual Vs year produced")
abline(a=0,b=0,col='red')

plot(car_price$body_typesedan,y=residuals(model_step2_sqrt),main="residual Vs body type")
abline(a=0,b=0,col='red')

plot(car_price$drivetrainfront,y=residuals(model_step2_sqrt),main="residual Vs drive train front")
abline(a=0,b=0,col='red')

plot(car_price$has_warrantyTRUE,y=residuals(model_step2_sqrt),main="residual Vs has warranty")
abline(a=0,b=0,col='red')

plot(car_price$transmissionautomatic,y=residuals(model_step2_sqrt),main="residual Vs transmission")
abline(a=0,b=0,col='red')

#Normality Test
qqnorm(residuals(model_step2_sqrt))
qqline(residuals(model_step2_sqrt),col=2)

#Checking Outliers

quantile(car_price$price_usd, probs = seq(0, 1, by= 0.01))


#-----------removing the outliers-------------------
cooks<-lm(price_usd~.,data=car_price)
cooksd <- cooks.distance(cooks)

influential <- as.numeric(names(cooksd)[(cooksd > 4/nrow(car_price))])

with_outliers<-car_price

dim(with_outliers)

no_outliers <- with_outliers[-influential, ]

dim(no_outliers)


#Afterremoving outliers

#Train the model stepwise selection
model_step3<- train(price_usd~., data =no_outliers, method = "leapSeq",
                    trControl = train.control)

# Summarize the results
print(model_step3)

coef(model_step3$finalModel,5)


#Verifying contast variance
plot(predict(model_step3),residuals(model_step3),main="p vs r")
abline(a=0,b=0,col='red')

#Linearity check for each X variable
plot(no_outliers$year_produced00_20,y=residuals(model_step3),main="residual Vs year produced")
abline(a=0,b=0,col='red')

plot(no_outliers$body_typesuv,y=residuals(model_step3),main="residual Vs body type")
abline(a=0,b=0,col='red')

plot(no_outliers$statenew,y=residuals(model_step3),main="residual Vs drive train front")
abline(a=0,b=0,col='red')

plot(no_outliers$is_exchangeableTRUE,y=residuals(model_step3),main="residual Vs is exchangable")
abline(a=0,b=0,col='red')

plot(no_outliers$transmissionautomatic,y=residuals(model_step3),main="residual Vs transmission")
abline(a=0,b=0,col='red')

#Normality Test
qqnorm(residuals(model_step3))
qqline(residuals(model_step3),col=2)

