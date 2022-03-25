dev.off()
rm(list=ls())



library(dplyr)
library(broom)
library(forecast)
library(readr)
library(tidyr)
library(lubridate)
library(glmnet)
library(gbm)
library(xts)
library(ggplot2)
library(sarima)
library(astsa)
library(prophet)
library(zoo)
library(pracma)

#To run Prophet
library(xts)
library(marima)
library(tidyverse)
library(prophet)
library(data.table)
library(reshape2)



setwd("C:/Users/Quinn/Desktop/Predictive Analytics/Projects/Mini Projects/Mini Project 1")

store.data <- read_csv("stores.csv")
features.data <- read_csv("features.csv")
train.data <- read_csv("train.csv")
test.data <- read_csv("test.csv")

millions <- 1000000
hundred.thousand <- 100000
thousands <- 100000

# With the data loaded we want to start with some basic and generalizing data exploration.
# The implementation reference that was provided is a very good reference point, but we want to be a little more thorough.

# Let's start with finding the number of store not only the training data, but the test data as well. If we have a match, we can use the match as an indicator that the data is split nicely.

# Total Number of Distinct Stores in each dataset
train.store.count <- train.data%>%summarize(Training_Data=n_distinct(Store))
test.store.count <- test.data%>%summarize(Test_Data=n_distinct(Store))
paste("The number of stores located in the Training Data is:",train.store.count)
paste("The number of stores located in the Test Data is:",test.store.count)

# Total Number of Distinct Departments in each dataset
train.dept.count <- train.data%>%summarize(Training_Data=n_distinct(Dept))
test.dept.count <- test.data%>%summarize(Test_Data=n_distinct(Dept))
paste("The number of Departments located in the Training Data is:",train.dept.count)
paste("The number of Departments located in the Test Data is:",test.dept.count)

# Total Number of Holiday Weeks in each dataset
train.weeks <- train.data%>%select(Date, IsHoliday)
train.holiday.weeks <- train.weeks%>%filter(IsHoliday==TRUE)
test.weeks <- test.data%>%select(Date, IsHoliday)
test.holiday.weeks <- test.weeks%>%filter(IsHoliday==TRUE)
count.of.holiday.weeks.train <- train.holiday.weeks%>%summarize(Training_Holiday_Weeks=length(Date))
count.of.holiday.weeks.test <- test.holiday.weeks%>%summarize(Test_Holiday_Weeks=length(Date))
paste("The total number of Holiday Weeks located in the Training Data is:",count.of.holiday.weeks.train)
paste("The total number of Holiday Weeks located in the Test Data is:",count.of.holiday.weeks.test)

train.data.new <- train.data
train.data.new$Dept <- as.factor(train.data.new$Dept)
train.data.new$year <- strftime(train.data.new$Date, "%Y")
train.data.new$month <- strftime(train.data.new$Date, "%m")
train.data.monthly <- train.data.new%>%group_by(year,month)%>%
  summarize(Monthly_Sales=sum(Weekly_Sales),.groups = 'keep')

train.data.monthly%>%ggplot(aes(x=month,y=Monthly_Sales/millions)) +
  geom_bar(stat="identity", fill="deepskyblue3") +
  facet_wrap(~ year, ncol=3) +
  labs(title = "Monthly Sales for Training Data", subtitle = "By YEAR",
       y="Monthly Sales (Millions)", x="Month") +
  theme_grey(base_size = 15)

train.data.dept.monthly <- train.data.new%>%group_by(Dept,year)%>%
  summarize(Dept_Yearly_Sales=sum(Weekly_Sales),.groups = 'keep')
train.data.dept.monthly%>%ggplot(aes( fill=Dept,x=year, y=Dept_Yearly_Sales/thousands)) +
  geom_bar(stat='identity', position = 'dodge') +
  labs(title="Yearly Sales by Department for Training Data",y="Yearly Sales",x="Year") +
  theme_grey(base_size = 15)

train.data.sales.store.joined <- train.data.new%>%left_join(store.data,by='Store')
train.data.top.dept.sales <- train.data.sales.store.joined%>%group_by(Store)%>%
  summarize(Monthly_Sales_by_Store=sum(Weekly_Sales),.groups='keep')%>%
  filter(Monthly_Sales_by_Store>150000000)
train.data.top.dept.sales%>%ggplot(aes(fill=Store,x=Store,y=Monthly_Sales_by_Store/millions)) +
  geom_bar(stat='identity') +
  labs(title="Stores Selling over $150,000,000",subtitle="Top Sales Performers over 3 years",y="Sales(Millions)",x="Stores")

# Top selling store, top selling departments.
train.data.sales.store.joined <- train.data.new%>%left_join(store.data,by='Store')
train.data.top.dept.sales.by.store <- train.data.sales.store.joined%>%group_by(Dept)%>%
  filter(Store==20)%>%
  summarize(Monthly_Sales_by_Dept=sum(Weekly_Sales),.groups='keep')%>%
  filter(Monthly_Sales_by_Dept>5000000)
train.data.top.dept.sales.by.store%>%ggplot(aes(fill=Dept,x=Dept,y=Monthly_Sales_by_Dept/hundred.thousand)) +
  geom_bar(stat='identity') +
  labs(title="Top Selling Store. Departments Seeling over $500K Monthly",subtitle="Store 20",y="Sales(Thousands)",x="Departments")

# I am commenting on this one as we needed to utilize this one from the implementation that was available for us.
# We did however, add in the additional variable which lets us see the Weekly Sales by Dept added in.
train.data.sales.store.joined%>%ggplot(aes(x=Weekly_Sales,fill=Dept)) +
  geom_histogram(bins=81) +
  facet_grid(Type~.) +
  labs(title="Histogram of Weekly Sales by Type", subtitle="Departments variable added in to show individual performance") +
  scale_x_log10()

train.data.new.features <- train.data.new%>%left_join(features.data, by=c('Store','Date'))
train.data.new.features <- train.data.new.features%>%mutate(MarkDown1=coalesce(MarkDown1,0),
                                                            MarkDown2=coalesce(MarkDown2,0),
                                                            MarkDown3=coalesce(MarkDown3,0),
                                                            MarkDown4=coalesce(MarkDown4,0),
                                                            MarkDown5=coalesce(MarkDown5,0))

# We wanted to see if any of the MarkDowns would have an effect on the Sales.
# We found that MarkDowns do indeed have a strong impact on the Sales per store.
# The visualization below take a little bit to run, but they do run properly.
train.data.new.features$Store <- as.factor(train.data.new.features$Store)
# MarkDown 1
train.data.new.features%>%ggplot(aes(x=MarkDown1,y=Weekly_Sales/hundred.thousand)) +
  geom_point(aes(color=Store)) +
  geom_line(aes(y=fitted(lm(Weekly_Sales~MarkDown1,
                            data=train.data.new.features))))
# MarkDown 2
train.data.new.features%>%ggplot(aes(x=MarkDown2,y=Weekly_Sales/hundred.thousand)) +
  geom_point(aes(color=Store)) +
  geom_line(aes(y=fitted(lm(Weekly_Sales~MarkDown2,
                            data=train.data.new.features))))
# MarkDown 3
train.data.new.features%>%ggplot(aes(x=MarkDown3,y=Weekly_Sales/hundred.thousand)) +
  geom_point(aes(color=Store)) +
  geom_line(aes(y=fitted(lm(Weekly_Sales~MarkDown3,
                            data=train.data.new.features))))
# MarkDown 4
train.data.new.features%>%ggplot(aes(x=MarkDown4,y=Weekly_Sales/hundred.thousand)) +
  geom_point(aes(color=Store)) +
  geom_line(aes(y=fitted(lm(Weekly_Sales~MarkDown4,
                            data=train.data.new.features))))
# MarkDown 5
train.data.new.features%>%ggplot(aes(x=MarkDown5,y=Weekly_Sales/hundred.thousand)) +
  geom_point(aes(color=Store)) +
  geom_line(aes(y=fitted(lm(Weekly_Sales~MarkDown5,
                            data=train.data.new.features))))

# Below shows the Sales and displays line graphs showing making anny trend spikes noticable.
train.data.new.topseller.monthly <- train.data.new.features%>%filter(Store==c(1,2,4,6,41))%>%
  group_by(Store,year,month)%>%summarize(Monthly_Sales=sum(Weekly_Sales),.groups='keep')
train.data.new.topseller.monthly.joined <- train.data.new.topseller.monthly%>%
  left_join(train.data.new.features, by = c('Store','year','month'))
# Displays Monthly Sales by YEAR. We can clearly see spikes in sales during holiday periods.
# This is a great indicator that the data is cyclical.
train.data.new.topseller.monthly.joined%>%ggplot(aes(x=Date,y=Monthly_Sales/millions, col=Store)) +
  geom_line() +
  labs(title="Sales Trends by Top Sellers", subtitle="Monthly Sales",y="Monthly Sales (Millions)")
# Here we filter down on Weekly Sales and only look at one YEAR(2011)
# This allowed us to see that the cyclical theme can also be seen within each individual year.
train.data.new.topseller.monthly.joined%>%filter(year==2011)%>%
  ggplot(aes(x=Date,y=Weekly_Sales/hundred.thousand, col=Store)) +
  geom_line() +
  labs(title="Sales Trends by Top Sellers", subtitle="Year - 2011",y="Weekly Sales (Hundred Thousand)")

# It seems that the Holiday times dive very large spikes in sales, but it is also worth noting that December is always the largest spike in sales.
# Something additional we noticed was that the time period in between New Years and Valentine's consistently displays the lowest point on the graph.
# The items spoken on above are additional indicators that this data does present with a cyclical pattern.

# Factoring "Store" & "Dept"
train.data$Store <- as.factor(train.data$Store)
train.data$Dept <- as.factor(train.data$Dept)
test.data$Store <- as.factor(test.data$Store)
test.data$Dept <- as.factor(test.data$Dept)
train.data$IsHoliday <- as.numeric(train.data$IsHoliday)
test.data$IsHoliday <- as.numeric(test.data$IsHoliday)

########################################################################################################################################################################################################

# Training Data Exploration with SARIMA
# SARIMA WITHOUT ADDITIONAL VARIABLES.
sales.no.vairables <- train.data[train.data[,'Store']==20 & train.data[,'Dept']==92,3:4]
ft <- features.data %>%
  filter(Store==20)
sales.no.vairables.2 <- ts(sales.no.vairables[,2])
# Plot the weekly sales time series
par(mfrow=c(1,2))
plot(sales.no.vairables.2, main = 'Weekly Sales') # We can see that there is no clear trend in the plot, and we can only see seasonality.
plot(diff(log(sales.no.vairables.2)), main = 'Transformed Weekly Sales') # Plot the transformed data 
# Plot the ACF and PACF
acf2(sales.no.vairables.2) 
# Train test split
n = length(sales.no.vairables.2)*0.8
train = sales.no.vairables.2[1:115]
test = sales.no.vairables.2[116:135]
par(mfrow=c(1,1))
sarima(train, 4,0,4)
pred = sarima.for(train, n.ahead = 20, 4,0,4)
mean((pred$pred-test)^2)
# MSPE is 90758265

########################################################################################################################################################################################################

# SARIMA WITH VARIABLES

sales.1 =train.data[train.data[,'Store']==20 & train.data[,'Dept']==92,]
features.data$Date = as.character(features.data$Date)#Changing variable type to character - one dataframe had dates and the other file didn't. We need to combine the 2 data sets
sales.1$Store = as.double(sales.1$Store)
sales.1$Date = as.character(sales.1$Date)
df<- sales.1 %>% left_join(store.data, by = "Store") %>% left_join(features.data, by = c("Store", "Date")) #Creating a dataframe by left joining by the store and bringing in the date 
train.numeric = df %>% select(Weekly_Sales, 'IsHoliday.x', 'Temperature', 'Fuel_Price', 'CPI', 'Unemployment')
train.numeric$IsHoliday.x = as.numeric(train.numeric$IsHoliday.x)
sales.1

########################################################################################################################################################################################################

cormat <- round(cor(train.numeric),2)
melted_cormat <- melt(cormat)
# Get lower triangle of the correlation matrix
get_lower_tri<-function(cormat){
  cormat[upper.tri(cormat)] <- NA
  return(cormat)
}
# Get upper triangle of the correlation matrix
get_upper_tri <- function(cormat){
  cormat[lower.tri(cormat)]<- NA
  return(cormat)
}

upper_tri <- get_upper_tri(cormat)
melted_cormat <- melt(upper_tri, na.rm = TRUE)
# Heatmap
reorder_cormat <- function(cormat){
  # Use correlation between variables as distance
  dd <- as.dist((1-cormat)/2)
  hc <- hclust(dd)
  cormat <-cormat[hc$order, hc$order]
}
# Reorder the correlation matrix
cormat <- reorder_cormat(cormat)
upper_tri <- get_upper_tri(cormat)
# Melt the correlation matrix
melted_cormat <- melt(upper_tri, na.rm = TRUE)
# Create a ggheatmap
ggheatmap <- ggplot(melted_cormat, aes(Var2, Var1, fill = value))+
  geom_tile(color = "white")+
  scale_fill_gradient2(low = "blue", high = "red", mid = "white", 
                       midpoint = 0, limit = c(-1,1), space = "Lab", 
                       name="Pearson\nCorrelation") +
  theme_minimal()+ # minimal theme
  theme(axis.text.x = element_text(angle = 45, vjust = 1, 
                                   size = 12, hjust = 1))+
  coord_fixed()
# Print the heatmap
dev.off()
print(ggheatmap)

ggheatmap + 
  geom_text(aes(Var2, Var1, label = value), color = "black", size = 4) +
  theme(
    axis.title.x = element_blank(),
    axis.title.y = element_blank(),
    panel.grid.major = element_blank(),
    panel.border = element_blank(),
    panel.background = element_blank(),
    axis.ticks = element_blank(),
    legend.justification = c(1, 0),
    legend.position = c(0.6, 0.7),
    legend.direction = "horizontal")+
  guides(fill = guide_colorbar(barwidth = 7, barheight = 1,
                               title.position = "top", title.hjust = 0.5))

########################################################################################################################################################################################

# DATA PROCESSING
# We will look at the weekly sales of Store 20 and Department 92
sales = train.data[train.data[,'Store']==20 & train.data[,'Dept']==92,3:4]
ft = features.data %>%
  filter(Store==20)%>%
  select(Date,Temperature,Fuel_Price,CPI,Unemployment)

########################################################################################################################################################################################

#MODELLING BEGINS

#1. SARIMA MODEL WITH NO ADDITIONAL PARAMETERS
# Convert the sales data into time series
sales2 = ts(sales[,2])
# Plot the weekly sales time series
par(mfrow=c(1,2))
plot(sales2, main = 'Weekly Sales') # We can see that there is no clear trend in the plot, and we can only see seasonality.
plot(diff(log(sales2)), main = 'Transformed Weekly Sales') # Plot the transformed data 
# Plot the ACF and PACF
acf2(sales2) 
# ACF tails off and PACF cuts off at lag 7, then we will try a AR(1) with seasonal period 
# of 7 for seasonality. 
# For the non-seasonal part, first we can say ACF cuts off at lag 7 and PACF tails off, 
# so we can try MA(1)
# Or we can say ACF tails off and PACF cuts off at lag 14, so we can try AR(2)
# Or, we can say ACF tails off and PACF tails off, so we try a AR(2,1) model
# Experimenting with model parameters to find the best model
sarima(sales2, 0,0,1,1,0,0,7) 
# For this model, we an see that the p values is around 0 after lag 6, which means the residuals are correlated 
sarima(sales2, 6,1,0,1,0,0,7) 
#For this model, we can see that all the p values are above 0, which means the residuals are independent 
sarima(sales2, 4,0,1,1,1,0,7) 
#For this model, we can see that the p values is 0 at lag 14 and lag 15, which means the residuals are correlated at lag 14 and lag 15 
#To sum up, among the three models, ARMA(2,0)*(1,0) is the best one, so we will choose this model for prediction

#################################
# Model Prediction
################################

# Train test split
n = length(sales2)*0.8
train = sales2[1:115]
test = sales2[116:143]
par(mfrow=c(1,1))
pred = sarima.for(train, n.ahead = 28, 6,1,0,1,0,0,7)
mean((pred$pred-test)^2)
# MSPE is 187218426

#ADDING ADDITINAL PARAMETERS FOR SARIMA MODEL
sales$Date <- as.character(sales$Date)
#1. Temperature
ft1 = merge(ft,sales, by="Date")
ft1
#lets play with feature - Temperature and see who our fair weather Walmart shoppers are
ftTemp <- ft1[,1:2]
ftTemp
#convert to time series
wmts<-ts(sales[,2],start=c(2010,6), end= c(2012,44), frequency = 52)
ftTempts<- ts(ftTemp[,2],start=c(2010,6), end= c(2012,44), frequency = 52)
#Plot of time series contained dates as points on the graph. 
#Zooreg was used to transform the time series.
ftTempz<-zooreg(ftTemp[,2], start= as.Date('2010-02-05'), end =as.Date('2012-10-26'), deltat = 7)
wmtsz<-zooreg(sales[,2], start= as.Date('2010-02-05'), end =as.Date('2012-10-26'), deltat = 7)
#lets look at our data
plot(wmtsz)
#Exploring the ACF and PACF of the time series
par(mfrow= c(2,1))
#Look at these three graphs together 
plot(wmtsz)
plot(diff(wmtsz))
par(mfrow= c(2,1))
acf(diff(wmtsz))
pacf(diff(wmtsz))
#Observations:-
#We observed the time series and we take the difference to see if we can remove any trend.
#We can see that the difference detrended the data and made it more stationary.
#We took the ACF and PACF to find the lag of the data to identify the p and q for the SARIMA model

# par(mfrow= c(1,1))
# ccf(wmtsz,ftTempz)

#Observations:-
#From the CCF plot, we see that there's a negative correlation between the 
#temperature and the weekly sales.As temperature decreases, sales somewhat increase.
sarima(wmtsz,8,2,4,1,1,1,2,xreg=ftTempz) #The residuals are close to zero indicating that the data is normalized
#Interpret?
#Splitting the training and testing dataset
train = 1:115
test = 116:14
fit <- Arima(wmtsz[train],c(8,2,4),seasonal=list(order=c(1,1,1),period=2),xreg=ftTempz[train,])
fit2 <- Arima(wmtsz[test],c(8,2,4),seasonal=list(order=c(1,1,1),period=2),xreg=ftTempz[test,],model=fit)
onestep <- fitted(fit2)
#Plotting the predicted sales from our fitted model and comparing against the test data
plot(wmtsz)
lines(time(wmtsz)[test],as.vector(onestep),col="red")
# The predictions are really close to the actual data
#MSPE
mean((wmtsz[test]-as.vector(onestep))^2)     
#MSPE = 206303743

#2. Unemployment
ft1 = merge(ft,sales)
View(ft1)
#Checking to see if unemployment has an effect on weekly sales
ftUnemp<- ft1[,4:5]
#convert to time series
wmts<-ts(sales[,2],start=c(2010,6), end= c(2012,44), frequency = 52)
ftUnempts<- ts(ftUnemp[,2],start=c(2010,6), end= c(2012,44), frequency = 52)
#Plot of time series contained dates as points on the graph. 
#Zooreg was used to transform the time series.
ftUnempz<-zooreg(ftUnemp[,2], start= as.Date('2010-02-05'), end =as.Date('2012-10-26'), deltat = 7)
wmtsz<-zooreg(sales[,2], start= as.Date('2010-02-05'), end =as.Date('2012-10-26'), deltat = 7)
#lets look at our data
plot(wmtsz)
#Exploring the ACF and PACF of the time series
par(mfrow= c(2,1))
#Look at these three graphs together 
plot(wmtsz)
plot(diff(wmtsz))
par(mfrow= c(2,1))
acf(diff(wmtsz))
pacf(diff(wmtsz))
#Observations:-
#We observed the time series and we take the difference to see if we can remove any trend.
#We can see that the difference detrended the data and made it more stationary.
#We took the ACF and PACF to find the lag of the data to identify the p and q for the SARIMA model

# par(mfrow= c(1,1))
# ccf(wmtsz,ftUnempz)

#Observations:-
#From the CCF plot, we see that there's a negative correlation between unemployment
# and the weekly sales.As unemployment decreases, sales somewhat increase.
sarima(wmtsz,9,2,4,1,1,1,2,xreg=ftUnempz) #The residuals are close to zero indicating that the data is normalized
#Interpret?
#Splitting the training and testing dataset
train = 1:115
test = 116:143
fit <- Arima(wmtsz[train],c(8,2,4),seasonal=list(order=c(1,1,1),period=2),xreg=ftUnempz[train,])
fit2 <- Arima(wmtsz[test],c(8,2,4),seasonal=list(order=c(1,1,1),period=2),xreg=ftUnempz[test,],model=fit)
onestep <- fitted(fit2)
#Plotting the predicted sales from our fitted model and comparing against the test data
plot(wmtsz)
lines(time(wmtsz)[test],as.vector(onestep),col="red")
# The predictions are really close to the actual data
#MSPE
mean((wmtsz[test]-as.vector(onestep))^2)     
#MSPE = 56494500

#3. MARKDOWNS
ft = features.data %>%
  filter(Store==20)%>%
  select(Date,Temperature,Fuel_Price,CPI,Unemployment,MarkDown1,MarkDown2,MarkDown3,MarkDown4,MarkDown5)%>%
  mutate(MarkDown1=coalesce(MarkDown1,0),
         MarkDown2=coalesce(MarkDown2,0),
         MarkDown3=coalesce(MarkDown3,0),
         MarkDown4=coalesce(MarkDown4,0),
         MarkDown5=coalesce(MarkDown5,0))

ft2 = merge(ft,sales)
ft2
#Checking to see if MARKDOWNS has an effect on weekly sales
ftMD1<- ft2[,5:10]
ftMD1
#convert to time series
wmts<-ts(sales[,2],start=c(2010,6), end= c(2012,44), frequency = 52)
ftMD1.ts<- ts(ftMD1[,2:6],start=c(2010,6), end= c(2012,44), frequency = 52)
#Plot of time series contained dates as points on the graph. 
#Zooreg was used to transform the time series.
ftMD1.z<-zooreg(ftMD1[,2:6], start= as.Date('2010-02-05'), end =as.Date('2012-10-26'), deltat = 7)
wmtsz<-zooreg(sales[,2], start= as.Date('2010-02-05'), end =as.Date('2012-10-26'), deltat = 7)
#lets look at our data
plot(wmtsz)
#Exploring the ACF and PACF of the time series
par(mfrow= c(2,1))
#Look at these three graphs together 
plot(wmtsz)
plot(diff(wmtsz))
par(mfrow= c(2,1))
acf(diff(wmtsz))
pacf(diff(wmtsz))
#Observations:-
#We observed the time series and we take the difference to see if we can remove any trend.
#We can see that the difference detrended the data and made it more stationary.
#We took the ACF and PACF to find the lag of the data to identify the p and q for the SARIMA model

sarima(wmtsz,4,2,3,1,0,1,12,xreg=ftMD1.z)
#Splitting the training and testing dataset
train = 1:115
test = 116:143
fit <- Arima(wmtsz[train],c(4,2,3),seasonal=list(order=c(1,0,1),period=4),xreg=ftMD1.z[train,])
fit2 <- Arima(wmtsz[test],c(4,2,3),seasonal=list(order=c(1,0,1),period=4),xreg=ftMD1.z[test,],model=fit)
onestep <- fitted(fit2)
#Plotting the predicted sales from our fitted model and comparing against the test data
plot(wmtsz)
lines(time(wmtsz)[test],as.vector(onestep),col="red")
# The predictions are really close to the actual data
#MSPE
mean((wmtsz[test]-as.vector(onestep))^2)     
#MSPE = 57472953
########################################################################################################################################################################################################

#PROPHET MODELLING
train2 = train.data[train.data[,'Store']==20 & train.data[,'Dept']==92,3:4]
#Renaming the columns to use in the Prophet model
train2 = rename(train2, ds = Date)
train2 = rename(train2, y = Weekly_Sales)
train2
#Splitting the test and training datasets 80:20.
train = head(train2, 115)
test = tail(train2, 28)
###Prophet model fitted with changing points depending on how many times we saw the ACF plot switching directions (a rough estimate)
#ACF Plot
acf(train$y, lag.max = 120)
#Looking at the ACF plot, there are 4 instances where the trend changes significantly.
#Hence we define n.changepoints argument in the Prophet model = 4
#Prophet Model training
m = prophet(train, n.changepoints = 4)
future <- make_future_dataframe(m, periods = 28, freq = 'week')
forecast <- predict(m, future)
#Plotting Prophet Model components
prophet_plot_components(m, forecast)
#Plotting the forecasted sales
plot(m, forecast, xlab="Week", ylab="Sales") +
  add_changepoints_to_plot(m, threshold = 0.1, cp_color = "red", cp_linetype="dashed", trend = TRUE)
#The red line is the line of best fit and the blue line is the model's predictions.
#Calculating MSPE
MSPE = mean((tail(forecast$yhat,28)-test$y)^2)
MSPE # = 231133165











