# ---------------------------------
# 1. Basic variables and data types
# ---------------------------------

# Assigning variables (<-)
x <- 2
y <- 4

# Data types
# 1. numeric, can use is.numeric() to check
# 2. integer (whole number without decimal, suffix with 'L'), is.integer()
# 3. character (string), is.character()
# 4. logical (TRUE/FALSE), is.logical()
# 5. factor (categorical), is.factor()
# we can also use class(x) to know what kind of object 'x' is

class(x)    #"numeric" 
class(5L)   #"integer"
class(TRUE) #"logical"

# What is factor?
# It is a special data type for representing categorical variables with ranking/level
# the level will follow the default order of items
gender <- c("male","female","non-binary","walmart grocery bag") 
gender_factor <- factor(gender)
gender_factor


# Create factor with custom level order
gender_factor2 <- factor(gender, levels = c("female", "walmart grocery bag", "male", "non-binary"))
print(gender_factor2)

# View levels
levels(gender_factor2)

# Add a new level and modify a value
levels(gender_factor2) <- c(levels(gender_factor2), "other")
gender_factor2[2] <- "other"
print(gender_factor2)

# ---------------------------------
# 2. print & cat
# ---------------------------------

print(x+y) # only prints one object/sentence
cat("The result of x + y is",x+y) # can mix text, numbers, etc.

# paste & paste0
# paste and paste0 build string objects out of pieces
# paste will add a space between two objects
# paste0 leave no space between objeccts

name <- "okok"
name_text <- paste("My name is",name)
name_text # will print out My name is okok

full_name <- paste0(name,"loh")
full_name # will print out okokloh (no space between okok and loh)


# ---------------------------------
# 3. Basic vectors (the c function)
# ---------------------------------

sales <- c(100,102,104,123,135,150)
sales [0:1] # prints out #0 100
length(sales) # how many elements in sales
mean(sales)
sd(sales)
sum(sales)


# ---------------------------------
# 4. Data frame
# ---------------------------------

df <- data.frame(
  month = c("Jan","Feb","Mar"),
  sales = c(120,135,128)
)
df$sales     # access the column sales
df[1,]       # get first row
df[,"sales"] # the	sales	column,	another	way
nrow(df) # get the row (how many data)
ncol(df) # get the column (how many features)


# ---------------------------------
# 5. Lists -- creating list and how to access
# ---------------------------------

result <- list(name = "model1", aic = 511.99, coefficients = c(1.15,-0.66,0.34))
result$aic # access by name
result[["coefficients"]] # access coefficients


# ---------------------------------
# 6. simple for loop and if else
# ---------------------------------

for (i in 1:5){
  print(i^2)
}

if (x>5){
  print("big")
} else {
  print("small")
}

# ---------------------------------
# 7. Time series forecasting,
# ---------------------------------
# install.packages(c("forecast","tseries","ggplot2")) use this to download packages
library(forecast)
library(tseries)
library(ggplot2)

sales <- c(112,	118, 132,	129, 121,	135, 148,	148, 136,	119, 104,	118, 
           120, 142, 153, 155, 142, 143, 161, 125, 147, 123, 134, 157,
           121,	135, 148,	148, 155, 142, 143, 161, 125, 136, 147, 170
          )
Y <- ts(
  sales,
  frequency = 12,   # 12 = monthly data, 4 for quarterly, 1 for yearly, 7 for daily (weekly seasonality)
  start = c(2020,1) # starts January 2020
)

Y # print Y	with	year/period	labels

frequency(Y)     #	confirms	the	frequency	you	set
start(Y); end(Y) #	first	and	last	time	points

# Plotting
plot(Y, ylab="Sales", xlab="Time", main="Monthly Sales")

# Transformations
Y_log <- log(Y)
plot(Y_log, ylab="log(Sales)", main="Log-Transformed Sales")

# Trend: Curve Fitting, Moving Averages, Differencing
t <- time(Y) # extract time index from a ts object
trend_model <- lm(Y~t) # least square trend (linear trend via regression)
summary(trend_model)
plot(Y)
abline(trend_model, col="red")


# Simple moving average
sma <- function(x, n) {
  filter(x, rep(1/n,n), sides=2) # order-n moving	average
}
Y_sma <- sma(Y, 5)
plot(Y, col="blue")
lines(Y_sma, col="red")


# Differencing
Y_diff1 <- diff(Y) # first difference
Y_diff2 <- diff(Y, differences = 2) # second difference
Y_seasdiff <- diff(Y, lag = 12) # seasonal difference at lag 12
plot(Y_diff1, main="First Difference")
plot(Y_diff2, main="Second Difference")


# Seasonality (Decomposition)
components	<-	decompose(Y)  #	additive	by	default
components	<-	decompose(Y,	type	=	"multiplicative")		#	or	multiplicative
plot(components) #	four	panels:	observed,	trend,	seasonal,	random									
components$trend #	extract	just	the	trend	estimate																	
components$seasonal #	extract	just	the	seasonal	indices									
components$random #	extract	just	the	residuals														

# Seasonal-Trend decomposition using Loess (STL decomposition)
Y_stl <- stl(Y, s.window = "periodic")
plot(Y_stl)


# ACF/PACF and Correlogram
par(mfrow = c(1,2))
acf(Y , main="ACF") # sample ACF	plot;	dashed lines	=	+-1.96/sqrt(n)
pacf(Y , main="Partial ACF")
par(mfrow=c(1,1))

# Box-Pierce and Ljung-Box
# how to decide the lag value?
# Non-seasonal data: lag = min(10, length(data)/5)
# seasonal data: lag = min(2 * m, length(data)/5) , where m = monthly seasonality
# As the decomposition shows seasonality in data, we use the second formula
h = min(2*12, (length(Y)/5))
print(h)
Box.test(Y, lag=h)
Box.test(Y, lag=h, type="Ljung-Box")

# ARIMA
set.seed(1)

fit <- auto.arima(Y , stepwise = FALSE, trace = TRUE)
fit
summary(fit)


# Grid-searching AIC
best_aic <- Inf
best_order <- NULL
for(p in 0:5){
  for(q in 0:5){
    fit_try <- tryCatch(arima(Y, order = c(p,1,q)), error=function(e) NULL)
    if(!is.null(fit_try) && fit_try$aic < best_aic){
      best_aic <- fit_try$aic
      best_order <- c(p,1,q)
    }
  }
}
best_order; best_aic

# Diagnostic check
checkresiduals(fit) # residual plot, ACF, Ljung Box

# Forecasting with a fitted ARIMA model
fc <- forecast(fit, h=12) # h = number of periods ahead to forecast
plot(fc) # forecast plot with widening prediction intervals
fc # point forecasts + 80% and 95% prediction interval bounds



# Lecture notes chapter 7 - Figure 7.6 Plot of data from InternetUser.txt
# Creating a vector to keep all the number of users over a 100-minute period
users <-  c(88, 84, 85, 85, 84, 85, 83, 85, 88, 89, 91, 99, 
            104, 112, 126, 138, 146, 151, 150, 148, 147, 149, 143, 132, 131, 
            139, 147, 150, 148, 145, 140, 134, 131, 131, 129, 126, 126, 132, 
            137, 140, 142, 150, 159, 167, 170, 171, 172, 172, 174, 175, 172, 
            172, 174, 174, 169, 165, 156, 142, 131, 121, 112, 104, 102, 99, 
            99, 95, 88, 84, 84, 87, 89, 88, 85, 86, 89, 91, 91, 94, 101, 110, 
            121, 135, 145, 149, 156, 165, 171, 175, 177, 182, 193, 204, 208, 
            210, 215, 222, 228, 226, 222, 220) 

# Turning vector into a time series object and plot it out
users <- ts(users)
plot(users, ylab="Number of users", main="Number of Internet Users")

# ACF and PACF
par(mfrow=c(1,2))  # makes it show two figures at once (1 row, 2 columns)
acf(users) ; pacf(users) 
par(mfrow=c(1,1))  # revert to default

frequency(users)
start(users); end(users)
