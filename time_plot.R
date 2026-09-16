# -----------------------------------------
# Tutorial 5 Q1
# -----------------------------------------

y <- c(1408, 768, 7040, 1984, 1344, 1664, 9600, 4480, 3200, 2304, 9344, 7040)
Y <- ts(y, frequency = 4, start = c(2008, 1))

component <- decompose(Y)

par(mfrow=c(2,2))

# Observed
plot(Y, 
     type = "o",      # Plots both points and lines overlapping
     pch = 16,        # Changes the point shape to a solid, bold circle
     cex = 1.5,       # Increases the size of the points (1 is default)
     lwd = 2,         # Thickens the connecting line (1 is default)
     col = "blue",    # Adds some color
     main = "Observed",
     ylab = "Unemployed people",
     xlab = "Time (Quarters)")

# Trend
plot(component$trend, 
     type = "o",      
     pch = 16,        
     cex = 1.5,       
     lwd = 2,         
     col = "blue",    
     main = "Trend (Centered Moving Average)",
     ylab = "Unemployed people",
     xlab = "Time (Quarters)")

# Seasonal
plot(component$seasonal, 
     type = "o",      
     pch = 16,        
     cex = 1.5,       
     lwd = 2,         
     col = "blue",    
     main = "Seasonal (Repeating 4-quarter pattern)",
     ylab = "Unemployed people",
     xlab = "Time (Quarters)")

# Random (Irregular)
plot(component$random, 
     type = "o",      
     pch = 16,        
     cex = 1.5,       
     lwd = 2,         
     col = "blue",    
     main = "Irregular Component (Residuals)",
     ylab = "Unemployed people",
     xlab = "Time (Quarters)")

par(mfrow=c(1,1))



# -----------------------------------------
# Tutorial 5 Q2
# -----------------------------------------

y <- c(306, 309, 310, 306, 312, 302, 310, 312, 305, 314, 308, 315, 317, 317, 313)
Y <- ts(y, frequency = 5, start=c(1,1))

component <- decompose(Y)

Y_diff <- diff(Y)
plot(Y_diff,
     type = "o",      # Plots both points and lines overlapping
     pch = 16,        # Solid, bold circle
     cex = 1.5,       # Increases point size
     lwd = 2,         # Thickens the connecting line
     col = "red",     # Using red to distinguish it from your original data
     main = "First Difference of Cash Sales",
     ylab = "Difference (RM)",
     xlab = "Time (Weeks)")

par(mfrow=c(2,2))

# Observed
plot(Y, 
     type = "o",      # Plots both points and lines overlapping
     pch = 16,        # Changes the point shape to a solid, bold circle
     cex = 1.5,       # Increases the size of the points (1 is default)
     lwd = 2,         # Thickens the connecting line (1 is default)
     col = "blue",    # Adds some color
     main = "Observed",
     ylab = "Cash sales (RM)",
     xlab = "Time (Weeks)")

# Trend
plot(component$trend, 
     type = "o",      
     pch = 16,        
     cex = 1.5,       
     lwd = 2,         
     col = "blue",    
     main = "Trend",
     ylab = "Cash sales (RM)",
     xlab = "Time (Weeks)")

# Seasonal
plot(component$seasonal, 
     type = "o",      
     pch = 16,        
     cex = 1.5,       
     lwd = 2,         
     col = "blue",    
     main = "Seasonal",
     ylab = "Cash sales (RM)",
     xlab = "Time (Weeks)")

# Random (Irregular)
plot(component$random, 
     type = "o",      
     pch = 16,        
     cex = 1.5,       
     lwd = 2,         
     col = "blue",    
     main = "Irregular (Residuals)",
     ylab = "Cash sales (RM)",
     xlab = "Time (Weeks)")

par(mfrow=c(1,1))



# -----------------------------------------
# Tutorial 5 Q4
# -----------------------------------------

y <- c(7.35, 6.65, 9.24, 7.21, 12.60, 21.70, 21.07, 9.80, 8.54, 11.83, 9.31, 16.17, 28.07, 27.16, 12.25, 10.85, 14.84, 11.41, 19.53, 33.53, 32.83) 
max_k <- length(y)-1

# 4a. Calculate ACF from 0 to n-1 (20)
autocorrelations <- acf(y, lag.max=max_k, plot=FALSE)
print(autocorrelations)

# 4b. plot the ACF
acf(y, 
    lag.max = max_k,
    main = "Correlogram of Daily Revenues (RM '000)", 
    xlab = "Lag k (Days)", 
    ylab = "Autocorrelation (rk)",
    lwd = 3,
    col = "blue"
)



# -----------------------------------------
# ACF/PACF for Tutorial 6 Q3
# -----------------------------------------

par(mfrow=c(3,2)) # Sets up a grid to view all 6 plots at once

# Q1: Unemployed People (Quarterly)
y1 <- ts(c(1408, 768, 7040, 1984, 1344, 1664, 9600, 4480, 3200, 2304, 9344, 7040), frequency = 4)
acf(y1, main="ACF - Q1 Unemployed")
pacf(y1, main="PACF - Q1 Unemployed")

# Q2: Cash Sales (Daily, 5 days/week)
y2 <- ts(c(306, 309, 310, 306, 312, 302, 310, 312, 305, 314, 308, 315, 317, 317, 313), frequency = 5)
acf(y2, main="ACF - Q2 Cash Sales")
pacf(y2, main="PACF - Q2 Cash Sales")

# Q3: Quarterly Export (Using the 14 known values)
y3 <- ts(c(2.7, 4.1, 3.9, 3.1, 3.8, 5.4, 5.3, 4.1, 4.9, 6.8, 6.6, 5.0, 5.9, 8.3), frequency = 4)
acf(y3, main="ACF - Q3 Export")
pacf(y3, main="PACF - Q3 Export")

par(mfrow=c(1,1)) # Resets the grid