# Tutorial 5 Q1

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


# Tutorial 5 Q2

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