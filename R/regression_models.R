# import libraries
library( forecast )

# ----------------------------------------------------------
# Input data
# ----------------------------------------------------------
# read csv file
ridership.data = read.csv( '../data/Amtrak_data.csv' )

# convert to time series object
ridership.ts = ts( ridership.data$Ridership, start=c( 1991, 1 ), end=c( 2004, 3 ), freq=12 )

# ----------------------------------------------------------
# Prepare data
# ----------------------------------------------------------
# split dataset in training and testing
nValid = 36
nTrain = length( ridership.ts ) - nValid 
train.ts = window( ridership.ts, start=c( 1991, 1 ), end=c( 2001, 3 ) )
valid.ts = window( ridership.ts, start=c( 2001, 4 ), end=c( 2004, 3 ) )

# ----------------------------------------------------------
# Modelling & Predict
# ----------------------------------------------------------
# model training - Linear Trend 
train.lm.linear.trend = tslm( train.ts ~ trend, lambda=1 )
train.lm.linear.trend.pred = forecast( train.lm.linear.trend, h=nValid, level=0 )

# model training - Exponential Trend
train.lm.expo.trend = tslm( train.ts ~ trend, lambda=0 )
train.lm.expo.trend.pred = forecast( train.lm.expo.trend, h=nValid, level=0 )

# model training - Polinomial Trend
train.lm.poly.trend = tslm( train.ts ~ trend + I( trend^2 ) )
train.lm.poly.trend.pred = forecast( train.lm.poly.trend, h=nValid, level=0 )

# ----------------------------------------------------------
# Visual Results
# ----------------------------------------------------------
par( mfrow=c(1,2) )
plot( train.lm.linear.trend.pred, xlim=c( 1991, 2006.25 ), ylim=c( 1300, 2600 ), ylab='Ridership', 
      xlab='Time', bty='l', xaxt='n', main='Linear Trend', flty=2 )
axis( 1, at=seq( 1991, 2006, 1 ), labels=format( seq( 1991, 2006, 1 ) ) )
lines( train.lm.pred$fitted, lwd=2, col='blue' )
lines( valid.ts, col='red' )

# Linear x Explonential Trend Plot
plot( train.lm.expo.trend.pred, ylim=c( 1300, 2600 ), ylab='Ridership', xlab='Time', bty='l', xaxt='n', 
      xlim=c( 1991, 2006.25 ), main='Linear, Exponential and Polynomial Trend', flty=2 )
axis( 1, at=seq( 1991, 2006, 1 ), labels=format( seq( 1991, 2006, 1 ) ) )
lines( train.lm.expo.trend.pred$fitted, lwd=2, col='blue' )
lines( train.lm.linear.trend.pred$fitted, lwd=2, col='blue' )
lines( train.lm.poly.trend.pred$fitted, lwd=2, col='blue', lty=3 )
lines( valid.ts, col='red' )
