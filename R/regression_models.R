# import libraries
library( forecast )

# ----------------------------------------------------------
# Input data
# ----------------------------------------------------------
# read csv file
ridership.data = read.csv( '../data/Amtrak_data.csv' )
ridership.data.s = read.csv( '../data/Amtrak_data.csv' )
ridership.data.s$Season = lapply( ridership.data.s$Month, sub, pattern='-[0-9]+', replacement='' )

# convert to time series object
ridership.ts = ts( ridership.data$Ridership, start=c( 1991, 1 ), end=c( 2004, 3 ), freq=12 )
ridership.s.ts = ts( ridership.data.s$Ridership, start=c( 1991, 1 ), end=c( 2004, 3 ), freq=12 )

# ----------------------------------------------------------
# Prepare data
# ----------------------------------------------------------
# split dataset in training and testing
nValid = 36
nTrain = length( ridership.ts ) - nValid 
train.ts = window( ridership.ts, start=c( 1991, 1 ), end=c( 2001, 3 ) )
valid.ts = window( ridership.ts, start=c( 2001, 4 ), end=c( 2004, 3 ) )

train.s.ts = window( ridership.s.ts, start=c( 1991, 1 ), end=c( 2001, 3 ) )
valid.s.ts = window( ridership.s.ts, start=c( 2001, 4 ), end=c( 2004, 3 ) )

## plot data
#plot( ridership.ts, ylim=c(1300, 2600 ) )
#lines( train.ts, col='blue' )
#lines( valid.ts, col='red' )

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

# model training - Seasonality
train.lm.season = tslm( train.s.ts ~ season )
train.lm.season.pred = forecast( train.lm.season, h=nValid, level=0 )

# model training - Seasonality + Trend
train.lm.trend.season = tslm( train.s.ts ~ trend + I(trend^2) + season )
train.lm.trend.season.pred = forecast( train.lm.trend.season, h=nValid, level=0 )

# ----------------------------------------------------------
# Visual Results
# ----------------------------------------------------------
dev.new()
par( mfrow=c( 2, 2 ) )

# Linear Trend
plot( train.lm.linear.trend.pred, xlim=c( 1991, 2006 ), ylim=c( 1300, 2600 ), ylab='Ridership', 
      xlab='Time', bty='l', xaxt='n', main='Linear Trend', flty=2 )
axis( 1, at=seq( 1991, 2006, 1 ), labels=format( seq( 1991, 2006, 1 ) ) )
lines( train.lm.linear.trend.pred$fitted, lwd=2, col='blue' )
lines( valid.ts, col='red' )

# Exponential Trend 
plot( train.lm.expo.trend.pred, ylim=c( 1300, 2600 ), ylab='Ridership', xlab='Time', bty='l', xaxt='n', 
      xlim=c( 1991, 2006 ), main='Exponential Trend', flty=2 )
axis( 1, at=seq( 1991, 2006, 1 ), labels=format( seq( 1991, 2006, 1 ) ) )
lines( train.lm.expo.trend.pred$fitted, lwd=2, col='blue' )
lines( valid.ts, col='red' )

# Poly Trend
plot( train.lm.poly.trend.pred, ylim=c( 1300, 2600 ), ylab='Ridership', xlab='Time', bty='l', xaxt='n',
      xlim=c( 1991, 2006 ), main='Polynomial Trend', flty=2 )
axis( 1, at=seq( 1991, 2006, 1 ), labels=format( seq( 1991, 2006, 1 ) ) )
lines( train.lm.poly.trend.pred$fitted, lwd=2, col='blue' )
lines( valid.ts, col='red' )

# Seasonality
dev.new()
par( mfrow=c( 2, 2 ) )
plot( train.lm.season.pred, ylim=c( 1300, 2600 ), ylab='Ridership', xlab='Time', bty='l', xaxt='n',
      xlim=c( 1991, 2006 ), main='Seasonality', flty=2 )
axis( 1, at=seq( 1991, 2006, 1 ), labels=format( seq( 1991, 2006, 1 ) ) )
lines( train.lm.season.pred$fitted, lwd=2, col='blue' )
lines( valid.ts, col='red' )

plot( train.lm.season.pred$residuals, ylab='Residuals', xlab='Time', bty='l', xaxt='n', 
      xlim=c( 1991, 2006 ), main='Residuals' )
axis( 1, at=seq( 1991, 2006, 1 ), labels=format( seq( 1991, 2006, 1 ) ) )

# Seasonality + Trend
plot( train.lm.trend.season.pred, ylim=c( 1300, 2600 ), ylab='Ridership', xlab='Time', bty='l', xaxt='n',
      xlim=c( 1991, 2006 ), main='Seasonality + Trend', flty=2 )
axis( 1, at=seq( 1991, 2006, 1 ), labels=format( seq( 1991, 2006, 1 ) ) )
lines( train.lm.trend.season.pred$fitted, lwd=2, col='blue' )
lines( valid.ts, col='red' )

plot( train.lm.trend.season.pred$residuals, ylab='Residuals', xlab='Time', bty='l', xaxt='n', 
      xlim=c( 1991, 2006 ), main='Residuals' )
axis( 1, at=seq( 1991, 2006, 1 ), labels=format( seq( 1991, 2006, 1 ) ) )
