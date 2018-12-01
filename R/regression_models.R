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

# model training - Additive Seasonality
train.lm.season.add = tslm( train.s.ts ~ season )
train.lm.season.add.pred = forecast( train.lm.season.add, h=nValid, level=0 )

# model training - Multiplicative Seasonality
train.lm.season.mul = tslm( train.s.ts ~ season, lambda=0 )
train.lm.season.mul.pred = forecast( train.lm.season.mul, h=nValid, level=0 )

# model training - Additive Seasonality + Polinomial Trend
train.lm.pol.trend.add.season = tslm( train.s.ts ~ trend + I(trend^2) + season, lambda=1 )
train.lm.pol.trend.add.season.pred = forecast( train.lm.pol.trend.add.season, h=nValid, level=0 )

# model training - Multiplicative Seasonality + Polinomial Trend
train.lm.pol.trend.mul.season = tslm( train.s.ts ~ trend + I(trend^2) + season, lambda=0 )
train.lm.pol.trend.mul.season.pred = forecast( train.lm.pol.trend.mul.season, h=nValid, level=0 )

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

# Additive Seasonality
dev.new()
par( mfrow=c( 2, 2 ) )
plot( train.lm.season.add.pred, ylim=c( 1300, 2600 ), ylab='Ridership', xlab='Time', bty='l', xaxt='n',
      xlim=c( 1991, 2006 ), main='Additive Seasonality', flty=2 )
axis( 1, at=seq( 1991, 2006, 1 ), labels=format( seq( 1991, 2006, 1 ) ) )
lines( train.lm.season.add.pred$fitted, lwd=2, col='blue' )
lines( valid.ts, col='red' )

# Multiplicative Seasonality
plot( train.lm.season.mul.pred, ylim=c( 1300, 2600 ), ylab='Ridership', xlab='Time', bty='l', xaxt='n',
      xlim=c( 1991, 2006 ), main='Multiplicative Seasonality', flty=2 )
axis( 1, at=seq( 1991, 2006, 1 ), labels=format( seq( 1991, 2006, 1 ) ) )
lines( train.lm.season.mul.pred$fitted, lwd=2, col='blue' )
lines( valid.ts, col='red' )

# Additive Seasonality + Polynomial Trend
plot( train.lm.pol.trend.add.season.pred, ylim=c( 1300, 2600 ), ylab='Ridership', xlab='Time', bty='l', 
      xaxt='n', xlim=c( 1991, 2006 ), main='Pol Seasonality + Add Trend', flty=2 )
axis( 1, at=seq( 1991, 2006, 1 ), labels=format( seq( 1991, 2006, 1 ) ) )
lines( train.lm.pol.trend.add.season.pred$fitted, lwd=2, col='blue' )
lines( valid.ts, col='red' )

# Mul Seasonality + Polynomial Trend
plot( train.lm.pol.trend.mul.season.pred, ylim=c( 1300, 2600 ), ylab='Ridership', xlab='Time', bty='l', 
      xaxt='n', xlim=c( 1991, 2006 ), main='Pol Seasonality + Add Trend', flty=2 )
axis( 1, at=seq( 1991, 2006, 1 ), labels=format( seq( 1991, 2006, 1 ) ) )
lines( train.lm.pol.trend.mul.season.pred$fitted, lwd=2, col='blue' )
lines( valid.ts, col='red' )
