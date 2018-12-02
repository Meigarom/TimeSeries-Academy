# import libraries
library( forecast )

# ----------------------------------------------------------
# Input data
# ----------------------------------------------------------
# read csv file
ridership.data = read.csv( '../data/Amtrak_data.csv' )
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
# model training - Trend and Seasonality
train.lm.trend.season = tslm( train.ts ~ trend + I(trend^2) + season )
train.lm.trend.season.pred = forecast( train.lm.trend.season, h=nValid, level=0 )

# model training - ARIMA
train.res.arima = Arima( train.lm.trend.season$residuals, order=c( 1, 0, 0 ) )
train.res.arima.pred = forecast( train.res.arima, h=nValid )

# ----------------------------------------------------------
# Modelling & Predict 
# ----------------------------------------------------------
# Residuals ACF plot
#Acf( train.lm.trend.season.pred$residuals, lag.max=12, main='' )

# 
plot( train.res.arima.pred$residuals, ylim=c( -250, 250 ), ylab='Residuals', xlab='Time', bty='l', xaxt='n', xlim=c( 1991, 2006.25 ), main='' )
axis( 1, at=seq( 1991, 2006, 1 ), labels=format( seq( 1991, 2006, 1 ) ) )
lines( train.res.arima.pred$fitted, lwd=2, col='blue' )
