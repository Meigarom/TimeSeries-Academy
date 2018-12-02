# import libraries
library( lubridate )
library( forecast )

# read csv file
bike.df = read.csv( '../data/BikeSharingDaily.csv' )

# feature engineering
bike.df$Date = as.Date( bike.df$dteday, format="%Y-%m-%d" )
bike.df$Month = month( bike.df$Date, label=TRUE )
bike.df$DOW = wday( bike.df$Date, label=TRUE )
bike.df$WorkingDay = factor( bike.df$workingday, levels=c(0, 1), labels=c("Not_Working", "Working") )
bike.df$Weather = factor( bike.df$weathersit, levels=c(1, 2, 3), labels=c("Clear", "Mist", "Rain_Snow"))

# dummie variables
Month.dummies = model.matrix( ~ 0 + Month, data=bike.df )
DOW.dummies = model.matrix( ~ 0 + DOW, data=bike.df )
WorkingDay_Weather.dummies = model.matrix( ~ 0 + WorkingDay:Weather, data=bike.df )
colnames( Month.dummies) <- gsub("Month", "", colnames(Month.dummies))
colnames( DOW.dummies) <- gsub("DOW", "", colnames(DOW.dummies))
colnames( WorkingDay_Weather.dummies ) = gsub( "WorkingDay", "", colnames( WorkingDay_Weather.dummies))
colnames( WorkingDay_Weather.dummies ) = gsub( "Weather", "", colnames( WorkingDay_Weather.dummies ) )
colnames( WorkingDay_Weather.dummies ) = gsub( ":", "_", colnames( WorkingDay_Weather.dummies ) )

# split data into training and testing 
x = as.data.frame( cbind( Month.dummies[, -12], DOW.dummies[, -7], WorkingDay_Weather.dummies[, -6] ) )
y = bike.df$cnt

nTotal = length(y)
nValid = 90
nTrain = nTotal - nValid
xTrain = x[1:nTrain, ]
yTrain = y[1:nTrain]
xValid = x[(nTrain + 1):nTotal, ]
yValid = y[(nTrain + 1):nTotal]
yTrain.ts = ts(yTrain)
(formula = as.formula(paste("yTrain.ts", paste(c("trend", colnames(xTrain)), collapse="+"), sep="~")))
bike.tslm = tslm( formula, data=xTrain, lambda=1 )
bike.tslm.pred = forecast( bike.tslm, newdata=xValid )
plot( bike.tslm.pred, ylim=c(0, 9000), xlab="Days", ylab="Daily Bike Rentals" )
