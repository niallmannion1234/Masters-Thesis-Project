# Time Series Forecasting
# For Windows
setwd("D:\\Niall Mannion")
# For Mac
setwd("/Users/Niall Mannion")

install.packages("plyr")
library(plyr)
library(reshape2)
# Preparing data for flexdashboard datatable
headers = read.csv('timeseries.csv', skip = 1, header = F, nrows = 1, as.is = T)
ts = read.csv('timeseries.csv', skip = 2, header = F)
colnames(ts)= headers
tsdf <- ts[ ,c(1,2,8,15,22,29,36,43,50,57,64,71,78,85,92,99,106,112,119,126,132,139,146,152)]
tsdf <- melt(tsdf, id.vars=c("Names"))
tsdf <- spread(tsdf, Names, value)
colnames(tsdf)
newsat <- tsdf[ ,c(1, 4, 5, 6, 7, 18, 21, 22, 23, 24, 29, 32, 34, 35, 39, 43, 44, 45, 46, 
                   51, 54, 55, 56, 62, 63, 64, 65, 70, 71, 72, 73, 74, 75, 76, 81,
                   82, 90, 91, 92, 96, 108, 109, 110, 111, 112, 126, 127, 128, 134, 138, 139)]
timeseries <- newsat
colnames(timeseries)
timeseries <- t(timeseries)
timeseries <- as.data.frame(timeseries)
write.csv(timeseries, "datatable_timeseries.csv")

# For Windows
setwd("D:\\Niall Mannion\\Documents")
# For Mac
setwd("/Users/Niall Mannion/Documents")
write.csv(trains, file= 'training_data')
write.csv(trains, file= 'test_data')
trains <- read.csv("training_data.csv", header=T, skip = 1, na.strings=c(""), stringsAsFactors = T)
test <- read.csv("test_data.csv", header=T, skip = 1, na.strings=c(""), stringsAsFactors = T)

timeseries <- read.csv("timeseries.csv", header=T, skip = 1, na.strings=c(""), stringsAsFactors = T)
tsdf <- timeseries[ ,c(1,2,8,15,22,29,36,43,50,57,64,71,78,85,92,99,106,112,119,126,132,139,146,152)]
news <- melt(tsdf, id.vars=c("Names"))
newsa <- spread(news, Names, value)
newsat <- newsa[ ,c(7, 91, 112)]
newsat

mymts = ts(newsat, frequency = 1, start = c(1995, 1))
plot(mymts, main = "Timeseries of Vaccination Rates")

# Outliers
newsat <- newsa[ ,c(112)] 
untreated = ts(newsat, frequency = 1, start = c(1995, 1))
outliers_excess_ts <- tso(untreated, types = c("TC", "AO", "LS", "IO", "SLS"))
outliers_excess_ts
plot(outliers_excess_ts)
title(main = "Outlier Plot of Texas Timeseries", cex.main = 1.5, font.main = 1, line = 2.25)
title(ylab = "Vaccination Rate", line = 3, cex.lab = 1.2)
title(xlab = "Year", line = 4, cex.lab = 1.2)

outliers_excess_ts$outliers
(outliers_idx <- outliers_excess_ts$outliers$ind)
n <- length(untreated)
mo_tc <- outliers("TC", outliers_idx)
tc <- outliers.effects(mo_tc, n)
coefhat <- as.numeric(outliers_excess_ts$outliers["coefhat"])
tc_effect <- coefhat*tc
Outlier_Plot <- ts(tc_effect, frequency = frequency(untreated), start = start(untreated))
Treated <- untreated - Outlier_Plot
plot(cbind(Treated, untreated, Outlier_Plot),
     main = "Plot of Texas Vaccination Time Series Outlier Treatment", xlab = "Year")
Texas <- Treated

newsat <- newsa[ ,c(7)] 
untreated = ts(newsat, frequency = 1, start = c(1995, 1))
outliers_excess_ts <- tso(untreated, types = c("TC", "AO", "LS", "IO", "SLS"))
outliers_excess_ts
plot(outliers_excess_ts)
title(main = "Outlier Plot of Arkansas Timeseries", cex.main = 1.5, font.main = 1, line = 2.25)
title(ylab = "Vaccination Rate", line = 3, cex.lab = 1.2)
title(xlab = "Year", line = 4, cex.lab = 1.2)

outliers_excess_ts$outliers
(outliers_idx <- outliers_excess_ts$outliers$ind)
n <- length(untreated)
mo_tc <- outliers("TC", outliers_idx)
tc <- outliers.effects(mo_tc, n)
coefhat <- as.numeric(outliers_excess_ts$outliers["coefhat"])
tc_effect <- coefhat*tc
Outlier_Plot <- ts(tc_effect, frequency = frequency(untreated), start = start(untreated))
Treated <- untreated - Outlier_Plot
plot(cbind(Treated, untreated, Outlier_Plot),
     main = "Plot of Arkansas Vaccination Time Series Outlier Treatment", xlab = "Year")
Arkansas <- Treated

newsat <- newsa[ ,c(91)] 
untreated = ts(newsat, frequency = 1, start = c(1995, 1))
outliers_excess_ts <- tso(untreated, types = c("TC", "AO", "LS", "IO", "SLS"))
outliers_excess_ts
plot(outliers_excess_ts)
title(main = "Outlier Plot of Oklahoma Timeseries", cex.main = 1.5, font.main = 1, line = 2.25)
title(ylab = "Vaccination Rate", line = 3, cex.lab = 1.2)
title(xlab = "Year", line = 4, cex.lab = 1.2)

outliers_excess_ts$outliers
(outliers_idx <- outliers_excess_ts$outliers$ind)
n <- length(untreated)
mo_tc <- outliers("TC", outliers_idx)
tc <- outliers.effects(mo_tc, n)
coefhat <- as.numeric(outliers_excess_ts$outliers["coefhat"])
tc_effect <- coefhat*tc
Outlier_Plot <- ts(tc_effect, frequency = frequency(untreated), start = start(untreated))
Treated <- untreated - Outlier_Plot
plot(cbind(Treated, untreated, Outlier_Plot),
     main = "Plot of Oklahoma Vaccination Time Series Outlier Treatment", xlab = "Year")
Oklahoma <- Treated

adjusted_ts <- cbind(Oklahoma, Texas, Arkansas)
apply(adjusted_ts, 2, adfTest, lags = 0, type = "c", 
      title = "ADF Testfor Vaccination Timeseries Data")

ts_window <- window(adjusted_ts, start = 1995, end = c(2012))
texas_ts <- adjusted_ts[,2]
stnry = diffM(ts_window)
var.a <- vars::VAR(stnry, lag.max = 10, ic = "AIC", type = "none") 
fcast = predict(var.a, n.ahead = 5)
Texas_fcast = fcast$fcst[2]
x = Texas_fcast$Texas[,1]
tail(ts_window)
x = cumsum(x) + 78.46

meanf_ts <- meanf(texas_ts, h = 5)
rwf_ts <- rwf(texas_ts, h = 5)
snaive_ts <- snaive(texas_ts, h = 5)

target_ts <- window(texas_ts, start = 2013)
target_ts <- as.numeric(target_ts)

forecast_ML <- x
accuracy(meanf_ts, target_ts)
accuracy(rwf_ts, target_ts)
accuracy(snaive_ts, target_ts)
accuracy(forecast_ML, target_ts)
meanf_ts
rwf_ts
snaive_ts
forecast_ML

stnry <- diffM(adjusted_ts)
autoplot(ts(stnry, start = c(1990, 1), frequency = 1), lwd =  1.6) +
  ggtitle("Time Series Plot of the stationary Texas Vaccination Time-Series") +
  ylab("Vaccination Rate") + xlab("Year")

var.a <- vars::VAR(stnry, lag.max = 10, ic = "AIC", type = "none")
fcast = predict(var.a, n.ahead = 5)
par(mar = c(2.5, 2.5, 2.5, 2.5))
plot(fcast)

Texas_fcast = fcast$fcst[2]
x = Texas_fcast$Texas[,1]
tail(adjusted_ts)
x = cumsum(x) + 74.1
par(mar = c(4, 4, 1, 4))
Forecast_Tex = ts(c(x), start= c(2017, 1), frequency = 1)
plot(Forecast_Tex, main = "Forecasted Vaccination Rates for Texas", xlab = "Year", ylab = "Vaccination Rates")
Texas_Forecast = ts(c(adjusted_ts[,2], x), start = c(1995, 1), frequency = 1)
Texas_df <- as.data.frame(Texas_Forecast[1:28])
colnames(Texas_df) <- c("Texas")

Arkansas_fcast = fcast$fcst[3] 
Arkansas_fcast
y = Arkansas_fcast$Arkansas[,1]
tail(adjusted_ts)
y = cumsum(y) + 73.89
par(mar = c(2.5,2.5,1,2.5))
Forecast_Ark = ts(c(y), start= c(2017, 1), frequency = 1)
plot(Forecast_Ark, main = "Forecasted Vaccination Rates for Arkansas", xlab = "Year", ylab = "Vaccination Rates")
Arkansas_Forecast =ts(c(adjusted_ts[,3], y), start = c(1995,1), frequency = 1)
Arkansas_df <- as.data.frame(Arkansas_Forecast[1:28]) 
colnames(Arkansas_df) <- c("y")

Oklahoma_fcast = fcast$fcst[1] 
Oklahoma_fcast
z = Oklahoma_fcast$Oklahoma[,1]
tail(adjusted_ts)
z = cumsum(z) + 71.4
par(mar = c(2.5,2.5,1,2.5))
Forecast_Okl = ts(c(z), start= c(2017, 1), frequency = 1)
plot(Forecast_Okl, main = "Forecasted Vaccination Rates for Oklahoma", xlab = "Year", ylab = "Vaccination Rates")
Oklahoma_Forecast =ts(c(adjusted_ts[,1], z), start = c(1995,1), frequency = 1)
Oklahoma_df <- as.data.frame(Oklahoma_Forecast[1:28]) 
colnames(Oklahoma_df) <- c("z")

combined_ts <- cbind(Texas_Forecast, Arkansas_Forecast, Oklahoma_Forecast)
combined_ts
combined_df <- as.data.frame(combined_ts)
combined_df$Year <- seq(1995, 2022, by = 1)
reshaped_ts <- melt(combined_df, id="Year")
forecast_theme <- theme(axis.title.x = element_text(size = 15),
                        axis.text.x = element_text(size = 15),
                        axis.title.y = element_text(size= 15),
                        plot.title = element_text(size = 17, hjust = 0.5))
ggplot(data = reshaped_ts, aes(x = Year, y = value, colour = variable)) + ylab("Immunization Rate") +
  ggtitle("Forecasted Immunization Rates for US States") + geom_line(size = 2.1) + forecast_theme