########################################################################
# Plot time series of daily/weekly/monthly average PM2.5
# Butters (wang.guansong@hotmail.com)
# Created: 20151015

########################################################################
# Plot time series of each city
cities <- c("Beijing", "Shenyang", "Shanghai", "Chengdu", "Guangzhou")

for (site in cities) {
  png(filename=paste("Plots/", site, "_Daily.png", sep=""),
      width=1200, height=900, res=144)
  with(DailyAQI,
       plot(Date[Site==site], Value[Site==site], type="l",
            main=paste("Daily Average AQI of", site),
            xlab="Date", ylab="AQI", log="y"))
  dev.off()
  
  png(filename=paste("Plots/", site, "_Weekly.png", sep=""),
      width=1200, height=900, res=144)
  with(WeeklyAQI,
       plot(Date[Site==site], Value[Site==site], type="l",
            main=paste("Weekly Average AQI of", site),
            xlab="Date", ylab="AQI", log="y"))
  dev.off()
  
  png(filename=paste("Plots/", site, "_Monthly.png", sep=""),
      width=1200, height=900, res=144)
  with(MonthlyAQI,
       plot(Date[Site==site], Value[Site==site], type="l",
            main=paste("Monthly Average AQI of", site),
            xlab="Date", ylab="AQI", log="y"))
  dev.off()
}

########################################################################
# Plot time series of five cities in one xy-space
# Only the recent two years are considered (2013-2014)
twoyears <- DailyAQI$Year %in% c(2013, 2014)

png(filename=paste("Plots/cities_Daily.png", sep=""),
    width=1200, height=900)
plot(range(DailyAQI$Date[twoyears]),
     range(DailyAQI$Value[twoyears], na.rm=T),
     main="Daily Average AQI of Five Cities",
     xlab="Date", ylab="AQI", log="y", type="n", cex=2, cex.lab=1.5)
linecolors <- rainbow(5)
for (i in 1:length(cities)) {
  site <- cities[i]
  with(DailyAQI,
       lines(Date[twoyears & Site==site], Value[twoyears & Site==site],
             type="l", lty=i, lwd=2, col=linecolors[i]))
}
legend("bottomleft", legend=cities,
       lty=1:5, lwd=2, col=linecolors, cex=2)
dev.off()

twoyears <- WeeklyAQI$Year %in% c(2013, 2014)
png(filename=paste("Plots/cities_Weekly.png", sep=""),
    width=1200, height=900)
plot(range(WeeklyAQI$Date[twoyears]),
     range(WeeklyAQI$Value[twoyears], na.rm=T),
     main="Weekly Average AQI of Five Cities",
     xlab="Date", ylab="AQI", log="y", type="n", cex=2, cex.lab=1.5)
linecolors <- rainbow(5)
for (i in 1:length(cities)) {
  site <- cities[i]
  with(WeeklyAQI,
       lines(Date[twoyears & Site==site], Value[twoyears & Site==site],
             type="l", lty=i, lwd=2, col=linecolors[i]))
}
legend("bottomleft", legend=cities,
       lty=1:5, lwd=2, col=linecolors, cex=2)
dev.off()

twoyears <- MonthlyAQI$Year %in% c(2013, 2014)
png(filename=paste("Plots/cities_Monthly.png", sep=""),
    width=1200, height=900)
plot(range(MonthlyAQI$Date[twoyears]),
     range(MonthlyAQI$Value[twoyears], na.rm=T),
     main="Monthly Average AQI of Five Cities",
     xlab="Date", ylab="AQI", log="y", type="n", cex=2, cex.lab=1.5)
linecolors <- rainbow(5)
for (i in 1:length(cities)) {
  site <- cities[i]
  with(MonthlyAQI,
       lines(Date[twoyears & Site==site], Value[twoyears & Site==site],
             type="l", lty=i, lwd=2, col=linecolors[i]))
}
legend("bottomleft", legend=cities,
       lty=1:5, lwd=2, col=linecolors, cex=2)
dev.off()


rm(i, site, cities, twoyears, linecolors)
