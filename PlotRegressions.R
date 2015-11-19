########################################################################
# Plot the regression results
# Butters (wang.guansong@hotmail.com)
# Created: 20151015

########################################################################
# Regression on cities

png(filename=paste("Plots/cities_reg_city.png", sep=""),
    width=1200, height=900, res=144)
tags <- c("Shenyang", "Beijing", "Chengdu", "Guangzhou", "Shanghai")
barheight <- with(regcity,
                  coefficients + c(0, rep(coefficients[1], 4)))
barplot(barheight, names.arg=tags,
        main="The AQI of Five Cities",
        xlab="City", ylab="Ave. AQI", ylim=c(0,100))
dev.off()

########################################################################
# Regression on hours
png(filename=paste("Plots/cities_reg_hour.png", sep=""),
    width=1200, height=900, res=144)
tags <- 0:23
barheight <- with(reghourpct,
                  coefficients + c(0, rep(coefficients[1], 23)))
barplot(barheight, names.arg=tags,
        main="The AQI of 24 Hours",
        xlab="Time", ylab="Ave. AQI Pct Deviation")
dev.off()

########################################################################
# Regression on months
png(filename=paste("Plots/cities_reg_month.png", sep=""),
    width=1200, height=900, res=144)
tags <- 1:12
barheight <- with(regmonth,
                  coefficients + c(0, rep(coefficients[1], 11)))
barplot(barheight, names.arg=tags,
        main="The AQI of 12 Months",
        xlab="Month", ylab="Ave. AQI")
dev.off()

########################################################################
# Regression on hours and months, each city
for (i in 1:length(reg.cities)) {
  png(filename=paste("Plots/", reg.cities[[i]]$Site, "_reg_hour.png",
                     sep=""),
      width=1200, height=900, res=144)
  tags <- 0:23
  barheight <- with(reg.cities[[i]]$RegHour,
                    coefficients + c(0, rep(coefficients[1], 23)))
  barplot(barheight, names.arg=tags,
          main=paste("The AQI of 24 Hours of", reg.cities[[i]]$Site),
          xlab="Time", ylab="Ave. AQI Pct Deviation")
  dev.off()
  png(filename=paste("Plots/", reg.cities[[i]]$Site, "_reg_month.png",
                     sep=""),
      width=1200, height=900, res=144)
  tags <- 1:12
  barheight <- with(reg.cities[[i]]$RegMonth,
                    coefficients + c(0, rep(coefficients[1], 11)))
  barplot(barheight, names.arg=tags,
          main=paste("The AQI of 12 Months of", reg.cities[[i]]$Site),
          xlab="Month", ylab="Ave. AQI")
  dev.off()

}

remove(tags, i, barheight)
