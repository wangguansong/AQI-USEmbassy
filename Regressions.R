########################################################################
# Regressions: cities, hours, months
# Butters (wang.guansong@hotmail.com)
# Created: 20151015


########################################################################
# Compute the deviation from the daily average of each hour.
# Only consider the days with full (24) hour records.
# Convert the deviation to percentage change.
DailyAQIrenamed <- DailyAQI[, 1:6]
names(DailyAQIrenamed)[6] <- "DailyAve"
HourDiff <- merge(USEmbassyAQI, DailyAQIrenamed)
HourDiff <- HourDiff[order(HourDiff$Site, HourDiff$Year,
                           HourDiff$Month, HourDiff$Day), ]
HourDiff$HourDiff <- HourDiff$Value - HourDiff$DailyAve

# The regression matrix.
# Add City dummy variables, Shenyang is left out.
aqimatrix <- HourDiff[HourDiff$Hours==24, ]
aqimatrix$HourDiffPct <- aqimatrix$HourDiff / aqimatrix$DailyAve
aqimatrix$Beijing <- aqimatrix$Site=="Beijing"
aqimatrix$Chengdu <- aqimatrix$Site=="Chengdu"
aqimatrix$Guangzhou <- aqimatrix$Site=="Guangzhou"
aqimatrix$Shanghai <- aqimatrix$Site=="Shanghai"

# Add Hour variables, 00 or 24 is left out.
hourmatrix <- data.frame(matrix(0, nrow(aqimatrix), 23))
for (i in 1:23) {
  hourmatrix[, i] <- aqimatrix$Hour==i
  colnames(hourmatrix)[i] <- paste("H", i, sep="")
}
# Add Month variables, Jan(1) is left out.
monthmatrix <- data.frame(matrix(0, nrow(aqimatrix), 11))
for (i in 2:12) {
  monthmatrix[, i] <- aqimatrix$Month==i
  colnames(monthmatrix)[i] <- paste("M", i, sep="")
}

aqimatrix <- cbind(aqimatrix, hourmatrix, monthmatrix)
remove(DailyAQIrenamed, hourmatrix, monthmatrix)

########################################################################
# Simple regressions on dummy variables.
regmodel <-
  as.formula(paste("HourDiffPct ~ ",
    paste(paste("H", 1:23, sep=""), collapse="+")))
reghourpct <- lm(regmodel, data=aqimatrix)

regmodel <-
  as.formula(paste("Value ~ ",
    paste("Beijing", "Chengdu", "Guangzhou", "Shanghai", sep="+")))
regcity <- lm(regmodel, data=aqimatrix)

regmodel <-
  as.formula(paste("Value ~ ",
    paste(paste("M", 2:12, sep=""), collapse="+")))
regmonth <- lm(regmodel, data=aqimatrix)

########################################################################
# Simple regressions on dummy variables, each city.

cities <- c("Beijing", "Shenyang", "Shanghai", "Chengdu", "Guangzhou")
reg.cities <- vector("list", length(cities))
for (i in 1:length(cities)) {
  site <- cities[i]
  siteid <- aqimatrix$Site==site
  regmodelhourpct <-
    as.formula(paste("HourDiffPct ~ ",
                     paste(paste("H", 1:23, sep=""), collapse="+")))
  regmodelmonth <-
    as.formula(paste("Value ~ ",
                     paste(paste("M", 2:12, sep=""), collapse="+")))
  reg.cities[[i]] <- list(Site=site,
                          RegHourPct=lm(regmodelhourpct,
                                        data=aqimatrix[siteid, ]),
                          RegMonth=lm(regmodelmonth,
                                      data=aqimatrix[siteid, ]))
}
remove(regmodel, i, cities, site, siteid, aqimatrix,
       regmodelhourpct, regmodelmonth)


