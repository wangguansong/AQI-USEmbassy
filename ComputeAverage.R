########################################################################
# Compute daily/weekly/monthly average PM2.5
# Butters (wang.guansong@hotmail.com)
# Created: 20150407

########################################################################
# Compute daily average

# Need to load dataframe "USEmbassyAQI"
if (!exist("USEmbassyAQI", mode="list")) {
  load("../PublicData/China/AQI/USEmbassyAQI.RData")
}

# The default value of invalid records is 999, set it to NA.
USEmbassyAQI$Value[ !USEmbassyAQI$Valid ] <- NA

valid.hours <- aggregate(USEmbassyAQI$Valid, USEmbassyAQI[, 1:4], sum)
daily.value <- aggregate(USEmbassyAQI$Value, USEmbassyAQI[, 1:4],
                         mean, na.rm=T)

colnames(valid.hours)[5] <- "Hours"
colnames(daily.value)[5] <- "Value"
DailyAQI <- merge(valid.hours, daily.value)
DailyAQI <- DailyAQI[order(DailyAQI$Site, DailyAQI$Year,
                           DailyAQI$Month, DailyAQI$Day),]
DailyAQI$Value[is.nan(DailyAQI$Value)] <- NA

rm(valid.hours, daily.value)

# Tag week of the year, and valid records
# Convert to POSIT Dates
DailyAQI$Date <- strptime(paste(DailyAQI$Year, DailyAQI$Month,
                                DailyAQI$Day),
                          format="%Y%m%d")
DailyAQI$Week <- format(DailyAQI$Date, format="%W")
DailyAQI$Valid <- ! is.na(DailyAQI$Value)

# Deal with the first days of a year
beginning <- DailyAQI$Week == "00"
DailyAQI$Week[!beginning] <- paste(DailyAQI$Year[!beginning],
                                   DailyAQI$Week[!beginning])
lastyear <- DailyAQI$Year[beginning] - 1    # for writing convenience
lastweek <- format(as.Date(paste(lastyear, 12, 31), format="%Y %m %d"),
                   format="%W")
DailyAQI$Week[beginning] <- paste(lastyear, lastweek)
rm(lastyear, lastweek, beginning)

########################################################################
# Compute weekly average

valid.days <- aggregate(DailyAQI$Valid,
                        DailyAQI[, c("Site", "Week")],
                        sum)
weekly.value <- aggregate(DailyAQI$Value,
                          DailyAQI[, c("Site", "Week")],
                          mean, na.rm=T)
colnames(valid.days)[3] <- "Days"
colnames(weekly.value)[3] <- "Value"
WeeklyAQI <- merge(valid.days, weekly.value)
WeeklyAQI$Year <- substring(WeeklyAQI$Week, 1, 4)
WeeklyAQI$Week <- substring(WeeklyAQI$Week, 6, 7)
WeeklyAQI <- WeeklyAQI[, c("Site", "Year", "Week", "Days", "Value")]
WeeklyAQI <- WeeklyAQI[order(WeeklyAQI$Site, WeeklyAQI$Year,
                             WeeklyAQI$Week), ]
WeeklyAQI$Date <- as.Date(paste(WeeklyAQI$Year, WeeklyAQI$Week, 1),
                          format="%Y %W %u")
rm(valid.days, weekly.value)

########################################################################
# Compute monthly average
valid.days <- aggregate(DailyAQI$Valid,
                        DailyAQI[, c("Site", "Year", "Month")],
                        sum)
monthly.value <- aggregate(DailyAQI$Value,
                           DailyAQI[, c("Site", "Year", "Month")],
                           mean, na.rm=T)
colnames(valid.days)[4] <- "Days"
colnames(monthly.value)[4] <- "Value"
MonthlyAQI <- merge(valid.days, monthly.value)
MonthlyAQI <- MonthlyAQI[order(MonthlyAQI$Site, MonthlyAQI$Year,
                               MonthlyAQI$Month), ]
MonthlyAQI$Date <- as.Date(paste(MonthlyAQI$Year, MonthlyAQI$Month, 1),
                           format="%Y %m %d")
rm(valid.days, monthly.value)

