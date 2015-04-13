# comment
Sys.setlocale("LC_TIME",  "en_US.UTF-8")
# Extraction phase
outside = read.delim("~/MyRproject/Quadfiles/weatherreport11april2015.txt", sep = ":", header = FALSE)
sensordata <- read.csv("~/quad-R-reports/alldata.csv", header=FALSE)

# Transformation phase

# example format weatherreport as.POSIXct(strptime("November 22, 2013 at 06", "%B %d, %Y"))
Sys.setlocale("LC_TIME",  "en_US.UTF-8")
outside$POSIX <- as.POSIXct(strptime(outside$V1, "%B %d, %Y"))
#outside$POSIX <- as.Date(outside$V1, "%B %d, %Y")

# sensordata date format "2014-05-25 00:03:07"
sensordata$POSIX <- as.POSIXct(strptime(sensordata$V2, "%Y-%m-%d %H:%M:%S"))
daysinrange <- cut(sensordata$POSIX, "day")
range(sensordata$POSIX)
#as.factor(sensordata$V1)

Delta <- subset(sensordata, V1 == "Delta" & V3 >=-1 )
Board <- subset(sensordata, V1 == "Board" & V3 >=-1 )
Heating <- subset(sensordata, V1 == "Heating" & V3 >=-1 )
Return <- subset(sensordata, V1 == "Return" & V3 >=-1 )

range(Delta$POSIX)
range(Board$POSIX)
range(Heating$POSIX)
range(Return$POSIX)
# we have more Return than Heating, more Heating than Delta measurements

# Cleaning
valid <-!is.na(outside$V4)
# names(outside$V4) <- "extT"

#outside$POSIX <- as.Date(outside$V1, "%B %d, %Y")
outside$POSIX <- as.POSIXct(strptime(outside$V1, "%B %d, %Y"))
outside <- subset(outside[c("V4", "POSIX")], valid)


# consolidation

deltabyday <- cut(Delta$POSIX, "day") # creates factor out of date. 
d <- aggregate(Delta[c("V3")], list(deltabyday), mean)
d$POSIX <-as.POSIXct(d$Group.1)
# Combination

# bd <- merge(Board, Delta, by="POSIX", all = TRUE)
# moet herhaald worden voor alle frames

#od <- merge(d, outside, by = "POSIX", all = TRUE)
od <- merge(d, outside, all = TRUE)
plotod <- od[c("POSIX","V3", "V4")]

plot(od$POSIX, od$V3, type = "s", col="red")
points(od$POSIX, od$V4, type = "s")
#stuck on comparison of two dates

# Presentation

plot(outside$POSIX, outside$V4)
plot(outside$POSIX, outside$V4, type = "l")
hist(outside$V4)
range(outside$POSIX)
range(outside$V4)

plot(Delta$POSIX, Delta$V3, type = "s", col="red")
points(Board$POSIX, Board$V3, type = "s")
mean(Delta$V3)


x <- by(Delta, daysinrange, mean)
d <- aggregate(Delta, list(daysinrange), mean)
plot(x$Group.1, x$V3, type = "l")
plot(d$Group.1, d$V3, type = "l")

daysinrange <- cut(Board$POSIX, "day")
b <- aggregate(Board, list(daysinrange), mean)
# b<- tapply(Board$V3, daysinrange, mean)

plot(b$Group.1, b$V3, type = "s")
