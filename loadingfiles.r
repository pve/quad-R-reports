# comment
# Extracting and processing temperature sensor data.

# Extraction phase
Sys.setlocale("LC_TIME",  "en_US.UTF-8")
outside = read.delim("~/MyRproject/Quadfiles/weatherreport11april2015.txt", sep = ":", header = FALSE)
sensordata <- read.csv("~/quad-R-reports/alldata.csv", header=FALSE)
names(outside) <- c("V1", "V2", "V3", "extT")

# Transformation phase

# example format weatherreport as.POSIXct(strptime("November 22, 2013 at 06", "%B %d, %Y"))
Sys.setlocale("LC_TIME",  "en_US.UTF-8")
outside$POSIX <- as.POSIXct(strptime(outside$V1, "%B %d, %Y"))
# we want full timestamps, so no as.Date(outside$V1, "%B %d, %Y")
# Dates without times are treated as being at midnight UTC. this is going to hurt us with summertime.

# sensordata date format "2014-05-25 00:03:07"
sensordata$POSIX <- as.POSIXct(strptime(sensordata$V2, "%Y-%m-%d %H:%M:%S"))
daysinrange <- cut(sensordata$POSIX, "day")
range(sensordata$POSIX)
#as.factor(sensordata$V1)

Delta <- subset(sensordata, V1 == "Delta" & V3 >=-2 & V3 <100)
Board <- subset(sensordata, V1 == "Board" & V3 >=-2 & V3 <100)
Heating <- subset(sensordata, V1 == "Heating" & V3 >=-2 & V3 <100)
Return <- subset(sensordata, V1 == "Return" & V3 >=-2 & V3 <100)

range(Delta$POSIX)
range(Board$POSIX)
range(Heating$POSIX)
range(Return$POSIX)
# we have more valid Return than Heating, more Heating than Delta measurements

# Cleaning out nonsense rows.
valid <-!is.na(outside$extT)

outside$POSIX <- as.POSIXct(strptime(outside$V1, "%B %d, %Y"))
outside <- subset(outside[c("extT", "POSIX")], valid)
range(outside$POSIX)

# consolidation / summaries

deltabyday <- cut(Delta$POSIX, "day") # creates factor out of date. 
d <- aggregate(Delta[c("V3")], list(deltabyday), mean)
d$POSIX <-as.POSIXct(d$Group.1) # messed up by summertime.
# Combination


# bd <- merge(Board, Delta, by="POSIX", all = TRUE)
# moet herhaald worden voor alle frames

# compute delta, issue: error in one wil bias result.
hr <-merge(Heating, Return, by = c("POSIX"))
hr$diff <- hr$V3.x - hr$V3.y
range(hr$diff)
# View(subset(hr, hr$diff < -5))
# View(subset(hr, hr$diff > 60))

#od <- merge(d, outside, by = "POSIX", all = TRUE)
od <- merge(d, outside, all = TRUE)

# wat we nu willen hebben is de verhouding tussen min(0, 20-buitentemp) en de hoeveel stook (mean diff).

# Presentation
plot(hr$POSIX, hr$diff, type = "s")

plot(od$POSIX, od$V3, type = "s", col="red") # Delta
plot(od$POSIX, od$extT, type = "s")

plot(outside$POSIX, outside$extT, type = "l")
hist(outside$extT)
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
