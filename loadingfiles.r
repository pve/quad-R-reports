# comment
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
View(daysinrange)

Delta <- subset(sensordata, V1 == "Delta" & V3 >=-1 )
Board <- subset(sensordata, V1 == "Board" & V3 >=-1 )
View(Delta$POSIX)
View(Board$POSIX)



# Cleaning
valid <-!is.na(outside$V4)
outside$POSIX <- as.Date(outside$V1, "%B %d, %Y")
outside <- subset(outside, valid)

plot(outside$POSIX, outside$V4)
plot(outside$POSIX, outside$V4, type = "l")
hist(outside$V4)
range(outside$POSIX)
range(outside$V4)

plot(Delta$POSIX, Delta$V3, type = "s")
plot(Board$POSIX, Board$V3, type = "s")
mean(Delta$V3)

daysinrange <- cut(sensordata$POSIX, "day")

x <- by(Delta, daysinrange, mean)
d <- aggregate(Delta, list(daysinrange), mean)
plot(x$Group.1, x$V3, type = "l")

daysinrange <- cut(Board$POSIX, "day")
b <- aggregate(Board, list(daysinrange), mean)
plot(b$Group.1, b$V3, type = "s")
