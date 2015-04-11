# comment
# Extraction phase
outside = read.delim("~/MyRproject/Quadfiles/weatherreport11april2015.txt", sep = ":", header = FALSE)
dim(outside)
sensordata <- read.csv("~/MyRproject/Quadfiles/02Jul2014.csv", header=FALSE)
sensordata <- read.csv("~/MyRproject/Quadfiles/24may2014.csv", header=FALSE)
# xxx  <- as.POSIXct(strptime("November 22, 2013 at 06", "%B %d, %Y"))
# Transformation phase
Sys.setlocale("LC_TIME",  "en_US.UTF-8")
outside$POSIX <- as.POSIXct(strptime(outside$V1, "%B %d, %Y"))
#outside$POSIX <- as.Date(outside$V1, "%B %d, %Y")

Delta <- subset(sensordata, V1 == "Delta" & V3 >=-1 )
# "2014-05-25 00:03:07"
Delta$POSIX <- as.POSIXct(strptime(Delta$V2, "%Y-%m-%d %H:%M:%S"))

# Cleaning
valid <-!is.na(outside$V4)
outside$POSIX <- as.Date(outside$V1, "%B %d, %Y")
outside <- subset(outside, valid)

plot(outside$POSIX, outside$V4)
plot(outside$POSIX, outside$V4, type = "l")
hist(outside$V4)

plot(Delta$POSIX, Delta$V3, type = "l")
mean(Delta$V3)
daysinrange <- cut(Delta$POSIX, "day")
x <- by(Delta, daysinrange, mean)
x <- aggregate(Delta, list(daysinrange), mean)
plot(x$Group.1, x$V3)

