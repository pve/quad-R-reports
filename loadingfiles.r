# Extracting and processing temperature sensor data.
#more hypotheses for modelling: sun inflow and body heat give some basic delta
#model by day of week, and light

# Ingestion phase; leads to dataframes
Sys.setlocale("LC_TIME",  "en_US.UTF-8")
outside = read.delim("~/MyRproject/Quadfiles/weatherreport11april2015.txt", sep = ":", header = FALSE)
# TODO use KNMI data. 
sensordata <- read.csv("~/quad-R-reports/alldata.csv", header=FALSE)
names(outside) <- c("V1", "V2", "reading", "extT")
names(sensordata) <- c("source", "timestamp", "reading")

# Transformation and cleaning phase

# Weather data
# example format weatherreport as.POSIXct(strptime("November 22, 2013 at 06", "%B %d, %Y"))
Sys.setlocale("LC_TIME",  "en_US.UTF-8")
outside$POSIX <- as.POSIXct(strptime(outside$V1, "%B %d, %Y"))
# we want full timestamps, so no as.Date(outside$V1, "%B %d, %Y")
# Cleaning out nonsense rows.
valid <-!is.na(outside$extT)
outside$POSIX <- as.POSIXct(strptime(outside$V1, "%B %d, %Y"))
outside <- subset(outside[c("extT", "POSIX")], valid)
range(outside$POSIX)

# Sensordata
# sensordata date format "2014-05-25 00:03:07"
# as.POSIXct(strptime("2014-05-25 00:03:07", "%Y-%m-%d %H:%M:%S"))
sensordata$POSIX <- as.POSIXct(strptime(sensordata$timestamp, "%Y-%m-%d %H:%M:%S"))

# faulty sensor data to get rid of
#which days did we not trust Heating or Return?
badsensordata <- subset(sensordata, (reading <1 | reading > 95) & (source=="Heating" | source=="Return"))
gooddeltadata <- subset(sensordata, (reading >0 & reading <95) & (source=="Heating" | source=="Return"))
deltabyday <- cut(badsensordata$POSIX, "DSTday") # creates factor out of date, taking care of summertime 
badsensordays <-aggregate(badsensordata[c("reading")], list(deltabyday), length)
#head(subset(badsensordata, (as.Date(badsensordata$POSIX) == "2014-02-15") ))
deltabyday <- cut(gooddeltadata$POSIX, "DSTday") # creates factor out of date, taking care of summertime 
gooddeltadays <- aggregate(gooddeltadata[c("reading")], list(deltabyday), length)
names(gooddeltadays) <- c("POSIX", "nrreading")
# Now we need something like 'merge' with a validsensordates

Board <- subset(sensordata, source == "Board" & reading >=-2 & reading <100)
Heating <- subset(sensordata, source == "Heating" & reading >=-2 & reading <100)
Return <- subset(sensordata, source == "Return" & reading >=-2 & reading <100)
     
# TODO fix systematic bias on sensors Heating and Return; Return seems to be too high.


# consolidation / summaries

# compute delta i.e. differential between influx temp and outflux temp, issue: error in one wil bias result.
hr <-merge(Heating, Return, by = c("POSIX")) #ignoring points where one reading is missing
hr$diff <- hr$reading.x - hr$reading.y -0.5
range(hr$diff)
#View(subset(hr, (hr$diff < -5) & (as.Date(hr$POSIX) == "2014-01-10")))
#View(subset(hr, (as.Date(hr$POSIX) == "2014-01-10")))
#View(subset(hr, (as.Date(hr$POSIX) == "2014-05-13")))
# View(subset(hr, hr$diff > 60))

hr <- subset(hr, hr$diff > 0 & hr$diff < 60 )
#filter out diff < -5, > 60

deltabyday <- cut(hr$POSIX, "DSTday") # creates factor out of date. 
d <- aggregate(hr[c("diff")], list(deltabyday), mean)
d$POSIX <-as.POSIXct(d$Group.1)
gooddeltadays$POSIX <- as.POSIXct(gooddeltadays$POSIX)

temp <- merge(d, outside, by = "POSIX")
od <- merge(temp, gooddeltadays, by = "POSIX")

#od <- merge(hr, outside, all = TRUE)
range(od$POSIX)
#od$leakage <- min(0, 20 - od$extT)/od$diff
# wat we nu willen hebben is de verhouding tussen min(0, 20-buitentemp) en de hoeveel stook (mean diff).
od$heatloss <- od$diff/ (19 - od$extT)
#plot(od$POSIX, min(0, 20 - od$extT)/od$diff, type = "s")

# Presentation

plot(od$POSIX, od$heatloss, type="s")
points(od$POSIX, od$extT/20, type="s", col ="red")

plot(hr$POSIX, hr$diff, type = "s")

plot(od$POSIX, od$diff, type = "s", col="red") # Delta
plot(od$POSIX, od$extT, type = "s")

plot(outside$POSIX, outside$extT, type = "l")
hist(outside$extT)
range(outside$POSIX)
range(outside$V4)

plot(Delta$POSIX, Delta$reading, type = "s", col="red")
points(Board$POSIX, Board$reading, type = "s")
mean(Delta$reading)


x <- by(Delta, daysinrange, mean)
d <- aggregate(Delta, list(daysinrange), mean)
plot(x$Group.1, x$reading, type = "l")
plot(d$Group.1, d$reading, type = "l")

daysinrange <- cut(Board$POSIX, "day")
b <- aggregate(Board, list(daysinrange), mean)
# b<- tapply(Board$reading, daysinrange, mean)

plot(b$Group.1, b$reading, type = "s")
