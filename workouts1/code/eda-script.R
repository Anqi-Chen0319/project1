# Data importing in R
library(readr)
library(maps)
library(ggplot2)
library(lubridate)

# using read_csv in readr to import file ibtracs-2010-2015.csv
dat <- read_csv("../data/ibtracs-2010-2015.csv",
         col_names=c("serial_num","season","num","basin","sub_basin","name","iso_time","nature","latitude","longitude","wind","press"),
         col_types=list(col_character(), col_integer(), col_character(), col_factor(), col_character(), col_character(), col_character(), col_character(), col_double(), col_double(), col_double(), col_double()),
         na=c("-999","-1.0","0.0"),
         skip = 1
         )

  

# summary data and output the data in data folder
sink("../output/data-summary.txt")
summary(dat)
sink()


# data visulaization
## plot1
png("../images/map-all-storms.png")
map(interior = TRUE,bg="white")
points(dat[, c("longitude", "latitude")], col = "blue")
abline(h = 0, lty = 2, col = "red")
title(main="Storms")
dev.off()

pdf("../images/map-all-storms.pdf")
map(interior = TRUE,bg="white")
points(dat[, c("longitude", "latitude")], col = "blue")
abline(h = 0, lty = 2, col = "red")
title(main="Storms")
dev.off()

## plot2
dat1 <- dat[which(dat$basin=="EP"|dat$basin=="NA"),]
a <- dat1[complete.cases(dat1[,9:10]),]
world_map <- map_data("world")

png("../images/map-ep-na-storms-by-year.png")
ggplot() +
  geom_polygon(world_map, mapping = aes(x = long, y = lat, group = group), fill = "lightyellow") +
  geom_point(data = dat1, mapping = aes(x = longitude, y = latitude, color = wind, shape = nature), size = 0.6) + 
  facet_wrap( ~ season)
dev.off()

pdf("../images/map-ep-na-storms-by-year.pdf")
ggplot() +
  geom_polygon(world_map, mapping = aes(x = long, y = lat, group = group), fill = "lightyellow") +
  geom_point(data = dat1, mapping = aes(x = longitude, y = latitude, color = wind, shape = nature), size = 0.6) + 
  facet_wrap( ~ season)
dev.off()

##plot3
dat$date = as.Date(dat$iso_time, format = "%Y/%m/%d")
month(dat$iso_time)
dat2 <- mutate(dat1, month_num = month(ymd_hms(dat2$iso_time)))
png("../images/map-ep-na-storms-by-month.png")
ggplot() +
  geom_polygon(world_map, mapping = aes(x = long, y = lat, group = group), fill = "lightyellow") +
  geom_point(data = dat2, mapping = aes(x = longitude, y = latitude, color = wind, shape = nature), size = 0.6,na.rm = TRUE) + 
  facet_wrap( ~ month_num)
dev.off()

pdf("../images/map-ep-na-storms-by-month.pdf")
ggplot() +
  geom_polygon(world_map, mapping = aes(x = long, y = lat, group = group), fill = "lightyellow") +
  geom_point(data = dat2, mapping = aes(x = longitude, y = latitude, color = wind, shape = nature), size = 0.6,na.rm = TRUE) + 
  facet_wrap( ~ month_num)
dev.off()

