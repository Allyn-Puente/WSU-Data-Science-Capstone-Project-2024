# ========== SET UP DATA ==========
library("tidyr") # for data manipulation
library("ggplot2") # for plotting
ap = read.csv("air pollution max 2023.csv")
head(ap)
dim(ap)

# camden: no ozone data in winter 23
# only measuring OZ and PM10

# ========== SEPARATING BY TYPE OF POLLUTION ==========
aq = ap[-c(1,2),]
names(aq) = ap[2,]
gen_names = c("Date","Liverpool", "Bringelly", "Bargo", "Oakdale", "Campbelltown West", "Camden")

OZONE_max = aq[,c(1,which(grepl("OZONE",names(aq),fixed=TRUE )== 'TRUE'))]
names(OZONE_max) = gen_names
head(OZONE_max)

PM10_max = aq[,c(1,which(grepl("PM10",names(aq),fixed=TRUE )== 'TRUE'))]
names(PM10_max) = gen_names
head(PM10_max)

# function to fix up data classes
change_classes = function(table_name){
  table_name[,1] = as.Date(table_name[,1], "%d/%m/%Y")
  for(x in 2:ncol(table_name)) {
    table_name[,x] = as.numeric(table_name[,x])
  }
  table_name$Avg = rowMeans(table_name[,-1], na.rm = TRUE)
  return(table_name)
}

OZONE_max = change_classes(OZONE_max)
PM10_max = change_classes(PM10_max)
# confirm changes
str(OZONE_max)
str(PM10_max)

# ========= making tables with maximum hourly average for the day ===========
PM10_dailymax = data.frame(Date = as.Date(unique(aq$Date),format = "%d/%m/%Y"))
OZONE_dailymax = data.frame(Date = as.Date(unique(aq$Date),format = "%d/%m/%Y"))

str(OZONE_dailymax)
str(PM10_dailymax)

OZONE_dailymax = OZONE_dailymax %>%
  left_join(aggregate(Liverpool~Date, OZONE_max, max),
            by = "Date") %>%
  left_join(aggregate(Bringelly~Date, OZONE_max, max),
            by = "Date") %>%
  left_join(aggregate(Bargo~Date, OZONE_max, max),
            by = "Date") %>%
  left_join(aggregate(Oakdale~Date, OZONE_max, max),
            by = "Date") %>%
  left_join(aggregate(`Campbelltown West`~Date, OZONE_max, max),
            by = "Date") %>%
  left_join(aggregate(Camden~Date, OZONE_max, max),
            by = "Date") %>%
  left_join(aggregate(Avg~Date, OZONE_max, max),
            by = "Date")

PM10_dailymax = PM10_dailymax %>%
  left_join(aggregate(Liverpool~Date, PM10_max, max),
            by = "Date") %>%
  left_join(aggregate(Bringelly~Date, PM10_max, max),
            by = "Date") %>%
  left_join(aggregate(Bargo~Date, PM10_max, max),
            by = "Date") %>%
  left_join(aggregate(Oakdale~Date, PM10_max, max),
            by = "Date") %>%
  left_join(aggregate(`Campbelltown West`~Date, PM10_max, max),
            by = "Date") %>%
  left_join(aggregate(Camden~Date, PM10_max, max),
            by = "Date") %>%
  left_join(aggregate(Avg~Date, PM10_max, max),
            by = "Date")

# ==== EXPORT ===== 
write.csv(OZONE_dailymax, file = "ozone data.csv", row.names = FALSE)
write.csv(PM10_dailymax, file = "pm10 data.csv", row.names = FALSE)




