# DMT = daily mean temperature
# high heat day: high heat day, non high heat day

# ========== IMPORT DATA ==========
wd = read.csv("Weather_data_export_v2.csv")
names(wd)
str(wd)

# ========== GRAPHS FOR DMT (WD DATA) ==========
avg_DMT = aggregate(DMT~Date, data = wd, FUN = mean)
avg_DMT$Date = as.Date(avg_DMT$Date, format = "%d/%m/%Y")
avg_DMT$MD = format(avg_DMT$Date, "%m-%d")
avg_DMT$MD = as.numeric(gsub("-", ".", avg_DMT$MD))
Month_Names = c("Jan", "Feb", "Mar", "Apr", "May", "June", "July","Aug", "Sep","Oct","Nov","Dec")

monthly_DMT = aggregate(avg_DMT$DMT~avg_DMT$MD, data = avg_DMT, FUN = mean)
names(monthly_DMT) = c("Date","Avg_DMT")
ggplot() + 
  geom_line(data = monthly_DMT, aes(x =Date, y = Avg_DMT), colour = "dark green") +
  scale_x_continuous(breaks = seq(1,12,1), labels = Month_Names) +
  labs(x = "Date", y = "Temperature", title = "Avg DMT by Day") 
length(avg_max_temp$Date)

# ========== FINDING HIGH HEAT DAYS FOR SEPTEMBER ==========
which(data$DMT_Highheatday == "High heat day")
data[49,]$Date
data[139,]$Date

hhd = (which(wd$DMT_Highheatday == "High heat day"))
hhd = unique(wd[hhd,]$Date)
hhd = as.Date(hhd, format = "%d/%m/%Y")
length(which(hhd > as.Date("31/12/2022", format = "%d/%m/%Y")))
hhd_23 = hhd[which(hhd > as.Date("31/12/2022", format = "%d/%m/%Y"))]

# finding severe heatwave
sh = which(wd$DMT_Heatwave_2 == "Severe heatwave")
sh = unique(wd[sh,]$Date)
sh = as.Date(sh, format = "%d/%m/%Y")
sh_23 = sh[which(sh > as.Date("31/12/2022", format = "%d/%m/%Y"))]
sh_23
as.numeric(wd$DMT_Heatwave_2 == "Severe heatwave")

# ========== CREATING DATAFRAMES ==========
# checks hhd and sh status for per day per station
dmt = data.frame(ws = wd$Weather_station, Date = as.Date(wd$Date, format = "%d/%m/%Y"), 
                 hhd = as.numeric(wd$DMT_Highheatday == "High heat day"), 
                 sh = as.numeric(wd$DMT_Heatwave_2 == "Severe heatwave"))

# filter to 2023 
dmt_23 = dmt[which(dmt$Date > as.Date("31/12/2022", format = "%d/%m/%Y") & 
                     dmt$Date < as.Date("01/01/2024", format = "%d/%m/%Y")),]

# finding months 23
sep_23 = dmt[which(dmt$Date >= as.Date("01/09/2023", format = "%d/%m/%Y") & 
                     dmt$Date <= as.Date("30/09/2023", format = "%d/%m/%Y")),]
feb_23 = dmt[which(dmt$Date >= as.Date("01/02/2023", format = "%d/%m/%Y") & 
                     dmt$Date <= as.Date("28/02/2023", format = "%d/%m/%Y")),]
Month_Names = c("Jan", "Feb", "Mar", "Apr", "May", "June", 
                "July","Aug", "Sep","Oct","Nov","Dec")

# ========== PLOTS FOR HHDs ==========
chosen dates = dmt_23

ggplot() +
  geom_col(data = dmt_23, aes(x = Date, y = hhd, colour = "High Heat Days"), colour = "Red") +
  scale_x_date(date_labels = "%b %d", breaks = "1 month")
labs(x = "Date", y = "Y/N", title = "HHD/SH")

ggplot(dmt_23, aes( x = hhd, y = sh)) +
  geom_point(aes(size = Date), colour = "dark red") +
  labs(x = "High Heat Days", y = "Severe Heatwave", title = "HHD/SH")

# finding range for high heat day
feb_23[which(feb_23$hhd == 1),]
ggplot() +
  geom_col(data = feb_23, aes(x = Date, y = hhd, colour = "High Heat Days"), colour = "Red", fill = "Red") +
  scale_x_date(date_labels = "%d %b", breaks = "1 week")+
  labs(x = "Date", y = "Weather stations that recorded high heat days", 
       title = "High Heat Days in February 2023") +
  theme(plot.margin = unit(c(10,2,2,2), 'mm'),
        plot.title = element_text(size = 18))


'settled for Feb 10 - 20'

# testing january and december
dates = dmt[which(dmt$Date > as.Date("01/03/2023", format = "%d/%m/%Y") & 
                    dmt$Date < as.Date("31/01/2023", format = "%d/%m/%Y")),]
ggplot() +
  geom_col(data = dates, aes(x = Date, y = hhd, colour = "High Heat Days"), colour = "Red", fill = "Red") +
  scale_x_date(date_labels = "%d", breaks = "1 day")
labs(x = "Date", y = "Y/N", title = "HHD/SH")

