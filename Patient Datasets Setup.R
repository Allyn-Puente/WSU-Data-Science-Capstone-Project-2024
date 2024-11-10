# ========== SETUP + FUNCTIONS ==========
library("dplyr") # for cleaning
## ==== functions ====
# to change text based variables into numerical
factorise = function(x){
  to.factor = factor(x)
  to.numeric = as.numeric(to.factor)
}

# apply CALD indicator flags
flags.patients = function(dataframe){
  dataframe$Nationality = ifelse(dataframe$Nationality == "AUSTRALIA",0,1)
  dataframe$Race =  ifelse(dataframe$Race == "Neither",0,1)
  dataframe$Citizenship = ifelse(dataframe$Citizenship == "Eligible Australian Resident",0,1)
  dataframe$Language = ifelse(dataframe$Language == "ENGLISH",0,1)
  dataframe$Interpreter_Required = ifelse(dataframe$Interpreter_Required != "Yes",0,1)
  dataframe$Interpreter_Type = ifelse(dataframe$Interpreter_Type == "ENGLISH",0,1)
  dataframe$Gender = factorise(dataframe$Gender)
  dataframe$Marital_Type = factorise(dataframe$Marital_Type)
  dataframe$Religion = factorise(dataframe$Religion)
  # new column so that name still in df
  dataframe$Location_Facility_Code = factorise(dataframe$Location_Facility) 
  dataframe$Date = as.Date(dataframe$Registration_DateTime_SydLocal)
  dataframe$Time = format(as.POSIXct(dataframe$Registration_DateTime_SydLocal), format = "%H:%M:%S")
  return(dataframe)
}

# set up CALD factors
CALD_cols = c('Nationality', 'Language','Citizenship', 'Interpreter_Required', 'Interpreter_Type')
# current version (test if possible to run through columns more efficiently)
cald_flag = function(dataframe){
  for (i in 1:nrow(dataframe)) {
    if (dataframe$Nationality[i] == '1'){
      dataframe$CALDflag[i] = 1
    } else if (dataframe$Language[i] == '1'){
      dataframe$CALDflag[i] = 1
    } else if (dataframe$Citizenship[i] == '1'){
      dataframe$CALDflag[i] = 1
    } else if (dataframe$Interpreter_Required[i] == '1'){
      dataframe$CALDflag[i] = 1
    } else if (dataframe$Interpreter_Type[i] == '1'){
      dataframe$CALDflag[i] = 1
    } else {
      dataframe$CALDflag[i] = 0
    }
  }
  return(dataframe$CALDflag)
}

# ===== WEATHER DATA =====
# checks hhd and sh status per day per station
temp_data = data.frame(Weather_Station = wd$Weather_station, Date = as.Date(wd$Date, format = "%d/%m/%Y"), 
                       High_Heat_Flag = as.numeric(wd$DMT_Highheatday == "High heat day"), 
                       DMT = wd$DMT,
                       EHF_Heatwave_Flag = as.numeric(wd$EHF_Heatwave_2 == "Severe heatwave"))

# filter to desired period 
get.temp.data = function(first_date,final_date){
  temp_data[which(temp_data$Date >= as.Date(first_date, format = "%d/%m/%Y") & 
                    temp_data$Date <= as.Date(final_date, format = "%d/%m/%Y")),]
}

# identify days with a DMT higher than the mean DMT
find.hdmt.days = function(dataframe){
  mean_dmt = mean(dataframe$DMT, na.rm = TRUE)
  HighDMT = ifelse(dataframe$DMT > mean_dmt, 1, 0)
  return(HighDMT)
}

# ========== AIR QUALITY ==========
## reshaping function
# converts dataframe so that each aq tower column become a unique record
reshape.aq = function(data,aq_type){
  reshape(data,
          varying = list(names(data)[-1]),
          v.names = aq_type,
          timevar = "AQ_Name",
          times = names(data)[-1],
          direction = "long")
}

# ========== FIRST TABLE: PATIENT RECORDS ==========
## ---- ensure objects all empty ----
patients_wp = 0
final_patients = 0
final_hdmt = 0
final_OZ = 0
final_PM = 0
final_wd = 0
reshape_final_PM = 0
reshape_final_OZ = 0

## ---- read original data ----
patients = read.csv(patient_data)
mapping = read.csv(mapping_data)
wd = read.csv(weather_data)
ozone = read.csv(ozone_data)
pm10 = read.csv(pm10_data)

dim(patients) # check data

## ---- weather data ----
final_wd = get.temp.data("01/01/2023","31/12/2023") 
final_hdmt = data.frame(final_wd, HighDMT = find.hdmt.days(final_wd))

## ---- air quality ----
reshape_final_OZ = reshape.aq(ozone,"OZONE")
mean_OZ = mean(reshape_final_OZ$OZONE, na.rm = TRUE)
# 1 = high ozone, 0 = avg/low ozone
reshape_final_OZ$Above_Avg_OZ = ifelse(reshape_final_OZ$OZONE >= mean_OZ,1,0)
reshape_final_OZ$Poor_OZ = ifelse(reshape_final_OZ$OZONE > 6.5, 1, 0)

reshape_final_PM = reshape.aq(pm10,"PM10")
mean_PM = mean(reshape_final_PM$PM10, na.rm = TRUE)
# 1 = high pm10, 0 = avg/low pm10
reshape_final_PM$Above_Avg_PM = ifelse(reshape_final_PM$PM10 >= mean_OZ,1,0)
reshape_final_PM$Poor_PM = ifelse(reshape_final_PM$PM10 > 50, 1, 0)

# check conversion
head(reshape_final_OZ)
head(reshape_final_PM)

## ---- patients -----
final_patients = 0
final_patients = flags.patients(patients)
final_patients$CALDflag = 0 # empty column
final_patients$CALDflag = cald_flag(final_patients)
final_patients$Suburb = tolower(final_patients$Suburb) # to ensure same when joining
final_patients$Location_Facility_Suburb = 0 # for records w/o swslhd suburbs

# ---- hospital suburb mapping for records with nonSWSLHD suburb ----
hospital_mapping = data.frame(Suburb = 
                                c("liverpool", "fairfield", "bankstown", "campbelltown"),
                              Location_Facility = 
                                unique(final_patients$Location_Facility))
for(i in 1:nrow(final_patients)){
  final_patients$Location_Facility_Suburb[i] = 
    hospital_mapping$Suburb[which(
      hospital_mapping$Location_Facility == final_patients$Location_Facility[i])]
}

dim(final_patients)
View(final_patients)

# ========== MAPPING WS AND AQ ==========
# mapping weather stations and air quality stations
# tests df of unique postcodes
map_join = 0
map_join = mapping %>%
  distinct(Suburb, WS_Daily_Code,WS_Daily_Name,AQ_Code,AQ_Name) # add suburb once patient data has it
map_join$Suburb = tolower(map_join$Suburb) # to ensure same case

head(map_join) # check data

## ===== joining weather data and air quality =====
final_patients = final_patients %>%
  left_join(map_join %>% # mapping weather and air quality stations
              select(Suburb, WS_Daily_Code, WS_Daily_Name,AQ_Code, AQ_Name), # add suburb once patient data has it (take out postcode?)
            by = c("Suburb" = "Suburb")) # join by suburb

# separate NAs and Non_NAs
non_NAs = final_patients %>%
  filter(is.na(WS_Daily_Code)==FALSE)

# map hospital suburb's weather and aq data onto NAs
NAs = final_patients %>%
  filter(is.na(WS_Daily_Code)==TRUE) %>%
  select(-WS_Daily_Code, -WS_Daily_Name,-AQ_Code, -AQ_Name) %>%
  left_join(map_join %>% # mapping ws and aq
              select(Suburb, WS_Daily_Code, WS_Daily_Name,AQ_Code, AQ_Name), # add suburb once patient data has it (take out postcode?)
            by = c("Location_Facility_Suburb" = "Suburb")) # join by suburb

# putting df back together
final_patients = rbind(non_NAs, NAs)
dim(final_patients)

# add in weather and air quality data
final_patients = final_patients %>%
  left_join(reshape_final_OZ %>% # joining ozone data
              select(AQ_Name, Date, OZONE,Poor_OZ,Above_Avg_OZ),
            by = c("Date", "AQ_Name")) %>%
  left_join(reshape_final_PM %>% # joining pm10 data
              select(AQ_Name, Date, PM10,Poor_PM,Above_Avg_PM),
            by = c("Date", "AQ_Name")) %>%
  left_join(final_hdmt, # joining ws data
            by = c("Date", "WS_Daily_Code" = "Weather_Station"))

dim(final_patients) # ensure no data lost
str(final_patients) # check classes
View(final_patients) # final check
# ==== EXPORT ===== 
write.csv(final_patients, file = "patient records dataset.csv", row.names = FALSE)

# ===== SECOND TABLE: DAILY RECORDS ======
# patient data
patient_data = 0
patient_data = final_patients %>%
  group_by(Date) %>%
  summarise(ED_presentations = n(),
            mean_DMT = mean(DMT, na.rm = TRUE),
            mean_High_DMT_Flag = mean(as.numeric(HighDMT), na.rm = TRUE),
            mean_High_Heat_Flag= mean(High_Heat_Flag, na.rm = TRUE),
            mean_EHF_Heatwave_Flag = mean(Heatwave_Flag, na.rm = TRUE),
            mean_OZONE = mean(OZONE, na.rm = TRUE),
            mean_Poor_OZ_Flag = mean(Poor_OZ, na.rm = TRUE),
            mean_PM10 = mean(PM10, na.rm = TRUE),
            mean_Poor_PM_Flag = mean(Poor_PM, na.rm = TRUE),
            .groups = 'drop')

cald_pats = 0
cald_pats = final_patients %>%
  group_by(Date) %>%
  filter(CALDflag==1)%>%
  summarise(CALD_presentations=n())

noncald_pats = 0
noncald_pats = final_patients %>%
  group_by(Date) %>%
  filter(CALDflag==0)%>%
  summarise(nonCALD_presentations=n())
## ========= final table =========
patient_data = patient_data %>%
  left_join(cald_pats, by = "Date") %>%
  left_join(noncald_pats, by = "Date")
View(patient_data)
# ====== EXPORT======
write.csv(patient_data, file = "daily records dataset.csv", row.names = FALSE)
