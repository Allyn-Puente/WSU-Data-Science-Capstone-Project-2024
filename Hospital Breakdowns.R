# ===== LOADING PACKAGES ======
library(gridExtra) # for plots
library(ggplot2) # for plots
library(dplyr) # for cleaning

# ========= Separating patient records by hospital =========
patients = read.csv(patient_records_data)
hosp_group = 0
hosp_group = patients%>%
  group_by(Date, Location_Facility_Code) %>%
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

hosp_group_mapping = data.frame(Location_Facility_Code = c(1,2,3,4), 
                                Location_Facility_Suburb = c('Bankstown',"Campbelltown",
                                                             'Fairfield','Liverpool'))
# separate CALD and nonCALD presentations
cald_pats = 0
cald_pats = patients %>%
  group_by(Date, Location_Facility_Code) %>%
  filter(CALDflag==1)%>%
  summarise(CALD_presentations=n(),
            .groups = 'drop')

noncald_pats = 0
noncald_pats = patients %>%
  group_by(Date, Location_Facility_Code) %>%
  filter(CALDflag==0)%>%
  summarise(nonCALD_presentations=n(),
            .groups = 'drop')

hosp_group = hosp_group %>%
  left_join(hosp_group_mapping %>% # joining hospital suburbs
              select(Location_Facility_Code,Location_Facility_Suburb),
            by = c("Location_Facility_Code")) %>%
  left_join(cald_pats, by = c("Date","Location_Facility_Code")) %>%
  left_join(noncald_pats, by = c("Date","Location_Facility_Code"))

hosp_group$Date = as.Date(hosp_group$Date)

# Bankstown
b_pd = 0
b_pd = hosp_group %>%
  filter(Location_Facility_Code ==1)
head(b_pd)

# Campbelltown
c_pd = 0
c_pd = hosp_group %>%
  filter(Location_Facility_Code ==2)
head(c_pd)

# Fairfield
f_pd = 0
f_pd = hosp_group %>%
  filter(Location_Facility_Code ==3)
head(f_pd)

# Liverpool
l_pd = 0
l_pd = hosp_group %>%
  filter(Location_Facility_Code ==4)
head(l_pd)

# ===== EXPORT =====
write.csv(b_pd, file = "bankstown_data.csv", row.names = FALSE)
write.csv(c_pd, file = "campbelltown_data.csv", row.names = FALSE)
write.csv(f_pd, file = "fairfield_data.csv", row.names = FALSE)
write.csv(l_pd, file = "liverpool_data.csv", row.names = FALSE)

# ===== PLOTTING ====
# general presentations
bplot = ggplot()+
  scale_x_date(date_labels = "%b", breaks = "1 month") + 
  geom_point(data = b_pd, aes(x = Date, y = ED_presentations, group = 1, colour = 'ED Presentations'),colour = 'blue')+
  labs(title = 'Bankstown-Lidcombe Hospital',
       y = 'ED Presentations') +
  theme(axis.text.x = element_text(angle = 25, hjust = 1))

cplot = ggplot()+
  scale_x_date(date_labels = "%b", breaks = "1 month") + 
  geom_point(data = c_pd, aes(x = Date, y = ED_presentations, group = 1, colour = 'Campbelltown'),colour = 'darkgreen') +
  labs(title = 'Campbelltown Hospital',
       y = 'ED Presentations') +
  theme(axis.text.x = element_text(angle = 25, hjust = 1))

fplot = ggplot()+
  scale_x_date(date_labels = "%b", breaks = "1 month") + 
  geom_point(data = f_pd, aes(x = Date, y = ED_presentations, group = 1, colour = 'Fairfield'),colour = 'red') +
  labs(title = 'Fairfield Hospital',
       y = 'ED Presentations') +
  theme(axis.text.x = element_text(angle = 25, hjust = 1))

lplot = ggplot()+
  scale_x_date(date_labels = "%b", breaks = "1 month") + 
  geom_point(data = l_pd, aes(x = Date, y = ED_presentations, group = 1, colour = 'Liverpool'),colour = 'purple')+
  labs(title = 'Liverpool Hospital',
       y = 'ED Presentations') +
  theme(axis.text.x = element_text(angle = 25, hjust = 1))

grid.arrange(bplot,cplot,fplot,lplot, layout_matrix = rbind(c(1,2),c(3,4)))

# CALD presentations
ggplot(data = hosp_group, aes(x = Date, y =CALD_presentations, color = Location_Facility_Suburb, fill = Location_Facility_Suburb))+
  scale_x_date(date_labels = "%b", breaks = "1 month") + 
  scale_y_continuous(labels = scales::percent) +
  geom_bar(stat = 'identity', position = 'stack') +
  labs(title = 'CALD Presentations by Hospital',
       y = 'CALD Presentations (Proportion)',
       fill = 'Hospitals') +
  guides(color = "none") 
