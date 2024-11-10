patients = read.csv('Climate-Output2.csv')
mapping = read.csv("Mapping_data_export_v4.csv")
patients_sept = read.csv('Climate-Output_Sep2023.csv')
patients_feb = read.csv('Climate-Output_Feb2023.csv')
census = read.csv('2021Census_G01_AUST_SA2.csv')
patients_comb = read.csv('Combined_patient_data(F1,MA,JA,JA,AA,S2).csv')

library('sqldf')
nationality_non_aus = sum(patients$Nationality != "AUSTRALIA")/length(patients$Nationality)*100
interpreter = sum(patients$Interpreter_Required == "Yes")/length(patients$Interpreter_Required)*100
citizen_non_aus = sum(patients$Citizenship != "Eligible Australian Resident")/length(patients$Citizenship)*100
LOTE = sum(patients$Language != "ENGLISH")/length(patients$Language)*100


# SEPT 2023 ===========================================
nationality_non_aus_sep = sum(patients_sept$Nationality != "AUSTRALIA")/length(patients_sept$Nationality)*100
interpreter_sep = sum(patients_sept$Interpreter_Required == "Yes")/length(patients_sept$Interpreter_Required)*100
citizen_non_aus_sep = sum(patients_sept$Citizenship != "Eligible Australian Resident")/length(patients_sept$Citizenship)*100
LOTE_sep = sum(patients_sept$Language != "ENGLISH")/length(patients_sept$Language)*100


# FEB 2023 ============================================
nationality_non_aus_feb = sum(patients_feb$Nationality != "AUSTRALIA")/length(patients_feb$Nationality)*100
interpreter_feb = sum(patients_feb$Interpreter_Required == "Yes")/length(patients_feb$Interpreter_Required)*100
citizen_non_aus_feb = sum(patients_feb$Citizenship != "Eligible Australian Resident")/length(patients_feb$Citizenship)*100
LOTE_feb = sum(patients_feb$Language != "ENGLISH")/length(patients_feb$Language)*100


# COMBINED ====================================
nationality_non_aus_comb = sum(patients_comb$Nationality == 1)/length(patients_comb$Nationality)*100
interpreter_comb = sum(patients_comb$Interpreter_Required == 1)/length(patients_comb$Interpreter_Required)*100
citizen_non_aus_comb = sum(patients_comb$Citizenship == 1)/length(patients_comb$Citizenship)*100
LOTE_comb = sum(patients_comb$Language == 1)/length(patients_comb$Language)*100

# DATE SEPERATED COMBINED ======================

library('ggplot2')

patient_data_feb = data.frame(
  Category = c("Nationality", "Interpreter", "Citizenship", "LOTE"), 
  Percentage = c(nationality_non_aus_feb, interpreter_feb, citizen_non_aus_feb, LOTE_feb)
)

patient_data_comb = data.frame(
  Category = c("Nationality", "Interpreter", "Citizenship", "LOTE"), 
  Percentage = c(nationality_non_aus_comb, interpreter_comb, citizen_non_aus_comb, LOTE_comb)
)

ggplot(patient_data_comb, aes (x = Category, y = Percentage)) +
  geom_bar(stat = "identity", fill = "skyblue") + 
  labs(x = "Category", y = "Percentage(%)") + 
  ggtitle("Percentage of CALD indicators - 2023") + 
  geom_text(aes(label = paste0(Percentage, "%")), vjust = -0.5) + 
  theme_minimal()


# Map patient data to LGA =============
patients_mapped_feb = sqldf("SELECT * FROM patients_feb JOIN mapping on patients_feb.Postal_Code = mapping.Postcode")
patients_mapped_feb = patients_mapped_feb[,-15] # removing extra column of postcode

# Combined
patients_mapped_comb = sqldf("SELECT * FROM patients_comb JOIN mapping on patients_comb.Postal_Code = mapping.Postcode")
patients_mapped_comb = patients_mapped_comb[,-15] # removing extra column of postcode


# Presentation counts by LGA 
LGA_pres_feb = sqldf("SELECT LGA, count(*) as count FROM patients_mapped_feb GROUP BY LGA")

# combined 
LGA_pres_comb = sqldf("SELECT LGA, count(*) as count FROM patients_mapped_comb GROUP BY LGA")

# Plot the presentation counts by LGA
ggplot(LGA_pres_feb, aes (x = LGA, y = count)) +
  geom_bar(stat = "identity", fill = "skyblue") + 
  labs(x = "LGA", y = "Count") + 
  ggtitle("Presentations by LGA - February 2023") + 
  geom_text(aes(label = paste0(count)), vjust = -0.5) + 
  theme_minimal()

# Combined
ggplot(LGA_pres_comb, aes (x = LGA, y = count)) +
  geom_bar(stat = "identity", fill = "skyblue") + 
  labs(x = "LGA", y = "Count") + 
  ggtitle("Presentations by LGA - 2023") + 
  geom_text(aes(label = paste0(count)), vjust = -0.5) + 
  theme_minimal()

# CALD proportion in each LGA presentaiton 

# LOTE
LOTE_LGA_pres_feb = sqldf("SELECT LGA, count(*) as count FROM patients_mapped_feb WHERE Language != 'ENGLISH' GROUP BY LGA")
LOTE_LGA_pres_feb = sqldf("SELECT LGA_pres_feb.LGA, LOTE_LGA_pres_feb.count as LOTE_count, LGA_pres_feb.count as Total FROM LOTE_LGA_pres_feb join LGA_pres_feb ON LOTE_LGA_pres_feb.LGA = LGA_pres_feb.LGA")
LOTE_LGA_prop_feb = data.frame(LGA = LOTE_LGA_pres_feb$LGA, Proportion = ((LOTE_LGA_pres_feb$LOTE_count/LOTE_LGA_pres_feb$Total)*100))


# combined 
LOTE_LGA_pres_comb = sqldf("SELECT LGA, count(*) as count FROM patients_mapped_comb WHERE Language != 0 GROUP BY LGA")
LOTE_LGA_pres_comb = sqldf("SELECT LGA_pres_comb.LGA, LOTE_LGA_pres_comb.count as LOTE_count, LGA_pres_comb.count as Total FROM LOTE_LGA_pres_comb join LGA_pres_comb ON LOTE_LGA_pres_comb.LGA = LGA_pres_comb.LGA")
LOTE_LGA_prop_comb = data.frame(LGA = LOTE_LGA_pres_comb$LGA, Proportion = ((LOTE_LGA_pres_comb$LOTE_count/LOTE_LGA_pres_comb$Total)*100))

# Interpreter 
Interpreter_LGA_feb = sqldf("SELECT LGA, count(*) as count FROM patients_mapped_feb WHERE Interpreter_Required = 'Yes' GROUP BY LGA")
Interpreter_prop_feb = data.frame(LGA = Interpreter_LGA_feb$LGA, Proportion = ((Interpreter_LGA_feb$count/LGA_pres_feb$count)*100))

# combined
Interpreter_LGA_comb = sqldf("SELECT LGA, count(*) as count FROM patients_mapped_comb WHERE Interpreter_Required = 1 GROUP BY LGA")
Interpreter_prop_comb = data.frame(LGA = Interpreter_LGA_comb$LGA, Proportion = ((Interpreter_LGA_comb$count/LGA_pres_comb$count)*100))

# Nationality 
Nationality_LGA_feb = sqldf("SELECT LGA, count(*) as count FROM patients_mapped_feb WHERE Nationality != 'AUSTRALIA' GROUP BY LGA")
Nationality_prop_feb = data.frame(LGA = Nationality_LGA_feb$LGA, Proprtion = ((Nationality_LGA_feb$count/LGA_pres_feb$count)*100))

# combined 
Nationality_LGA_comb = sqldf("SELECT LGA, count(*) as count FROM patients_mapped_comb WHERE Nationality != 0 GROUP BY LGA")
Nationality_prop_comb = data.frame(LGA = Nationality_LGA_comb$LGA, Proprtion = ((Nationality_LGA_comb$count/LGA_pres_comb$count)*100))

# join 3 tables 
LGA_feb = merge(LOTE_LGA_prop_feb, Interpreter_prop_feb, by = "LGA", all.x = TRUE)
LGA_feb = merge(LGA_feb, Nationality_prop_feb, by  = "LGA", all.x = TRUE)
colnames(LGA_feb) = c("LGA", "Proportion LOTE", "Proportion Interpreter", "Proportion Nationality not Australian")

# combined 
LGA_comb = merge(LOTE_LGA_prop_comb, Interpreter_prop_comb, by = "LGA", all.x = TRUE)
LGA_comb = merge(LGA_comb, Nationality_prop_comb, by  = "LGA", all.x = TRUE)
colnames(LGA_comb) = c("LGA", "Proportion LOTE", "Proportion Interpreter", "Proportion Nationality not Australian")
library('tidyr')

long_LGA_feb = LGA_feb %>%
  pivot_longer(cols = starts_with("P"),
               names_to = "Variable", 
               values_to = "Value")
# combined 
long_LGA_comb = LGA_comb %>%
  pivot_longer(cols = starts_with("P"),
               names_to = "Variable", 
               values_to = "Value")


ggplot(long_LGA_sep, aes(x = LGA, y = Value, fill = Variable)) +
  geom_bar(stat = "identity", position = position_dodge()) + 
  labs(title = "Proportions of CALD indicators in the presentations by Local Government Areas (LGAs) \n- September 2023", 
       x = "LGA", 
       y = "Percentage (%)") +
  theme_minimal() +
  geom_text(aes(label = paste0(round(Value))), vjust = -0.5)
  scale_fill_brewer(palette = "Set1")  # Optional color palette

  # combined 
ggplot(long_LGA_comb, aes(x = LGA, y = Value, fill = Variable)) +
    geom_bar(stat = "identity", position = position_dodge()) + 
    labs(title = "Proportions of CALD indicators in the presentations by Local Government Areas (LGAs) \n 2023", 
         x = "LGA", 
         y = "Percentage (%)") +
    theme_minimal() +
  scale_fill_brewer(palette = "Set1")  # Optional color palette
  
# Patient data with weather ================
patient_comb = read.csv("patient_data_combined.csv")

patients_feb = patients_feb[,-16] 
patients_sept = patients_sept[,-16]
combine_year = rbind(patients,patients_sept, patients_feb)
write.csv(combine_year, "Combined_patient.csv", row.names = FALSE)

# LGA OVER TIME ===========================

# sort the patient data by date and group by LGA 
library('dplyr')
LGA_count = sqldf("Select Date, LGA, DMT from patients_mapped_comb")
LGA_count = LGA_count %>% 
  filter(LGA == "Wingecarribee") %>%
  group_by(Date,LGA) %>%
  summarise(total_count = n(), DMT = mean(DMT, na.rm = TRUE),.groups = 'drop')

patients_comb = sqldf('SELECT Date, LGA, DMT  FROM patients_mapped_comb WHERE patients_mapped_comb.Nationality = 1 or patients_mapped_comb.Citizenship = 1 or patients_mapped_comb.Language = 1 or patients_mapped_comb.Interpreter_Required = 1')


select_LGA =  patients_comb %>% 
  filter(LGA == "Wingecarribee") %>%
  group_by(Date, LGA) %>%
  summarise(count=n(), DMT = mean(DMT, na.rm=TRUE),.groups = 'drop')

select_LGA =  patients_comb %>% 
  filter(LGA == "Camden") %>%
  group_by(Date, LGA) %>%
  summarise(count=n(), DMT = mean(DMT, na.rm=TRUE),.groups = 'drop')

select_LGA = sqldf("Select * from select_LGA JOIN LGA_count ON select_LGA.Date = LGA_count.Date WHERE select_LGA.LGA = 'Wingecarribee' ")
select_LGA = select_LGA[,-(c(1,2,4))]
select_LGA['CALDprop'] = select_LGA['count']/select_LGA['total_count']

patients_comb =  patients_comb %>% 
  group_by(Date, LGA) %>%
  summarise(count=n(), DMT = mean(DMT, na.rm=TRUE),.groups = 'drop')
select_LGA$Date = as.Date(select_LGA$Date)

patients_comb$Date = as.Date(patients_comb$Date)
ggplot(select_LGA, aes(x = Date, y = count, color = DMT)) +
  geom_point(size = 3) +
  labs(title = "CALD ED presentations for Wollondilly 2023", 
       x = "Date", 
       y = "Count", 
       color = "DMT") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, hjust =1)) +
  scale_x_date(date_breaks = "5 days", date_labels = "%Y-%m-%d")

# Plot the cald prop ed presentations 
ggplot(select_LGA, aes(x = Date, y = CALDprop, color = DMT)) +
  geom_point(size = 1) +
  labs(title = "CALD ED presentations for Wingecarribee 2023", 
       x = "Date", 
       y = "Count", 
       color = "DMT") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, hjust =1)) +
  scale_x_date(date_breaks = "5 days", date_labels = "%Y-%m-%d")



