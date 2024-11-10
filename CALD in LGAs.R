# Reading the LGA and SA2 code mapping data  =========================
mapping_v3 = read.csv("Mapping_data_export_v3.csv")
mapping_v4 = read.csv("Mapping_data_export_v4.csv")
colnames(mapping_v4)
mappingDF = data.frame(LGA = mapping_v4$LGA, Suburb = mapping_v4$Suburb, Postcode = mapping_v4$Postcode, SA2Code = mapping_v4$SA2_2016_Code, SA2Name = mapping_v4$SA2_2016_Name, AQCode = mapping_v4$AQ_Code, AQName = mapping_v4$AQ_Name)


Patient = read.csv("Patient-Data-Output.csv")

library('sqldf')
library('readxl')
library('ggplot2')

# Clean patient data ===========================
PatientDF = data.frame(Language = Patient$Language, Translator = Patient$Interpreter_Required)

# Proportion of translators required 
translator = 0
for (p in PatientDF){
 for (i in p){
   if (i == "Yes"){
     translator = translator +1
   }
 }
}
translatorProp = translator/nrow(PatientDF)*100 # proportion of patients that presented requiring translator 

# Proportion of language other than English 
LOTE = 0 
for (i in PatientDF$Language) {
  if (i != "ENGLISH")
  {
    LOTE = LOTE + 1
  }
}
LOTEProp = LOTE/nrow(PatientDF)*100 # proportion of patients that presented that spoke languages other than english at home 


## Clean census 2021 data ===================
census2021a = read.csv('2021Census_G02_AUST_SA2.csv')
colnames(census2021a)
census2021b = read.csv('2021Census_G01_AUST_SA2.csv')
colnames(census2021b)
census2021Gender = data.frame(SA2Code = census2021b$SA2_CODE_2021, Female_population = census2021b$Tot_P_F, Male_population = census2021b$Tot_P_M, Total_population = census2021b$Tot_P_P)
census2021GenderMapped = sqldf("SELECT * FROM census2021Gender JOIN mappingDF on census2021Gender.SA2Code = mappingDF.SA2Code")
GenderMapped = census2021GenderMapped[,-8]

Indigenous = data.frame(SA2Codes = census2021b$SA2_CODE_2021, Indigenous_population = census2021b$Indigenous_psns_Aboriginal_P, Indigenous_prop = census2021b$Indigenous_P_Tot_P/census2021b$Tot_P_P)
IndigenousMapped = sqldf("SELECT * FROM Indigenous JOIN mappingDF on Indigenous.SA2Codes = mappingDF.SA2Code")

BirthOutOfAustralia = data.frame(SA2Code = census2021b$SA2_CODE_2021, BornElsewherePopulation = census2021b$Birthplace_Elsewhere_P, BornElsewhereProp = (census2021b$Birthplace_Elsewhere_P/census2021b$Tot_P_P)*100, LOTE = census2021b$Lang_used_home_Oth_Lang_P, LOTE_prop = (census2021b$Lang_used_home_Oth_Lang_P/census2021b$Tot_P_P)*100)
CALDMapped = sqldf("SELECT * FROM BirthOutOfAustralia JOIN mappingDF on BirthOutOfAustralia.SA2Code = mappingDF.SA2Code ORDER BY BornElsewhereProp DESC")
CALDMapped = CALDMapped[,-9]
CALDLGA = sqldf("SELECT avg(BornElsewhereProp) as MeanBEP, BornElsewhereProp, LGA FROM CALDMapped GROUP BY LGA")


barplot(CALDLGA$MeanBEP~CALDLGA$LGA , main = "Mean Born Elsewhere proportion in each LGA", xlab = "Local Government Areas (LGA)", ylab =  "Proportion of population born overseas (%)")

## Clean census 2016 Data =========================

census2016 = read.csv('2016Census_G01_AUS_SA2.csv')
colnames(census2016)

# create a data frame with the born outside of australia population, the proportion of the total, language at home other than english population, and its proportion from the total
census2016BOA = data.frame(SA2Code = census2016$SA2_MAINCODE_2016, BornElsewherePopluation = census2016$Birthplace_Elsewhere_P, BornElsewhereProp = (census2016$Birthplace_Elsewhere_P/census2016$Tot_P_P)*100, LOTE = census2016$Lang_spoken_home_Oth_Lang_P, LOTEProp = (census2016$Lang_spoken_home_Oth_Lang_P/census2016$Tot_P_P)*100)
CALDMapped2016 = sqldf("SELECT * FROM census2016BOA JOIN mappingDF on census2016BOA.SA2Code = mappingDF.SA2Code Order by BornElsewhereProp DESC") # Join the mapping table and the census table and sort by the bornelsewere proportion
CALDMapped2016 = CALDMapped2016[,-9]    # remove the repeat column with SA2 Code                         
CALDLGA2016 = sqldf("SELECT AVG(BornElsewhereProp) as MeanBEP, BornElsewhereProp, LGA FROM CALDMapped2016 GROUP BY LGA")                           


## Born outside Australia ===================
BOA2016_2021 = rbind(CALDLGA$MeanBEP, CALDLGA2016$MeanBEP) # combine the proportion of born out of australia for 2016 and 2021 

rownames(BOA2016_2021) = c("2021", "2016")
colnames(BOA2016_2021) = CALDLGA2016$LGA                           
barplot(BOA2016_2021, beside = TRUE, col = c("blue", "red"), 
        ylim = c(0, 100), 
        main = 'Proportion of population born outside of Australia in different Local Government Areas (LGAs)', 
        xlab = "Local Government Areas (LGAs)", ylab = "Percentage (%)", legend = rownames(BOA2016_2021))                           
                           
## LOTE  ======================

LOTELGA2016 = sqldf("SELECT AVG(LOTEProp) as MeanLOTE, LOTEProp, LGA from CALDMapped2016 GROUP BY LGA") 
LOTELGA2021 = sqldf("SELECT AVG(LOTE_Prop) as MeanLOTE, LOTE_Prop, LGA from CALDMapped GROUP BY LGA")

LOTE2016_2021 = rbind(LOTELGA2016$MeanLOTE, LOTELGA2021$MeanLOTE)

rownames(LOTE2016_2021) = c("2016", "2021")
colnames(LOTE2016_2021) = LOTELGA2016$LGA
barplot(LOTE2016_2021, beside = TRUE, col = c("red", "blue"), 
        ylim = c(0,100), 
        main = "Proportion of population that speak languages other \n than English at home in different Local Government Areas (LGAs)", 
        xlab = 'Local Government Areas (LGAs)', ylab = "Percentage (%)", legend = rownames(LOTE2016_2021))

# Indigenous ===============

Indigenous2016 = data.frame(SA2Code = census2016$SA2_MAINCODE_2016, Aboriginal_prop = (census2016$Indigenous_P_Tot_P/census2016$Tot_P_P)*100, TI_prop = (census2016$Indig_psns_Torres_Strait_Is_P/census2016$Tot_P_P)*100, Aboriginal_TI_Prop = (census2016$Indig_Bth_Abor_Torres_St_Is_P/census2016$Tot_P_P)*100)
Indigenous2016Mapped = sqldf("SELECT * FROM Indigenous2016 JOIN mappingDF on Indigenous2016.SA2Code = mappingDF.SA2Code Order by Aboriginal_Prop DESC")
Indigenous2016Mapped = Indigenous2016Mapped [,-8] # get rid of the duplicate SA2Code column 
Ind2016 = sqldf("SELECT AVG(Aboriginal_prop) as MeanAboriginalProp, Aboriginal_prop, LGA FROM Indigenous2016Mapped GROUP BY LGA")


Indigenous2021 = data.frame(SA2Code = census2021b$SA2_CODE_2021, Aboriginal_prop = (census2021b$Indigenous_psns_Aboriginal_P/census2021b$Tot_P_P)*100, TI_prop = (census2021b$Indig_psns_Torres_Strait_Is_P/census2021b$Tot_P_P)*100, Aboriginal_TI_Prop = (census2021b$Indig_Bth_Abor_Torres_St_Is_P/census2021b$Tot_P_P)*100)
Indigenous2021Mapped = sqldf("SELECT * FROM Indigenous2021 JOIN mappingDF on Indigenous2021.SA2Code = mappingDF.SA2Code Order by Aboriginal_Prop DESC")
Indigenous2021Mapped = Indigenous2021Mapped [,-8] # get rid of the duplicate SA2Code column 
Ind2021 = sqldf("SELECT AVG(Aboriginal_prop) as MeanAboriginalProp, Aboriginal_prop, LGA FROM Indigenous2021Mapped GROUP BY LGA")

IndLGA2016_2021 = rbind(Ind2016$MeanAboriginalProp, Ind2021$MeanAboriginalProp)

rownames(IndLGA2016_2021) = c("2016", "2021")
colnames(IndLGA2016_2021) = Ind2021$LGA
barplot(IndLGA2016_2021, beside = TRUE, col = c("light blue", "light yellow"), 
        ylim = c(0,10), 
        main = "Aboriginal Proportion in different Local Government Areas (LGAs)", 
        xlab = "Local Government Areas(LGAs)", ylab = "Proportion of Aboriginal population in LGAs", legend = rownames(IndLGA2016_2021))

# SEIFA (Socio-Economic Indexes for Areas) data ==========================

SA2SEIFA = read_excel("/Users/sadigautam/Downloads/Statistical Area Level 2, Indexes, SEIFA 2021.xlsx", sheet = "Table 1")
write.csv(SA2SEIFA, "SA2SEIFA.csv", row.names = FALSE)
LGASEIFA = read_excel("/Users/sadigautam/Downloads/Local Government Area, Indexes, SEIFA 2021 (1).xlsx", sheet = "Table 2")
write.csv(LGASEIFA, "LGASEIFA.csv", row.names = TRUE )
LGASEIFA = read_excel("/Users/sadigautam/Downloads/Local Government Area, Indexes, SEIFA 2021 (1).xlsx", sheet = "Table 4")
write.csv(LGASEIFA, "LGASEIFAEcon.csv", row.names = TRUE )

# CLEAN UP SA2 DATA AND FORMAT 
SA2SEIFA_csv = read.csv("SA2SEIFA.csv")
SA2SEIFA_csv = SA2SEIFA_csv[-c(0:3),]
colnames(SA2SEIFA_csv) = NULL
colnames(SA2SEIFA_csv) = SA2SEIFA_csv[2,]
SA2SEIFA_csv = SA2SEIFA_csv[-2,] 
colnames(SA2SEIFA_csv)[3] = ("	
Index of Relative Socio-economic Disadvantage (IRSAD) Score")
colnames(SA2SEIFA_csv)[4] = ("	
Index of Relative Socio-economic Disadvantage (IRSAD) Decile")
colnames(SA2SEIFA_csv)[5] = ("Index of Relative Socio-economic Disadvantage (IRSD) Score")
colnames(SA2SEIFA_csv)[6] = ("Index of Relative Socio-economic Disadvantage (IRSD) Decile")
colnames(SA2SEIFA_csv)[7] = ("Index of Education and Occupation (IEO) Score")
colnames(SA2SEIFA_csv)[8] = ("Index of Education and Occupation (IEO) Decile")
colnames(SA2SEIFA_csv)[9] = ("Index of Economic Resources (IER) Score")
colnames(SA2SEIFA_csv)[10] = ("Index of Economic Resources (IER) Decile")
colnames(SA2SEIFA_csv)[1] = ("SA2Code")
SA2SEIFA_csv = SA2SEIFA_csv[-1,]
SA2SEIFA = SA2SEIFA_csv

# Mapping the SA2SEIFA DATA 
mappedSEIFA = sqldf("SELECT * FROM SA2SEIFA JOIN mappingDF ON SA2SEIFA.SA2code = mappingDF.SA2Code")
mappedSEIFA = mappedSEIFA[,-15]

# CLEAN UP LGA DATA AND FORMAT 
LGASEIFA = read.csv("LGASEIFA.csv")
LGASEIFA = LGASEIFA[-c(0:3),]
colnames(LGASEIFA) = LGASEIFA[2,]
LGASEIFA = LGASEIFA[,-6]
LGASEIFA = LGASEIFA[,-9]
colnames(LGASEIFA)[6] = ("Decile within Australia")
colnames(LGASEIFA)[7] = ("Percentile within Australia")
colnames(LGASEIFA)[8] = ("State/Territory")
colnames(LGASEIFA)[9] = ("Ranking within State or Territories")
colnames(LGASEIFA)[10] = ("Decile within State or Territories")
colnames(LGASEIFA)[11] = ("Percentile within State or Territories")
colnames(LGASEIFA)[12] = ("Minimum score for SA1s in area")
LGASEIFA = LGASEIFA[,-1]
LGASEIFA = LGASEIFA[-1,]
LGASEIFA = LGASEIFA[-1,]
colnames(LGASEIFA)[2] = ("LGA")

# CLEAN UP ECON RESOURCE DATA 
ECON = read.csv("LGASEIFAEcon.csv")
ECON = ECON[-c(0:3),]
colnames(ECON) = ECON





# Map LGA data by the SA2Codes 
LGASEIFA_Mapped = sqldf("Select * from LGASEIFA where LGASEIFA.LGA = 'Liverpool' OR LGASEIFA.LGA = 'Fairfield' OR LGASEIFA.LGA = 'Bankstown' OR LGASEIFA.LGA = 'Wingecarribee'
 OR LGASEIFA.LGA = 'Wollondilly' OR LGASEIFA.LGA = 'Camden' OR LGASEIFA.LGA = 'Campbelltown (NSW)'")

# Plots =============================

# Disadvantage ranking within NSW
LGAdf = sqldf("SELECT LGA, [Ranking within State or Territories] as Rank FROM LGASEIFA_Mapped")
ggplot(LGAdf, aes (x = reorder(LGAdf$LGA, -as.integer(LGAdf$Rank)), y = as.integer(LGAdf$Rank))) + 
  geom_segment(aes(x = reorder(LGAdf$LGA, -as.integer(LGAdf$Rank)), xend = reorder(LGAdf$LGA, - as.integer(LGAdf$Rank)), y= 0, yend = as.integer(LGAdf$Rank)), color = "grey")+
  geom_point(size = 3, color = "blue") + 
  scale_y_continuous(limits = c (0, 150)) +
  labs(title = "Local Government Areas (LGA) Index of Relative Socio-economic Disadvantage, 2021", x = "Local Government Areas", y = "Ranking within NSW") + 
  theme_minimal()

# Disadvantage scores
ScoreDF = sqldf("SELECT LGA, Score FROM LGASEIFA_Mapped")
ScoreDF$LGA = replace(ScoreDF$LGA, ScoreDF$LGA == "Campbelltown (NSW)", "Campbelltown")
ScorePlot = barplot(as.integer(Score) ~ LGA, data = ScoreDF, log = "y", xlab = "Local Government Areas", ylab = "Scores", main = "Local Government Area (LGA) Index of Relative socio-economic Disadvantage, 2021", col = "Lightblue")
text(ScorePlot, as.integer(ScoreDF$Score) - 15 , labels = as.integer(ScoreDF$Score), col = "Black")
