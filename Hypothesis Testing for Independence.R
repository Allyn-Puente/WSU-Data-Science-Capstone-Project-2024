# ======== X2 HHD ==========
# load data
patients = read.csv(patient_data)
daily = read.csv(daily_data)

# H0 = features and cald ed pres are independent
# HA = features and cald ed pres are NOT independent

## High DMT
patients.table = table(patients$CALDflag, patients$HighDMT) # tabulate records
patients.matrix = matrix(patients.table, 
                         ncol = ncol(patients.table), 
                         dimnames = dimnames(patients.table)) # create matrix
class(mat.pat) # confirm matrix
# hypothesis tests
chisq.test(patients.matrix, simulate.p.value = TRUE)
fisher.test(patients.matrix, simulate.p.value = TRUE)

## Poor_PM10
patients.table = table(patients$CALDflag, patients$Poor_PM) # tabulate records
patients.matrix = matrix(patients.table, 
                         ncol = ncol(patients.table), 
                         dimnames = dimnames(patients.table)) # create matrix
class(mat.pat) # confirm matrix
# hypothesis tests
chisq.test(patients.matrix, simulate.p.value = TRUE)
fisher.test(patients.matrix, simulate.p.value = TRUE)

## Poor_OZONE
patients.table = table(patients$CALDflag, patients$Poor_OZ) # tabulate records
patients.matrix = matrix(patients.table, 
                         ncol = ncol(patients.table), 
                         dimnames = dimnames(patients.table)) # create matrix
class(mat.pat) # confirm matrix
# hypothesis tests
chisq.test(patients.matrix, simulate.p.value = TRUE)
fisher.test(patients.matrix, simulate.p.value = TRUE)

ggplot(data = patients, aes(x=DMT,y=CALD))

# ===== Logistic Regression for DMT =====
logreg = glm(CALDflag~DMT, data = patients, family = binomial)
exp(coef(logreg))
summary(logreg)

# line of best fit to investigate CALD presentations against DMT
ggplot(daily, aes(x = mean_DMT, y = CALD_presentations)) +
  geom_point() +  # Add scatter plot points
  geom_smooth(method = "gam", color = "red") +  # Add linear model line
  labs(title = "CALD Presentations by DMT", x = "DMT(°C)", y = "CALD presentations")
# find gradient for mean_DMT
summary(lm(CALD_presentations~mean_DMT, data = patdat))
