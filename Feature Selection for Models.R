# --------- load libraries and packages ----------
library(boot)
patients = (patient_data)

# ---------- feature finding ----------
pd = patdat[,2:12]
pairs(pd, panel=panel.smooth)
cor(pd)[1,]

# --------- narrow features down --------
# cv glm with poisson count for feature
m1 = glm(ED_presentations~mean_DMT, data = patient_data, family = poisson)
m2 = glm(ED_presentations~mean_OZONE, data = patient_data, family = poisson)
m3 = glm(ED_presentations~mean_PM10, data = patient_data, family = poisson)
m4 = glm(ED_presentations~mean_High_DMT_Flag, data = patient_data, family = poisson)
m5 = glm(ED_presentations~mean_DMT+mean_OZONE, data = patient_data, family = poisson)
m6 = glm(ED_presentations~mean_DMT+mean_PM10, data = patient_data, family = poisson)
m7 = glm(ED_presentations~mean_DMT+mean_High_DMT_Flag, data = patient_data, family = poisson)
m8 = glm(ED_presentations~mean_OZONE+mean_PM10, data = patient_data, family = poisson)
m9 = glm(ED_presentations~mean_OZONE+mean_High_DMT_Flag, data = patient_data, family = poisson)
m10 = glm(ED_presentations~mean_PM10+mean_High_DMT_Flag, data = patient_data, family = poisson)
m11 = glm(ED_presentations~mean_DMT+mean_OZONE+mean_PM10, data = patient_data, family = poisson)
m12 = glm(ED_presentations~mean_DMT+mean_OZONE+mean_High_DMT_Flag, data = patient_data, family = poisson)
m13 = glm(ED_presentations~mean_DMT+mean_PM10+mean_High_DMT_Flag, data = patient_data, family = poisson)
m14 = glm(ED_presentations~mean_OZONE+mean_PM10+mean_High_DMT_Flag, data = patient_data, family = poisson)
m15 = glm(ED_presentations~mean_DMT+mean_OZONE+mean_PM10+mean_High_DMT_Flag, data = patient_data, family = poisson)

# cv errors
cv_ad_error = rep(0,15)
cv_ad_error[1] = cv.glm(patient_data, m1, K=10)$delta[1]
cv_ad_error[2] = cv.glm(patient_data, m2, K=10)$delta[1]
cv_ad_error[3] = cv.glm(patient_data, m3, K=10)$delta[1]
cv_ad_error[4] = cv.glm(patient_data, m4, K=10)$delta[1]
cv_ad_error[5] = cv.glm(patient_data, m5, K=10)$delta[1]
cv_ad_error[6] = cv.glm(patient_data, m6, K=10)$delta[1]
cv_ad_error[7] = cv.glm(patient_data, m7, K=10)$delta[1]
cv_ad_error[8] = cv.glm(patient_data, m8, K=10)$delta[1]
cv_ad_error[9] = cv.glm(patient_data, m9, K=10)$delta[1]
cv_ad_error[10] = cv.glm(patient_data, m10, K=10)$delta[1]
cv_ad_error[11] = cv.glm(patient_data, m11, K=10)$delta[1]
cv_ad_error[12] = cv.glm(patient_data, m12, K=10)$delta[1]
cv_ad_error[13] = cv.glm(patient_data, m13, K=10)$delta[1]
cv_ad_error[14] = cv.glm(patient_data, m14, K=10)$delta[1]
cv_ad_error[15] = cv.glm(patient_data, m15, K=10)$delta[1]

cv_ad_error[order(cv_ad_error)[1]] # lowest MSE

plot(cv_ad_error~c(1:15), type = 'b', xlab = 'model', ylab = '10-fold CV - MSE')
# mean DMT and mean OZONE

# ------- check linear or nonlinear ---------
cv_10f_error = rep(0,10)
for (i in 1:10){
  cv_10f = glm(ED_presentations~poly(mean_DMT,i)+poly(mean_OZONE,i),
               data = patient_data)
  cv_10f_error[i] = cv.glm(patient_data, cv_10f, K=10)$delta[1] #delta[1] is cv raw error
}

plot(c(1:10), cv_10f_error, type = "b", xlab = 'polynomial order', ylab = '10-fold CV - MSE')

bmind = order(cv_10f_error)[1] #find the lowest MSE
lowest_MSE = cv_10f_error[bmind] #find the model that had the lowest MSE
cat("Model", bmind, " has the lowest MSE at", lowest_MSE)
