# -*- coding: utf-8 -*-

##### 1. DATA DESCRIPTION #####
# importing and reading data
import pandas as pd
import matplotlib.pyplot as plt
import numpy as np
from sklearn.model_selection import train_test_split
from sklearn.preprocessing import StandardScaler
from sklearn.linear_model import Ridge
from sklearn.model_selection import cross_val_score
import matplotlib.dates as mdates

def get_RMSE_mean(model,x,y):
    scores = cross_val_score(model, x, y, scoring ="neg_mean_squared_error", cv =10)
    rmse_score = np.sqrt(-scores)
    print(f"Mean RMSE: {rmse_score.mean()}")

# read data
data = pd.read_csv(data)

# new test based on R's CV
x =  data.iloc[:,[0,2,6]] # data = 0: Date, 2: mean_DMT, 6: mean_OZONE (retain dates for plots)
y = data.iloc[:,1] # target = 1: ED presentations
x_train, x_test, y_train, y_test = train_test_split(x, y, test_size=0.3, random_state=(42))
x_test.iloc[:,0] = pd.to_datetime(x_test.iloc[:,0]) # convert to datetime for plots
# remove dates from data for fitting model
x_train = x_train.iloc[:,[1,2]]
x_test1 = x_test.iloc[:,[1,2]]

# ridge regression requires scaling
scaler = StandardScaler()
x_train_scaled = scaler.fit_transform(x_train)
x_test_scaled = scaler.transform(x_test1)

# fit ridge alpha with cv
alphas = [0.01,0.05,0.1,0.5,1,5,10,50,100,500] # alphas to test
ridge_rmse = []

for i in alphas:
    ridge = Ridge(alpha=i)
    ridge.fit(x_train_scaled, y_train)
    mse = cross_val_score(ridge,x_train_scaled, y_train,
                          scoring = 'neg_mean_squared_error', cv = 10)
    rmse = np.sqrt(-mse)
    ridge_rmse.append(rmse.mean())

ridge_rmse.index(sorted(ridge_rmse)[0])
alphas[4]

ridge_reg = Ridge(alpha=1)
ridge_reg.fit(x_train_scaled, y_train)
mse = cross_val_score(ridge_reg, x_train_scaled, y_train, scoring ="neg_mean_squared_error", cv =10)
rmse = np.sqrt(-mse)
print(f"Mean RMSE: {rmse.mean()}")
print("Ridge Regression Intercept:", ridge_reg.intercept_)
print("Ridge Regression Coefficients:", ridge_reg.coef_)

### PREDICTING ###
y_pred = ridge_reg.predict(x_test_scaled)
# append columns to x_test for plots
x_test['actual visits'] = y_test
x_test['predicted visits'] = y_pred

### PLOTTING ###
# ED presentations by Date
x_test = x_test.sort_values(by='Date')
plt.plot(x_test['Date'], x_test['predicted visits'], 
         color='red', linewidth=2, label = 'Predicted\nPresentations') 
plt.scatter(x_test['Date'], x_test['actual visits'], 
            color='blue', label = 'Actual\nPresentations') 
plt.title('Ridge Regression: SWSLHD Emergency Department Presentations in 2023')
plt.xlabel('Date')
plt.ylabel('Hospital Visits')
plt.legend(loc='lower left', fontsize=10) 
plt.gca().xaxis.set_major_formatter(mdates.DateFormatter('%b'))
plt.gca().xaxis.set_major_locator(mdates.MonthLocator())
plt.show()

# ED Presentations v. DMT
x_test = x_test.sort_values(by='mean_DMT')
plt.plot(x_test['mean_DMT'], x_test['predicted visits'], 
         color='red', linewidth=2, label = 'Predicted\nPresentations') 
plt.scatter(x_test['mean_DMT'], x_test['actual visits'], 
            color='blue', label = 'Actual\nPresentations') 
plt.xlabel('Daily Mean Temperature (°C)')
plt.ylabel('Emergency Department Presentations')
plt.legend(loc='lower right',fontsize=10)
plt.title('Ridge Regression: Emergency Department Presentations by DMT')
plt.show()

# ED Presentations v. OZONE
x_test = x_test.sort_values(by='mean_OZONE')
plt.plot(x_test['mean_OZONE'], x_test['predicted visits'], 
         color='red', linewidth=2, label = 'Predicted Presentations') 
plt.scatter(x_test['mean_OZONE'], x_test['actual visits'], 
            color='blue', label = 'Actual Presentations') 
plt.xlabel('Ozone(ppm)')
plt.ylabel('Emergency Department Presentations')
plt.legend(loc='lower right', fontsize=10) 
plt.title('Ridge Regression: Emergency Department Presentations by Ozone Levels')
plt.show()


##### FITTING RIDGE MODEL FOR ALL DATA #####
allx = x.iloc[:,[1,2]] # taking from all records
all_scaled = scaler.transform(allx) # scale x
year_pred = ridge_reg.predict(all_scaled) # predict values

year_comp = data.iloc[:,[0,1,2,6]] # combined x and y into one dataframe
year_comp.iloc[:,0] = pd.to_datetime(year_comp.iloc[:,0]) # converting dates
year_comp['predictions'] = year_pred # appending predictions onto dataframe


# new data frame with all days in 2023
dates = pd.DataFrame({'Date':pd.date_range(start='2023-01-01', 
                                           end='2023-12-31', freq='D')})
# NAs where there is missing data, avoids line plots from connecting points through missing values
dates = pd.merge(dates,year_comp,on='Date',how = 'left')

# plotting full year
plt.plot(dates['Date'], dates['ED_presentations'], color='blue', linewidth = 0.5,
            linestyle = 'dotted') 
plt.scatter(dates['Date'], dates['ED_presentations'], color='blue', alpha = 0.5,
            s = 3,label = 'Actual Presentations') 
plt.plot(dates['Date'], dates['predictions'], color='red', linewidth = 2, 
         label = 'Predicted Presentations') 
plt.legend(loc='upper center',fontsize= 10, bbox_to_anchor=(0.5, -0.2), ncol=2)
plt.gca().xaxis.set_major_formatter(mdates.DateFormatter('%b'))
plt.gca().xaxis.set_major_locator(mdates.MonthLocator())
plt.xlabel('Date')
plt.ylabel('Hospital Visits')
plt.title('SWSLHD Emergency Department Presentations in 2023')
plt.tight_layout()