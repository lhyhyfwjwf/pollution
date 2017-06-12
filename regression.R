
library('plm')
panel = read.csv("D:/STA 160/regression data/Dashboard.csv")

transform(panel, air_temp_avg = as.numeric(as.character(air_temp_avg)))
transform(panel, year = as.factor(as.character(air_temp_avg)))
transform(panel, air_temp_avg = as.numeric(as.character(air_temp_avg)))
transform(panel, sea_lvl_pressure_avg = as.numeric(as.character(sea_lvl_pressure_avg)))
sapply(panel, class)

panel = data.frame(panel, air_temp_avg = as.numeric(as.character(panel$air_temp_avg)),
                   sea_lvl_pressure_avg = as.numeric(as.character(panel$sea_lvl_pressure_avg)))

pm2.5 = panel$DAILY_AQI_VALUE_PM2.5
ozone = panel$DAILY_AQI_VALUE_ozone 
no2 = panel$DAILY_AQI_VALUE_no2
##fixed effect
fixed.dum <-plm(pm2.5 ~ air_temp_avg.1+dew_pt_temp_avg+sea_lvl_pressure_avg.1+l_precip_1_sum+air_temp_min+
                  air_temp_max+ weekday_dum + factor(City)+factor(season), data = panel, model = 'within')

random.dum <-plm(pm2.5 ~ air_temp_avg.1+dew_pt_temp_avg+sea_lvl_pressure_avg.1+l_precip_1_sum+air_temp_min+
                  air_temp_max+ weekday_dum + factor(City)+factor(season), data = panel, model = 'random')
phtest(fixed.dum, random.dum)



fixed.dum <-lm(pm2.5 ~ as.numeric(as.character(panel$air_temp_avg))+panel$dew_pt_temp_avg+ 
                 as.numeric(as.character(panel$sea_lvl_pressure_avg))+panel$l_precip_1_sum
               +panel$air_temp_min+panel$air_temp_max+ panel$weekday_dum+ 
                 factor(panel$City), model = 'within', na.rm = TRUE)

fixed.dum <-lm(pm2.5 ~ as.numeric(as.character(panel$air_temp_avg))+panel$dew_pt_temp_avg+ 
                 as.numeric(as.character(panel$sea_lvl_pressure_avg))+panel$l_precip_1_sum
               +panel$air_temp_min+panel$air_temp_max+ panel$weekday_dum+ 
                 factor(panel$City), model = 'within', na.rm = TRUE)





bigglm(pm2.5 ~ air_temp_avg+dew_pt_temp_avg+sea_lvl_pressure_avg+l_precip_1_sum+air_temp_min+
         air_temp_max+ weekday_dum + factor(City), data = panel, model = 'within')