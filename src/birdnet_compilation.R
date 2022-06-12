library(dplyr) #data manipulation
library(tibble) #data manipulation
library(lubridate) #manipulating date and time
library(hms) #manipulate time
library(zoo) #for na.approx to approximate missing values in weather dataset
library(ggplot2) #graphs
library(gridExtra) #ggplot multi panels
library(cowplot)
library(lme4) #lmm and glmm analysis
library(lmerTest) #get p-values for lmm tests
library(reshape2) #???
library(suncalc)
library(zoo)
library(car)
library(multcomp) #posthoc tests for ANOVA type III effects
library(bbmle) #AIC comparisons


# Load CSV Files ----------------------------------------------------------
arus = as.list(c("aru01","aru02","aru03","aru04","aru05","ws01","ws02","ws03","ws04","ws05","ws06","ws11","ws12","ws13","ws14","ws15"))
# arus= as.list(c("aru01","aru02","aru03","aru04","aru05","wg01", "wg02", "wg03", "wg04", "wg05"))

results_final = NULL
for(i in arus){
  setwd(paste0("/Volumes/LaCie/aridity_project/sswma/sswma_audio_files/",i))
  
  # setwd(paste0("C:/Users/meely/OneDrive - University of Oklahoma/University of Oklahoma/Ross Lab/Aridity and Song Attenuation/Sound Analysis/birdnet_analysis/validation_data/sswma/pabu_filtered/",
        # i,"_pabu_filtered"))
  birdnet = list.files(pattern = "BirdNET.results.csv")
  for(j in 1:length(birdnet)){
    
      results_temp = read.csv(birdnet[[j]], header = TRUE, sep = ",", fill = TRUE)
      if(nrow(results_temp) == 0){
        results_temp = rbind(results_temp, data.frame("filepath"= NA, "start" = NA, "end" = NA, "scientific_name" = NA, "common_name" = NA, "confidence" = NA, "lat" = NA, "lon" = NA, "week" = NA, "overlap" = NA, "sensitivity" = NA, "min_conf" = NA, "species_list" = NA, "model" = NA))
      }
      results_temp$date_time = as_datetime(substr(birdnet[[j]],1,15)) #change to 15,30 if you are using bird filtered data
      results_temp$date = date(results_temp$date_time)
      results_temp$time = hms(substr(results_temp$date_time,10,15))
      results_temp$aru = i   
      results_temp$site = "sswma"
    results_final = rbind(results_final,results_temp)
  }

}

sswma_aru_results = results_final %>% dplyr::filter(is.na(confidence) != TRUE)
# setwd("C:/Users/meely/OneDrive - University of Oklahoma/University of Oklahoma/Ross Lab/Aridity and Song Attenuation/Sound Analysis/birdnet_analysis/validation_data/")
setwd("/Volumes/LaCie/aridity_project/sswma/sswma_audio_files/")
save(sswma_aru_results, file = "sswma_aru_results.Rdata")
gc(reset = TRUE)
setwd("/Volumes/LaCie/aridity_project/")
birdnet_data = rbind(lwma_aru_results,sswma_aru_results,cbma_aru_results,kiowa_aru_results)
save(birdnet_data, file = "birdnet_data.Rdata")

# Combining Site Datasets, adding sunlight and sunaltitude variables, and adding weather data from NiceMapR --------------------------------------
sites = as.list(c("lwma","sswma","cbma","kiowa"))
# sites = as.list(c("sswma"))
arid_full = NULL
water_full = NULL
for(s in sites){
  setwd("C:/Users/meely/OneDrive - University of Oklahoma/University of Oklahoma/Ross Lab/Aridity and Song Attenuation/Sound Analysis/data_clean/")
  
  if(s == "lwma"){
    load("lwma_aru_results.Rdata")
    data = lwma_aru_results
    load("weather_data/lwma_ncep_weather.Rdata")
    weather_data = lwma_data
  } else if(s == "sswma"){
    load("sswma_aru_results.Rdata")
    data = sswma_aru_results
    load("weather_data/sswma_ncep_weather.Rdata")
    weather_data = sswma_data
  } else if(s == "cbma"){
    load("cbma_aru_results.Rdata")
    data = cbma_aru_results
    load("weather_data/cbma_ncep_weather.Rdata")
    weather_data = cbma_data
  } else if(s == "kiowa"){
    load("kiowa_aru_results.Rdata")
    data = kiowa_aru_results
    load("weather_data/kiowa_ncep_weather.Rdata")
    weather_data = kiowa_data
  }
  
  
  if(s == "kiowa"){
    tz = "US/Mountain"
    #i want to only subtract 3600s from 06/17/21 and 06/18/21
  } else {
    tz = "US/Central"
  }
  
  data_temp = data %>%
    mutate(date = as_date(date),
           date_time = force_tz(date_time, tz = tz),
           time = as_hms(date_time),
           hour = hour(date_time)) %>%
    # filter(date > "2021-04-30 CDT")%>%
    dplyr::filter(is.na(common_name) == FALSE) %>% 
    group_by(date,time,date_time,hour,lat,lon,aru,site) %>%
    summarise(num_vocals = n(),
              species_diversity = n_distinct(common_name))%>%
    arrange(date_time) 
    if(s == "kiowa"){
      kiowa_time_bad = data_temp %>% dplyr::filter(hour ==12)%>%
        mutate(date_time = date_time-3600,
               time = as_hms(date_time),
               hour = hour(date_time))
      kiowa_time_good = data_temp %>% dplyr::filter(hour !=12)
      data_temp = rbind(kiowa_time_bad,kiowa_time_good) %>%
        arrange(date_time)

    } else {
    
  }
  
  sunrise_final = NULL
  for(i in 1:length(data_temp$date_time)){
    sunrise_time = getSunlightTimes(date = data_temp$date[i], lat = data_temp$lat[i], lon = data_temp$lon[i], tz = tz, keep = c("sunrise"))
    sunrise_loc = getSunlightPosition(date = as.POSIXct(data_temp$date_time[i]), lat = data_temp$lat[i], lon = data_temp$lon[i], keep = c("altitude"))
    sunrise_time$altitude = sunrise_loc$altitude
    sunrise_time$date_time = data_temp$date_time[i]
    sunrise_time$site = data_temp$site[i]
    sunrise_time$aru = data_temp$aru[i]
    
    sunrise_final = rbind(sunrise_time,sunrise_final)
  }
  
  sunrise_final = sunrise_final %>%
    # mutate(date = as_date(date, tz = tz))%>%
    arrange(date)
  
  data_temp = inner_join(data_temp,sunrise_final, by = c("site","aru","date", "date_time","lat","lon"))
  
  # data_temp$sunrise = sunrise_final$sunrise
  # data_temp$sun_alt = sunrise_final$altitude
  
  weather_data = weather_data %>%
    mutate(date_time = local_time) %>%
    arrange(date_time)
  data_temp = left_join(data_temp, weather_data, by = "date_time")
  
  data_temp = data_temp %>% 
    arrange(date,aru)
  data_temp$temperature = na.approx(data_temp$temperature, na.rm = FALSE)
  data_temp$humidity = na.approx(data_temp$humidity, na.rm = FALSE)
  data_temp$pressure = na.approx(data_temp$pressure, na.rm = FALSE)
  data_temp$windspeed = na.approx(data_temp$windspeed, na.rm = FALSE)
  data_temp$winddir = na.approx(data_temp$winddir, na.rm = FALSE)
  data_temp$emissivity = na.approx(data_temp$emissivity, na.rm = FALSE)
  data_temp$cloudcover = na.approx(data_temp$cloudcover, na.rm = FALSE)
  data_temp$netlong = na.approx(data_temp$netlong, na.rm = FALSE)
  data_temp$uplong = na.approx(data_temp$uplong, na.rm = FALSE)
  data_temp$downlong = na.approx(data_temp$downlong, na.rm = FALSE)
  data_temp$rad_dni = na.approx(data_temp$rad_dni, na.rm = FALSE)
  data_temp$rad_dif = na.approx(data_temp$rad_dif, na.rm = FALSE)
  data_temp$szenith = na.approx(data_temp$szenith, na.rm = FALSE)
  data_temp$relh = na.approx(data_temp$relh, na.rm = FALSE)
  
  data_temp = data_temp %>%
    filter(is.na(temperature) == FALSE) %>% 
    mutate(mas = as.numeric(difftime(date_time,sunrise,units = c("mins"))),
           # bin1 = seq(from = min(mas), to = max(mas), by=5),
           dew = temperature-((100-relh)/5),
           arid = -dew,
           site = factor(site, levels=c("lwma","sswma","cbma","kiowa")))
  
  data_temp_arid = data_temp%>%
    filter(aru == "aru01" | aru == "aru02"| aru == "aru03"| aru == "aru04"| aru == "aru05")
  
  data_temp_water = data_temp %>%
  filter(aru == "ws01" | aru == "ws02"| aru == "ws03"| aru == "ws04"| aru == "ws05" |
         aru == "ws06" | aru == "ws07"| aru == "ws08"| aru == "ws09"| aru == "ws10" | 
         aru == "ws11" | aru == "ws12"| aru == "ws13"| aru == "ws14"| aru == "ws15" |
         aru == "wg01" | aru == "wg02"| aru == "wg03"| aru == "wg04"| aru == "wg05")
  
  arid_full = rbind(data_temp_arid,arid_full) %>%
    arrange(site,aru,hour)
  
  water_full = rbind(data_temp_water,water_full) %>%
    arrange(site,aru,hour)
  
}

# Saving Compilation Results ----------------------------------------------
#Aridity Gradient Data for statistical analyses
full_arid = arid_full %>% #saving it a different name so you don't overwrite it
  dplyr::filter(is.na(mas)==FALSE) %>%
  dplyr::filter(hour <13) %>%
  dplyr::filter(year(date) != 2106) %>%
  mutate(id = paste0(site,"_",aru),
         arid =(1/dew))

full_arid$hour_utc = ifelse(full_arid$site == "kiowa", full_arid$hour+6, full_arid$hour+5)

max(full_arid$mas)
which.max(full_arid$mas)
mas_check = full_arid[which.max(full_arid$mas),];mas_check
setwd("C:/Users/meely/OneDrive - University of Oklahoma/University of Oklahoma/Ross Lab/Aridity and Song Attenuation/Sound Analysis/data_clean/")
save(full_arid, file = "aridity_gradient_ml.Rdata")
load("aridity_gradient_ml.Rdata")


# Water Supplementation Data Compilation ----------------------------------

full_water = water_full %>% #saving it a different name so you don't overwrite it
  dplyr::filter(is.na(mas)==FALSE) %>%
  dplyr::filter(hour <13) %>%
  dplyr::filter(year(date) != 2106) %>%
  dplyr::filter(is.na(mas)==FALSE)%>%
  mutate(id = paste0(site,"_",aru),
         arid = (1/dew))

full_water$hour_utc = ifelse(full_water$site == "kiowa", full_water$hour+6, full_water$hour+5)

full_water1 = full_water %>%
  filter(aru == "wg01" | aru == "wg02" | aru == "wg03") %>%
  mutate(water = ifelse(date >= "2021-06-04" & date <"2021-06-25"| date >= "2021-07-19" & date < "2021-08-02", 0,1),
         ws_site = 1) #1 = water access open

full_water2 = full_water %>%
  filter(aru == "wg04" | aru == "wg05") %>%
  mutate(water = 1,
         ws_site = 2)

cbma_full_water = rbind(full_water1, full_water2)


sswma_full_water1 = full_water %>%
  filter(aru == "ws01"| aru == "ws02"| aru == "ws03"| aru == "ws04"| aru == "ws05")%>%
  mutate(water = ifelse(date >= "2021-05-17" & date <"2021-05-30"| date >= "2021-06-13" & date < "2021-07-02", 1,0),
         ws_site = 1)

sswma_full_water2 = full_water %>%
  filter(aru == "ws06"| aru == "ws07"| aru == "ws08"| aru == "ws09"| aru == "ws10") %>%
  mutate(water = ifelse(date >= "2021-05-30" & date <"2021-06-12"| date >= "2021-07-03" & date < "2021-08-07", 1,0),
         ws_site = 2)

sswma_full_water3 = full_water %>%
  filter(aru == "ws11"| aru == "ws12"| aru == "ws13"| aru == "ws14"| aru == "ws15") %>%
  mutate(water = 0,
         ws_site = 3)

sswma_full_water = rbind(sswma_full_water1, sswma_full_water2, sswma_full_water3)
water_compiled = rbind(cbma_full_water,sswma_full_water)


#Checking to see if there are any outliers in the minutes after sunrise
max(water_compiled$mas)
which.max(water_compiled$mas)
mas_check = water_compiled[which.max(water_compiled$mas),];mas_check
  
setwd("C:/Users/meely/OneDrive - University of Oklahoma/University of Oklahoma/Ross Lab/Aridity and Song Attenuation/Sound Analysis/birdnet_analysis/data_clean/")
save(water_compiled, file = "water_supp_ml.Rdata")
load("water_supp_ml.Rdata")


# Averaging Data Across Hours and Months ----------------------------------
#Data is averaged by hour across aru and breeding season
graph_arid_hour = full_arid %>%
  dplyr::filter(is.na(arid) == FALSE) %>%
  arrange(site,aru,hour) %>%
  group_by(site,lat,lon,hour,hour_utc) %>%
  summarise(mean_sunalt = mean(altitude),
            mean_num_vocals = mean(num_vocals),
            mean_species = mean(species_diversity),
            mean_dew = mean(dew),
            mean_arid = mean(arid))%>%
  arrange(hour)

#Plotting mean num_vocals across sun altitude per hour
ggplot(data = full_arid, aes(x = date_time, y = altitude, color = site))+ #sun altitude does not overlap between sites, so good metric for time AND location
  geom_line()

#creating two panel graph for dew temperature fluctuation and 
hour_species = ggplot(data = graph_arid_hour) +
  geom_line(aes(x = hour_utc, y = mean_num_vocals, color = site))

hour_arid = ggplot(data = graph_arid_hour) +
  geom_bar(aes(x = hour_utc, y = mean_dew, fill = site), stat = "identity", position = position_dodge())

hour_out <- plot_grid(hour_species, hour_arid, align = "v", ncol = 1, rel_heights = c(.5, .5))
plot(hour_out)

  labs(x="Year",y="Number of Courses Sold")+
  scale_y_continuous(sec.axis=sec_axis(mean_arid,name="Aridity"))
  labs(title = "Correlation Matrix of Validation Data, Acoustic Indices, and BirdNet-Lite Analysis", 
       x = "", y = "", fill = "Correlation \n Measure")
  geom_text(aes(x = Var1, y = Var2, label = round(value, 2)), color = "black", 
            fontface = "bold", size = 5)+
  theme_classic(base_size = 20) +
  theme(axis.text.x = element_text(angle = 45, hjust=1))

graph_arid_week = full_arid %>%
  arrange(site,aru,date_time) %>%
  mutate(week = week(date_time))%>%
  group_by(site,lat,lon,week) %>%
  summarise(mean_sunalt = mean(altitude),
            mean_num_vocals = mean(num_vocals),
            mean_species = mean(species_diversity),
            mean_dew = mean(dew),
            mean_arid = mean(arid))

vocals_week = ggplot(data = graph_arid_week) +
  geom_line(aes(x = mean_sunalt, y = mean_dew, color = site));vocals_week

arid_week = ggplot(data = graph_arid_week) +
  geom_bar(aes(x = week, y = mean_dew, fill = site), stat = "identity", position = position_dodge())
week_out <- plot_grid(vocals_week, arid_week, align = "v", ncol = 1, rel_heights = c(.5, .5))
plot(week_out)

graph_arid_month = full_arid %>%
  arrange(site,aru,hour) %>%
  mutate(month = month(date_time))%>%
  group_by(site,lat,lon,month) %>%
  summarise(mean_sunalt = mean(altitude),
            mean_num_vocals = mean(num_vocals),
            mean_species = mean(species_diversity),
            mean_dew = mean(dew),
            mean_arid = mean(arid))

month_species = ggplot(data = graph_arid_month) +
  geom_line(aes(x = month, y = mean_species, color = site))

month_arid = ggplot(data = graph_arid_month) +
  geom_bar(aes(x = month, y = mean_dew, fill = site), stat = "identity", position = position_dodge())

month_out <- plot_grid(month_species, month_arid, align = "v", ncol = 1, rel_heights = c(.5, .5))
plot(month_out)
  

setwd("C:/Users/meely/OneDrive - University of Oklahoma/University of Oklahoma/Ross Lab/Aridity and Song Attenuation/Sound Analysis/birdnet_analysis/data_clean/")
save(graph_hour, file = "graph_hour.Rdata")

# Testing for Collinearity ------------------------------------------------
cor_arid = full_arid %>%
  dplyr::select(site,aru,arid,mas,altitude,num_vocals,species_diversity)
cor(cor_arid[,c(6,9:13)])
cor.test(cor_arid$num_vocals,cor_arid$species_diversity)
#altitude and mas are highly positively correlated so cannot use them in same model
# Testing for Best Random effect -------------------------------------------
re1 = glmer(num_vocals ~ altitude + arid + (1|site), family = "poisson",data = full_arid)
re2 = glmer(num_vocals ~ altitude + arid + (1|aru), family = "poisson",data = full_arid)
re3 = glmer(num_vocals ~ altitude + arid + (1|site/aru), family = "poisson",data = full_arid)
AICctab(re1,re2,re3, nobs = 44597, base=TRUE,delta=TRUE, sort=TRUE, weights=TRUE)
#best random effect is aru nested in site
setwd("C:/Users/meely/OneDrive - University of Oklahoma/University of Oklahoma/Ross Lab/Aridity and Song Attenuation/Sound Analysis/birdnet_analysis/data_clean/")

v1 = glmer(num_vocals ~ 1 + (1|site/aru), family = "poisson", data = full_arid)
v2 = glmer(num_vocals ~ arid + (1|site/aru), family = "poisson", data = full_arid)
v3 = glmer(num_vocals ~ altitude + (1|site/aru), family = "poisson", data = full_arid)
v4 = glmer(num_vocals ~ altitude+arid + (1|site/aru), family = "poisson", data = full_arid)
v5 = glmer(num_vocals ~ scale(mas) + (1|site/aru), family = "poisson", data = full_arid)
v6 = glmer(num_vocals ~ scale(mas)+arid + (1|site/aru), family = "poisson", data = full_arid)
AICctab(v1,v2,v3,v4,v5,v6, nobs = 44597, base=TRUE,delta=TRUE, sort=TRUE, weights=TRUE)

summary(v4) #as altitude increases (further west/increasing time of day) number of vocalizations decrease, and as aridity increases number of vocalizations decrease
# Anova(v4, type = "III")
# contrasts = glht(v4, linfct = mcp(site = "Tukey"))
# summary(contrasts)

#Mean Species Diversity
s1 = glmer(species_diversity ~ 1 + (1|site/aru), family = "poisson", data = full_arid)
s2 = glmer(species_diversity ~ arid + (1|site/aru), family = "poisson", data = full_arid)
s3 = glmer(species_diversity ~ altitude + (1|site/aru), family = "poisson", data = full_arid)
s4 = glmer(species_diversity ~ altitude+arid + (1|site/aru), family = "poisson", data = full_arid)
s5 = glmer(species_diversity ~ scale(mas) + (1|site/aru), family = "poisson", data= full_arid)
s6 = glmer(species_diversity ~ scale(mas) +arid + (1|site/aru), family = "poisson", data= full_arid)
AICctab(s1,s2,s3,s4,s5,s6, nobs = 44597, base=TRUE,delta=TRUE, sort=TRUE, weights=TRUE)

summary(s2) #as aridity increases, species diversity decreases
summary(s6) #ardity increases, species diversity decreases, no significant effect of scale(mas)
summary(s4) #ardity increases, species diversity decreases, no significant effect of altitude

# Aridity Gradient Dataset WITHOUT OUTLIER!!!! ----------------------------
which.max(full_arid$arid)
full_arid$arid[34191,]
out_df = full_arid %>% dplyr::filter(site == "kiowa" && date == "2021-05-19")
ag_noout = full_arid %>% dplyr::filter(dew > 0.65)

# No Outlier - Testing for Best Random effect -------------------------------------------
cor_arid = ag_noout %>%
  dplyr::select(site,aru,arid,mas,altitude,num_vocals,species_diversity)
cor(cor_arid[,c(6,9:13)]) #no significant correlations between covariates
cor.test(cor_arid$num_vocals,cor_arid$species_diversity)

re1out = glmer(num_vocals ~ altitude + arid + (1|site), family = "poisson",data = ag_noout)
re2out = glmer(num_vocals ~ altitude + arid + (1|aru), family = "poisson",data = ag_noout)
re3out = glmer(num_vocals ~ altitude + arid + (1|site/aru), family = "poisson",data = ag_noout)
AICctab(re1out,re2out,re3out, nobs = 44594, base=TRUE,delta=TRUE, sort=TRUE, weights=TRUE)
#best random effect is aru nested in site
setwd("C:/Users/meely/OneDrive - University of Oklahoma/University of Oklahoma/Ross Lab/Aridity and Song Attenuation/Sound Analysis/birdnet_analysis/data_clean/")

v1out = glmer(num_vocals ~ 1 + (1|site/aru), family = "poisson", data = ag_noout)
v2out = glmer(num_vocals ~ arid + (1|site/aru), family = "poisson", data = ag_noout)
v3out = glmer(num_vocals ~ altitude + (1|site/aru), family = "poisson", data = ag_noout)
v4out = glmer(num_vocals ~ altitude+arid + (1|site/aru), family = "poisson", data = ag_noout)
v5out = glmer(num_vocals ~ scale(mas) + (1|site/aru), family = "poisson", data = ag_noout)
v6out = glmer(num_vocals ~ scale(mas)+arid + (1|site/aru), family = "poisson", data = ag_noout)

AICctab(v1out,v2out,v3out,v4out,v5out,v6out, nobs = 44594, base=TRUE,delta=TRUE, sort=TRUE, weights=TRUE)


summary(v4out) #as altitude increases (further west/later in the day) number of vocalizations decrease, and as aridity decreases number of vocalizations increase
# Anova(v4, type = "III")
# contrasts = glht(v4, linfct = mcp(site = "Tukey"))
# summary(contrasts)

# Mean Species Diversity = No Aridity Outlier! ----------------------------
#Mean Species Diversity - No Outlier
s1out = glmer(species_diversity ~ 1 + (1|site/aru), family = "poisson", data = ag_noout)
s2out = glmer(species_diversity ~ arid + (1|site/aru), family = "poisson", data = ag_noout)
s3out = glmer(species_diversity ~ altitude + (1|site/aru), family = "poisson", data = ag_noout)
s4out = glmer(species_diversity ~ altitude+arid + (1|site/aru), family = "poisson", data = ag_noout)
s5out = glmer(species_diversity ~ scale(mas) + (1|site/aru), family = "poisson", data = ag_noout)
s6out = glmer(species_diversity ~ scale(mas)+arid + (1|site/aru), family = "poisson", data = ag_noout)

AICctab(s1out,s2out,s3out,s4out,s5out,s6out, nobs = 44594, base=TRUE,delta=TRUE, sort=TRUE, weights=TRUE)




summary(s2out) #as aridity increases, species diversity decreases
summary(s6out) #as aridity increases, species diversity decreases, no effect of scale(mas)
summary(s4out) #as aridity increases, species diversity decreases, no effect of altitude


# Water Supplementation Experiment ----------------------------------------

water_compiled$water_int = interaction(water_compiled$site,water_compiled$ws_site,water_compiled$water)
wre1 = glmer(num_vocals ~ altitude+water_int+ (1|site), family = "poisson", data = water_compiled)
wre2 = glmer(num_vocals ~ altitude+water_int + (1|aru), family = "poisson", data = water_compiled)
wre3 = glmer(num_vocals ~ altitude+water_int + (1|site/aru), family = "poisson", data = water_compiled)
AICctab(wre1,wre2,wre3, nobs = 34998, base=TRUE,delta=TRUE, sort=TRUE, weights=TRUE)
#if site is included in the water_int interaction variable, then aru is the best random effect
#would be better to analyze water sswma and cbma separatley?

#Water Supplementation - Number of Vocalizations
wv1 = glmer(num_vocals ~ 1 + (1|aru), family = "poisson", data = water_compiled)
wv2 = glmer(num_vocals ~ arid + (1|aru), family = "poisson", data = water_compiled)
wv3 = glmer(num_vocals ~ altitude + (1|aru), family = "poisson", data = water_compiled)
wv4 = glmer(num_vocals ~ water_int + (1|aru), family = "poisson", data = water_compiled)
wv5 = glmer(num_vocals ~ water_int+arid + (1|aru), family = "poisson", data = water_compiled)
wv6 = glmer(num_vocals ~ water_int+altitude + (1|aru), family = "poisson", data = water_compiled)
AICctab(wv1,wv2,wv3,wv4,wv5,wv6, nobs = 34998, base=TRUE,delta=TRUE, sort=TRUE, weights=TRUE)

summary(wv6)
# Running Water Supplementation SSWMA and CBMA Separately -----------------
water_compiled$water_int2 = interaction(water_compiled$ws_site,water_compiled$water)
water_compiled$water_int3 = interaction(water_compiled$ws_site,water_compiled$water,water_compiled$arid)

sswma_water = water_compiled %>%
  dplyr::filter(site == "sswma")
cbma_water = water_compiled %>%
  dplyr::filter(site == "cbma")

#SSWMA Water Supplementation GLMMs
swv1 = glmer(num_vocals ~ 1 + (1|aru), family = "poisson", data = sswma_water)
swv2 = glmer(num_vocals ~ arid + (1|aru), family = "poisson", data = sswma_water)
swv3 = glmer(num_vocals ~ altitude + (1|aru), family = "poisson", data = sswma_water)
swv4 = glmer(num_vocals ~ water_int2 + (1|aru), family = "poisson", data = sswma_water)
swv5 = glmer(num_vocals ~ water_int2+arid + (1|aru), family = "poisson", data = sswma_water)
swv6 = glmer(num_vocals ~ water_int2+altitude + (1|aru), family = "poisson", data = sswma_water)
swv7 = glmer(num_vocals ~ water_int2+scale(mas) + (1|aru), family = "poisson", data = sswma_water)
AICctab(swv1,swv2,swv3,swv4,swv5,swv6,swv7,nobs = 22674, base=TRUE,delta=TRUE, sort=TRUE, weights=TRUE)

summary(swv6) #water interaction and altitude are most significant factors, num vocals decreases with increasing altitude, and ws_site 1 increases with water, but ws_site 2 decreases with water
Anova(swv6, type = "III")
contrasts = glht(swv6, linfct = mcp(water_int2 = "Tukey"))
summary(contrasts)

##CBMA Water Supplementation GLMMs
cwv1 = glmer(num_vocals ~ 1 + (1|aru), family = "poisson", data = cbma_water)
cwv2 = glmer(num_vocals ~ arid + (1|aru), family = "poisson", data = cbma_water)
cwv3 = glmer(num_vocals ~ altitude + (1|aru), family = "poisson", data = cbma_water)
cwv4 = glmer(num_vocals ~ water_int2 + (1|aru), family = "poisson", data = cbma_water)
cwv5 = glmer(num_vocals ~ water_int2+arid + (1|aru), family = "poisson", data = cbma_water)
cwv6 = glmer(num_vocals ~ water_int2+altitude + (1|aru), family = "poisson", data = cbma_water)
cwv7 = glmer(num_vocals ~ water_int2+scale(mas) + (1|aru), family = "poisson", data = cbma_water)
AICctab(cwv1,cwv2,cwv3,cwv4,cwv5,cwv6,cwv7, nobs = 12324, base=TRUE,delta=TRUE, sort=TRUE, weights=TRUE)


summary(cwv6) #num vocals decrease with altitude, increase with access to water in site 1
Anova(cwv6, type = "III")
contrasts = glht(cwv6, linfct = mcp(water_int2 = "Tukey"))
summary(contrasts)


# Water Supplementation Species Diversity GLMMs --------------------------------------------------
#SSWMA Water Supplementation GLMMs
swsd1 = glmer(species_diversity ~ 1 + (1|aru), family = "poisson", data = sswma_water)
swsd2 = glmer(species_diversity ~ arid + (1|aru), family = "poisson", data = sswma_water)
swsd3 = glmer(species_diversity ~ altitude + (1|aru), family = "poisson", data = sswma_water)
swsd4 = glmer(species_diversity ~ water_int2 + (1|aru), family = "poisson", data = sswma_water)
swsd5 = glmer(species_diversity ~ water_int2+arid + (1|aru), family = "poisson", data = sswma_water)
swsd6 = glmer(species_diversity ~ water_int2+altitude + (1|aru), family = "poisson", data = sswma_water)
swsd7 = glmer(species_diversity ~ water_int2+scale(mas) + (1|aru), family = "poisson", data = sswma_water)
AICctab(swsd1,swsd2,swsd3,swsd4,swsd5,swsd6,swsd7, nobs = 22674, base=TRUE,delta=TRUE, sort=TRUE, weights=TRUE)

summary(swv5) #species diversity increases with aridity
Anova(swv5, type = "III")
contrasts = glht(swv5, linfct = mcp(water_int2 = "Tukey"))
summary(contrasts) #water increases species_diversity at ws_site 1, but decreases at ws_site 2

##CBMA Water Supplementation GLMMs
cwsd1 = glmer(species_diversity ~ 1 + (1|aru), family = "poisson", data = cbma_water)
cwsd2 = glmer(species_diversity ~ arid + (1|aru), family = "poisson", data = cbma_water)
cwsd3 = glmer(species_diversity ~ altitude + (1|aru), family = "poisson", data = cbma_water)
cwsd4 = glmer(species_diversity ~ water_int2 + (1|aru), family = "poisson", data = cbma_water)
cwsd5 = glmer(species_diversity ~ water_int2+arid + (1|aru), family = "poisson", data = cbma_water)
cwsd6 = glmer(species_diversity ~ water_int2+altitude + (1|aru), family = "poisson", data = cbma_water)
cwsd7 = glmer(species_diversity ~ water_int2+scale(mas) + (1|aru), family = "poisson", data = sswma_water)
AICctab(cwsd1,cwsd2,cwsd3,cwsd4,cwsd5,cwsd6,cwsd7, nobs = 12324, base=TRUE,delta=TRUE, sort=TRUE, weights=TRUE)


summary(cwsd5) #as aridity increases so does species diversity
Anova(cwsd5, type = "III")
contrasts = glht(cwsd5, linfct = mcp(water_int2 = "Tukey"))
summary(contrasts) #presence of water increases species diversity across ws_site 1, and ws_site 2 has higher species diversity than ws_site 1 with restrected access, but is not significantly different (p = 0.0698) from ws_site 1 with access to water


# Checking Unique Species at each Site ------------------------------------
setwd("C:/Users/meely/OneDrive - University of Oklahoma/University of Oklahoma/Ross Lab/Aridity and Song Attenuation/Sound Analysis/birdnet_analysis/data_clean/")
load("birdnet_data.Rdata")
lwma_ag = birdnet_data %>%
  dplyr::filter(site == "lwma")%>%
  dplyr::filter(common_name == "Northern Shoveler")%>%
  summarize(species = unique(common_name))


# Comparing against Manual Detections in Validation Files -----------------
setwd("C:/Users/meely/OneDrive - University of Oklahoma/University of Oklahoma/Ross Lab/Aridity and Song Attenuation/Sound Analysis/birdnet_analysis/validation_data/")
arus = as.list(c("aru01","aru04","aru05","ws01","ws02","ws03","ws04","ws05","ws06","ws11","ws12","ws13","ws14","ws15"))
valid_final = NULL
for(i in arus){
   setwd(paste0("C:/Users/meely/OneDrive - University of Oklahoma/University of Oklahoma/Ross Lab/Aridity and Song Attenuation/Sound Analysis/birdnet_analysis/validation_data/sswma/",
                i))
   birdnet = list.files(pattern = ".txt")
   for(j in 1:length(birdnet)){
      
      valid_temp = read.table(birdnet[[j]], header = FALSE, sep = "", fill = TRUE)
      names(valid_temp) = c("start","end","id") 
      valid_temp$id = substr(valid_temp$id,1,4)
      valid_temp$date_time = as_datetime(substr(birdnet[[j]],1,15))
      valid_temp$date = date(valid_temp$date_time)
      valid_temp$time = hms(substr(valid_temp$date_time,12,19))
      valid_temp$aru = i   
      valid_temp$site = "sswma"
      valid_final= rbind(valid_final,valid_temp)
   }
   
}

#Saving validation data
sswma_valid = valid_final
valid_species_hour = sswma_valid %>%
   filter(time@hour <=13) %>% 
   group_by(date_time,aru,site) %>%
   summarise(n = n(),
species_diversity = n_distinct(id))
setwd("C:/Users/meely/OneDrive - University of Oklahoma/University of Oklahoma/Ross Lab/Aridity and Song Attenuation/Sound Analysis/birdnet_analysis/data_clean/")
write.csv(valid_species_hour, "validation_diversity.csv", row.names = FALSE)
# rename(time = "time@hour") %>%

mmp_valid = full_join(valid_species_hour,species_hour, by = c("date_time","aru")) %>%
   rename("mmp_num_vocals" = n,
          "mmp_species_diversity" = species_diversity.x,
          "birdnet_num_vocals" = num_vocals,
          "birdnet_species_diversity" = species_diversity.y) %>%
   select(mmp_num_vocals,
          mmp_species_diversity,
          birdnet_num_vocals,
          birdnet_species_diversity) %>%
   mutate(mmp_num_vocals = as.numeric(mmp_num_vocals),
          mmp_species_diversity = as.numeric(mmp_species_diversity),
          birdnet_num_vocals = as.numeric(birdnet_num_vocals),
          birdnet_species_diversity = as.numeric(birdnet_species_diversity)) %>%
   na.omit()
mmp_cor = cor(mmp_valid[, 3:6], use ="everything")
cor.test(mmp_valid$mmp_species_diversity,mmp_valid$birdnet_species_diversity)
cor.test(mmp_valid$mmp_num_vocals, mmp_valid$birdnet_num_vocals)

setwd("C:/Users/meely/OneDrive - University of Oklahoma/University of Oklahoma/Ross Lab/Aridity and Song Attenuation/Sound Analysis/birdnet_analysis/validation_data/")
write.csv(mmp_valid, "mmp_valid_data.csv", row.names = FALSE)

# Correlating Validation Data ---------------------------------------------
setwd("C:/Users/meely/OneDrive - University of Oklahoma/University of Oklahoma/Ross Lab/Aridity and Song Attenuation/Sound Analysis/birdnet_analysis/validation_data/")

valid_data = read.csv("validation_acoustic_indices.csv", header = TRUE) %>%
   na.omit()


valid_cor = cor(valid_data[, 4:17], use ="everything")
cor.test(valid_data$user_num_vocals,valid_data$birdnet_unfiltered_num_vocals) #r = 0.244 (0.091-0.386), p = 0.002, 
cor.test(valid_data$num_vocals,valid_data$pabu_aei) #r =-0.463, p = 0.035

melted_valid <- melt(valid_cor)
head(melted_valid)
# lower_valid <- valid_cor
# 
# # Make lower triangular matrix by setting NA to upper triangular part:
# lower_valid[upper.tri(lower_valid)] <- NA
# lower_m_valid <- melt(lower_valid, na.rm = TRUE)


# Ggplot lower triangular correlation matrix:

ggplot(data = melted_valid, aes(x = Var1, y = Var2, fill = value)) +
   geom_tile() +
   scale_fill_gradient2(midpoint = 0.5, mid ="grey70", 
                        limits = c(-1, +1)) +
   labs(title = "Correlation Matrix of Validation Data, Acoustic Indices, and BirdNet-Lite Analysis", 
        x = "", y = "", fill = "Correlation \n Measure") +
   theme(plot.title = element_text(hjust = 0.5, colour = "blue"), 
         axis.title.x = element_text(face="bold", colour="darkgreen", size = 12),
         axis.title.y = element_text(face="bold", colour="darkgreen", size = 12),
         legend.title = element_text(face="bold", colour="brown", size = 10)) +
   geom_text(aes(x = Var1, y = Var2, label = round(value, 2)), color = "black", 
             fontface = "bold", size = 5)+
   theme_classic(base_size = 20) +
   theme(axis.text.x = element_text(angle = 45, hjust=1))



cbma_data$date_time = cbma_data$local_time
miss_weather = anti_join(results_final, cbma_data, by = "date_time")
combined = full_join(results_final, cbma_data, by = "date_time")
combined2 = combined %>%
  filter(year(date_time) != 1970)
combined2$temperature = na.approx(combined2$temperature) #approximate dewpoint temperatuer
combined2$relh = na.approx(combined2$relh) #approximate air temperature
combined2$pressure = na.approx(combined2$pressure) #approximate relative humidity


setwd("C:/Users/meely/OneDrive - University of Oklahoma/University of Oklahoma/Ross Lab/Aridity and Song Attenuation/Sound Analysis/birdnet_analysis/data_clean")
 # save(combined, file = "cbma_aru_species_results.Rdata")
save(combined2, file = "cbma_wg_spcecies_results.Rdata")
write.csv(combined2, file = "cbma_wg_spcecies_results.csv", row.names = FALSE)

cbma_wg_aci = read.csv("aci_water_cbma.csv", header = TRUE)
cbma_comb_data = full_join(combined2%>%dplyr::filter(aru == "wg01"),cbma_wg_aci%>%dplyr::filter(aru == "wgo1"), by = "aru")


 combined %>%
   group_by(Common.name) %>%
   filter(date_time == max(date_time)) %>%
   ungroup()

 species_hour = combined2 %>%
   group_by(date, time@hour, Common.name) %>%
   summarise(n = n(),
             temp = mean(temperature),
             relh = mean(relh)) %>%
   rename(time = "time@hour") %>%
   filter(time <=13) %>% filter(is.na(Common.name) == FALSE)
 
 ggplot(data = species_hour, aes(x = time, y = n, color = Common.name))+
   geom_line()+
   # geom_bar(stat = "identity", position = position_dodge(0.2))+
   scale_y_continuous(limits = c(0,1250)) +
   facet_wrap(~Common.name)
   
m1 = lmer(n ~ scale(date) * time + (1|Common.name), data = species_hour, REML= FALSE)
summary(m1)
m2 = lmer(n ~ temp*relh*scale(date) + (1|Common.name), data = species_hour, REML= FALSE)
summary(m2)
