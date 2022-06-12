library(dplyr)
library(tibble)
library(lubridate)
library(zoo) #for na.approx to approximate missing values in weather dataset
library(ggplot2)
library(lme4)
library(lmerTest)
library(reshape2)

#set working directory
setwd("C:/Users/meely/OneDrive - University of Oklahoma/University of Oklahoma/Ross Lab/Aridity and Song Attenuation/Sound Analysis/birdnet_analysis/validation_data/")

ci = c(0.1,0.2,0.3,0.4,0.5)
conf_matx = NULL
for(i in ci){
  
}






#load 0.5 CI csv first because it is easiest to validate
ci0.5 = read.csv("lwma/birdnet_analyzed/20210603_050000_lwma_aru04_0.5ci.csv", header = TRUE) %>%
  select(start,end,common_name)

conf_matx0.5 = read.csv("lwma/birdnet_analyzed/20210603_050000_lwma_aru04_0.5ci.csv", header = TRUE) %>%
  group_by(valid) %>% tally()

ci0.4 = read.csv("lwma/birdnet_analyzed/20210603_050000_lwma_aru04_0.4ci.csv", header = TRUE) %>%
# ci0.4 = read.delim(file = "cbma/birdnet_analyzed/20210707_070000_cbma_aru01_ebird_data_0.0overlap_0.4ci.csv", header = TRUE, sep = "\t")%>%
  select(start,end,common_name)

# ci_combined = union(ci0.4,ci0.5) %>% #combine the files and remove the duplicates
#   arrange(Begin.Time..s.)
ci0.4_shared = intersect(ci0.5,ci0.4) %>%
  mutate(valid = "tp") %>%
  arrange(start)
ci0.4_unique = anti_join(ci0.4,ci0.5) %>%
  mutate(valid = "fp")%>%
  arrange(start)
ci0.5_unique = anti_join(ci0.5,ci0.4) %>%
  mutate(valid = "fn")%>%
  arrange(start)

ci0.4_combined = rbind(ci0.4_shared,ci0.4_unique,ci0.5_unique) %>%
  arrange(start)
  
# write.csv(ci0.4_combined, "cbma/precision_recall/20210707_070000_cbma_aru01_validation_0.4ci.csv",row.names = FALSE)

conf_matx0.4 = ci0.4_combined %>%
  group_by(valid) %>% tally()



