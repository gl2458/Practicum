install.packages("lcmm")

library(lcmm)
library(plyr)
library(tidyverse)

#data 

cmpst_all2 <- read_csv(file = "/Users/rachellee/Google Drive/Practicum/Data/cmpst_all2.csv") %>%
  janitor::clean_names() %>%
  filter(visit == "Baseline" | visit == "Week3" | visit == "Week6" | visit == "Week9" | visit == "Week14")

cmpst_all2$visit <- revalue(cmpst_all2$visit, c("Baseline"=0, "Week3" = 3, "Week6" =6, "Week9"=9, "Week14"=14)) %>%
  as.numeric(cmpst_all2$visit)

head(cmpst_all2)

summary(cmpst_all2)


#exposure: madrs_total, pos_affect, neg_affect
#outcome: ham24tot

lcmm(ham24tot ~ visit + madrs_total, mixture= 'visit', subject = 'patient_id', ng = 2, data = cmpst_all2)




