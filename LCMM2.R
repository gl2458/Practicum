install.packages("lcmm")
install.packages("xlsx")
library(lcmm)
library(plyr)
library(tidyverse)
library(writexl)

#data import
cmpst_change <- read_csv(file = "/Users/rachellee/Google Drive/Practicum/Data/cmpst_all2.csv") %>%
  janitor::clean_names() %>%
  filter(visit == "Baseline" | visit == "Week3" | visit == "Week6" | visit == "Week9" | visit == "Week14")


cmpst_change <- data.frame(cmpst_change)


cmpst_hamd <- cmpst_change %>%
  select(patient_id, tx_text, gender, visit, site, ham24tot) %>%
  pivot_wider(
    names_from = "visit",
    values_from = "ham24tot",
  ) %>%
  mutate(
    change3 = Week3 - Baseline,
    change6 = Week6 - Baseline,
    change9 = Week9 - Baseline,
    change14 = Week14 - Baseline
  )



