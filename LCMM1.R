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

cmpst_all2 <- data.frame(cmpst_all2)

head(cmpst_all2)
head(cmpst_all2$patient_id)
class(cmpst_all2$patient_id)

summary(cmpst_all2)

#####################################################
# ng = 2
#####################################################
#linear link function
hamd_linear <- lcmm(ham24tot ~ visit, random = ~visit, subject = 'patient_id', mixture = ~visit, ng = 2, data = cmpst_all2, link = "linear", idiag = FALSE, na.action = 1)
summary(hamd_linear)
plot(hamd_linear, which="linkfunction",bty="l")

#beta link function
hamd_beta <- lcmm(ham24tot ~ visit, random = ~visit, subject = 'patient_id', mixture = ~visit, ng = 2, data = cmpst_all2, link = "beta", idiag = FALSE, na.action = 1)
summary(hamd_beta)
plot(hamd_beta, which="linkfunction",bty="l")

# I-splines with 3 equidistant nodes
hamd_spline3 <- lcmm(ham24tot ~ visit, random = ~visit, subject = 'patient_id', mixture = ~visit, ng = 2, data = cmpst_all2, link="3-equi-splines", idiag = FALSE, na.action = 1)
summary(hamd_spline3)
plot(hamd_spline3, which = "linkfunction", bty = "l")


# I-splines with 5 nodes at quantiles
hamd_spline5 <- lcmm(ham24tot ~ visit, random = ~visit, subject = 'patient_id', mixture = ~visit, ng = 2, data = cmpst_all2, link="5-quant-splines", idiag = FALSE, na.action = 1)
summary(hamd_spline5)
plot(hamd_spline5, which = "linkfunction", bty = "l")



#####################################################
# ng = 3
#####################################################
#linear link function
hamd_linear <- lcmm(ham24tot ~ visit, random = ~visit, subject = 'patient_id', mixture = ~visit, ng = 3, data = cmpst_all2, link = "linear", idiag = FALSE, na.action = 1)
summary(hamd_linear)
plot(hamd_linear, which="linkfunction",bty="l")

#beta link function
hamd_beta <- lcmm(ham24tot ~ visit, random = ~visit, subject = 'patient_id', mixture = ~visit, ng = 3, data = cmpst_all2, link = "beta", idiag = FALSE, na.action = 1)
summary(hamd_beta)
plot(hamd_beta, which="linkfunction",bty="l")

# I-splines with 3 equidistant nodes
hamd_spline3 <- lcmm(ham24tot ~ visit, random = ~visit, subject = 'patient_id', mixture = ~visit, ng = 3, data = cmpst_all2, link="3-equi-splines", idiag = FALSE, na.action = 1)
summary(hamd_spline3)
plot(hamd_spline3, which = "linkfunction", bty = "l")


# I-splines with 5 nodes at quantiles

hamd_spline5 <- lcmm(ham24tot ~ visit, random = ~visit, subject = 'patient_id', mixture = ~visit, ng = 3, data = cmpst_all2, link="5-quant-splines", idiag = FALSE, na.action = 1)
summary(hamd_spline5)
plot(hamd_spline5, which = "linkfunction", bty = "l")
