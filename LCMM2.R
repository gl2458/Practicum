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

#calculate change from baseline 

cmpst_hamd_change_w <- cmpst_change %>%
  select(patient_id, tx_text, gender, visit, site, ham24tot) %>%
  pivot_wider(
    names_from = "visit",
    values_from = "ham24tot",
  ) %>%
  mutate(
    change0 = 0,
    change3 = Week3 - Baseline,
    change6 = Week6 - Baseline,
    change9 = Week9 - Baseline,
    change14 = Week14 - Baseline
  )

#long format
cmpst_hamd_change_l <- cmpst_hamd_change_w %>%
  pivot_longer(
    change0:change14 , 
    names_to = "visit" ,
    values_to = "hamd_change"
  ) %>%
  select(patient_id, tx_text, gender, site, visit, hamd_change)


cmpst_hamd_change_l$visit <- revalue(cmpst_hamd_change_l$visit, c("change0" = 0, "change3" = 3, "change6" =6, "change9"=9, "change14"=14)) %>%
as.numeric(cmpst_hamd_change_l$visit)

cmpst_hamd_change_l <- data.frame(cmpst_hamd_change_l)

#####################################################
#linear link function
#####################################################

#pre-lcmm plot
ggplot(cmpst_hamd_change_l, aes(visit, hamd_change, group = patient_id)) + geom_point(alpha = 0.3) + geom_line(alpha = 0.3) 

#ng = 2
hamd_change_linear_2 <- lcmm(hamd_change ~ visit, random = ~ visit, subject = 'patient_id', mixture = ~visit, ng = 2, data = cmpst_hamd_change_l, link = "linear", idiag = FALSE, na.action = 1)
summary(hamd_change_linear_2)
plot(hamd_change_linear_2, which="linkfunction", bty="l")
plot(hamd_change_linear_2, which="postprob", bty="l")

class_hamd_change_linear_2 <- left_join(cmpst_hamd_change_l, data.frame(hamd_change_linear_2$pprob), by = "patient_id", copy = FALSE)
class_hamd_change_linear_2$class <- as.factor(class_hamd_change_linear_2$class)

ggplot(class_hamd_change_linear_2, aes(visit, hamd_change, group = patient_id, color = class)) + geom_point(alpha = 0.3) + geom_line(alpha = 0.3) +
  ggtitle("linear, ng=2") +
  theme(plot.title = element_text(hjust = 0.5))

#ng = 3
hamd_change_linear_3 <- lcmm(hamd_change ~ visit, random = ~ visit, subject = 'patient_id', mixture = ~visit, ng = 3, data = cmpst_hamd_change_l, link = "linear", idiag = FALSE, na.action = 1)
summary(hamd_change_linear_3)
plot(hamd_change_linear_3, which="linkfunction", bty="l")
plot(hamd_change_linear_3, which="postprob", bty="l")

class_hamd_change_linear_3 <- left_join(cmpst_hamd_change_l, data.frame(hamd_change_linear_3$pprob), by = "patient_id", copy = FALSE)
class_hamd_change_linear_3$class <- as.factor(class_hamd_change_linear_3$class)

ggplot(class_hamd_change_linear_3, aes(visit, hamd_change, group = patient_id, color = class)) + geom_point(alpha = 0.3) + geom_line(alpha = 0.3) +
  ggtitle("linear, ng=3") +
  theme(plot.title = element_text(hjust = 0.5))



#ng = 4
hamd_change_linear_4 <- lcmm(hamd_change ~ visit, random = ~ visit, subject = 'patient_id', mixture = ~visit, ng = 4, data = cmpst_hamd_change_l, link = "linear", idiag = FALSE, na.action = 1)
summary(hamd_change_linear_4)
plot(hamd_change_linear_4, which="linkfunction", bty="l")
plot(hamd_change_linear_4, which="postprob", bty="l")

class_hamd_change_linear_4 <- left_join(cmpst_hamd_change_l, data.frame(hamd_change_linear_4$pprob), by = "patient_id", copy = FALSE)
class_hamd_change_linear_4$class <- as.factor(class_hamd_change_linear_4$class)

ggplot(class_hamd_change_linear_4, aes(visit, hamd_change, group = patient_id, color = class)) + geom_point(alpha = 0.3) + geom_line(alpha = 0.3) +
  ggtitle("linear, ng=4") +
  theme(plot.title = element_text(hjust = 0.5))



#####################################################
#beta link function
#####################################################

#ng = 2
hamd_change_beta_2 <- lcmm(hamd_change ~ visit, random = ~ visit, subject = 'patient_id', mixture = ~visit, ng = 2, data = cmpst_hamd_change_l, link = "beta", idiag = FALSE, na.action = 1)
summary(hamd_change_beta_2)
plot(hamd_change_beta_2, which="linkfunction", bty="l")
plot(hamd_change_beta_2, which="postprob", bty="l")


class_hamd_change_beta_2 <- left_join(cmpst_hamd_change_l, data.frame(hamd_change_beta_2$pprob), by = "patient_id", copy = FALSE)
class_hamd_change_beta_2$class <- as.factor(class_hamd_change_beta_2$class)

ggplot(class_hamd_change_beta_2, aes(visit, hamd_change, group = patient_id, color = class)) + geom_point(alpha = 0.3) + geom_line(alpha = 0.3) +
  ggtitle("beta, ng=2") +
  theme(plot.title = element_text(hjust = 0.5))


#ng = 3
hamd_change_beta_3 <- lcmm(hamd_change ~ visit, random = ~ visit, subject = 'patient_id', mixture = ~visit, ng = 3, data = cmpst_hamd_change_l, link = "beta", idiag = FALSE, na.action = 1)
summary(hamd_change_beta_3)
plot(hamd_change_beta_3, which="linkfunction", bty="l")
plot(hamd_change_beta_3, which="postprob", bty="l")

class_hamd_change_beta_3 <- left_join(cmpst_hamd_change_l, data.frame(hamd_change_beta_3$pprob), by = "patient_id", copy = FALSE)
class_hamd_change_beta_3$class <- as.factor(class_hamd_change_beta_3$class)

ggplot(class_hamd_change_beta_3, aes(visit, hamd_change, group = patient_id, color = class)) + geom_point(alpha = 0.3) + geom_line(alpha = 0.3) +
  ggtitle("beta, ng=3") +
  theme(plot.title = element_text(hjust = 0.5))


#ng = 4
hamd_change_beta_4 <- lcmm(hamd_change ~ visit, random = ~ visit, subject = 'patient_id', mixture = ~visit, ng = 4, data = cmpst_hamd_change_l, link = "beta", idiag = FALSE, na.action = 1)
summary(hamd_change_beta_4)
plot(hamd_change_beta_4, which="linkfunction", bty="l")
plot(hamd_change_beta_4, which="postprob", bty="l")

class_hamd_change_beta_4 <- left_join(cmpst_hamd_change_l, data.frame(hamd_change_beta_4$pprob), by = "patient_id", copy = FALSE)
class_hamd_change_beta_4$class <- as.factor(class_hamd_change_beta_4$class)

ggplot(class_hamd_change_beta_4, aes(visit, hamd_change, group = patient_id, color = class)) + geom_point(alpha = 0.3) + geom_line(alpha = 0.3) +
  ggtitle("beta, ng=4") +
  theme(plot.title = element_text(hjust = 0.5))




#####################################################
#spline-3 link function
#####################################################

#ng =2 

hamd_change_spline3_2 <- lcmm(hamd_change ~ visit, random = ~visit, subject = 'patient_id', mixture = ~visit, ng = 2, data = cmpst_hamd_change_l, link="3-equi-splines", idiag = FALSE, na.action = 1)
summary(hamd_change_spline3_2)
plot(hamd_change_spline3_2, which = "linkfunction", bty = "l")
plot(hamd_change_spline3_2, which="postprob", bty="l")

class_hamd_change_spline3_2 <- left_join(cmpst_hamd_change_l, data.frame(hamd_change_spline3_2$pprob), by = "patient_id", copy = FALSE)
class_hamd_change_spline3_2$class <- as.factor(class_hamd_change_spline3_2$class)

ggplot(class_hamd_change_spline3_2, aes(visit, hamd_change, group = patient_id, color = class)) + geom_point(alpha = 0.3) + geom_line(alpha = 0.3) +
  ggtitle("spline3, ng=2") +
  theme(plot.title = element_text(hjust = 0.5))

#ng =3

hamd_change_spline3_3 <- lcmm(hamd_change ~ visit, random = ~visit, subject = 'patient_id', mixture = ~visit, ng = 3, data = cmpst_hamd_change_l, link="3-equi-splines", idiag = FALSE, na.action = 1)
summary(hamd_change_spline3_3)
plot(hamd_change_spline3_3, which = "linkfunction", bty = "l")
plot(hamd_change_spline3_3, which="postprob", bty="l")

class_hamd_change_spline3_3 <- left_join(cmpst_hamd_change_l, data.frame(hamd_change_spline3_3$pprob), by = "patient_id", copy = FALSE)
class_hamd_change_spline3_3$class <- as.factor(class_hamd_change_spline3_3$class)

ggplot(class_hamd_change_spline3_3, aes(visit, hamd_change, group = patient_id, color = class)) + geom_point(alpha = 0.3) + geom_line(alpha = 0.3) +
  ggtitle("spline3, ng=3") +
  theme(plot.title = element_text(hjust = 0.5))



#ng = 4
hamd_change_spline3_4 <- lcmm(hamd_change ~ visit, random = ~visit, subject = 'patient_id', mixture = ~visit, ng = 4, data = cmpst_hamd_change_l, link="3-equi-splines", idiag = FALSE, na.action = 1)
summary(hamd_change_spline3_4)
plot(hamd_change_spline3_4, which = "linkfunction", bty = "l")
plot(hamd_change_spline3_4, which="postprob", bty="l")

class_hamd_change_spline3_4 <- left_join(cmpst_hamd_change_l, data.frame(hamd_change_spline3_4$pprob), by = "patient_id", copy = FALSE)
class_hamd_change_spline3_4$class <- as.factor(class_hamd_change_spline3_4$class)

ggplot(class_hamd_change_spline3_4, aes(visit, hamd_change, group = patient_id, color = class)) + geom_point(alpha = 0.3) + geom_line(alpha = 0.3) +
  ggtitle("spline3, ng=4") +
  theme(plot.title = element_text(hjust = 0.5))




#####################################################
#spline-5 link function
#####################################################

#ng = 2

hamd_change_spline5_2 <- lcmm(hamd_change ~ visit, random = ~visit, subject = 'patient_id', mixture = ~visit, ng = 2, data = cmpst_hamd_change_l, link="5-quant-splines", idiag = FALSE, na.action = 1)
summary(hamd_change_spline5_2)
plot(hamd_change_spline5_2, which = "linkfunction", bty = "l")
plot(hamd_change_spline5_2, which="postprob", bty="l")

class_hamd_change_spline5_2 <- left_join(cmpst_hamd_change_l, data.frame(hamd_change_spline5_2$pprob), by = "patient_id", copy = FALSE)
class_hamd_change_spline5_2$class <- as.factor(class_hamd_change_spline5_2$class)

ggplot(class_hamd_change_spline5_2, aes(visit, hamd_change, group = patient_id, color = class)) + geom_point(alpha = 0.3) + geom_line(alpha = 0.3) +
  ggtitle("spline5, ng=2") +
  theme(plot.title = element_text(hjust = 0.5))


#ng = 3

hamd_change_spline5_3 <- lcmm(hamd_change ~ visit, random = ~visit, subject = 'patient_id', mixture = ~visit, ng = 3, data = cmpst_hamd_change_l, link="5-quant-splines", idiag = FALSE, na.action = 1)
summary(hamd_change_spline5_3)
plot(hamd_change_spline5_3, which = "linkfunction", bty = "l")
plot(hamd_change_spline5_3, which="postprob", bty="l")

class_hamd_change_spline5_3 <- left_join(cmpst_hamd_change_l, data.frame(hamd_change_spline5_3$pprob), by = "patient_id", copy = FALSE)
class_hamd_change_spline5_3$class <- as.factor(class_hamd_change_spline5_3$class)

ggplot(class_hamd_change_spline5_3, aes(visit, hamd_change, group = patient_id, color = class)) + geom_point(alpha = 0.3) + geom_line(alpha = 0.3) +
  ggtitle("spline5, ng=3") +
  theme(plot.title = element_text(hjust = 0.5))


#ng = 4 

hamd_change_spline5_4 <- lcmm(hamd_change ~ visit, random = ~visit, subject = 'patient_id', mixture = ~visit, ng = 4, data = cmpst_hamd_change_l, link="5-quant-splines", idiag = FALSE, na.action = 1)
summary(hamd_change_spline5_4)
plot(hamd_change_spline5_4, which = "linkfunction", bty = "l")
plot(hamd_change_spline5_4, which="postprob", bty="l")

class_hamd_change_spline5_4 <- left_join(cmpst_hamd_change_l, data.frame(hamd_change_spline5_4$pprob), by = "patient_id", copy = FALSE)
class_hamd_change_spline5_4$class <- as.factor(class_hamd_change_spline5_4$class)

ggplot(class_hamd_change_spline5_4, aes(visit, hamd_change, group = patient_id, color = class)) + geom_point(alpha = 0.3) + geom_line(alpha = 0.3) +
  ggtitle("spline5, ng=4") +
  theme(plot.title = element_text(hjust = 0.5))











#summary table

sum_table <- data.frame(
  summarytable(
    hamd_change_beta_2, 
    hamd_change_linear_2, 
    hamd_change_spline3_2,
    hamd_change_spline5_2, 
    hamd_change_beta_3, 
    hamd_change_linear_3, 
    hamd_change_spline3_3,
    hamd_change_spline5_3, 
    hamd_change_beta_4, 
    hamd_change_linear_4, 
    hamd_change_spline3_4, 
    hamd_change_spline5_4, 
    which = c("G", "loglik", "npm", "conv", "AIC", "BIC", "%class")))

sum_table$link <- c("hamd_beta_2", 
                    "linear", 
                    "spline",
                    "spline5", 
                    "beta", 
                    "linear", 
                    "spline3",
                    "spline5", 
                    "beta", 
                    "linear", 
                    "spline3", 
                    "spline5")

names(sum_table)

sum_table <- sum_table[ , c(11, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10)]
sum_table

write_xlsx(sum_table,"/Users/rachellee/Google Drive/Practicum/LCMM/class extraction/class_change_sum_table2.xlsx")


#################################
#residual plots
#################################

plot(hamd_change_beta_2, cex.main=0.5)
plot(hamd_change_linear_2, cex.main=0.5)
plot(hamd_change_spline3_2,cex.main=0.5)
plot(hamd_change_spline5_2, cex.main=0.5)
plot(hamd_change_beta_3, cex.main=0.5)
plot(hamd_change_linear_3, cex.main=0.5)
plot(hamd_change_spline3_3,cex.main=0.5)
plot(hamd_change_spline5_3, cex.main=0.5)
plot(hamd_change_beta_4, cex.main=0.5)
plot(hamd_change_linear_4, cex.main=0.5)
plot(hamd_change_spline3_4, cex.main=0.5)
plot(hamd_change_spline5_4, cex.main=0.5)


#####################
#logistic regression
#####################

#merge class data and original data


##linear, ng=2, baseline data extraction
class_hamd_change_linear_2_bl <- class_hamd_change_linear_2 %>%
  filter(visit == 0) %>%
  select(patient_id, class, prob1, prob2, visit)


##linear, ng=3, baseline data extraction
class_hamd_change_linear_3_bl <- class_hamd_change_linear_3 %>%
  filter(visit == 0) %>%
  select(patient_id, class, prob1, prob2, visit)


cmpst_change_bl <- cmpst_change %>%
  filter(visit == "Baseline")


###################################
#link = linear, ng = 2 
##data prepared for logistic regression 

cmpst_change_logit_linear_2 <- left_join(cmpst_change_bl, class_hamd_change_linear_2_bl, by = "patient_id", copy = FALSE) 

#logistic regression

pos_affect_fit <- glm(class ~ gender + age + pos_affect, data = cmpst_change_logit_linear_2, family = binomial() )

whodastot_fit <- glm(class ~ gender + age + whodastot, data = cmpst_change_logit_linear_2, family = binomial() )

neg_affect_fit <- glm(class ~ gender + age + neg_affect, data = cmpst_change_logit_linear_2, family = binomial() )

madrs_total_fit <- glm(class ~ gender + age + madrs_total, data = cmpst_change_logit_linear_2, family = binomial() )

dssi_net_fit <- glm(class ~ gender + age + dssi_net, data = cmpst_change_logit_linear_2, family = binomial() )



###################################
#link = linear, ng = 3
##data prepared for logistic regression 

cmpst_change_logit_linear_3 <- left_join(cmpst_change_bl, class_hamd_change_linear_3_bl, by = "patient_id", copy = FALSE) 


