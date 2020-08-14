install.packages("lcmm")
install.packages("xlsx")
install.packages("Amelia")
library(lcmm)
library(plyr)
library(tidyverse)
library(writexl)
library(nnet)

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


#all patient longitudinal plot

ggplot(class_hamd_change_linear_2, aes(visit, hamd_change, group = patient_id, color = class))  + geom_line(alpha = 0.3) +
  ggtitle("link = linear, ng = 2") +
  theme(
    plot.title = element_text(hjust = 0.5, size = 12)
  ) +
  scale_x_continuous(breaks = c(0, 3, 6, 9, 14)) + 
  labs(x = "visit", y = "HAMD Change from Baseline") +
  theme(
    axis.title.x = element_text(size = 10),
    axis.title.y = element_text(size = 10)
  )

#class mean plot
ggplot(class_hamd_change_linear_2, aes(x = visit, y = hamd_change, group= patient_id , color= class )) + geom_line(alpha = 0.3) + 
  geom_smooth(alpha = 0.5, aes(group=class), method="loess", size=1.2, se=F) + 
  labs(x="visit",y="HAMD Change from Baseline",color= "Latent Class") + 
  ggtitle("link = linear, ng = 2") +
  theme(
    plot.title = element_text(hjust = 0.5, size = 12)
  ) +
  scale_x_continuous(breaks = c(0, 3, 6, 9, 14)) + 
  theme(
    axis.title.x = element_text(size = 10),
    axis.title.y = element_text(size = 10)
  )


write_xlsx(class_hamd_change_linear_2,"/Users/rachellee/Google Drive/Practicum/LCMM/class extraction/class_hamd_change_linear_2.xlsx")









#ng = 3
hamd_change_linear_3 <- lcmm(hamd_change ~ visit, random = ~ visit, subject = 'patient_id', mixture = ~visit, ng = 3, data = cmpst_hamd_change_l, link = "linear", idiag = FALSE, na.action = 1)
summary(hamd_change_linear_3)
plot(hamd_change_linear_3, which="linkfunction", bty="l")
plot(hamd_change_linear_3, which="postprob", bty="l")

class_hamd_change_linear_3 <- left_join(cmpst_hamd_change_l, data.frame(hamd_change_linear_3$pprob), by = "patient_id", copy = FALSE)
class_hamd_change_linear_3$class <- as.factor(class_hamd_change_linear_3$class)

#re-ordering class
class_hamd_change_linear_3$reclass <- class_hamd_change_linear_3$class
as.data.frame(class_hamd_change_linear_3)
class_hamd_change_linear_3$reclass <- recode(class_hamd_change_linear_3$reclass, `1` = "b", `2` = "a", `3` = "c") %>%
class_hamd_change_linear_3$reclass <- recode(class_hamd_change_linear_3$reclass, "a" = 1, "b" = 2, "c" = 3)
class_hamd_change_linear_3$reclass <- as.factor(class_hamd_change_linear_3$reclass)

reclass_hamd_change_linear_3 <- class_hamd_change_linear_3 %>%
  select(-class) %>%
  rename( class = reclass)



#all patient longitudinal plot
ggplot(reclass_hamd_change_linear_3, aes(visit, hamd_change, group = patient_id, color = class))  + geom_line(alpha = 0.3) +
  ggtitle("link = linear, ng=3") +
  theme(
    plot.title = element_text(hjust = 0.5, size = 12)
    ) +
  scale_x_continuous(breaks = c(0, 3, 6, 9, 14)) + 
  labs(x = "visit", y = "HAMD Change from Baseline") +
  theme(
    axis.title.x = element_text(size = 10),
    axis.title.y = element_text(size = 10)
        )

#class mean plot
ggplot(reclass_hamd_change_linear_3, aes(x = visit, y = hamd_change, group= patient_id , color= class )) + geom_line(alpha = 0.3) + 
  geom_smooth(alpha = 0.5, aes(group=class), method="loess", size=1.2, se=F) + 
  labs(x="visit",y="HAMD Change from Baseline",color= "Latent Class") + 
  ggtitle("link = linear, ng = 3") +
  theme(
    plot.title = element_text(hjust = 0.5, size = 12)
  ) +
  scale_x_continuous(breaks = c(0, 3, 6, 9, 14)) + 
  theme(
    axis.title.x = element_text(size = 10),
    axis.title.y = element_text(size = 10)
  )















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








######################################################################

#                       logistic regression

######################################################################


#merge class extraction data and original data

#original data baseline extraction
cmpst_change_bl <- cmpst_change %>%
  filter(visit == "Baseline")

##linear, ng=2, baseline data extraction
class_hamd_change_linear_2_bl <- class_hamd_change_linear_2 %>%
  filter(visit == 0) %>%
  select(patient_id, class, prob1, prob2, visit)


##linear, ng=3, baseline data extraction
class_hamd_change_linear_3_bl <- class_hamd_change_linear_3 %>%
  filter(visit == 0) %>%
  select(patient_id, class, prob1, prob2, visit)






# missingness map
missingness <- missmap(cmpst_change_logit_linear_2, col=c("blue", "red"))

cmpst_change_logit_linear_2 <- left_join(cmpst_change_bl, class_hamd_change_linear_2_bl, by = "patient_id", copy = FALSE) 

cmpst_change_logit_linear_2$gender <- as.factor(cmpst_change_logit_linear_2$gender)
cmpst_change_logit_linear_2$race <- as.factor(cmpst_change_logit_linear_2$race)
cmpst_change_logit_linear_2$race2 <- cmpst_change_logit_linear_2$race 
cmpst_change_logit_linear_2$race2 <- recode_factor(cmpst_change_logit_linear_2$race2, "Asian" = "Other", "More than one race" = "Other", "American Indian/Alaska Native" = "Other" )
cmpst_change_logit_linear_2$race2 <- na_if(cmpst_change_logit_linear_2$race2, "Unknown/not reported")
cmpst_change_logit_linear_2$race <- cmpst_change_logit_linear_2$race2
relevel(cmpst_change_logit_linear_2$race, ref = "Other")



#  1. logistic regression (adjusting for age and gender)
    
#link = linear, ng = 2 
    

    # model fitting

age_fit_linear2 <- glm(class ~ gender + age, data = cmpst_change_logit_linear_2, family = binomial(link = "logit") )

race_fit_linear2 <- glm(class ~ gender + age + race, data = cmpst_change_logit_linear_2, family = binomial(link = "logit") )

treatment_fit_linear2 <- glm(class ~ gender + age + tx_text, data = cmpst_change_logit_linear_2, family = binomial(link = "logit") )

race_fit_linear2 <- glm(class ~ gender + age + race, data = cmpst_change_logit_linear_2, family = binomial(link = "logit") )

site_fit_linear2 <- glm(class ~ gender + age + site, data = cmpst_change_logit_linear_2, family = binomial(link = "logit") )

madrs_fit_linear2 <- glm(class ~ gender + age + madrs_total, data = cmpst_change_logit_linear_2, family = binomial(link = "logit") )

pos_affect_fit_linear2 <- glm(class ~ gender + age + pos_affect, data = cmpst_change_logit_linear_2, family = binomial() )

neg_affect_fit_linear2 <- glm(class ~ gender + age + neg_affect, data = cmpst_change_logit_linear_2, family = binomial() )

dssi_si_fit_linear2 <- glm(class ~ gender + age + dssi_si, data = cmpst_change_logit_linear_2, family = binomial() )

dssi_ss_fit_linear2 <- glm(class ~ gender + age + dssi_ss, data = cmpst_change_logit_linear_2, family = binomial() )

dssi_is_fit_linear2 <- glm(class ~ gender + age + dssi_is, data = cmpst_change_logit_linear_2, family = binomial() )

dssi_net_fit_linear2 <- glm(class ~ gender + age + dssi_net, data = cmpst_change_logit_linear_2, family = binomial() )

hvlt_immed_fit_linear2 <- glm(class ~ gender + age + hvlt_immed, data = cmpst_change_logit_linear_2, family = binomial() )

hvlt_discrim_fit_linear2 <- glm(class ~ gender + age + hvlt_discrim, data = cmpst_change_logit_linear_2, family = binomial() )

trs_total_fit_linear2 <- glm(class ~ gender + age + trs_total, data = cmpst_change_logit_linear_2, family = binomial() )

stroop_word_fit_linear2 <- glm(class ~ gender + age + stroop_word, data = cmpst_change_logit_linear_2, family = binomial() )

stroop_color_fit_linear2 <- glm(class ~ gender + age + stroop_color, data = cmpst_change_logit_linear_2, family = binomial() )

stroop_color_word_fit_linear2 <- glm(class ~ gender + age + stroop_color_word, data = cmpst_change_logit_linear_2, family = binomial() )

    #odds ratio calculation

treatment_fit_linear2 
race_fit_linear2 
site_fit_linear2 
madrs_fit_linear2 
pos_affect_fit_linear2 
neg_affect_fit_linear2 
dssi_si_fit_linear2 
dssi_ss_fit_linear2 
dssi_is_fit_linear2 
dssi_net_fit_linear2 
hvlt_immed_fit_linear2
hvlt_discrim_fit_linear2 
trs_total_fit_linear2 
stroop_word_fit_linear2 
stroop_color_fit_linear2
stroop_color_word_fit_linear2 

pr1 <- function(x_arg) {
  print(summary(x_arg))
}

pr1(age_fit_linear2)


#OR and CI function

or_ci <- function (x) {
  or <- exp(tail(coef(x), n = 1)) 
  ci <- exp(tail(confint.default(x), n =1))

  data.frame(cbind(or, ci))
}


#race
data.frame(exp(coef(race_fit_linear2)))
data.frame(exp(confint.default(race_fit_linear2)))


#combined dataset
logistic_ng2 <- rbind( 
  or_ci(age_fit_linear2)
  or_ci(treatment_fit_linear2),
  or_ci(race_fit_linear2),
  or_ci(site_fit_linear2) ,
  or_ci(madrs_fit_linear2),
  or_ci(pos_affect_fit_linear2) ,
  or_ci(neg_affect_fit_linear2),
  or_ci(dssi_si_fit_linear2),
  or_ci(dssi_ss_fit_linear2),
  or_ci(dssi_is_fit_linear2),
  or_ci(dssi_net_fit_linear2),
  or_ci(hvlt_immed_fit_linear2),
  or_ci(hvlt_discrim_fit_linear2),
  or_ci(stroop_word_fit_linear2),
  or_ci(stroop_color_fit_linear2),
  or_ci(stroop_color_word_fit_linear2)
  )


write_xlsx(logistic_ng2,"/Users/rachellee/Google Drive/Practicum/LCMM/logit_linear_2.xlsx")




















###################################

#link = linear, ng = 3

##data prepared for logistic regression 

cmpst_change_logit_linear_3 <- left_join(cmpst_change_bl, class_hamd_change_linear_3_bl, by = "patient_id", copy = FALSE) 
cmpst_change_logit_linear_3$class <- relevel(cmpst_change_logit_linear_3$class, ref = 1)
cmpst_change_logit_linear_3$gender <- as.factor(cmpst_change_logit_linear_3$gender)
cmpst_change_logit_linear_3$site <- as.factor(cmpst_change_logit_linear_3$site)

cmpst_change_logit_linear_3$race <- as.factor(cmpst_change_logit_linear_3$race)
cmpst_change_logit_linear_3$race2 <- cmpst_change_logit_linear_3$race 
cmpst_change_logit_linear_3$race2 <- recode_factor(cmpst_change_logit_linear_3$race2, "Asian" = "Other", "More than one race" = "Other", "American Indian/Alaska Native" = "Other" )
cmpst_change_logit_linear_3$race2 <- na_if(cmpst_change_logit_linear_3$race2, "Unknown/not reported")
cmpst_change_logit_linear_3$race <- cmpst_change_logit_linear_3$race2
relevel(cmpst_change_logit_linear_3$race, ref = "Other")

cmpst_change_logit_linear_3$race3 <- NA
cmpst_change_logit_linear_3$race3[cmpst_change_logit_linear_3$race == "White"] <- "White"
cmpst_change_logit_linear_3$race3[cmpst_change_logit_linear_3$race == "Black/African American"] <- "Black/African American"
cmpst_change_logit_linear_3$race3[cmpst_change_logit_linear_3$race == "Other"] <- "Other"
cmpst_change_logit_linear_3$race3 <- as.factor(cmpst_change_logit_linear_3$race3)
cmpst_change_logit_linear_3$race3 <- relevel(cmpst_change_logit_linear_3$race3, ref = "Other")

# model fitting

age_fit_linear3 <-  multinom(class ~ age + gender , data = cmpst_change_logit_linear_3 )

race_fit_linear3 <-  multinom(class ~ gender + age + race3 , data = cmpst_change_logit_linear_3, na.action = na.exclude)

race2_fit_linear3 <-  multinom(class ~ gender + age + race2 , data = cmpst_change_logit_linear_3, na.action = na.exclude)

treatment_fit_linear3 <- multinom(class ~ gender + age + tx_text, data = cmpst_change_logit_linear_3 )

race_fit_linear3 <- multinom(class ~ gender + age + race, data = cmpst_change_logit_linear_3 )

site_fit_linear3 <- multinom(class ~ gender + age + site, data = cmpst_change_logit_linear_3 )

madrs_fit_linear3 <- multinom(class ~ gender + age + madrs_total, data = cmpst_change_logit_linear_3 )

pos_affect_fit_linear3 <- multinom(class ~ gender + age + pos_affect, data = cmpst_change_logit_linear_3 )

neg_affect_fit_linear3 <- multinom(class ~ gender + age + neg_affect, data = cmpst_change_logit_linear_3 )

dssi_si_fit_linear3 <- multinom(class ~ gender + age + dssi_si, data = cmpst_change_logit_linear_3 )

dssi_ss_fit_linear3 <- multinom(class ~ gender + age + dssi_ss, data = cmpst_change_logit_linear_3 )

dssi_is_fit_linear3 <- multinom(class ~ gender + age + dssi_is, data = cmpst_change_logit_linear_3 )

dssi_net_fit_linear3 <- multinom(class ~ gender + age + dssi_net, data = cmpst_change_logit_linear_3 )

hvlt_immed_fit_linear3 <- multinom(class ~ gender + age + hvlt_immed, data = cmpst_change_logit_linear_3 )

hvlt_discrim_fit_linear3 <- multinom(class ~ gender + age + hvlt_discrim, data = cmpst_change_logit_linear_3 )

stroop_word_fit_linear3 <- multinom(class ~ gender + age + stroop_word, data = cmpst_change_logit_linear_3 )

stroop_color_fit_linear3 <- multinom(class ~ gender + age + stroop_color, data = cmpst_change_logit_linear_3 )

stroop_color_word_fit_linear3 <- multinom(class ~ gender + age + stroop_color_word, data = cmpst_change_logit_linear_3 )


# OR calculation

or_multi <- function (x) {
  or_1 <- exp( (coef(x)[1,4] )) 
  or_2 <- exp( (coef(x)[2,4] ))

  data.frame( cbind(or_1, or_2) )
}


logistic_ng3 <- rbind(
or_multi(treatment_fit_linear3),
or_multi(site_fit_linear3),
or_multi(madrs_fit_linear3),
or_multi(pos_affect_fit_linear3),
or_multi(neg_affect_fit_linear3),
or_multi(dssi_si_fit_linear3),
or_multi(dssi_ss_fit_linear3),
or_multi(dssi_is_fit_linear3),
or_multi(dssi_net_fit_linear3),
or_multi(hvlt_immed_fit_linear3),
or_multi(hvlt_discrim_fit_linear3),
or_multi(stroop_word_fit_linear3),
or_multi(stroop_color_fit_linear3),
or_multi(stroop_color_word_fit_linear3) )


write_xlsx(logistic_ng3,"/Users/rachellee/Google Drive/Practicum/LCMM/multinom_linear_3.xlsx")

#### CI code
ci_multi <- function(x){
  tail(as.data.frame(exp(confint(x))), n = 1)
}

logistic_ci_ng3 <- rbind(
  ci_multi(treatment_fit_linear3),
  ci_multi(site_fit_linear3),
  ci_multi(madrs_fit_linear3),
  ci_multi(pos_affect_fit_linear3),
  ci_multi(neg_affect_fit_linear3),
  ci_multi(dssi_si_fit_linear3),
  ci_multi(dssi_ss_fit_linear3),
  ci_multi(dssi_is_fit_linear3),
  ci_multi(dssi_net_fit_linear3),
  ci_multi(hvlt_immed_fit_linear3),
  ci_multi(hvlt_discrim_fit_linear3),
  ci_multi(stroop_word_fit_linear3),
  ci_multi(stroop_color_fit_linear3),
  ci_multi(stroop_color_word_fit_linear3) )

write_xlsx(logistic_ci_ng3,"/Users/rachellee/Google Drive/Practicum/LCMM/multinom_ci_linear_3.xlsx")



### P - value
#teststat = summary(multinom)$coefficients/summary(multinom)$standard.errors
pval = (1 - pnorm(abs(teststat), 0, 1)) * 2

teststat = summary(treatment_fit_linear3)$coefficients/summary(treatment_fit_linear3)$standard.errors
pval = (1- pnorm(abs(tstat), 0, 1)) *2

pr2 <- function(x_arg){
  teststat = summary(x_arg)$coefficients/summary(x_arg)$standard.errors
  
  pval = (1- pnorm(abs(teststat), 0, 1)) *2
  
  cbind(pval[1,4], pval[2,4])
}


logistic_pval_ng3 <- as.data.frame( rbind(
  pr2(treatment_fit_linear3),
  pr2(site_fit_linear3),
  pr2(madrs_fit_linear3),
  pr2(pos_affect_fit_linear3),
  pr2(neg_affect_fit_linear3),
  pr2(dssi_si_fit_linear3),
  pr2(dssi_ss_fit_linear3),
  pr2(dssi_is_fit_linear3),
  pr2(dssi_net_fit_linear3),
  pr2(hvlt_immed_fit_linear3),
  pr2(hvlt_discrim_fit_linear3),
  pr2(stroop_word_fit_linear3),
  pr2(stroop_color_fit_linear3),
  pr2(stroop_color_word_fit_linear3) ) )



write_xlsx(logistic_pval_ng3,"/Users/rachellee/Google Drive/Practicum/LCMM/multinom_pval_linear_3.xlsx")

#age & gender 
agetest <- summary(age_fit_linear3)$coefficients/summary(age_fit_linear3)$standard.errors
agepval <- (1- pnorm(abs(agetest), 0, 1)) *2

#race multinomal logistic regression (ng = 3)
race_or <- as.data.frame(exp(coef(race_fit_linear3)) )
race_ci <-as.data.frame(exp(confint(race_fit_linear3)))
race_test <- summary(race_fit_linear3)$coefficients/summary(race_fit_linear3)$standard.errors
race_pval <- (1- pnorm(abs(race_test), 0, 1)) *2

write_xlsx(race_ci,"/Users/rachellee/Google Drive/Practicum/LCMM/multinom_race_ci_linear_3.xlsx")





#data for table 1 by class

cmpst_class <- read_csv(file = "/Users/rachellee/Google Drive/Practicum/Data/cmpst_all2.csv") %>%
  janitor::clean_names() %>%
  filter(visit == "Baseline")


cmpst_class <- left_join(cmpst_class, class_hamd_change_linear_2_bl, by = "patient_id", copy = FALSE)
write_xlsx(cmpst_class,"/Users/rachellee/Google Drive/Practicum/Data/cmpst_class.xlsx")

cmpst_class <- as.data.frame(cmpst_class)


######################################################
#QUESTION#
logit <- function(x, y) {
  multinom(class ~ gender + age + !!as.name(x), data = y , family = binomial())
}

logit( x ="pos_affect", cmpst_change_logit_linear_2)

######################################################







