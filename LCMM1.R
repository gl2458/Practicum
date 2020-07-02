install.packages("lcmm")
install.packages("xlsx")
library(lcmm)
library(plyr)
library(tidyverse)
library(writexl)

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
hamd_linear_2 <- lcmm(ham24tot ~ visit, random = ~visit, subject = 'patient_id', mixture = ~visit, ng = 2, data = cmpst_all2, link = "linear", idiag = FALSE, na.action = 1)
summary(hamd_linear_2)
plot(hamd_linear_2, which="linkfunction", bty="l")
plot(hamd_linear_2, which="postprob", bty="l")

summary(hamd_linear_2$pprob)
names(hamd_linear_2)
count(unique(hamd_linear_2$pprob))




#beta link function
hamd_beta_2 <- lcmm(ham24tot ~ visit, random = ~visit, subject = 'patient_id', mixture = ~visit, ng = 2, data = cmpst_all2, link = "beta", idiag = FALSE, na.action = 1)
summary(hamd_beta_2)
plot(hamd_beta_2, which="linkfunction",bty="l")
plot(hamd_beta_2, which="postprob", bty="l")

count(unique(hamd_beta_2$pprob))
class_hamd_beta_2 <- left_join(cmpst_all2, data.frame(hamd_beta_2$pprob), by = "patient_id", copy = FALSE)

# I-splines with 3 equidistant nodes
hamd_spline3_2 <- lcmm(ham24tot ~ visit, random = ~visit, subject = 'patient_id', mixture = ~visit, ng = 2, data = cmpst_all2, link="3-equi-splines", idiag = FALSE, na.action = 1)
summary(hamd_spline3_2)
plot(hamd_spline3_2, which = "linkfunction", bty = "l")
plot(hamd_spline3_2, which="postprob", bty="l")

# I-splines with 5 nodes at quantiles
hamd_spline5_2 <- lcmm(ham24tot ~ visit, random = ~visit, subject = 'patient_id', mixture = ~visit, ng = 2, data = cmpst_all2, link="5-quant-splines", idiag = FALSE, na.action = 1)
summary(hamd_spline5_2)
plot(hamd_spline5_2, which = "linkfunction", bty = "l")
plot(hamd_spline5_2, which="postprob", bty="l")


#class extraction
class_hamd_linear_2 <- left_join(cmpst_all2, data.frame(hamd_linear_2$pprob), by = "patient_id", copy = FALSE)
class_hamd_linear_2$class <- as.factor(class_hamd_linear_2$class)

class_hamd_beta_2 <- left_join(cmpst_all2, data.frame(hamd_beta_2$pprob), by = "patient_id", copy = FALSE)
class_hamd_beta_2$class <- as.factor(class_hamd_beta_2$class)

class_hamd_spline3_2 <- left_join(cmpst_all2, data.frame(hamd_spline3_2$pprob), by = "patient_id", copy = FALSE)
class_hamd_spline3_2$class <- as.factor(class_hamd_spline3_2$class)

class_hamd_spline5_2 <- left_join(cmpst_all2, data.frame(hamd_spline5_2$pprob), by = "patient_id", copy = FALSE)
class_hamd_spline5_2$class <- as.factor(class_hamd_spline5_2$class)

write_xlsx(class_hamd_linear_2,"/Users/rachellee/Google Drive/Practicum/LCMM/class extraction/class_hamd_linear_2.xlsx")


#plot

ggplot(class_hamd_linear_2, aes(visit, ham24tot, group = patient_id, color = class)) + geom_point(alpha = 0.3) + geom_line(alpha = 0.3) +
  ggtitle("linear, ng=2") +
  theme(plot.title = element_text(hjust = 0.5))

ggplot(class_hamd_beta_2, aes(visit, ham24tot, group = patient_id, color = class)) + geom_point(alpha = 0.3) + geom_line(alpha = 0.3) +
  ggtitle("beta, ng=2") +
  theme(plot.title = element_text(hjust = 0.5))

ggplot(class_hamd_spline3_2, aes(visit, ham24tot, group = patient_id, color = class)) + geom_point(alpha = 0.3) + geom_line(alpha = 0.3) +
  ggtitle("spline3, ng=2") +
  theme(plot.title = element_text(hjust = 0.5))

ggplot(class_hamd_spline5_2, aes(visit, ham24tot, group = patient_id, color = class)) + geom_point(alpha = 0.3) + geom_line(alpha = 0.3) +
  ggtitle("spline5, ng=2") +
  theme(plot.title = element_text(hjust = 0.5))





#####################################################
# ng = 3
#####################################################
#linear link function
hamd_linear_3 <- lcmm(ham24tot ~ visit, random = ~visit, subject = 'patient_id', mixture = ~visit, ng = 3, data = cmpst_all2, link = "linear", idiag = FALSE, na.action = 1)
summary(hamd_linear_3)
plot(hamd_linear_3, which="linkfunction",bty="l")
plot(hamd_linear_3, which="postprob",bty="l")

#beta link function
hamd_beta_3 <- lcmm(ham24tot ~ visit, random = ~visit, subject = 'patient_id', mixture = ~visit, ng = 3, data = cmpst_all2, link = "beta", idiag = FALSE, na.action = 1)
summary(hamd_beta_3)
plot(hamd_beta_3, which="linkfunction",bty="l")
plot(hamd_beta_3, which="postprob",bty="l")

# I-splines with 3 equidistant nodes
hamd_spline3_3 <- lcmm(ham24tot ~ visit, random = ~visit, subject = 'patient_id', mixture = ~visit, ng = 3, data = cmpst_all2, link="3-equi-splines", idiag = FALSE, na.action = 1)
summary(hamd_spline3_3)
plot(hamd_spline3_3, which = "linkfunction", bty = "l")
plot(hamd_spline3_3, which="postprob",bty="l")


# I-splines with 5 nodes at quantiles

hamd_spline5_3 <- lcmm(ham24tot ~ visit, random = ~visit, subject = 'patient_id', mixture = ~visit, ng = 3, data = cmpst_all2, link="5-quant-splines", idiag = FALSE, na.action = 1)
summary(hamd_spline5_3)
plot(hamd_spline5_3, which = "linkfunction", bty = "l")
plot(hamd_spline5_3, which="postprob",bty="l")

#class extraction
class_hamd_linear_3 <- left_join(cmpst_all2, data.frame(hamd_linear_3$pprob), by = "patient_id", copy = FALSE)
class_hamd_linear_3$class <- as.factor(class_hamd_linear_3$class)

class_hamd_beta_3 <- left_join(cmpst_all2, data.frame(hamd_beta_3$pprob), by = "patient_id", copy = FALSE)
class_hamd_beta_3$class <- as.factor(class_hamd_beta_3$class)

class_hamd_spline3_3 <- left_join(cmpst_all2, data.frame(hamd_spline3_3$pprob), by = "patient_id", copy = FALSE)
class_hamd_spline3_3$class <- as.factor(class_hamd_spline3_3$class)

class_hamd_spline5_3 <- left_join(cmpst_all2, data.frame(hamd_spline5_3$pprob), by = "patient_id", copy = FALSE)
class_hamd_spline5_3$class <- as.factor(class_hamd_spline5_3$class)


#plot

ggplot(class_hamd_linear_3, aes(visit, ham24tot, group = patient_id, color = class)) + geom_point(alpha = 0.3) + geom_line(alpha = 0.3) +
  ggtitle("linear, ng=3") +
  theme(plot.title = element_text(hjust = 0.5))

ggplot(class_hamd_beta_3, aes(visit, ham24tot, group = patient_id, color = class)) + geom_point(alpha = 0.3) + geom_line(alpha = 0.3) +
  ggtitle("beta, ng=3") +
  theme(plot.title = element_text(hjust = 0.5))

ggplot(class_hamd_spline3_3, aes(visit, ham24tot, group = patient_id, color = class)) + geom_point(alpha = 0.3) + geom_line(alpha = 0.3) +
  ggtitle("spline3, ng=3") +
  theme(plot.title = element_text(hjust = 0.5))

ggplot(class_hamd_spline5_3, aes(visit, ham24tot, group = patient_id, color = class)) + geom_point(alpha = 0.3) + geom_line(alpha = 0.3) +
  ggtitle("spline5, ng=3") +
  theme(plot.title = element_text(hjust = 0.5))










#####################################################
# ng = 4
#####################################################
#linear link function
hamd_linear_4 <- lcmm(ham24tot ~ visit, random = ~visit, subject = 'patient_id', mixture = ~visit, ng = 4, data = cmpst_all2, link = "linear", idiag = FALSE, na.action = 1)
summary(hamd_linear_4)
plot(hamd_linear_4, which="linkfunction",bty="l")
plot(hamd_linear_4, which="postprob",bty="l")

#beta link function
hamd_beta_4 <- lcmm(ham24tot ~ visit, random = ~visit, subject = 'patient_id', mixture = ~visit, ng = 4, data = cmpst_all2, link = "beta", idiag = FALSE, na.action = 1)
summary(hamd_beta_4)
plot(hamd_beta_4, which="linkfunction",bty="l")
plot(hamd_beta_4, which = "postprob", bty = "l")

# I-splines with 3 equidistant nodes
hamd_spline3_4 <- lcmm(ham24tot ~ visit, random = ~visit, subject = 'patient_id', mixture = ~visit, ng = 4, data = cmpst_all2, link="3-equi-splines", idiag = FALSE, na.action = 1)
summary(hamd_spline3_4)
plot(hamd_spline3_4, which = "linkfunction", bty = "l")
plot(hamd_spline3_4, which = "postprob", bty = "l")

# I-splines with 5 nodes at quantiles
hamd_spline5_4 <- lcmm(ham24tot ~ visit, random = ~visit, subject = 'patient_id', mixture = ~visit, ng = 4, data = cmpst_all2, link="5-quant-splines", idiag = FALSE, na.action = 1)

summary(hamd_spline5_4)
plot(hamd_spline5_4, which = "linkfunction", bty = "l")
plot(hamd_spline5_4, which = "postprob", bty = "l")

#class extraction
class_hamd_linear_4 <- left_join(cmpst_all2, data.frame(hamd_linear_4$pprob), by = "patient_id", copy = FALSE)
class_hamd_linear_4$class <- as.factor(class_hamd_linear_4$class)

class_hamd_beta_4 <- left_join(cmpst_all2, data.frame(hamd_beta_4$pprob), by = "patient_id", copy = FALSE)
class_hamd_beta_4$class <- as.factor(class_hamd_beta_4$class)

class_hamd_spline3_4 <- left_join(cmpst_all2, data.frame(hamd_spline3_4$pprob), by = "patient_id", copy = FALSE)
class_hamd_spline3_4$class <- as.factor(class_hamd_spline3_4$class)

class_hamd_spline5_4 <- left_join(cmpst_all2, data.frame(hamd_spline5_4$pprob), by = "patient_id", copy = FALSE)
class_hamd_spline5_4$class <- as.factor(class_hamd_spline5_4$class)

#plot
ggplot(class_hamd_linear_4, aes(visit, ham24tot, group = patient_id, color = class)) + geom_point(alpha = 0.3) + geom_line(alpha = 0.3) +
  ggtitle("linear, ng=4") +
  theme(plot.title = element_text(hjust = 0.5))

ggplot(class_hamd_beta_4, aes(visit, ham24tot, group = patient_id, color = class)) + geom_point(alpha = 0.3) + geom_line(alpha = 0.3) +
  ggtitle("beta, ng=4") +
  theme(plot.title = element_text(hjust = 0.5))

ggplot(class_hamd_spline3_4, aes(visit, ham24tot, group = patient_id, color = class)) + geom_point(alpha = 0.3) + geom_line(alpha = 0.3) +
  ggtitle("spline3, ng=4") +
  theme(plot.title = element_text(hjust = 0.5))

ggplot(class_hamd_spline5_4, aes(visit, ham24tot, group = patient_id, color = class)) + geom_point(alpha = 0.3) + geom_line(alpha = 0.3) +
  ggtitle("spline5, ng=4") +
  theme(plot.title = element_text(hjust = 0.5))




#summary table

sum_table <- data.frame(
  summarytable(
    hamd_beta_2, 
    hamd_linear_2, 
    hamd_spline3_2,
    hamd_spline5_2, 
    hamd_beta_3, 
    hamd_linear_3, 
    hamd_spline3_3,
    hamd_spline5_3, 
    hamd_beta_4, 
    hamd_linear_4, 
    hamd_spline3_4, 
    hamd_spline5_4, 
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

write_xlsx(sum_table,"/Users/rachellee/Google Drive/Practicum/LCMM/class extraction/class_sum_table.xlsx")



