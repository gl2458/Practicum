library(plyr)
library(tidyverse)
library(writexl)
install.packages("readxl")
library("readxl")


cmpst_class <- as.data.frame(read_excel("/Users/rachellee/Google Drive/Practicum/Data/cmpst_class.xlsx"))
cmpst_class$class <- as.factor(cmpst_class$class)
#age
tbl1 <- as.data.frame(cmpst_class %>%
  group_by(class) %>%
  summarise(
    n(),
    mean(age),
    sd(age)
  ))
tbl1 <- cbind(tbl1[1, ], tbl1[2,])


#race
tbl2 <- as.data.frame(cmpst_class) %>%
  group_by(class, gender) %>%
  count()
tbl2 <- cbind(tbl2[1:2,], tbl2[3:4,])

tbl3 <- as.data.frame(cmpst_class) %>%
  group_by(class, race) %>%
  count()


#site
tbl4 <- as.data.frame(cmpst_class) %>%
  group_by(class, site) %>%
  count()

#madrs
tbl5 <- as.data.frame(cmpst_class %>%
                        group_by(class) %>%
                        drop_na(madrs_total)%>%
                        summarise(
                          n(),
                          mean(madrs_total),
                          sd(madrs_total)
                        ))

#hamd
tbl6 <- as.data.frame(cmpst_class %>%
                        group_by(class) %>%
                        drop_na(ham24tot)%>%
                        summarise(
                          n(),
                          mean(ham24tot),
                          sd(ham24tot)
                        ))

#pos_affect
tbl7 <- as.data.frame(cmpst_class %>%
                        group_by(class) %>%
                        drop_na(pos_affect)%>%
                        summarise(
                          n(),
                          mean(pos_affect),
                          sd(pos_affect)
                        ))

#neg_affect
tbl8 <- as.data.frame(cmpst_class %>%
                        group_by(class) %>%
                        drop_na(neg_affect)%>%
                        summarise(
                          n(),
                          mean(neg_affect),
                          sd(neg_affect)
                        ))

#dssi_si
tbl9 <- as.data.frame(cmpst_class %>%
                        group_by(class) %>%
                        drop_na(dssi_si)%>%
                        summarise(
                          n(),
                          mean(dssi_si),
                          sd(dssi_si)
                        ))

#dssi_ss
tbl10 <- as.data.frame(cmpst_class %>%
                        group_by(class) %>%
                        drop_na(dssi_ss)%>%
                        summarise(
                          n(),
                          mean(dssi_ss),
                          sd(dssi_ss)
                        ))



#dssi_is
tbl11 <- as.data.frame(cmpst_class %>%
                         group_by(class) %>%
                         drop_na(dssi_is)%>%
                         summarise(
                           n(),
                           mean(dssi_is),
                           sd(dssi_is)
                         ))

#dssi_net
tbl12 <- as.data.frame(cmpst_class %>%
                         group_by(class) %>%
                         drop_na(dssi_net)%>%
                         summarise(
                           n(),
                           mean(dssi_net),
                           sd(dssi_net)
                         ))

tbl13 <- as.data.frame(cmpst_class %>%
                         group_by(class) %>%
                         drop_na(hvlt_immed)%>%
                         summarise(
                           n(),
                           mean(hvlt_immed),
                           sd(hvlt_immed)
                         ))


tbl14 <- as.data.frame(cmpst_class %>%
                         group_by(class) %>%
                         drop_na(hvlt_discrim)%>%
                         summarise(
                           n(),
                           mean(hvlt_discrim),
                           sd(hvlt_discrim)
                         ))



tbl15 <- as.data.frame(cmpst_class %>%
                         group_by(class) %>%
                         drop_na(stroop_word)%>%
                         summarise(
                           n(),
                           mean(stroop_word),
                           sd(stroop_word)
                         ))


tbl16 <- as.data.frame(cmpst_class %>%
                         group_by(class) %>%
                         drop_na(stroop_color)%>%
                         summarise(
                           n(),
                           mean(stroop_color),
                           sd(stroop_color)
                         ))

tbl17 <- as.data.frame(cmpst_class %>%
                         group_by(class) %>%
                         drop_na(stroop_color_word)%>%
                         summarise(
                           n(),
                           mean(stroop_color_word),
                           sd(stroop_color_word)
                         ))

cb <- function(x) {
  as.matrix(cbind(x[1,], x[2,]))
}


a<- as.data.frame(rbind(cb(tbl5), cb(tbl6), cb(tbl7), cb(tbl8),cb(tbl9),cb(tbl10),cb(tbl11),cb(tbl12),cb(tbl13),cb(tbl14),cb(tbl15),cb(tbl16),cb(tbl17)))


write_xlsx(a,"/Users/rachellee/Google Drive/Practicum/sum stat/CMPST Table 1 class.xlsx")

cmpst_class %>%
  group_by(class) %>%
  select(stroop_color_word) %>%
  summarise_all(funs(sum(is.na(.))))



t.test(cmpst_class$stroop_color_word ~ class, cmpst_class)


