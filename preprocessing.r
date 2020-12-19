library(tidyverse)
library(janitor)
library(ggplot2)
install.packages("papeR")
install.packages("devtools")
library("devtools")
install_github("hofnerb/papeR")
library("papeR")
li

small <- read.csv("clean_scc.csv", stringsAsFactors=TRUE)
summarize_factor(small, group = "gender")  

mydata = read.csv(“new_cc.csv”)
library(“devtools”)
library(“papeR”)


cc_1 = readxl::read_xls("Bail Reform Case Records 05092019 Seg 1.xls")
cc_2 = readxl::read_xls("Bail Reform Case Records 05092019 Seg 2.xls")
cc_3 = readxl::read_xls("Bail Reform Case Records 05092019 Seg 3.xls")
cc_4 = readxl::read_xls("Bail Reform Case Records 05092019 Seg 4.xls")
cc_5 = readxl::read_xls("Bail Reform Case Records 05092019 Seg 5.xls")
cc_6 = readxl::read_xls("Bail Reform Case Records 05092019 Seg 6.xls")
cc = bind_rows(cc_1, cc_2, cc_3, cc_4, cc_5, cc_6)
scc = cc %>%
  sample_frac(0.1) %>% 
  clean_names() %>% 
  write_csv("clean_scc.csv")
names(scc)
new_cc = cc %>%
  clean_names() %>% 
  write_csv("new_cc.csv")


write_csv("cc.csv")

write_csv("cc.csv")
unique(cc$PrePostGOInidcator)


before_go <- subset(data, data$pre_post_go_inidcator == "Pre G.O. 18.8A")
after_go <- subset(cc, cc$PrePostGOInidcator == "Post G.O. 18.8A")

#df
df <- data.frame(matrix(NA, nrow = 6, ncol = 3))

df$x<-unique(cc$PSASuperRec)
cc %>% count(cc$PSASuperRec, cc$PrePostGOInidcator)


ggplot(cc, aes(x=cc$InitialBondOrder, fill=cc$PrePostGOIndicator))+
  geom_bar(position="dodge", width=0.7)+
  theme_minimal()
after_go %>% count(RaceEthnicity)

ggplot(before_go, aes(x=factor(InitialBondOrder)))+
  geom_bar(stat="count", width=0.7, fill="steelblue")+
  theme_minimal()

ggplot(after_go, aes(x=factor(InitialBondOrder)))+
  geom_bar(stat="count", width=0.7, fill="steelblue")+
  theme_minimal()

#tables (done)
prop_tab_all <- prop.table(table(after_go$InitialBondOrder,after_go$PSASuperRec), margin=2)%>% `*`(100) %>% round(2)
prop_tab_all

prop_tab_allb <- (prop.table(table(before_go$InitialBondOrder,before_go$PSASuperRec), margin=2))%>% `*`(100) %>% round(2)
prop_tab_allb

prop_tab_pre <- prop.table(table(pre$InitialBondOrder,before_go$PSASuperRec), margin=2)%>% `*`(100) %>% round(2)
prop_tab_pre

prop_tab_post <- prop.table(table(after_go$InitialBondOrder,after_go$PSASuperRec), margin=2)%>% `*`(100) %>% round(2)
prop_tab_post

white <- subset(before_go, before_go$RaceEthnicity == "White Non-Hispanic")
brown <- subset(before_go, before_go$RaceEthnicity != "White Non-Hispanic")
black <- subset(before_go, before_go$RaceEthnicity == "Black")

white2 <- subset(after_go, after_go$RaceEthnicity == "White Non-Hispanic")
brown2 <- subset(after_go, after_go$RaceEthnicity != "White Non-Hispanic")
black2 <- subset(after_go, after_go$RaceEthnicity == "Black")

print("white before GO")
prop_tab_white <- prop.table(table(white$InitialBondOrder,white$PSASuperRec), margin=2)%>% `*`(100) %>% round(2)
prop_tab_white

print("black before GO")
prop_tab_black <- prop.table(table(black$InitialBondOrder,black$PSASuperRec), margin=2)%>% `*`(100) %>% round(2)
prop_tab_black

print("brown before GO")
prop_tab_brown <- prop.table(table(brown$InitialBondOrder,brown$PSASuperRec), margin=2)%>% `*`(100) %>% round(2)
prop_tab_brown

#AFTER
print("white after GO")
prop_tab_white2 <- prop.table(table(white2$InitialBondOrder,white2$PSASuperRec), margin=2)%>% `*`(100) %>% round(2)
prop_tab_white2


print("black after GO")
prop_tab_black2 <- prop.table(table(black2$InitialBondOrder,black2$PSASuperRec), margin=2)%>% `*`(100) %>% round(2)
prop_tab_black2

print("brown after GO")
prop_tab_brown2 <- prop.table(table(brown2$InitialBondOrder,brown2$PSASuperRec), margin=2)%>% `*`(100) %>% round(2)
prop_tab_brown2

library(xtable)

print(xtable(prop_tab_all, type = "latex"), file = "filename2.tex")
xtable(prop_tab_all)

#summary statistics table
install.packages('rmarkdown')
sum

summarize(pre, type="factor")


pre$pre_post_go_inidcator





mydata = read.csv(“new_cc.csv”)
library(“devtools”)
library(“papeR”)

is.ldf(mydata)
labels(mydata)

labels(mydata) <- c(“Record ID”,“Post General Order 18.8A Indicator”,“Charge Category”, “Charge Class”, “Accompanying Matter”,“Gender” , “Race”,“Age group”,“PSA Recommendation”,“Failure to Appear Score”,“New Criminal Activity Score” ,“PSA Violence Flag”,“Initial Bond Order”,“Individual Recognizance Bond Order” ,“Deposit Bond Order”,“Electronic Monitoring Order” ,“No Bail Order”,“Deposit Bond Category”,“Deposit Bond Amount”,“Pretrial Release Indicator”,“Failure to Appear Indicator”,“New Criminal Activity Indicator”,“New Violent Criminal Activity Indicator”)
