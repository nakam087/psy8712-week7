# Script Settings and Resources
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
library(tidyverse)
library(GGally)



# Data Import and Cleaning
week7_tbl <- read_csv("../data/week3.csv")%>% #i think it read in okay?
  mutate(timeStart=ymd_hms(timeStart))%>%
  mutate(condition=recode_factor(condition, "A"="Block A", "B"="Block B", "C"="Control"))%>%#is there a way to use replace here?
  mutate(gender=recode_factor(gender, "M"="Male","F"="Female"))%>% 
  mutate(timeSpent=timeEnd-timeStart)%>%
  mutate(timeSpent=as.numeric(timeSpent))%>%
  filter(q6==1)%>%
  select(-q6)




# Visualization
select(week7_tbl,q1:q10)%>%
ggpairs()

(ggplot(week7_tbl,aes(timeStart,q1))+
  geom_point()+
  labs(x="Date of Experiment",y="Q1 Score"))%>%
ggsave("../figs/fig1.png",.,height=4, width=7, dpi=600) #tried to make it be as close to the pics


(ggplot(week7_tbl, aes(q1,q2,color=gender))+
  geom_jitter()+
  labs(color="Participant Gender"))%>%
  ggsave("../figs/fig2.png",.,height=4, width=7, dpi=600)

(ggplot(week7_tbl, aes(q1,q2))+
    facet_wrap(week7_tbl$gender)+
    geom_jitter()+
    labs(x="Score on Q1",y="Score on Q2"))%>%
  ggsave("../figs/fig3.png",.,height=4,width=7, dpi=600)


(ggplot(week7_tbl,aes(gender,timeSpent))+
  geom_boxplot()+
  labs(x="Gender", y="Time Elapsed (mins)"))%>%
  ggsave("../figs/fig4.png",.,height=4,width=7, dpi=600)


(ggplot(week7_tbl,aes(q5,q7,color=condition))+
  geom_jitter()+
  geom_smooth(method="lm",se=F)+
  theme(legend.position="bottom", legend.background = element_rect("#DFDFDF"))+ #87.5 RGB
  labs(x="Score on Q5", y="Score on Q7", color="Experimental Condition"))%>%
  ggsave("../figs/fig5.png",.,height=4,width=7, dpi=600)
