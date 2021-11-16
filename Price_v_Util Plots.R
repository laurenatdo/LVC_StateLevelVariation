rm(list=ls())
library(ggplot2)
library(dplyr)
library(readxl)
library(scales)
library(ggrepel)
library(ggpubr)
library(extrafont)
library(extrafontdb)

Agg<- read_excel("H:/1. Low Value Care Project/Graphs/3. State Level Variations/07.29.2021/Aggregated Collapsed.xlsx", sheet="Sheet1")
CancerScrn<- read_excel("H:/1. Low Value Care Project/Graphs/3. State Level Variations/07.29.2021/Cancer Screenings Collapsed.xlsx", sheet="Sheet1")
DiagPrevTest<- read_excel("H:/1. Low Value Care Project/Graphs/3. State Level Variations/07.29.2021/Diagnostic Collapsed.xlsx", sheet="Sheet1")
PreopTest<- read_excel("H:/1. Low Value Care Project/Graphs/3. State Level Variations/07.29.2021/Preoperative Collapsed.xlsx", sheet="Sheet1")
Imaging<- read_excel("H:/1. Low Value Care Project/Graphs/3. State Level Variations/07.29.2021/Imaging Collapsed.xlsx", sheet="Sheet1")
Cardio<- read_excel("H:/1. Low Value Care Project/Graphs/3. State Level Variations/07.29.2021/Cardiovascular Collapsed.xlsx", sheet="Sheet1")
Musc<- read_excel("H:/1. Low Value Care Project/Graphs/3. State Level Variations/07.29.2021/Musculoskeletal Collapsed.xlsx", sheet="Sheet1")


###############################Aggregated##################################
ggplot(Agg, aes(x=tc_COM_MCR_LN, y=Util_COM_MCR_LN)) +
  geom_smooth(method='lm', color='firebrick', fill='gray82') +
  stat_regline_equation(label.y=8.1, label.x= 5.72, aes(label = ..eq.label..), size=7, color='firebrick', family="Palatino Linotype") +
  stat_regline_equation(label.y=8.05, label.x= 5.72, aes(label = ..rr.label..), size=7, color='firebrick', family="Palatino Linotype") +
  geom_point(size=3, color='firebrick') +
  scale_x_continuous(labels=scales::dollar_format(), name = "Mean Procedure Cost ($USD)") +
  scale_y_continuous(labels = comma, name = "Utilization Rate per 100,000") +
  ggtitle("23 Low-Value Care Measures") +
  theme_bw() + theme(
    legend.position = "none",
    text=element_text(family="Palatino Linotype"),
    axis.text=element_text(size=14, color = "black"),
    axis.title.x = element_text(size=18, face="bold", margin=margin(t=10)),
    axis.title.y = element_text(size=18, face="bold", margin=margin(r=10)),
    axis.line = element_line(colour = "black"),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    plot.title = element_text(hjust = 0.5, size=15, face = "bold")) +
  geom_text_repel(aes(label=STATE),size=4.25, family="Palatino Linotype") 

###############################Cancer Screening##################################
ggplot(CancerScrn, aes(x=tc_COM_MCR_LN, y=Util_COM_MCR_LN)) +
  geom_smooth(method='lm', color='firebrick', fill='gray82') +
  stat_regline_equation(label.y=9.75, label.x= 4.9, aes(label = ..eq.label..), size=7, color='firebrick', family="Palatino Linotype") +
  stat_regline_equation(label.y=9.7, label.x= 4.9, aes(label = ..rr.label..), size=7, color='firebrick', family="Palatino Linotype") +
  geom_point(size=3, color='firebrick') +
  scale_x_continuous(labels=scales::dollar_format(), name = "Mean Procedure Cost ($USD)") +
  scale_y_continuous(labels = comma, name = "Utilization Rate per 100,000") +
  ggtitle("Cancer Screenings") +
  theme_bw() + theme(
    legend.position = "none",
    text=element_text(family="Palatino Linotype"),
    axis.text=element_text(size=14, color = "black"),
    axis.title.x = element_text(size=18, face="bold", margin=margin(t=10)),
    axis.title.y = element_text(size=18, face="bold", margin=margin(r=10)),
    axis.line = element_line(colour = "black"),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    plot.title = element_text(hjust = 0.5, size=15, face = "bold")) +
  geom_text_repel(aes(label=STATE),size=4.25, family="Palatino Linotype") 

###############################Diagnostic & Preventive##################################
ggplot(DiagPrevTest, aes(x=tc_COM_MCR_LN, y=Util_COM_MCR_LN)) +
  geom_smooth(method='lm', color='firebrick', fill='gray82') +
  stat_regline_equation(label.y=8.5, label.x= 4.05, aes(label = ..eq.label..), size=7, color='firebrick', family="Palatino Linotype") +
  stat_regline_equation(label.y=8.4, label.x= 4.05, aes(label = ..rr.label..), size=7, color='firebrick', family="Palatino Linotype") +
  geom_point(size=3, color='firebrick') +
    scale_x_continuous(labels=scales::dollar_format(), name = "Mean Procedure Cost ($USD)") +
    scale_y_continuous(labels = comma, name = "Utilization Rate per 100,000")+
  ggtitle("Diagnositic & Preventive Testing") +
  theme_bw() + theme(
    legend.position = "none",
    text=element_text(family="Palatino Linotype"),
    axis.text=element_text(size=14, color = "black"),
    axis.title.x = element_text(size=18, face="bold", margin=margin(t=10)),
    axis.title.y = element_text(size=18, face="bold", margin=margin(r=10)),
    axis.line = element_line(colour = "black"),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    plot.title = element_text(hjust = 0.5, size=15, face = "bold")) +
 geom_text_repel(aes(label=STATE),size=4.25, family="Palatino Linotype")

###############################Preoperative##################################
ggplot(PreopTest, aes(x=tc_COM_MCR_LN, y=Util_COM_MCR_LN)) +
  geom_smooth(method='lm', color='firebrick', fill='gray82') +
  stat_regline_equation(label.y=8.9, label.x= 2.75, aes(label = ..eq.label..), size=7, color='firebrick', family="Palatino Linotype") +
  stat_regline_equation(label.y=8.8, label.x=2.75, aes(label = ..rr.label..), size=7, color='firebrick', family="Palatino Linotype") +
  geom_point(size=3, color='firebrick') +
    scale_x_continuous(labels=scales::dollar_format(), name = "Mean Procedure Cost ($USD)") +
    scale_y_continuous(labels = comma, name = "Utilization Rate per 100,000") +
  ggtitle("Preoperative Testing") +
  theme_bw() + theme(
    legend.position = "none",
    text=element_text(family="Palatino Linotype"),
    axis.text=element_text(size=14, color = "black"),
    axis.title.x = element_text(size=18, face="bold", margin=margin(t=10)),
    axis.title.y = element_text(size=18, face="bold", margin=margin(r=10)),
    axis.line = element_line(colour = "black"),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    plot.title = element_text(hjust = 0.5, size=15, face = "bold")) +
 geom_text_repel(aes(label=STATE),size=4.25, family="Palatino Linotype")

###############################Imaging##################################
ggplot(Imaging, aes(x=tc_COM_MCR_LN, y=Util_COM_MCR_LN)) +
  geom_smooth(method='lm', color='firebrick', fill='gray82') +
  stat_regline_equation(label.y=8.6, label.x= 6.2, aes(label = ..eq.label..), size=7, color='firebrick', family="Palatino Linotype") +
  stat_regline_equation(label.y=8.54, label.x= 6.2, aes(label = ..rr.label..), size=7, color='firebrick', family="Palatino Linotype") +
  geom_point(size=3, color='firebrick') +
    scale_x_continuous(labels=scales::dollar_format(), name = "Mean Procedure Cost ($USD)") +
    scale_y_continuous(labels = comma, name = "Utilization Rate per 100,000") +
  ggtitle("Imaging") +
  theme_bw() + theme(
    legend.position = "none",
    text=element_text(family="Palatino Linotype"),
    axis.text=element_text(size=14, color = "black"),
    axis.title.x = element_text(size=18, face="bold", margin=margin(t=10)),
    axis.title.y = element_text(size=18, face="bold", margin=margin(r=10)),
    axis.line = element_line(colour = "black"),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    plot.title = element_text(hjust = 0.5, size=15, face = "bold")) +
 geom_text_repel(aes(label=STATE),size=4.25, family="Palatino Linotype")

###############################Cardiovascular##################################
ggplot(Cardio, aes(x=tc_COM_MCR_LN, y=Util_COM_MCR_LN)) +
  geom_smooth(method='lm', color='firebrick', fill='gray82') + 
  stat_regline_equation(label.y=7.2, label.x= 6.8, aes(label = ..eq.label..), size=7, color='firebrick', family="Palatino Linotype") +
  stat_regline_equation(label.y=7.1, label.x= 6.8, aes(label = ..rr.label..), size=7, color='firebrick', family="Palatino Linotype") +
  geom_point(size=3, color='firebrick') +
    scale_x_continuous(labels=scales::dollar_format(), name = "Mean Procedure Cost ($USD)") +
    scale_y_continuous(labels = comma, name = "Utilization Rate per 100,000") + 
  ggtitle("Cardiovasular Procedures") +
  theme_bw() + theme(
    legend.position = "none",
    text=element_text(family="Palatino Linotype"),
    axis.text=element_text(size=14, color = "black"),
    axis.title.x = element_text(size=18, face="bold", margin=margin(t=10)),
    axis.title.y = element_text(size=18, face="bold", margin=margin(r=10)),
    axis.line = element_line(colour = "black"),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    plot.title = element_text(hjust = 0.5, size=15, face = "bold")) +
  geom_text_repel(aes(label=STATE),size=4.25, family="Palatino Linotype")

###############################Musculoskeletal##################################
ggplot(Musc, aes(Util_COM_MCR_LN, tc_COM_MCR_LN)) +
  geom_smooth(method='lm', color='firebrick', fill='gray82') + 
  stat_regline_equation(label.y=7.8, label.x= 6.65, aes(label = ..eq.label..), size=7, color='firebrick', family="Palatino Linotype") +
  stat_regline_equation(label.y=7.73, label.x= 6.65, aes(label = ..rr.label..), size=7, color='firebrick', family="Palatino Linotype") +
  geom_point(size=3, color='firebrick') +
    scale_x_continuous(labels=scales::dollar_format(), name = "Mean Procedure Cost ($USD)") +
    scale_y_continuous(labels = comma, name = "Utilization Rate per 100,000") +
  ggtitle("Musculoskeletal Surgeries & Procedures") +
  theme_bw() + theme(
    legend.position = "none",
    text=element_text(family="Palatino Linotype"),
    axis.text=element_text(size=14, color = "black"),
    axis.title.x = element_text(size=18, face="bold", margin=margin(t=10)),
    axis.title.y = element_text(size=18, face="bold", margin=margin(r=10)),
    axis.line = element_line(colour = "black"),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    plot.title = element_text(hjust = 0.5, size=15, face = "bold")) +
  geom_text_repel(aes(label=STATE),size=4.25, family="Palatino Linotype")