rm(list=ls())
library(ggplot2)
library(readxl)
library(scales)
library(statebins)
library(hrbrthemes)
library(tidyverse)
library(colorspace)

CancerScrn<- read_excel("H:/1. Low Value Care Project/Graphs/3. State Level Variations/Data_State_Variation.xlsx", sheet="CancerScrn")
DiagPrevTest<- read_excel("H:/1. Low Value Care Project/Graphs/3. State Level Variations/Data_State_Variation.xlsx", sheet="DiagPrevTest")
PreopTest<- read_excel("H:/1. Low Value Care Project/Graphs/3. State Level Variations/Data_State_Variation.xlsx", sheet="PreopTest")
Imaging<- read_excel("H:/1. Low Value Care Project/Graphs/3. State Level Variations/Data_State_Variation.xlsx", sheet="Imaging")
PreopCataract<- read_excel("H:/1. Low Value Care Project/Graphs/3. State Level Variations/Data_State_Variation.xlsx", sheet="PreopCataract")
Cardio<- read_excel("H:/1. Low Value Care Project/Graphs/3. State Level Variations/Data_State_Variation.xlsx", sheet="Cardio")
Musc<- read_excel("H:/1. Low Value Care Project/Graphs/3. State Level Variations/Data_State_Variation.xlsx", sheet="Musc")

##################CANCER SCREENING CATEGORY##################
ggplot(CancerScrn, aes(state=STATE, fill=mean_tc_MCR)) +
  geom_statebins(lbl_size = 4) + 
  coord_equal() + 
  theme_statebins(legend_position = "right", base_size = 12) +
  labs(title="Mean Unit Cost of Low-Value Cancer Screening", subtitle="2009-2019, Medicare Advantage Enrollees") + 
  theme(plot.title = element_text(hjust = .5, size=16, face = "bold")) +
  theme(plot.subtitle = element_text(hjust = .65, size=14)) +
  scale_fill_continuous_sequential(name="Cost per Procedure ($USD)", limits=c(190,400), breaks = seq(190,400, by = 60), palette = "Greens", labels=comma)

##################DIAGNOSTIC & PREVENTATIVE TESTING CATEGORY##################
ggplot(DiagPrevTest, aes(state=STATE, fill=mean_tc_MCR)) +
  geom_statebins(lbl_size = 4) + 
  coord_equal() + 
  theme_statebins(legend_position = "right", base_size = 12) +
  labs(title="Mean Service Cost of Low-Value Diagnostic & Preventative Testing", subtitle="2009-2019, Medicare Advantage Enrollees") + 
  theme(plot.title = element_text(hjust = .5, size=16, face = "bold")) +
  theme(plot.subtitle = element_text(hjust = .65, size=14)) +
  scale_fill_continuous_sequential(name="Cost per Procedure ($USD)", limits=c(0,170), breaks = seq(0,170, by = 50), palette = "Greens", labels=comma)

##################PREOPERATIVE TESTING CATEGORY##################
ggplot(PreopTest, aes(state=STATE, fill=mean_tc_MCR)) +
  geom_statebins(lbl_size = 4) + 
  coord_equal() + 
  theme_statebins(legend_position = "right", base_size = 12) +
  labs(title="Mean Service Cost of Low-Value Preoperative Testing", subtitle="2009-2019, Medicare Advantage Enrollees") + 
  theme(plot.title = element_text(hjust = .5, size=16, face = "bold")) +
  theme(plot.subtitle = element_text(hjust = .65, size=14)) +
  scale_fill_continuous_sequential(name="Cost per Procedure ($USD)", limits=c(0,420), breaks = seq(0,420, by = 140), palette = "Greens", labels=comma)

##################IMAGING CATEGORY##################
ggplot(Imaging, aes(state=STATE, fill=mean_tc_MCR)) +
  geom_statebins(lbl_size = 4) + 
  coord_equal() + 
  theme_statebins(legend_position = "right", base_size = 12) +
  labs(title="Mean Service Cost of Low-Value Imaging", subtitle="2009-2019, Medicare Advantage Enrollees") + 
  theme(plot.title = element_text(hjust = .5, size=16, face = "bold")) +
  theme(plot.subtitle = element_text(hjust = .65, size=14)) +
  scale_fill_continuous_sequential(name="Cost per Procedure ($USD)", limits=c(130,350), breaks = seq(130,350, by = 70), palette = "Greens", labels=comma)

##########PREOPERATIVE CARDIAC TESTS FOR CATARACT SURGERIES CATEGORY##########
ggplot(PreopCataract, aes(state=STATE, fill=mean_tc_MCR)) +
  geom_statebins(lbl_size = 4) + 
  coord_equal() + 
  theme_statebins(legend_position = "right", base_size = 12) +
  labs(title="Mean Service Cost of Preoperative Cardiac Tests for Cataract Surgeries", subtitle="2009-2019, Medicare Advantage Enrollees") + 
  theme(plot.title = element_text(hjust = .5, size=16, face = "bold")) +
  theme(plot.subtitle = element_text(hjust = .65, size=14)) +
  scale_fill_continuous_sequential(name="Cost per Procedure ($USD)", limits=c(0,150), breaks = seq(0,150, by = 50), palette = "Greens", labels=comma)

##########cARDIOVASCULAR TESTING & PROCEDURES CATEGORY##########
ggplot(Cardio, aes(state=STATE, fill=mean_tc_MCR)) +
  geom_statebins(lbl_size = 4) + 
  coord_equal() + 
  theme_statebins(legend_position = "right", base_size = 12) +
  labs(title="Mean Service Cost of Cardiovascular Testing & Procedures", subtitle="2009-2019, Medicare Advantage Enrollees") + 
  theme(plot.title = element_text(hjust = .5, size=16, face = "bold")) +
  theme(plot.subtitle = element_text(hjust = .65, size=14)) +
  scale_fill_continuous_sequential(name="Cost per Procedure ($USD)", limits=c(50,1400), breaks = seq(50,1400, by = 400), palette = "Greens", labels=comma)

##########MUSCULOSKELETAL SURGERIES & PROCEDURES CATEGORY##########
ggplot(Musc, aes(state=STATE, fill=mean_tc_MCR)) +
  geom_statebins(lbl_size = 4) + 
  coord_equal() + 
  theme_statebins(legend_position = "right", base_size = 12) +
  labs(title="Mean Service Cost of Musculoskeletal Surgeries & Procedures", subtitle="2009-2019, Medicare Advantage Enrollees") + 
  theme(plot.title = element_text(hjust = .5, size=16, face = "bold")) +
  theme(plot.subtitle = element_text(hjust = .65, size=14)) +
  scale_fill_continuous_sequential(name="Cost per Procedure ($USD)", limits=c(100,2200), breaks = seq(100,2200, by = 600), palette = "Greens", labels=comma) + 
  guides(fill = guide_colourbar(barwidth = 20, barheight = .75, raster=TRUE, label.position = "top"))
