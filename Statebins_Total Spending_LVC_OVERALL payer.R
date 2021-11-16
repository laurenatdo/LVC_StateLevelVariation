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
Cardio<- read_excel("H:/1. Low Value Care Project/Graphs/3. State Level Variations/Data_State_Variation.xlsx", sheet="Cardio")
Musc<- read_excel("H:/1. Low Value Care Project/Graphs/3. State Level Variations/Data_State_Variation.xlsx", sheet="Musc")
Agg<- read_excel("H:/1. Low Value Care Project/Graphs/3. State Level Variations/Data_State_Variation.xlsx", sheet="Aggregated")
Ind<- read_excel("H:/1. Low Value Care Project/Graphs/3. State Level Variations/Data_State_Variation.xlsx", sheet="CanScrnInd")

##################CANCER SCREENING CATEGORY##################
ggplot(CancerScrn, aes(state=STATE, fill=TotalSpend_COM_MCR)) +
  geom_statebins(lbl_size = 4) + 
  coord_equal(.95) + 
  theme_statebins(legend_position = "bottom", base_size = 12) +
  labs(title=" Mean Total Spending on Low-Value Cancer Screening", subtitle="2009-2019s") + 
  theme(plot.title = element_text(hjust = .5, size=16, face = "bold")) +
  theme(plot.subtitle = element_text(hjust = .65, size=14)) +
  scale_fill_continuous_sequential(name=" Mean Total Spending\n($USD in Thousands)", limits=c(1600,3800), breaks = seq(1600,3800, by = 650), palette = "Greens", labels=comma) + 
  guides(fill = guide_colourbar(barwidth = 20, barheight = .75, raster=TRUE))

##################DIAGNOSTIC & PREVENTATIVE TESTING CATEGORY##################
ggplot(DiagPrevTest, aes(state=STATE, fill=TotalSpend_COM_MCR)) +
  geom_statebins(lbl_size = 4) + 
  coord_equal(.95) + 
  theme_statebins(legend_position = "right", base_size = 12) +
  labs(title=" Mean Total Spending on Low-Value Diagnostic & Preventative Testing", subtitle="2009-2019s") + 
  theme(plot.title = element_text(hjust = .5, size=16, face = "bold")) +
  theme(plot.subtitle = element_text(hjust = .65, size=14)) +
  scale_fill_continuous_sequential(name=" Mean Total Spending\n($USD in Thousands)", limits=c(0,1000), breaks = seq(0,1000, by = 300), palette = "Greens", labels=comma)

##################PREOPERATIVE TESTING CATEGORY##################
ggplot(PreopTest, aes(state=STATE, fill=TotalSpend_COM_MCR)) +
  geom_statebins(lbl_size = 4) + 
  coord_equal(.95) + 
  theme_statebins(legend_position = "right", base_size = 12) +
  labs(title=" Mean Total Spending on Low-Value Preoperative Testing", subtitle="2009-2019s") + 
  theme(plot.title = element_text(hjust = .5, size=16, face = "bold")) +
  theme(plot.subtitle = element_text(hjust = .65, size=14)) +
  scale_fill_continuous_sequential(name=" Mean Total Spending\n($USD in Thousands)", limits=c(50,3100), breaks = seq(50,3100, by = 1000), palette = "Greens", labels=comma)

##################IMAGING CATEGORY##################
ggplot(Imaging, aes(state=STATE, fill=TotalSpend_COM_MCR)) +
  geom_statebins(lbl_size = 4) + 
  coord_equal(.95) + 
  theme_statebins(legend_position = "right", base_size = 12) +
  labs(title=" Mean Total Spending on Low-Value Imaging", subtitle="2009-2019s") + 
  theme(plot.title = element_text(hjust = .5, size=16, face = "bold")) +
  theme(plot.subtitle = element_text(hjust = .65, size=14)) +
  scale_fill_continuous_sequential(name=" Mean Total Spending\n($USD in Thousands)", limits=c(350,1600), breaks = seq(350,1600, by = 400), palette = "Greens", labels=comma)

##########PREOPERATIVE CARDIAC TESTS FOR CATARACT SURGERIES CATEGORY##########
ggplot(PreopCataract, aes(state=STATE, fill=TotalSpend_COM_MCR)) +
  geom_statebins(lbl_size = 4) + 
  coord_equal(.95) + 
  theme_statebins(legend_position = "right", base_size = 12) +
  labs(title=" Mean Total Spending on Preoperative Cardiac Tests for Cataract Surgeries", subtitle="2009-2019s") + 
  theme(plot.title = element_text(hjust = .5, size=16, face = "bold")) +
  theme(plot.subtitle = element_text(hjust = .65, size=14)) +
  scale_fill_continuous_sequential(name=" Mean Total Spending\n($USD in Thousands)", limits=c(0,1100), breaks = seq(0,1100, by = 300), palette = "Greens", labels=comma)

##########cARDIOVASCULAR TESTING & PROCEDURES CATEGORY##########
ggplot(Cardio, aes(state=STATE, fill=TotalSpend_COM_MCR)) +
  geom_statebins(lbl_size = 4) + 
  coord_equal(.95) + 
  theme_statebins(legend_position = "right", base_size = 12) +
  labs(title=" Mean Total Spending on Low-Value Cardiovascular Testing & Procedures", subtitle="2009-2019s") + 
  theme(plot.title = element_text(hjust = .5, size=16, face = "bold")) +
  theme(plot.subtitle = element_text(hjust = .65, size=14)) +
  scale_fill_continuous_sequential(name=" Mean Total Spending\n($USD in Thousands)", limits=c(50,2700), breaks = seq(50,2700, by = 700), palette = "Greens", labels=comma)

##########MUSCULOSKELETAL SURGERIES & PROCEDURES CATEGORY##########
ggplot(Musck, aes(state=STATE, fill=TotalSpend_COM_MCR)) +
  geom_statebins(lbl_size = 4) + 
  coord_equal(.95) + 
  theme_statebins(legend_position = "right", base_size = 12) +
  labs(title=" Mean Total Spending on Low-Value Musculoskeletal Surgeries and Procedures", subtitle="2009-2019s") + 
  theme(plot.title = element_text(hjust = .5, size=16, face = "bold")) +
  theme(plot.subtitle = element_text(hjust = .65, size=14)) +
  scale_fill_continuous_sequential(name=" Mean Total Spending\n($USD in Thousands)", limits=c(80,2400), breaks = seq(80,2400, by = 650), palette = "Greens", labels=comma)

##########23 SERVICES AGGREGATED##########
ggplot(Agg, aes(state=STATE, fill=TotalSpend_COM_MCR)) +
  geom_statebins(lbl_size = 5.5, radius = grid::unit(9, "pt"), na.rm = TRUE) + 
  coord_equal(.95) + 
  theme_statebins(legend_position = "bottom", base_size = 12) +
  #labs(title=" Mean Total Spending on Low-Value Care Services", subtitle="2009-2019") + 
  #theme(plot.title = element_text(hjust = .5, size=16, face = "bold")) +
  theme(plot.subtitle = element_text(hjust = .65, size=14)) +
  scale_fill_continuous_sequential(name=" Mean Total Spending\n($USD in Millions)", limits=c(9,260), breaks = c(25, 50, 75,100,125,150,175,200, 225, 250), palette = "Greens", labels=comma) + 
  guides(fill = guide_colourbar(barwidth = 38, barheight = .75, raster=TRUE, label.position = "top"))

##########23 INDICATED SERVICES AGGREGATED##########
ggplot(Ind, aes(state=STATE, fill=TotalSpend_COM_MCR)) +
  geom_statebins(lbl_size = 4, radius = grid::unit(2.5, "pt"), na.rm = TRUE) + 
  coord_equal(.95) + 
  theme_statebins(legend_position = "bottom", base_size = 12) +
  labs(title=" Mean Total Spending on Appropriate Cancer Screening", subtitle="2009-2019") + 
  theme(plot.title = element_text(hjust = .5, size=16, face = "bold")) +
  theme(plot.subtitle = element_text(hjust = .65, size=14)) +
  scale_fill_continuous_sequential(name=" Mean Total Spending\n($USD in Millions)", limits=c(53,176), breaks = c(75,100,125,150,175), palette = "Greens", labels=comma) + 
  guides(fill = guide_colourbar(barwidth = 20, barheight = .75, raster=TRUE, label.position = "top"))
