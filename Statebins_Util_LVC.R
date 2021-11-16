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
Agg<- read_excel("H:/1. Low Value Care Project/Graphs/3. State Level Variations/Data_State_Variation.xlsx", sheet="Aggregated")
Ind<- read_excel("H:/1. Low Value Care Project/Graphs/3. State Level Variations/Data_State_Variation.xlsx", sheet="CanScrnInd")


##################CANCER SCREENING CATEGORY##################
ggplot(CancerScrn, aes(state=STATE, fill=Util_MCR)) +
  geom_statebins(lbl_size = 4) + 
  coord_equal() + 
  theme_statebins(legend_position = "bottom", base_size = 12) +
  labs(title="Low-Value Care\nCancer Screening Utilization Rate", subtitle="2009-2019, Medicare Advantage Enrollees") + 
  theme(plot.title = element_text(hjust = .5, size=16, face = "bold")) +
  theme(plot.subtitle = element_text(hjust = .65, size=14)) +
  scale_fill_continuous_sequential(name="Utilization Rate\n(Per 100,000)", limits=c(6000,13500), breaks = c(7000,8000,9000,10000,11000,12000,13000), palette = "Reds", labels=comma) + 
  guides(fill = guide_colourbar(barwidth = 20, barheight = .75, raster=TRUE))

##################DIAGNOSTIC & PREVENTATIVE TESTING CATEGORY##################
ggplot(DiagPrevTest, aes(state=STATE, fill=Util_MCR)) +
  geom_statebins(lbl_size = 4) + 
  coord_equal() + 
  theme_statebins(legend_position = "right", base_size = 12) +
  labs(title="Low-Value Care\nDiagnostic & Preventive Testing Utilization Rate", subtitle="2009-2019, Medicare Advantage Enrollees") + 
  theme(plot.title = element_text(hjust = .5, size=16, face = "bold")) +
  theme(plot.subtitle = element_text(hjust = .65, size=14)) +
  scale_fill_continuous_sequential(name="Utilization Rate (Per 100,000)", limits=c(600,8000), breaks = seq(600,8000, by = 2000), palette = "Reds", labels=comma)

##################PREOPERATIVE TESTING CATEGORY##################
ggplot(PreopTest, aes(state=STATE, fill=Util_MCR)) +
  geom_statebins(lbl_size = 4) + 
  coord_equal() + 
  theme_statebins(legend_position = "right", base_size = 12) +
  labs(title="Low-Value Care\nPreoperative Testing Utilization Rate", subtitle="2009-2019, Medicare Advantage Enrollees") + 
  theme(plot.title = element_text(hjust = .5, size=16, face = "bold")) +
  theme(plot.subtitle = element_text(hjust = .65, size=14)) +
  scale_fill_continuous_sequential(name="Utilization Rate (Per 100,000)", limits=c(3000,9600), breaks = seq(3000,9000, by = 2000), palette = "Reds", labels=comma)

##################IMAGING CATEGORY##################
ggplot(Imaging, aes(state=STATE, fill=Util_MCR)) +
  geom_statebins(lbl_size = 4) + 
  coord_equal() + 
  theme_statebins(legend_position = "right", base_size = 12) +
  labs(title="Low-Value Care\nImaging Utilization Rate", subtitle="2009-2019, Medicare Advantage Enrollees") + 
  theme(plot.title = element_text(hjust = .5, size=16, face = "bold")) +
  theme(plot.subtitle = element_text(hjust = .65, size=14)) +
  scale_fill_continuous_sequential(name="Utilization Rate (Per 100,000)", limits=c(2500,7000), breaks = seq(2500,7000, by = 1500), palette = "Reds", labels=comma)

##########PREOPERATIVE CARDIAC TESTS FOR CATARACT SURGERIES CATEGORY##########
ggplot(PreopCataract, aes(state=STATE, fill=Util_MCR)) +
  geom_statebins(lbl_size = 4) + 
  coord_equal() + 
  theme_statebins(legend_position = "bottom", base_size = 12) +
  labs(title="Low-Value Care\nPreoperative Cardiac Tests for Cataract Surgeries Utilization Rate", subtitle="2009-2019, Medicare Advantage Enrollees") + 
  theme(plot.title = element_text(hjust = .5, size=16, face = "bold")) +
  theme(plot.subtitle = element_text(hjust = .65, size=14)) +
  scale_fill_continuous_sequential(name="Utilization Rate\n(Per 100,000)", limits=c(250,9000), breaks = c(1000,3000,5000,7000,9000), palette = "Reds", labels=comma) + 
  guides(fill = guide_colourbar(barwidth = 20, barheight = .75, raster=TRUE))

##########cARDIOVASCULAR TESTING & PROCEDURES CATEGORY##########
ggplot(Cardio, aes(state=STATE, fill=Util_MCR)) +
  geom_statebins(lbl_size = 4) + 
  coord_equal() + 
  theme_statebins(legend_position = "bottom", base_size = 12) +
  labs(title="Low-Value Care\nCardiovascular Testing & Procedures Utilization Rate", subtitle="2009-2019, Medicare Advantage Enrollees") + 
  theme(plot.title = element_text(hjust = .5, size=16, face = "bold")) +
  theme(plot.subtitle = element_text(hjust = .65, size=14)) +
  scale_fill_continuous_sequential(name="Utilization Rate\n(Per 100,000)", limits=c(600,2100), breaks = seq(600,2100, by = 500), palette = "Reds", labels=comma) + 
  guides(fill = guide_colourbar(barwidth = 20, barheight = .75, raster=TRUE))

##########MUSCULOSKELETAL SURGERIES & PROCEDURES CATEGORY##########
ggplot(Musc, aes(state=STATE, fill=Util_MCR)) +
  geom_statebins(lbl_size = 4) + 
  coord_equal() + 
  theme_statebins(legend_position = "bottom", base_size = 12) +
  labs(title="Low-Value Care\nMusculoskeletal Surgeries & Procedures Utilization Rate", subtitle="2009-2019, Medicare Advantage Enrollees") + 
  theme(plot.title = element_text(hjust = .5, size=16, face = "bold")) +
  theme(plot.subtitle = element_text(hjust = .65, size=14)) +
  scale_fill_continuous_sequential(name="Utilization Rate\n(Per 100,000)", limits=c(300,1500), breaks = c(350, 700 , 1050, 1400), palette = "Reds", labels=comma) + 
  guides(fill = guide_colourbar(barwidth = 20, barheight = .75, raster=TRUE))

##########27 INDICATED SERVICES AGGREGATED##########
ggplot(Ind, aes(state=STATE, fill=Util_OVERALL)) +
  geom_statebins(lbl_size = 4, radius = grid::unit(2.5, "pt"), na.rm = TRUE) + 
  coord_equal(.85) + 
  theme_statebins(legend_position = "bottom", base_size = 12) +
  labs(title="Appropriate Cancer Screening Utilization Rate", subtitle="2009-2019") + 
  theme(plot.title = element_text(hjust = .5, size=16, face = "bold")) +
  theme(plot.subtitle = element_text(hjust = .5, size=14)) +
  scale_fill_continuous_sequential(name="Utilization Rate\n(Per 100,000)", limits=c(1200,21500), breaks = c(5000, 10000, 15000, 20000), palette = "Reds", labels=comma) +
  guides(fill = guide_colourbar(barwidth = 20, barheight = .75, raster=TRUE))


