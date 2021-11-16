rm(list=ls())
library(ggplot2)
library(readxl)
library(scales)
library(statebins)
library(hrbrthemes)
library(tidyverse)
library(colorspace)
library(ggrepel)
library(gridExtra)

CancerScrn<- read_excel("H:/1. Low Value Care Project/Graphs/3. State Level Variations/Data_State_Variation.xlsx", sheet="CancerScrn")
Ind<- read_excel("H:/1. Low Value Care Project/Graphs/3. State Level Variations/06.14.2021/Yearly_2009-2019_Indicated_Cancer Screenings_Aggregated.xlsx", sheet="Sheet 1")
DiagPrevTest<- read_excel("H:/1. Low Value Care Project/Graphs/3. State Level Variations/Data_State_Variation.xlsx", sheet="DiagPrevTest")
PreopTest<- read_excel("H:/1. Low Value Care Project/Graphs/3. State Level Variations/Data_State_Variation.xlsx", sheet="PreopTest")
Imaging<- read_excel("H:/1. Low Value Care Project/Graphs/3. State Level Variations/Data_State_Variation.xlsx", sheet="Imaging")
Cardio<- read_excel("H:/1. Low Value Care Project/Graphs/3. State Level Variations/Data_State_Variation.xlsx", sheet="Cardio")
Musc<- read_excel("H:/1. Low Value Care Project/Graphs/3. State Level Variations/Data_State_Variation.xlsx", sheet="Musc")
Agg<- read_excel("H:/1. Low Value Care Project/Graphs/3. State Level Variations/Data_State_Variation.xlsx", sheet="Aggregated")


##################CANCER SCREENING CATEGORY##################
plot1 <- ggplot(CancerScrn, aes(state=STATE, fill=Util_COM_MCR)) +
  geom_statebins(lbl_size = 5.5, radius = grid::unit(9, "pt"), na.rm = TRUE) + 
  coord_equal(.95) + 
  theme_statebins(legend_position = "bottom", base_size = 12) +
  labs(title="Low-Value Care") + 
  theme(plot.title = element_text(hjust = .5, size=16, face = "bold")) +
  theme(plot.subtitle = element_text(hjust = .65, size=14)) +
  scale_fill_continuous_sequential(name="Utilization Rate\n(Per 100,000)", limits=c(6000,16000), breaks = c(7000,9000,11000,13000, 15000), palette = "Reds", labels=comma) + 
  guides(fill = guide_colourbar(barwidth = 38, barheight = .75, raster=TRUE))

##########23 INDICATED SERVICES AGGREGATED##########
plot2 <- ggplot(Ind, aes(state=STATE, fill=Util_COM_MCR)) +
  geom_statebins(lbl_size = 5.5, radius = grid::unit(9, "pt"), na.rm = TRUE) + 
  coord_equal(.95) + 
  theme_statebins(legend_position = "bottom", base_size = 12) +
  labs(title="Appropriate Care") + 
  theme(plot.title = element_text(hjust = .5, size=16, face = "bold")) +
  theme(plot.subtitle = element_text(hjust = .65, size=14)) +
  scale_fill_continuous_sequential(name="", limits=c(12000,21500), breaks = c(13000, 15000, 17000, 19000, 21000), palette = "Blues", labels=comma) +
  guides(fill = guide_colourbar(barwidth = 38, barheight = .75, raster=TRUE))

grid.arrange(plot1, plot2, ncol=2)

##################DIAGNOSTIC & PREVENTATIVE TESTING CATEGORY##################
ggplot(DiagPrevTest, aes(state=STATE, fill=Util_COM_MCR)) +
  geom_statebins(lbl_size = 4) + 
  coord_equal(.95) + 
  theme_statebins(legend_position = "right", base_size = 12) +
  labs(title="Low-Value Care\nDiagnostic & Preventive Testing Utilization Rate", subtitle="2009-2019") + 
  theme(plot.title = element_text(hjust = .5, size=16, face = "bold")) +
  theme(plot.subtitle = element_text(hjust = .65, size=14)) +
  scale_fill_continuous_sequential(name="Utilization Rate (Per 100,000)", limits=c(600,8000), breaks = seq(600,8000, by = 2000), palette = "Reds", labels=comma)

##################PREOPERATIVE TESTING CATEGORY##################
ggplot(PreopTest, aes(state=STATE, fill=Util_COM_MCR)) +
  geom_statebins(lbl_size = 4) + 
  coord_equal(.95) + 
  theme_statebins(legend_position = "right", base_size = 12) +
  labs(title="Low-Value Care\nPreoperative Testing Utilization Rate", subtitle="2009-2019") + 
  theme(plot.title = element_text(hjust = .5, size=16, face = "bold")) +
  theme(plot.subtitle = element_text(hjust = .65, size=14)) +
  scale_fill_continuous_sequential(name="Utilization Rate (Per 100,000)", limits=c(3000,9600), breaks = seq(3000,9000, by = 2000), palette = "Reds", labels=comma)

##################IMAGING CATEGORY##################
ggplot(Imaging, aes(state=STATE, fill=Util_COM_MCR)) +
  geom_statebins(lbl_size = 4) + 
  coord_equal(.95) + 
  theme_statebins(legend_position = "right", base_size = 12) +
  labs(title="Low-Value Care\nImaging Utilization Rate", subtitle="2009-2019") + 
  theme(plot.title = element_text(hjust = .5, size=16, face = "bold")) +
  theme(plot.subtitle = element_text(hjust = .65, size=14)) +
  scale_fill_continuous_sequential(name="Utilization Rate (Per 100,000)", limits=c(2500,7000), breaks = seq(2500,7000, by = 1500), palette = "Reds", labels=comma)

##########PREOPERATIVE CARDIAC TESTS FOR CATARACT SURGERIES CATEGORY##########
ggplot(PreopCataract, aes(state=STATE, fill=Util_COM_MCR)) +
  geom_statebins(lbl_size = 4) + 
  coord_equal(.95) + 
  theme_statebins(legend_position = "bottom", base_size = 12) +
  labs(title="Low-Value Care\nPreoperative Cardiac Tests for Cataract Surgeries Utilization Rate", subtitle="2009-2019") + 
  theme(plot.title = element_text(hjust = .5, size=16, face = "bold")) +
  theme(plot.subtitle = element_text(hjust = .65, size=14)) +
  scale_fill_continuous_sequential(name="Utilization Rate\n(Per 100,000)", limits=c(250,9000), breaks = c(1000,3000,5000,7000,9000), palette = "Reds", labels=comma) + 
  guides(fill = guide_colourbar(barwidth = 38, barheight = .75, raster=TRUE))

##########cARDIOVASCULAR TESTING & PROCEDURES CATEGORY##########
ggplot(Cardio, aes(state=STATE, fill=Util_COM_MCR)) +
  geom_statebins(lbl_size = 4) + 
  coord_equal(.95) + 
  theme_statebins(legend_position = "bottom", base_size = 12) +
  labs(title="Low-Value Care\nCardiovascular Testing & Procedures Utilization Rate", subtitle="2009-2019") + 
  theme(plot.title = element_text(hjust = .5, size=16, face = "bold")) +
  theme(plot.subtitle = element_text(hjust = .65, size=14)) +
  scale_fill_continuous_sequential(name="Utilization Rate\n(Per 100,000)", limits=c(600,2100), breaks = seq(600,2100, by = 500), palette = "Reds", labels=comma) + 
  guides(fill = guide_colourbar(barwidth = 38, barheight = .75, raster=TRUE))

##########MUSCULOSKELETAL SURGERIES & PROCEDURES CATEGORY##########
ggplot(Musc, aes(state=STATE, fill=Util_COM_MCR)) +
  geom_statebins(lbl_size = 4) + 
  coord_equal(.95) + 
  theme_statebins(legend_position = "bottom", base_size = 12) +
  labs(title="Low-Value Care\nMusculoskeletal Surgeries & Procedures Utilization Rate", subtitle="2009-2019") + 
  theme(plot.title = element_text(hjust = .5, size=16, face = "bold")) +
  theme(plot.subtitle = element_text(hjust = .65, size=14)) +
  scale_fill_continuous_sequential(name="Utilization Rate\n(Per 100,000)", limits=c(300,1500), breaks = c(350, 700 , 1050, 1400), palette = "Reds", labels=comma) + 
  guides(fill = guide_colourbar(barwidth = 38, barheight = .75, raster=TRUE))



##########23 Low-Value Care SERVICES AGGREGATED##########
ggplot(Agg, aes(state=STATE, fill=Util_COM_MCR)) +
  geom_statebins(lbl_size = 5.5, radius = grid::unit(9, "pt"), na.rm = TRUE) + 
  coord_equal(.95) + 
  theme_statebins(legend_position = "bottom", base_size = 12) +
  #labs(title="Low-Value Care Utilization Rate", subtitle="2009-2019") + 
  #theme(plot.title = element_text(hjust = .5, size=16, face = "bold")) +
  theme(plot.subtitle = element_text(hjust = .65, size=14)) +
  scale_fill_continuous_sequential(name="Utilization Rate\n(Per 100,000)", limits=c(1100,3100), breaks = c(1200, 1500, 1800, 2100, 2400, 2700, 3000), palette = "Blues", labels=comma) +
  guides(fill = guide_colourbar(barwidth = 38, barheight = .75, raster=TRUE))

