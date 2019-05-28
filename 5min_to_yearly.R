setwd("C:/Users/Watersheds/Documents/RProjects/Discharge-editing/2019-05-21")

library(tidyverse)
library(lubridate)
library(zoo)
library(dplyr)
library(plotly)
library(data.table)

# Read in Q volume and Q rate data (all watersheds), as downloaded from sensor network
# Prepare and edit datasets
Qrate_5min <- read_csv("data/Qrate_5min.csv",
                       col_types = cols(
                         Discharge626_Q_flags = col_character(),
                         Discharge626 = col_double(),
                         Discharge626_Min = col_double(),
                         Discharge626_Max = col_double(),
                         Discharge693_Q_flags = col_character(),
                         Discharge693 = col_double(),
                         Discharge693_Min = col_double(),
                         Discharge693_Max = col_double(),
                         Discharge703_Q_flags = col_character(),
                         Discharge703 = col_double(),
                         Discharge703_Min = col_double(),
                         Discharge703_Max = col_double(),
                         Discharge819_Q_flags = col_character(),
                         Discharge819 = col_double(),
                         Discharge819_Min = col_double(),
                         Discharge819_Max = col_double(),
                         Discharge844_Q_flags = col_character(),
                         Discharge844 = col_double(),
                         Discharge844_Min = col_double(),
                         Discharge844_Max = col_double(),
                         Discharge1015_Q_flags = col_character(),
                         Discharge1015 = col_double(),
                         Discharge1015_Min = col_double(),
                         Discharge1015_Max = col_double()))
Qrate_5min <- Qrate_5min[-c(1,2,3),]
Qrate_5min$yearmonth <- parse_date_time(paste(Qrate_5min$Year,Qrate_5min$Month, sep = "-"), "ym")

Qvol_5min <- read_csv("data/Qvol_5min.csv",
                      col_types = cols(
                        DischargeVolume626_Q_flags = col_character(),
                        DischargeVolume626 = col_double(),
                        DischargeVolume626_Min = col_double(),
                        DischargeVolume626_Max = col_double(),
                        DischargeVolume693_Q_flags = col_character(),
                        DischargeVolume693 = col_double(),
                        DischargeVolume693_Min = col_double(),
                        DischargeVolume693_Max = col_double(),
                        DischargeVolume703_Q_flags = col_character(),
                        DischargeVolume703 = col_double(),
                        DischargeVolume703_Min = col_double(),
                        DischargeVolume703_Max = col_double(),
                        DischargeVolume819_Q_flags = col_character(),
                        DischargeVolume819 = col_double(),
                        DischargeVolume819_Min = col_double(),
                        DischargeVolume819_Max = col_double(),
                        DischargeVolume844_Q_flags = col_character(),
                        DischargeVolume844 = col_double(),
                        DischargeVolume844_Min = col_double(),
                        DischargeVolume844_Max = col_double(),
                        DischargeVolume1015_Q_flags = col_character(),
                        DischargeVolume1015 = col_double(),
                        DischargeVolume1015_Min = col_double(),
                        DischargeVolume1015_Max = col_double()))      
Qvol_5min <- Qvol_5min[-c(1,2,3),]
Qvol_5min$yearmonth <- parse_date_time(paste(Qvol_5min$Year,Qvol_5min$Month, sep = "-"), "ym")

# Watershed 626-------------------------------
Qrate_flag <- "Discharge626_Q_flags"
Qrate <- "Discharge626"
Qrate_min <- "Discharge626_Min"
Qrate_max <- "Discharge626_Max"
Qvol <- "DischargeVolume626"
Qvol_min <- "DischargeVolume626_Min"
Qvol_max <- "DischargeVolume626_Max"

Qrate_yearly <- Qrate_5min %>% 
  select(Year, Qrate_flag, Qrate, Qrate_min, Qrate_max)
Qrate_yearly$Discharge626_Q_flags <- substr(Qrate_yearly$Discharge626_Q_flags,1,2)
Qyearly_flags <- data.table(Qrate_yearly[,c(1,2)])
Qyearly_flags <- data.frame(rbind(table(Qyearly_flags)))
colnames(Qyearly_flags) <- c("AV_626","EV_626","SVC_626")

Qrate_yearly_626 <- Qrate_yearly %>%
  group_by(Year) %>% 
  summarise_at(c(Qrate, Qrate_min, Qrate_max), mean, na.rm = TRUE)
Qrate_yearly_626 <- bind_cols(Qrate_yearly_626, Qyearly_flags)

Qvol_yearly <- Qvol_5min %>% 
  select(Year,Qvol,Qvol_min,Qvol_max)
Qvol_yearly_626 <- Qvol_yearly %>%
  group_by(Year) %>% 
  summarise_at(c(Qvol, Qvol_min, Qvol_max), sum, na.rm = TRUE)

# Watershed 693-------------------------------
Qrate_flag <- "Discharge693_Q_flags"
Qrate <- "Discharge693"
Qrate_min <- "Discharge693_Min"
Qrate_max <- "Discharge693_Max"
Qvol <- "DischargeVolume693"
Qvol_min <- "DischargeVolume693_Min"
Qvol_max <- "DischargeVolume693_Max"

Qrate_yearly <- Qrate_5min %>% 
  select(Year, Qrate_flag, Qrate, Qrate_min, Qrate_max)
Qrate_yearly$Discharge693_Q_flags <- substr(Qrate_yearly$Discharge693_Q_flags,1,2)
Qyearly_flags <- data.table(Qrate_yearly[,c(1,2)])
Qyearly_flags <- data.frame(rbind(table(Qyearly_flags)))
colnames(Qyearly_flags) <- c("AV_693","EV_693","SVC_693")

Qrate_yearly_693 <- Qrate_yearly %>%
  group_by(Year) %>% 
  summarise_at(c(Qrate, Qrate_min, Qrate_max), mean, na.rm = TRUE)
Qrate_yearly_693 <- bind_cols(Qrate_yearly_693, Qyearly_flags)

Qvol_yearly <- Qvol_5min %>% 
  select(Year,Qvol,Qvol_min,Qvol_max)
Qvol_yearly_693 <- Qvol_yearly %>%
  group_by(Year) %>% 
  summarise_at(c(Qvol, Qvol_min, Qvol_max), sum, na.rm = TRUE)

# Watershed 703-------------------------------
Qrate_flag <- "Discharge703_Q_flags"
Qrate <- "Discharge703"
Qrate_min <- "Discharge703_Min"
Qrate_max <- "Discharge703_Max"
Qvol <- "DischargeVolume703"
Qvol_min <- "DischargeVolume703_Min"
Qvol_max <- "DischargeVolume703_Max"

Qrate_yearly <- Qrate_5min %>% 
  select(Year, Qrate_flag, Qrate, Qrate_min, Qrate_max)
Qrate_yearly$Discharge703_Q_flags <- substr(Qrate_yearly$Discharge703_Q_flags,1,2)
Qyearly_flags <- data.table(Qrate_yearly[,c(1,2)])
Qyearly_flags <- data.frame(rbind(table(Qyearly_flags)))
colnames(Qyearly_flags) <- c("AV_703","EV_703","SVC_703")

Qrate_yearly_703 <- Qrate_yearly %>%
  group_by(Year) %>% 
  summarise_at(c(Qrate, Qrate_min, Qrate_max), mean, na.rm = TRUE)
Qrate_yearly_703 <- bind_cols(Qrate_yearly_703, Qyearly_flags)

Qvol_yearly <- Qvol_5min %>% 
  select(Year,Qvol,Qvol_min,Qvol_max)
Qvol_yearly_703 <- Qvol_yearly %>%
  group_by(Year) %>% 
  summarise_at(c(Qvol, Qvol_min, Qvol_max), sum, na.rm = TRUE)

# Watershed 708-------------------------------
Qrate_flag <- "Discharge708_Q_flags"
Qrate <- "Discharge708"
Qrate_min <- "Discharge708_Min"
Qrate_max <- "Discharge708_Max"
Qvol <- "DischargeVolume708"
Qvol_min <- "DischargeVolume708_Min"
Qvol_max <- "DischargeVolume708_Max"

Qrate_yearly <- Qrate_5min %>% 
  select(Year, Qrate_flag, Qrate, Qrate_min, Qrate_max)
Qrate_yearly$Discharge708_Q_flags <- substr(Qrate_yearly$Discharge708_Q_flags,1,2)
Qyearly_flags <- data.table(Qrate_yearly[,c(1,2)])
Qyearly_flags <- data.frame(rbind(table(Qyearly_flags)))
colnames(Qyearly_flags) <- c("AV_708","EV_708","SVC_708")

Qrate_yearly_708 <- Qrate_yearly %>%
  group_by(Year) %>% 
  summarise_at(c(Qrate, Qrate_min, Qrate_max), mean, na.rm = TRUE)
Qrate_yearly_708 <- bind_cols(Qrate_yearly_708, Qyearly_flags)

Qvol_yearly <- Qvol_5min %>% 
  select(Year,Qvol,Qvol_min,Qvol_max)
Qvol_yearly_708 <- Qvol_yearly %>%
  group_by(Year) %>% 
  summarise_at(c(Qvol, Qvol_min, Qvol_max), sum, na.rm = TRUE)

# Watershed 819-------------------------------
Qrate_flag <- "Discharge819_Q_flags"
Qrate <- "Discharge819"
Qrate_min <- "Discharge819_Min"
Qrate_max <- "Discharge819_Max"
Qvol <- "DischargeVolume819"
Qvol_min <- "DischargeVolume819_Min"
Qvol_max <- "DischargeVolume819_Max"

Qrate_yearly <- Qrate_5min %>% 
  select(Year, Qrate_flag, Qrate, Qrate_min, Qrate_max)
Qrate_yearly$Discharge819_Q_flags <- substr(Qrate_yearly$Discharge819_Q_flags,1,2)
Qyearly_flags <- data.table(Qrate_yearly[,c(1,2)])
Qyearly_flags <- data.frame(rbind(table(Qyearly_flags)))
colnames(Qyearly_flags) <- c("AV_819","EV_819","SVC_819")

Qrate_yearly_819 <- Qrate_yearly %>%
  group_by(Year) %>% 
  summarise_at(c(Qrate, Qrate_min, Qrate_max), mean, na.rm = TRUE)
Qrate_yearly_819 <- bind_cols(Qrate_yearly_819, Qyearly_flags)

Qvol_yearly <- Qvol_5min %>% 
  select(Year,Qvol,Qvol_min,Qvol_max)
Qvol_yearly_819 <- Qvol_yearly %>%
  group_by(Year) %>% 
  summarise_at(c(Qvol, Qvol_min, Qvol_max), sum, na.rm = TRUE)

# Watershed 844-------------------------------
Qrate_flag <- "Discharge844_Q_flags"
Qrate <- "Discharge844"
Qrate_min <- "Discharge844_Min"
Qrate_max <- "Discharge844_Max"
Qvol <- "DischargeVolume844"
Qvol_min <- "DischargeVolume844_Min"
Qvol_max <- "DischargeVolume844_Max"

Qrate_yearly <- Qrate_5min %>% 
  select(Year, Qrate_flag, Qrate, Qrate_min, Qrate_max)
Qrate_yearly$Discharge844_Q_flags <- substr(Qrate_yearly$Discharge844_Q_flags,1,2)
Qyearly_flags <- data.table(Qrate_yearly[,c(1,2)])
Qyearly_flags <- data.frame(rbind(table(Qyearly_flags)))
colnames(Qyearly_flags) <- c("AV_844","EV_844","SVC_844")

Qrate_yearly_844 <- Qrate_yearly %>%
  group_by(Year) %>% 
  summarise_at(c(Qrate, Qrate_min, Qrate_max), mean, na.rm = TRUE)
Qrate_yearly_844 <- bind_cols(Qrate_yearly_844, Qyearly_flags)

Qvol_yearly <- Qvol_5min %>% 
  select(Year,Qvol,Qvol_min,Qvol_max)
Qvol_yearly_844 <- Qvol_yearly %>%
  group_by(Year) %>% 
  summarise_at(c(Qvol, Qvol_min, Qvol_max), sum, na.rm = TRUE)

# Watershed 1015-------------------------------
Qrate_flag <- "Discharge1015_Q_flags"
Qrate <- "Discharge1015"
Qrate_min <- "Discharge1015_Min"
Qrate_max <- "Discharge1015_Max"
Qvol <- "DischargeVolume1015"
Qvol_min <- "DischargeVolume1015_Min"
Qvol_max <- "DischargeVolume1015_Max"

Qrate_yearly <- Qrate_5min %>% 
  select(Year, Qrate_flag, Qrate, Qrate_min, Qrate_max)
Qrate_yearly$Discharge1015_Q_flags <- substr(Qrate_yearly$Discharge1015_Q_flags,1,2)
Qyearly_flags <- data.table(Qrate_yearly[,c(1,2)])
Qyearly_flags <- data.frame(rbind(table(Qyearly_flags)))
colnames(Qyearly_flags) <- c("AV_1015","EV_1015","SVC_1015")

Qrate_yearly_1015 <- Qrate_yearly %>%
  group_by(Year) %>% 
  summarise_at(c(Qrate, Qrate_min, Qrate_max), mean, na.rm = TRUE)
Qrate_yearly_1015 <- bind_cols(Qrate_yearly_1015, Qyearly_flags)

Qvol_yearly <- Qvol_5min %>% 
  select(Year,Qvol,Qvol_min,Qvol_max)
Qvol_yearly_1015 <- Qvol_yearly %>%
  group_by(Year) %>% 
  summarise_at(c(Qvol, Qvol_min, Qvol_max), sum, na.rm = TRUE)

# Merge all wtyearly Q totals to one file
Q_yearly1 <- merge(Qrate_yearly_626, Qrate_yearly_693, by.x = "Year")
Q_yearly2 <- merge(Q_yearly1, Qrate_yearly_703, by.x = "Year")
Q_yearly3 <- merge(Q_yearly2, Qrate_yearly_708, by.x = "Year")
Q_yearly4 <- merge(Q_yearly3, Qrate_yearly_819, by.x = "Year")
Q_yearly5 <- merge(Q_yearly4, Qrate_yearly_844, by.x = "Year")
Qrate_yearly <- merge(Q_yearly5, Qrate_yearly_1015, by.x = "Year")
Q_yearly1 <- merge(Qvol_yearly_626, Qvol_yearly_693, by.x = "Year")
Q_yearly2 <- merge(Q_yearly1, Qvol_yearly_703, by.x = "Year")
Q_yearly3 <- merge(Q_yearly2, Qvol_yearly_708, by.x = "Year")
Q_yearly4 <- merge(Q_yearly3, Qvol_yearly_819, by.x = "Year")
Q_yearly5 <- merge(Q_yearly4, Qvol_yearly_844, by.x = "Year")
Qvol_yearly <- merge(Q_yearly5, Qvol_yearly_1015, by.x = "Year")

Q_yearly <- merge(Qrate_yearly, Qvol_yearly, by.x = "Year")

Qrate <- c("Discharge626", "Discharge693", "Discharge703", "Discharge708", "Discharge819", "Discharge844", "Discharge1015")
Qrate_min <- c("Discharge626_Min", "Discharge693_Min", "Discharge703_Min", "Discharge708_Min", "Discharge819_Min", "Discharge844_Min", "Discharge1015_Min")
Qrate_max <- c("Discharge626_Max", "Discharge693_Max", "Discharge703_Max", "Discharge708_Max", "Discharge819_Max", "Discharge844_Max", "Discharge1015_Max")
Qvol <- c("DischargeVolume626", "DischargeVolume693", "DischargeVolume703", "DischargeVolume708", "DischargeVolume819", "DischargeVolume844", "DischargeVolume1015")
Qvol_min <- c("DischargeVolume626_Min", "DischargeVolume693_Min", "DischargeVolume703_Min", "DischargeVolume708_Min", "DischargeVolume819_Min", "DischargeVolume844_Min", "DischargeVolume1015_Min")
Qvol_max <- c("DischargeVolume626_Max", "DischargeVolume693_Max", "DischargeVolume703_Max", "DischargeVolume708_Max", "DischargeVolume819_Max", "DischargeVolume844_Max", "DischargeVolume1015_Max")
AV <- c("AV_626", "AV_693", "AV_703", "AV_708", "AV_819", "AV_844", "AV_1015")
EV <- c("EV_626", "EV_693", "EV_703", "EV_708", "EV_819", "EV_844", "EV_1015")
SVC <- c("SVC_626", "SVC_693", "SVC_703", "SVC_708", "SVC_819", "SVC_844", "SVC_1015")

Qrate_yearly <- Q_yearly %>%
  gather(key = Watershed, value = Qrate, Qrate) %>% 
  select(Year, Watershed, Qrate) %>%
  mutate(Watershed = gsub("Discharge", "WTS", Watershed))
Qrate_min_yearly <- Q_yearly %>%
  gather(key = Watershed, value = Qrate_min, Qrate_min) %>% 
  select(Year, Qrate_min) 
Qrate_max_yearly <- Q_yearly %>%
  gather(key = Watershed, value = Qrate_max, Qrate_max) %>% 
  select(Year, Qrate_max)
Qvol_yearly <- Q_yearly %>%
  gather(key = Watershed, value = Qvol, Qvol) %>% 
  select(Year, Qvol)
Qvol_min_yearly <- Q_yearly %>%
  gather(key = Watershed, value = Qvol_min, Qvol_min) %>% 
  select(Year, Qvol_min)
Qvol_max_yearly <- Q_yearly %>%
  gather(key = Watershed, value = Qvol_max, Qvol_max) %>% 
  select(Year, Qvol_max)
AV_yearly <- Q_yearly %>%
  gather(key = Watershed, value = AV, AV) %>% 
  select(Year, AV)
EV_yearly <- Q_yearly %>%
  gather(key = Watershed, value = EV, EV) %>% 
  select(Year, EV)
SVC_yearly <- Q_yearly %>%
  gather(key = Watershed, value = SVC, SVC) %>% 
  select(Year, SVC)

Q_yearly1 <- cbind(Qrate_yearly, Qrate_min_yearly$Qrate_min)
colnames(Q_yearly1)[4] <- "Qrate_min"
Q_yearly2 <- cbind(Q_yearly1, Qrate_max_yearly$Qrate_max)
colnames(Q_yearly2)[5] <- "Qrate_max"
Q_yearly3 <- cbind(Q_yearly2, Qvol_yearly$Qvol)
colnames(Q_yearly3)[6] <- "Qvol"
Q_yearly4 <- cbind(Q_yearly3, Qvol_min_yearly$Qvol_min)
colnames(Q_yearly4)[7] <- "Qvol_min"
Q_yearly5 <- cbind(Q_yearly4, Qvol_max_yearly$Qvol_max)
colnames(Q_yearly5)[8] <- "Qvol_max"
Q_yearly6 <- cbind(Q_yearly5, AV_yearly$AV)
colnames(Q_yearly6)[9] <- "AV"
Q_yearly7 <- cbind(Q_yearly6, EV_yearly$EV)
colnames(Q_yearly7)[10] <- "EV"
Q_yearly <- cbind(Q_yearly7, SVC_yearly$SVC)
colnames(Q_yearly)[11] <- "SVC"

Q_yearly <- na.omit(Q_yearly)

Watershed <- c("WTS626", "WTS693", "WTS703", "WTS708", "WTS819", "WTS844", "WTS1015")
Area_km2 <- c(3.2,9.3,12.8,7.8,4.8,5.7,3.3)
WTS_area <- data.frame(Watershed,Area_km2)

Q_yearly <- merge(Q_yearly, WTS_area, by.x = 'Watershed')
Q_yearly <- Q_yearly %>%
  mutate(Qmm = Qvol/(Area_km2*1000)) %>%
  mutate(Qmm = round(Qmm, digits = 0)) %>% 
  mutate(Qmm_min = Qvol_min/(Area_km2*1000)) %>%
  mutate(Qmm_min = round(Qmm_min, digits = 0)) %>% 
  mutate(Qmm_max = Qvol_max/(Area_km2*1000)) %>% 
  mutate(Qmm_max = round(Qmm_max, digits = 0)) %>% 
  mutate(Qrate = round(Qrate, digits = 3)) %>%
  mutate(Qrate_min = round(Qrate_min, digits = 3)) %>% 
  mutate(Qrate_max = round(Qrate_max, digits = 3)) %>% 
  mutate(Qvol = round(Qvol, digits = 0)) %>% 
  mutate(Qvol_min = round(Qvol_min, digits = 0)) %>% 
  mutate(Qvol_max = round(Qvol_max, digits = 0)) %>% 
  select(-Area_km2)
Q_yearly <- Q_yearly[,c(1,2,9,10,11,3,4,5,6,7,8,12,13,14)]

write_csv(Q_yearly, "data_output/Q_yearly.csv")
