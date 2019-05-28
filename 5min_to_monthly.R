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
Qrate_5min$Yearmonth <- format(parse_date_time(paste(Qrate_5min$Year,Qrate_5min$Month, sep = "-"), "ym"), '%Y-%m')

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
Qvol_5min$Yearmonth <- format(parse_date_time(paste(Qvol_5min$Year,Qvol_5min$Month, sep = "-"), "ym"), '%Y-%m')

# Watershed 626-------------------------------
Qrate_flag <- "Discharge626_Q_flags"
Qrate <- "Discharge626"
Qrate_min <- "Discharge626_Min"
Qrate_max <- "Discharge626_Max"
Qvol <- "DischargeVolume626"
Qvol_min <- "DischargeVolume626_Min"
Qvol_max <- "DischargeVolume626_Max"

Qrate_monthly <- Qrate_5min %>% 
  select(Yearmonth,Qrate_flag,Qrate,Qrate_min,Qrate_max)
Qrate_monthly$Discharge626_Q_flags <- substr(Qrate_monthly$Discharge626_Q_flags,1,2)
Qmonthly_flags <- data.table(Qrate_monthly[,c(1,2)])
Qmonthly_flags <- data.frame(rbind(table(Qmonthly_flags)))
colnames(Qmonthly_flags) <- c("AV_626","EV_626","SVC_626")

Qrate_monthly_626 <- Qrate_monthly %>%
  group_by(Yearmonth) %>% 
  summarise_at(c(Qrate, Qrate_min, Qrate_max), mean, na.rm = TRUE)
Qrate_monthly_626 <- bind_cols(Qrate_monthly_626, Qmonthly_flags)

Qvol_monthly <- Qvol_5min %>% 
  select(Yearmonth,Qvol,Qvol_min,Qvol_max)
Qvol_monthly_626 <- Qvol_monthly %>%
  group_by(Yearmonth) %>% 
  summarise_at(c(Qvol, Qvol_min, Qvol_max), sum, na.rm = TRUE)

# Watershed 693-------------------------------
Qrate_flag <- "Discharge693_Q_flags"
Qrate <- "Discharge693"
Qrate_min <- "Discharge693_Min"
Qrate_max <- "Discharge693_Max"
Qvol_flag <- "DischargeVolume693_Q_flags"
Qvol <- "DischargeVolume693"
Qvol_min <- "DischargeVolume693_Min"
Qvol_max <- "DischargeVolume693_Max"

Qrate_monthly <- Qrate_5min %>% 
  select(Yearmonth,Qrate_flag,Qrate,Qrate_min,Qrate_max)
Qrate_monthly$Discharge693_Q_flags <- substr(Qrate_monthly$Discharge693_Q_flags,1,2)
Qmonthly_flags <- data.table(Qrate_monthly[,c(1,2)])
Qmonthly_flags <- data.frame(rbind(table(Qmonthly_flags)))
colnames(Qmonthly_flags) <- c("AV_693","EV_693","SVC_693")

Qrate_monthly_693 <- Qrate_monthly %>%
  group_by(Yearmonth) %>% 
  summarise_at(c(Qrate, Qrate_min, Qrate_max), mean, na.rm = TRUE)
Qrate_monthly_693 <- bind_cols(Qrate_monthly_693, Qmonthly_flags)


Qvol_monthly <- Qvol_5min %>% 
  select(Yearmonth,Qvol,Qvol_min,Qvol_max)
Qvol_monthly_693 <- Qvol_monthly %>%
  group_by(Yearmonth) %>% 
  summarise_at(c(Qvol, Qvol_min, Qvol_max), sum, na.rm = TRUE)

# Watershed 703-------------------------------
Qrate_flag <- "Discharge703_Q_flags"
Qrate <- "Discharge703"
Qrate_min <- "Discharge703_Min"
Qrate_max <- "Discharge703_Max"
Qvol_flag <- "DischargeVolume703_Q_flags"
Qvol <- "DischargeVolume703"
Qvol_min <- "DischargeVolume703_Min"
Qvol_max <- "DischargeVolume703_Max"

Qrate_monthly <- Qrate_5min %>% 
  select(Yearmonth,Qrate_flag,Qrate,Qrate_min,Qrate_max)
Qrate_monthly$Discharge703_Q_flags <- substr(Qrate_monthly$Discharge703_Q_flags,1,2)
Qmonthly_flags <- data.table(Qrate_monthly[,c(1,2)])
Qmonthly_flags <- data.frame(rbind(table(Qmonthly_flags)))
colnames(Qmonthly_flags) <- c("AV_703","EV_703","SVC_703")

Qrate_monthly_703 <- Qrate_monthly %>%
  group_by(Yearmonth) %>% 
  summarise_at(c(Qrate, Qrate_min, Qrate_max), mean, na.rm = TRUE)
Qrate_monthly_703 <- bind_cols(Qrate_monthly_703, Qmonthly_flags)

Qvol_monthly <- Qvol_5min %>% 
  select(Yearmonth,Qvol,Qvol_min,Qvol_max)
Qvol_monthly_703 <- Qvol_monthly %>%
  group_by(Yearmonth) %>% 
  summarise_at(c(Qvol, Qvol_min, Qvol_max), sum, na.rm = TRUE)

# Watershed 708-------------------------------
Qrate_flag <- "Discharge708_Q_flags"
Qrate <- "Discharge708"
Qrate_min <- "Discharge708_Min"
Qrate_max <- "Discharge708_Max"
Qvol_flag <- "DischargeVolume703_Q_flags"
Qvol <- "DischargeVolume708"
Qvol_min <- "DischargeVolume708_Min"
Qvol_max <- "DischargeVolume708_Max"

Qrate_monthly <- Qrate_5min %>% 
  select(Yearmonth,Qrate_flag,Qrate,Qrate_min,Qrate_max)
Qrate_monthly$Discharge708_Q_flags <- substr(Qrate_monthly$Discharge708_Q_flags,1,2)
Qmonthly_flags <- data.table(Qrate_monthly[,c(1,2)])
Qmonthly_flags <- data.frame(rbind(table(Qmonthly_flags)))
colnames(Qmonthly_flags) <- c("AV_708","EV_708","SVC_708")

Qrate_monthly_708 <- Qrate_monthly %>%
  group_by(Yearmonth) %>% 
  summarise_at(c(Qrate, Qrate_min, Qrate_max), mean, na.rm = TRUE)
Qrate_monthly_708 <- bind_cols(Qrate_monthly_708, Qmonthly_flags)

Qvol_monthly <- Qvol_5min %>% 
  select(Yearmonth,Qvol,Qvol_min,Qvol_max)
Qvol_monthly_708 <- Qvol_monthly %>%
  group_by(Yearmonth) %>% 
  summarise_at(c(Qvol, Qvol_min, Qvol_max), sum, na.rm = TRUE)

# Watershed 819-------------------------------
Qrate_flag <- "Discharge819_Q_flags"
Qrate <- "Discharge819"
Qrate_min <- "Discharge819_Min"
Qrate_max <- "Discharge819_Max"
Qvol_flag <- "DischargeVolume819_Q_flags"
Qvol <- "DischargeVolume819"
Qvol_min <- "DischargeVolume819_Min"
Qvol_max <- "DischargeVolume819_Max"

Qrate_monthly <- Qrate_5min %>% 
  select(Yearmonth,Qrate_flag,Qrate,Qrate_min,Qrate_max)
Qrate_monthly$Discharge819_Q_flags <- substr(Qrate_monthly$Discharge819_Q_flags,1,2)
Qmonthly_flags <- data.table(Qrate_monthly[,c(1,2)])
Qmonthly_flags <- data.frame(rbind(table(Qmonthly_flags)))
colnames(Qmonthly_flags) <- c("AV_819","EV_819","SVC_819")

Qrate_monthly_819 <- Qrate_monthly %>%
  group_by(Yearmonth) %>% 
  summarise_at(c(Qrate, Qrate_min, Qrate_max), mean, na.rm = TRUE)
Qrate_monthly_819 <- bind_cols(Qrate_monthly_819, Qmonthly_flags)

Qvol_monthly <- Qvol_5min %>% 
  select(Yearmonth,Qvol,Qvol_min,Qvol_max)
Qvol_monthly_819 <- Qvol_monthly %>%
  group_by(Yearmonth) %>% 
  summarise_at(c(Qvol, Qvol_min, Qvol_max), sum, na.rm = TRUE)

# Watershed 844-------------------------------
Qrate_flag <- "Discharge844_Q_flags"
Qrate <- "Discharge844"
Qrate_min <- "Discharge844_Min"
Qrate_max <- "Discharge844_Max"
Qvol_flag <- "DischargeVolume844_Q_flags"
Qvol <- "DischargeVolume844"
Qvol_min <- "DischargeVolume844_Min"
Qvol_max <- "DischargeVolume844_Max"

Qrate_monthly <- Qrate_5min %>% 
  select(Yearmonth,Qrate_flag,Qrate,Qrate_min,Qrate_max)
Qrate_monthly$Discharge844_Q_flags <- substr(Qrate_monthly$Discharge844_Q_flags,1,2)
Qmonthly_flags <- data.table(Qrate_monthly[,c(1,2)])
Qmonthly_flags <- data.frame(rbind(table(Qmonthly_flags)))
colnames(Qmonthly_flags) <- c("AV_844","EV_844","SVC_844")

Qrate_monthly_844 <- Qrate_monthly %>%
  group_by(Yearmonth) %>% 
  summarise_at(c(Qrate, Qrate_min, Qrate_max), mean, na.rm = TRUE)
Qrate_monthly_844 <- bind_cols(Qrate_monthly_844, Qmonthly_flags)

Qvol_monthly <- Qvol_5min %>% 
  select(Yearmonth,Qvol,Qvol_min,Qvol_max)
Qvol_monthly_844 <- Qvol_monthly %>%
  group_by(Yearmonth) %>% 
  summarise_at(c(Qvol, Qvol_min, Qvol_max), sum, na.rm = TRUE)

# Watershed 1015-------------------------------
Qrate_flag <- "Discharge1015_Q_flags"
Qrate <- "Discharge1015"
Qrate_min <- "Discharge1015_Min"
Qrate_max <- "Discharge1015_Max"
Qvol_flag <- "DischargeVolume1015_Q_flags"
Qvol <- "DischargeVolume1015"
Qvol_min <- "DischargeVolume1015_Min"
Qvol_max <- "DischargeVolume1015_Max"

Qrate_monthly <- Qrate_5min %>% 
  select(Yearmonth,Qrate_flag,Qrate,Qrate_min,Qrate_max)
Qrate_monthly$Discharge1015_Q_flags <- substr(Qrate_monthly$Discharge1015_Q_flags,1,2)
Qmonthly_flags <- data.table(Qrate_monthly[,c(1,2)])
Qmonthly_flags <- data.frame(rbind(table(Qmonthly_flags)))
colnames(Qmonthly_flags) <- c("AV_1015","EV_1015","SVC_1015")

Qrate_monthly_1015 <- Qrate_monthly %>%
  group_by(Yearmonth) %>% 
  summarise_at(c(Qrate, Qrate_min, Qrate_max), mean, na.rm = TRUE)
Qrate_monthly_1015 <- bind_cols(Qrate_monthly_1015, Qmonthly_flags)

Qvol_monthly <- Qvol_5min %>% 
  select(Yearmonth,Qvol,Qvol_min,Qvol_max)
Qvol_monthly_1015 <- Qvol_monthly %>%
  group_by(Yearmonth) %>% 
  summarise_at(c(Qvol, Qvol_min, Qvol_max), sum, na.rm = TRUE)

# Merge all monthly Q totals to one file
Q_monthly1 <- merge(Qrate_monthly_626, Qrate_monthly_693, by = "Yearmonth")
Q_monthly2 <- merge(Q_monthly1, Qrate_monthly_703, by = "Yearmonth")
Q_monthly3 <- merge(Q_monthly2, Qrate_monthly_708, by = "Yearmonth")
Q_monthly4 <- merge(Q_monthly3, Qrate_monthly_819, by = "Yearmonth")
Q_monthly5 <- merge(Q_monthly4, Qrate_monthly_844, by = "Yearmonth")
Qrate_monthly <- merge(Q_monthly5, Qrate_monthly_1015, by = "Yearmonth")
Q_monthly1 <- merge(Qvol_monthly_626, Qvol_monthly_693, by = "Yearmonth")
Q_monthly2 <- merge(Q_monthly1, Qvol_monthly_703, by = "Yearmonth")
Q_monthly3 <- merge(Q_monthly2, Qvol_monthly_708, by = "Yearmonth")
Q_monthly4 <- merge(Q_monthly3, Qvol_monthly_819, by = "Yearmonth")
Q_monthly5 <- merge(Q_monthly4, Qvol_monthly_844, by = "Yearmonth")
Qvol_monthly <- merge(Q_monthly5, Qvol_monthly_1015, by = "Yearmonth")

Q_monthly <- merge(Qrate_monthly, Qvol_monthly, by.x = "Yearmonth")

Qrate <- c("Discharge626", "Discharge693", "Discharge703", "Discharge708", "Discharge819", "Discharge844", "Discharge1015")
Qrate_min <- c("Discharge626_Min", "Discharge693_Min", "Discharge703_Min", "Discharge708_Min", "Discharge819_Min", "Discharge844_Min", "Discharge1015_Min")
Qrate_max <- c("Discharge626_Max", "Discharge693_Max", "Discharge703_Max", "Discharge708_Max", "Discharge819_Max", "Discharge844_Max", "Discharge1015_Max")
Qvol <- c("DischargeVolume626", "DischargeVolume693", "DischargeVolume703", "DischargeVolume708", "DischargeVolume819", "DischargeVolume844", "DischargeVolume1015")
Qvol_min <- c("DischargeVolume626_Min", "DischargeVolume693_Min", "DischargeVolume703_Min", "DischargeVolume708_Min", "DischargeVolume819_Min", "DischargeVolume844_Min", "DischargeVolume1015_Min")
Qvol_max <- c("DischargeVolume626_Max", "DischargeVolume693_Max", "DischargeVolume703_Max", "DischargeVolume708_Max", "DischargeVolume819_Max", "DischargeVolume844_Max", "DischargeVolume1015_Max")
AV <- c("AV_626", "AV_693", "AV_703", "AV_708", "AV_819", "AV_844", "AV_1015")
EV <- c("EV_626", "EV_693", "EV_703", "EV_708", "EV_819", "EV_844", "EV_1015")
SVC <- c("SVC_626", "SVC_693", "SVC_703", "SVC_708", "SVC_819", "SVC_844", "SVC_1015")

Qrate_monthly <- Q_monthly %>%
  gather(key = Watershed, value = Qrate, Qrate) %>% 
  select(Yearmonth, Watershed, Qrate) %>%
  mutate(Watershed = gsub("Discharge", "WTS", Watershed))
Qrate_min_monthly <- Q_monthly %>%
  gather(key = Watershed, value = Qrate_min, Qrate_min) %>% 
  select(Yearmonth, Qrate_min) 
Qrate_max_monthly <- Q_monthly %>%
  gather(key = Watershed, value = Qrate_max, Qrate_max) %>% 
  select(Yearmonth, Qrate_max)
Qvol_monthly <- Q_monthly %>%
  gather(key = Watershed, value = Qvol, Qvol) %>% 
  select(Yearmonth, Qvol)
Qvol_min_monthly <- Q_monthly %>%
  gather(key = Watershed, value = Qvol_min, Qvol_min) %>% 
  select(Yearmonth, Qvol_min)
Qvol_max_monthly <- Q_monthly %>%
  gather(key = Watershed, value = Qvol_max, Qvol_max) %>% 
  select(Yearmonth, Qvol_max)
AV_monthly <- Q_monthly %>%
  gather(key = Watershed, value = AV, AV) %>% 
  select(Yearmonth, AV)
EV_monthly <- Q_monthly %>%
  gather(key = Watershed, value = EV, EV) %>% 
  select(Yearmonth, EV)
SVC_monthly <- Q_monthly %>%
  gather(key = Watershed, value = SVC, SVC) %>% 
  select(Yearmonth, SVC)

Q_monthly1 <- cbind(Qrate_monthly, Qrate_min_monthly$Qrate_min)
colnames(Q_monthly1)[4] <- "Qrate_min"
Q_monthly2 <- cbind(Q_monthly1, Qrate_max_monthly$Qrate_max)
colnames(Q_monthly2)[5] <- "Qrate_max"
Q_monthly3 <- cbind(Q_monthly2, Qvol_monthly$Qvol)
colnames(Q_monthly3)[6] <- "Qvol"
Q_monthly4 <- cbind(Q_monthly3, Qvol_min_monthly$Qvol_min)
colnames(Q_monthly4)[7] <- "Qvol_min"
Q_monthly5 <- cbind(Q_monthly4, Qvol_max_monthly$Qvol_max)
colnames(Q_monthly5)[8] <- "Qvol_max"
Q_monthly6 <- cbind(Q_monthly5, AV_monthly$AV)
colnames(Q_monthly6)[9] <- "AV"
Q_monthly7 <- cbind(Q_monthly6, EV_monthly$EV)
colnames(Q_monthly7)[10] <- "EV"
Q_monthly <- cbind(Q_monthly7, SVC_monthly$SVC)
colnames(Q_monthly)[11] <- "SVC"

Q_monthly <- na.omit(Q_monthly)

Watershed <- c("WTS626", "WTS693", "WTS703", "WTS708", "WTS819", "WTS844", "WTS1015")
Area_km2 <- c(3.2,9.3,12.8,7.8,4.8,5.7,3.3)
WTS_area <- data.frame(Watershed,Area_km2)

Q_monthly <- merge(Q_monthly, WTS_area, by.x = 'Watershed')
Q_monthly <- Q_monthly %>%
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
Q_monthly <- Q_monthly[,c(1,2,9,10,11,3,4,5,6,7,8,12,13,14)]

write_csv(Q_monthly, "data_output/Q_monthly.csv")
