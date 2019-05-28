setwd("C:/Users/Watersheds/Documents/RProjects/Discharge-editing/2019-05-21")

library(tidyverse)
library(lubridate)
library(zoo)
library(dplyr)
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
Qrate_5min <- Qrate_5min[-c(2,3,4),]

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
Qvol_5min <- Qvol_5min[-c(2,3,4),]

# Watershed 626-------------------------------
Qflag <- "Discharge626_Q_flags"
Qrate <- "Discharge626"
Qrate_min <- "Discharge626_Min"
Qrate_max <- "Discharge626_Max"
Qvol <- "DischargeVolume626"
Qvol_min <- "DischargeVolume626_Min"
Qvol_max <- "DischargeVolume626_Max"

Qrate_5min_626 <- Qrate_5min %>% 
  select(Datetime,Qflag,Qrate,Qrate_min,Qrate_max)
colnames(Qrate_5min_626) <- c("Datetime", "Qflag", "Qrate", "Qrate_min", "Qrate_max")
Qvol_5min_626 <- Qvol_5min %>% 
  select(Qvol,Qvol_min,Qvol_max)
colnames(Qvol_5min_626) <- c("Qvol", "Qvol_min", "Qvol_max")

Q_5min_626 <- bind_cols(Qrate_5min_626, Qvol_5min_626)
Q_5min_626$Area <- 3.2
Q_5min_626 <- Q_5min_626 %>%
  mutate(Qrate = round(Qrate, digits = 3)) %>% 
  mutate(Qrate_min = round(Qrate_min, digits = 3)) %>% 
  mutate(Qrate_max = round(Qrate_max, digits = 3)) %>% 
  mutate(Qvol = round(Qvol, digits = 3)) %>% 
  mutate(Qvol_min = round(Qvol_min, digits = 3)) %>% 
  mutate(Qvol_max = round(Qvol_max, digits = 3)) %>% 
  mutate(Qmm = Qvol/(Area*1000)) %>% 
  mutate(Qmm = round(Qmm, digits = 3)) %>% 
  mutate(Qmm_min = Qvol_min/(Area*1000)) %>% 
  mutate(Qmm_min = round(Qmm_min, digits = 3)) %>% 
  mutate(Qmm_max = Qvol_max/(Area*1000)) %>% 
  mutate(Qmm_max = round(Qmm_max, digits = 3)) %>% 
  select(-Area)
Q_5min_626$Watershed <- "WTS626"

Q_5min_626 <- na.omit(Q_5min_626)
Q_5min_626 <- Q_5min_626[,c(12,1,2,3,4,5,6,7,8,9,10,11)]

write_csv(Q_5min_626, "data_output/Q_5min_626.csv")


# Watershed 693-------------------------------
Qflag <- "Discharge693_Q_flags"
Qrate <- "Discharge693"
Qrate_min <- "Discharge693_Min"
Qrate_max <- "Discharge693_Max"
Qvol <- "DischargeVolume693"
Qvol_min <- "DischargeVolume693_Min"
Qvol_max <- "DischargeVolume693_Max"

Qrate_5min_693 <- Qrate_5min %>% 
  select(Datetime,Qflag,Qrate,Qrate_min,Qrate_max)
colnames(Qrate_5min_693) <- c("Datetime", "Qflag", "Qrate", "Qrate_min", "Qrate_max")
Qvol_5min_693 <- Qvol_5min %>% 
  select(Qvol,Qvol_min,Qvol_max)
colnames(Qvol_5min_693) <- c("Qvol", "Qvol_min", "Qvol_max")

Q_5min_693 <- bind_cols(Qrate_5min_693, Qvol_5min_693)
Q_5min_693$Area <- 3.2
Q_5min_693 <- Q_5min_693 %>%
  mutate(Qrate = round(Qrate, digits = 3)) %>% 
  mutate(Qrate_min = round(Qrate_min, digits = 3)) %>% 
  mutate(Qrate_max = round(Qrate_max, digits = 3)) %>% 
  mutate(Qvol = round(Qvol, digits = 3)) %>% 
  mutate(Qvol_min = round(Qvol_min, digits = 3)) %>% 
  mutate(Qvol_max = round(Qvol_max, digits = 3)) %>% 
  mutate(Qmm = Qvol/(Area*1000)) %>% 
  mutate(Qmm = round(Qmm, digits = 3)) %>% 
  mutate(Qmm_min = Qvol_min/(Area*1000)) %>% 
  mutate(Qmm_min = round(Qmm_min, digits = 3)) %>% 
  mutate(Qmm_max = Qvol_max/(Area*1000)) %>% 
  mutate(Qmm_max = round(Qmm_max, digits = 3)) %>% 
  select(-Area)
Q_5min_693$Watershed <- "WTS693"

Q_5min_693 <- na.omit(Q_5min_693)
Q_5min_693 <- Q_5min_693[,c(12,1,2,3,4,5,6,7,8,9,10,11)]

write_csv(Q_5min_693, "data_output/Q_5min_693.csv")

# Watershed 703-------------------------------
Qflag <- "Discharge703_Q_flags"
Qrate <- "Discharge703"
Qrate_min <- "Discharge703_Min"
Qrate_max <- "Discharge703_Max"
Qvol <- "DischargeVolume703"
Qvol_min <- "DischargeVolume703_Min"
Qvol_max <- "DischargeVolume703_Max"

Qrate_5min_703 <- Qrate_5min %>% 
  select(Datetime,Qflag,Qrate,Qrate_min,Qrate_max)
colnames(Qrate_5min_703) <- c("Datetime", "Qflag", "Qrate", "Qrate_min", "Qrate_max")
Qvol_5min_703 <- Qvol_5min %>% 
  select(Qvol,Qvol_min,Qvol_max)
colnames(Qvol_5min_703) <- c("Qvol", "Qvol_min", "Qvol_max")

Q_5min_703 <- bind_cols(Qrate_5min_703, Qvol_5min_703)
Q_5min_703$Area <- 3.2
Q_5min_703 <- Q_5min_703 %>%
  mutate(Qrate = round(Qrate, digits = 3)) %>% 
  mutate(Qrate_min = round(Qrate_min, digits = 3)) %>% 
  mutate(Qrate_max = round(Qrate_max, digits = 3)) %>% 
  mutate(Qvol = round(Qvol, digits = 3)) %>% 
  mutate(Qvol_min = round(Qvol_min, digits = 3)) %>% 
  mutate(Qvol_max = round(Qvol_max, digits = 3)) %>% 
  mutate(Qmm = Qvol/(Area*1000)) %>% 
  mutate(Qmm = round(Qmm, digits = 3)) %>% 
  mutate(Qmm_min = Qvol_min/(Area*1000)) %>% 
  mutate(Qmm_min = round(Qmm_min, digits = 3)) %>% 
  mutate(Qmm_max = Qvol_max/(Area*1000)) %>% 
  mutate(Qmm_max = round(Qmm_max, digits = 3)) %>% 
  select(-Area)
Q_5min_703$Watershed <- "WTS703"

Q_5min_703 <- na.omit(Q_5min_703)
Q_5min_703 <- Q_5min_703[,c(12,1,2,3,4,5,6,7,8,9,10,11)]

write_csv(Q_5min_703, "data_output/Q_5min_703.csv")

# Watershed 708-------------------------------
Qflag <- "Discharge708_Q_flags"
Qrate <- "Discharge708"
Qrate_min <- "Discharge708_Min"
Qrate_max <- "Discharge708_Max"
Qvol <- "DischargeVolume708"
Qvol_min <- "DischargeVolume708_Min"
Qvol_max <- "DischargeVolume708_Max"

Qrate_5min_708 <- Qrate_5min %>% 
  select(Datetime,Qflag,Qrate,Qrate_min,Qrate_max)
colnames(Qrate_5min_708) <- c("Datetime", "Qflag", "Qrate", "Qrate_min", "Qrate_max")
Qvol_5min_708 <- Qvol_5min %>% 
  select(Qvol,Qvol_min,Qvol_max)
colnames(Qvol_5min_708) <- c("Qvol", "Qvol_min", "Qvol_max")

Q_5min_708 <- bind_cols(Qrate_5min_708, Qvol_5min_708)
Q_5min_708$Area <- 3.2
Q_5min_708 <- Q_5min_708 %>%
  mutate(Qrate = round(Qrate, digits = 3)) %>% 
  mutate(Qrate_min = round(Qrate_min, digits = 3)) %>% 
  mutate(Qrate_max = round(Qrate_max, digits = 3)) %>% 
  mutate(Qvol = round(Qvol, digits = 3)) %>% 
  mutate(Qvol_min = round(Qvol_min, digits = 3)) %>% 
  mutate(Qvol_max = round(Qvol_max, digits = 3)) %>% 
  mutate(Qmm = Qvol/(Area*1000)) %>% 
  mutate(Qmm = round(Qmm, digits = 3)) %>% 
  mutate(Qmm_min = Qvol_min/(Area*1000)) %>% 
  mutate(Qmm_min = round(Qmm_min, digits = 3)) %>% 
  mutate(Qmm_max = Qvol_max/(Area*1000)) %>% 
  mutate(Qmm_max = round(Qmm_max, digits = 3)) %>% 
  select(-Area)
Q_5min_708$Watershed <- "WTS708"

Q_5min_708 <- na.omit(Q_5min_708)
Q_5min_708 <- Q_5min_708[,c(12,1,2,3,4,5,6,7,8,9,10,11)]

write_csv(Q_5min_708, "data_output/Q_5min_708.csv")

# Watershed 819-------------------------------
Qflag <- "Discharge819_Q_flags"
Qrate <- "Discharge819"
Qrate_min <- "Discharge819_Min"
Qrate_max <- "Discharge819_Max"
Qvol <- "DischargeVolume819"
Qvol_min <- "DischargeVolume819_Min"
Qvol_max <- "DischargeVolume819_Max"

Qrate_5min_819 <- Qrate_5min %>% 
  select(Datetime,Qflag,Qrate,Qrate_min,Qrate_max)
colnames(Qrate_5min_819) <- c("Datetime", "Qflag", "Qrate", "Qrate_min", "Qrate_max")
Qvol_5min_819 <- Qvol_5min %>% 
  select(Qvol,Qvol_min,Qvol_max)
colnames(Qvol_5min_819) <- c("Qvol", "Qvol_min", "Qvol_max")

Q_5min_819 <- bind_cols(Qrate_5min_819, Qvol_5min_819)
Q_5min_819$Area <- 3.2
Q_5min_819 <- Q_5min_819 %>%
  mutate(Qrate = round(Qrate, digits = 3)) %>% 
  mutate(Qrate_min = round(Qrate_min, digits = 3)) %>% 
  mutate(Qrate_max = round(Qrate_max, digits = 3)) %>% 
  mutate(Qvol = round(Qvol, digits = 3)) %>% 
  mutate(Qvol_min = round(Qvol_min, digits = 3)) %>% 
  mutate(Qvol_max = round(Qvol_max, digits = 3)) %>% 
  mutate(Qmm = Qvol/(Area*1000)) %>% 
  mutate(Qmm = round(Qmm, digits = 3)) %>% 
  mutate(Qmm_min = Qvol_min/(Area*1000)) %>% 
  mutate(Qmm_min = round(Qmm_min, digits = 3)) %>% 
  mutate(Qmm_max = Qvol_max/(Area*1000)) %>% 
  mutate(Qmm_max = round(Qmm_max, digits = 3)) %>% 
  select(-Area)
Q_5min_819$Watershed <- "WTS819"

Q_5min_819 <- na.omit(Q_5min_819)
Q_5min_819 <- Q_5min_819[,c(12,1,2,3,4,5,6,7,8,9,10,11)]

write_csv(Q_5min_819, "data_output/Q_5min_819.csv")

# Watershed 844-------------------------------
Qflag <- "Discharge844_Q_flags"
Qrate <- "Discharge844"
Qrate_min <- "Discharge844_Min"
Qrate_max <- "Discharge844_Max"
Qvol <- "DischargeVolume844"
Qvol_min <- "DischargeVolume844_Min"
Qvol_max <- "DischargeVolume844_Max"

Qrate_5min_844 <- Qrate_5min %>% 
  select(Datetime,Qflag,Qrate,Qrate_min,Qrate_max)
colnames(Qrate_5min_844) <- c("Datetime", "Qflag", "Qrate", "Qrate_min", "Qrate_max")
Qvol_5min_844 <- Qvol_5min %>% 
  select(Qvol,Qvol_min,Qvol_max)
colnames(Qvol_5min_844) <- c("Qvol", "Qvol_min", "Qvol_max")

Q_5min_844 <- bind_cols(Qrate_5min_844, Qvol_5min_844)
Q_5min_844$Area <- 3.2
Q_5min_844 <- Q_5min_844 %>%
  mutate(Qrate = round(Qrate, digits = 3)) %>% 
  mutate(Qrate_min = round(Qrate_min, digits = 3)) %>% 
  mutate(Qrate_max = round(Qrate_max, digits = 3)) %>% 
  mutate(Qvol = round(Qvol, digits = 3)) %>% 
  mutate(Qvol_min = round(Qvol_min, digits = 3)) %>% 
  mutate(Qvol_max = round(Qvol_max, digits = 3)) %>% 
  mutate(Qmm = Qvol/(Area*1000)) %>% 
  mutate(Qmm = round(Qmm, digits = 3)) %>% 
  mutate(Qmm_min = Qvol_min/(Area*1000)) %>% 
  mutate(Qmm_min = round(Qmm_min, digits = 3)) %>% 
  mutate(Qmm_max = Qvol_max/(Area*1000)) %>% 
  mutate(Qmm_max = round(Qmm_max, digits = 3)) %>% 
  select(-Area)
Q_5min_844$Watershed <- "WTS844"

Q_5min_844 <- na.omit(Q_5min_844)
Q_5min_844 <- Q_5min_844[,c(12,1,2,3,4,5,6,7,8,9,10,11)]

write_csv(Q_5min_844, "data_output/Q_5min_844.csv")

# Watershed 1015-------------------------------
Qflag <- "Discharge1015_Q_flags"
Qrate <- "Discharge1015"
Qrate_min <- "Discharge1015_Min"
Qrate_max <- "Discharge1015_Max"
Qvol <- "DischargeVolume1015"
Qvol_min <- "DischargeVolume1015_Min"
Qvol_max <- "DischargeVolume1015_Max"

Qrate_5min_1015 <- Qrate_5min %>% 
  select(Datetime,Qflag,Qrate,Qrate_min,Qrate_max)
colnames(Qrate_5min_1015) <- c("Datetime", "Qflag", "Qrate", "Qrate_min", "Qrate_max")
Qvol_5min_1015 <- Qvol_5min %>% 
  select(Qvol,Qvol_min,Qvol_max)
colnames(Qvol_5min_1015) <- c("Qvol", "Qvol_min", "Qvol_max")

Q_5min_1015 <- bind_cols(Qrate_5min_1015, Qvol_5min_1015)
Q_5min_1015$Area <- 3.2
Q_5min_1015 <- Q_5min_1015 %>%
  mutate(Qrate = round(Qrate, digits = 3)) %>% 
  mutate(Qrate_min = round(Qrate_min, digits = 3)) %>% 
  mutate(Qrate_max = round(Qrate_max, digits = 3)) %>% 
  mutate(Qvol = round(Qvol, digits = 3)) %>% 
  mutate(Qvol_min = round(Qvol_min, digits = 3)) %>% 
  mutate(Qvol_max = round(Qvol_max, digits = 3)) %>% 
  mutate(Qmm = Qvol/(Area*1000)) %>% 
  mutate(Qmm = round(Qmm, digits = 3)) %>% 
  mutate(Qmm_min = Qvol_min/(Area*1000)) %>% 
  mutate(Qmm_min = round(Qmm_min, digits = 3)) %>% 
  mutate(Qmm_max = Qvol_max/(Area*1000)) %>% 
  mutate(Qmm_max = round(Qmm_max, digits = 3)) %>% 
  select(-Area)
Q_5min_1015$Watershed <- "WTS1015"

Q_5min_1015 <- na.omit(Q_5min_1015)
Q_5min_1015 <- Q_5min_1015[,c(12,1,2,3,4,5,6,7,8,9,10,11)]

write_csv(Q_5min_1015, "data_output/Q_5min_1015.csv")
