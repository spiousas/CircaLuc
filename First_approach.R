library(tidyverse)
library(zoo)
library(pracma)
library(forecast)

setwd("~/Dropbox/Estadistica/CircaLuc")
data.df <- read_csv("./data/data.csv") %>%
           gather(key = "Subject", 
                  value = "Lumin", 
                  -ZTTime , 
                  factor_key=TRUE) %>%
           group_by(Subject) %>%
           summarise(ZTTime = ZTTime[1:length(na.trim(Lumin))], 
                     Lumin=na.trim(Lumin)) %>% # Clean trailing NAs in Lumin
           mutate(ZTTime = na.approx(ZTTime), # Linear approximation of missing points the time vector
                  Lumin = na.approx(Lumin), # Linear approximation of missing luminosity
                  Z_Lumin = (Lumin-mean(Lumin, na.rm=TRUE))/sd(Lumin, na.rm=TRUE)) # Z-scored lumin

fig.mean.time <- data.df %>%
  filter(Subject == "A01") %>%
  ggplot(aes(x = ZTTime, y = Z_Lumin)) + 
                geom_line()
fig.mean.time

det <- 48
ZTcorte <- 48
fus <- 30 # Sampling time en minutos
# NACHO - Parámetros que se van a usar más adelante
mu <- fus/60; # Sampling time en horas
su <- 60/fus; # sampling time en segundos
smo <- 12

ZTLD <- 120
ZTDD <- 168

# NACHO - Longitud de la ventana de detrending?
ventana <- det * su
convi <- rep(1, ventana)

# Aca iria la parte de interpolacion
data.df <- data.df %>%
  group_by(Subject) %>% 
  mutate(Ys = convolve(abs(Lumin), convi, type = "open")[1:length(Lumin)],
         Ys = c(rep(Ys[ventana/2+1], ventana/2), 
                Ys[(ventana/2+1):(length(Ys)-ventana/2)], 
                rep(Ys[length(Ys)-ventana/2], ventana/2)),
         Lumin_weighted = Lumin/Ys,
         Section = ifelse(ZTTime>ZTcorte & ZTTime<=ZTLD, "LD",
                          ifelse(ZTTime>ZTLD & ZTTime<=ZTDD, "DD", NA))) %>%
  ungroup() %>%
  group_by(Subject, Section) %>%
  mutate(Lumin_detrended = detrend(Lumin_weighted, "linear"),
         Lumin_smoothed = as.numeric(ma(Lumin_detrended, smo)))

fig.mean.convolved <- data.df %>%
  filter((Section == "LD")) %>%
  ggplot(aes(x = ZTTime, y = Lumin_smoothed, colour = Subject)) + 
  geom_line()
fig.mean.convolved
