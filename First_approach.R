library(tidyverse)
library(zoo)
#library(pracma)
#library(forecast)
#library(ggrepel)
library(ggpubr)
library(gsignal)
library(RColorBrewer)

setwd("~/Dropbox/Estadistica/CircaLuc")
data.df <- read_csv("./data/data.csv") %>%
  pivot_longer(!ZTTime, 
               names_to = "well", 
               values_to = "lumin") %>%
           group_by(well) %>%
           summarise(ZTTime = ZTTime[1:length(na.trim(lumin))], 
                     lumin = na.trim(lumin)) %>% # Clean trailing NAs in Lumin
           mutate(ZTTime = na.approx(ZTTime), # Linear approximation of missing points the time vector
                  lumin = na.approx(lumin), # Linear approximation of missing luminosity
                  Z_lumin = (lumin-mean(lumin, na.rm=TRUE))/sd(lumin, na.rm=TRUE)) # Z-scored lumin

fig.mean.time <- data.df %>%
  dplyr::filter(well == "A01") %>%
  ggplot(aes(x = ZTTime, y = Z_lumin)) + 
                geom_line() +
  theme_pubclean() +
  labs(x = "Time", y = "Luminosity")
fig.mean.time

det <- 48 # Ancho de la ventana de detrending en horas
fus <- 30 # Sampling time en minutos
# NACHO - Parámetros que se van a usar más adelante
mu <- fus/60; # Sampling time en horas
su <- 60/fus; # Samples por hora
smo <- 12

ZTcorte <- 48 # Inicio de la señal a considerar
ZTLD <- 120 # Fin de la sección LD
ZTDD <- 168 # Fin de la sección DD

# NACHO - Loa ventana de detrending
win_det <- det * su # Ancho de la ventana de detrending en samples
convwin_det <- rep(1, win_det) # La ventana

# NACHO - Loa ventana de detrending
win_smo <- smo * su # Ancho de la ventana de smoothing en samples
convwin_smo <- rep(1, win_smo) # La ventana

# Aca iria la parte de interpolacion
data.df <- data.df %>%
  group_by(well) %>% 
  mutate(Ys = gsignal::conv(abs(lumin), convwin_det, shape = "same"),
         # Ys = c(rep(Ys[det*su/2+1], det*su/2), 
         #        Ys[(det*su/2+1):(length(Ys)-det*su/2)], 
         #        rep(Ys[length(Ys)-det*su/2], det*su/2)),
         lumin_weighted = lumin/Ys,
         section = ifelse(ZTTime>=ZTcorte & ZTTime<=ZTLD, "LD",
                          ifelse(ZTTime>ZTLD & ZTTime<=ZTDD, "DD", "NonU"))) %>%
  ungroup() %>%
  group_by(well, section) %>%
  mutate(lumin_detrended = detrend(lumin_weighted, "linear")[,1],
         lumin_smoothed = gsignal::conv(lumin_detrended, convwin_smo, shape = "same")) %>%
  ungroup() %>% 
  select(-c(Ys, lumin_weighted))

fig.mean.convolved <- data.df %>%
  dplyr::filter(section == "LD") %>%
  ggplot(aes(x = ZTTime, 
             y = lumin_smoothed,
             colour = well)) + 
  geom_line() + 
  scale_color_manual(values = rep(brewer.pal(5,"Accent"),times=10)) +
  theme_pubclean() +
  labs(x = "Time", y = "Luminosity") +
  theme(legend.position = "none")
fig.mean.convolved

source("period_estim_funs.R")

periods <- data.df %>%
  dplyr::filter(section %in% c("LD", "DD")) %>%
  group_by(well, section) %>%
  summarise(LS_period(signal = lumin_smoothed,
                      time = ZTTime,
                      from_freq = 0.035,
                      to_freq = .05,
                      oversampling_freq = 30 ))

periods %>% ggplot(aes(x = period,
           color = section,
           fill = section)) +
  geom_histogram() +
  theme_pubclean()

# Cosinor
