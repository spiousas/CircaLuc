library(shiny)
library(viridis)
library(tidyverse)
library(zoo)
library(shinyjs)
library(gghalves)
library(systemfonts)
library(ggpubr)
library(scales)
library(gsignal)
library(here)
library(RColorBrewer)

setwd("~/Dropbox/Estadistica/CircaLuc")
data.df <- read_csv("./data/data.csv") %>%
  pivot_longer(!ZTTime,
               names_to = "well",
               values_to = "lumin") %>%
  group_by(well) %>%
  summarise(ZTTime = ZTTime[1:length(na.trim(lumin))],
            lumin = na.trim(lumin)) %>% # Clean trailing NAs in Lumin
  mutate(
    ZTTime = na.approx(ZTTime),
    # Linear approximation of missing points the time vector
    lumin = na.approx(lumin),
    # Linear approximation of missing luminosity
    Z_lumin = (lumin - mean(lumin, na.rm = TRUE)) / sd(lumin, na.rm =
                                                         TRUE) # Z-scored lumin
  )

fig.mean.time <- data.df %>%
  dplyr::filter(well == "A01") %>%
  ggplot(aes(x = ZTTime, y = Z_lumin)) + 
                geom_line() +
  theme_pubclean() +
  labs(x = "Time", y = "Luminosity")
fig.mean.time

det <- 48 # Ancho de la ventana de detrending en horas
sp <- 30 # Sampling time en minutos
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

smoothed.df <- data.df %>%
    group_by(well) %>%
    mutate(
      Ys = gsignal::conv(abs(lumin),
                         rep(1, 60 * det / sp),
                         shape = "same"),
      lumin_weighted = lumin / Ys,
      section = ifelse(
        ZTTime >= ZTcorte & ZTTime <= ZTLD,
        "LD",
        ifelse(ZTTime > ZTLD &
                 ZTTime <= ZTDD, "DD", "NonU")
      )
    ) %>%
    ungroup() %>%
    group_by(well, section) %>%
    mutate(
      lumin_detrended = pracma::detrend(lumin_weighted, "linear")[, 1],
      lumin_smoothed = gsignal::conv(lumin_detrended,
                                     rep(1, 60 * smo / sp),
                                     shape = "same")
    ) %>%
    select(-c(Ys, lumin_weighted)) %>%
    ungroup()

fig.mean.convolved <- smoothed.df %>%
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

periods <- smoothed.df %>%
  dplyr::filter(section %in% c("LD", "DD")) %>%
  group_by(well, section) %>%
  summarise(LS_period(signal = lumin_smoothed,
                      time = ZTTime,
                      from_freq = 0.02,
                      to_freq = .05,
                      oversampling_freq = 30 )) %>%
  ungroup()

periods %>% ggplot(aes(x = period,
           color = section,
           fill = section)) +
  geom_histogram() +
  theme_pubclean()

# Cosinor
library(cosinor)
library(broom)
library(parameters)
library(modelr)
test <- smoothed.df %>% dplyr::filter(well == "A01" & section == "LD") %>%
  mutate(ZTTime = ZTTime - 48)
per_i <- periods %>% dplyr::filter(well == "A01" & section == "LD") %>% select(period)
m1 <- cosinor.lm(lumin_smoothed ~ time(ZTTime), period = per_i[[1]], data = test)

model_parameters(m1$fit)
summary(m1)
summary(m1$fit)
r2(m1$fit)

m2 <- nls(lumin_smoothed ~ alpha + amp * cos(2*pi*ZTTime/per_i[[1]] - acro),
          data = test,
          start = list(alpha = 0, amp = 1, acro = 1))
summary(m2)
a <- summary(m2)
rsquare(m2, test)

test <- test %>% 
  mutate(predicted_lum = a$parameters[1,1] + a$parameters[2,1] * cos(2*pi*ZTTime/per_i[[1]] - a$parameters[3,1]))

test %>% ggplot(aes(x = ZTTime)) + 
  geom_point(aes(y = lumin_smoothed)) + 
  geom_point(aes(y = predicted_lum), color = "red") +
  theme_minimal()

smoothed.df_periods <- smoothed.df %>% 
  dplyr::filter(section %in% c("LD", "DD")) %>% 
  group_by(well, section) %>% 
  nest() %>% 
  left_join(periods, by = c("well", "section")) %>%
  mutate(
    fit = map(data, ~nls(lumin_smoothed ~ alpha + amp * cos(2*pi*ZTTime/period - acro),
                         data = .x,
                         start = list(alpha = 0, amp = 1, acro = 1))),
    tidied = map(fit, broom::tidy),
    R = map2(fit, data, modelr::rsquare)
  ) %>% 
  unnest(tidied)  %>% 
  unnest(R) %>%
  select(all_of(c("well", "section", "period", "term", "estimate", "R"))) %>%
  pivot_wider(names_from = "term",
              values_from = "estimate")
  

smoothed.df_periods
