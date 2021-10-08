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
    group_by(well) %>%
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
  mutate(method = if_else(section == "LD", "twentyfour", "ls")) %>%
  group_by(well, section, method) %>%
  nest() %>%
  mutate(period = map(data, ~period_estimation(signal = .x$lumin_smoothed,
                                               time = .x$ZTTime,
                                               from_freq = 0.02,
                                               to_freq = .05,
                                               oversampling_freq = 30,
                                               method = method))) %>%
  select(-data) %>%
  unnest(c(period)) %>%
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
library(performance)

test <- smoothed.df %>% dplyr::filter(well == "D04" & section == "LD") %>%
  mutate(ZTTime = ZTTime - 48)
per_i <- periods %>% dplyr::filter(well == "D04" & section == "LD") %>% select(period)

m1 <- cosinor.lm(lumin_smoothed ~ time(ZTTime), period = per_i[[1]], data = test)

model_parameters(m1$fit)
summary(m1)
summary(m1$fit)
r2(m1$fit)[[1]]

m2 <- nls(lumin_smoothed ~ alpha + amp * cos(2*pi*ZTTime/per_i[[1]] - acro),
          data = test,
          start = list(alpha = 0, amp = 1, acro = 1))

summary(m2)
a <- summary(m2)
rsquare(m2, test)

test <- test %>% 
  mutate(predicted_lum = a$parameters[1,1] + a$parameters[2,1] * cos(2*pi*ZTTime/per_i[[1]] - a$parameters[3,1]),
         predicted_cos = m1$fit$coefficients[1] + sqrt(m1$fit$coefficients[[2]]^2+m1$fit$coefficients[[3]]^2) * cos(2*pi*ZTTime/per_i[[1]] - atan(m1$fit$coefficients[[3]]/m1$fit$coefficients[[2]])))

test %>% ggplot(aes(x = ZTTime)) + 
  geom_line(aes(y = lumin_smoothed), color = "black") + 
  geom_line(aes(y = predicted_lum), color = "red", linetype = "dashed") +
  geom_line(aes(y = predicted_cos), color = "blue", linetype = "dashed") +
  theme_minimal()

# With nls
smoothed.df_periods <- smoothed.df %>% 
  mutate(ZTTime = ZTTime - 48) %>%
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
              values_from = "estimate") %>%
  ungroup() %>%
  group_by(well) %>%
  mutate(
    acro = if_else(amp<0, pi+acro, acro),
    acro = if_else(acro<0, 2*pi-acro, acro),
    acro = if_else(abs(acro)>2*pi, acro%%(2*pi), acro),
    amp = if_else(amp<0, -amp, amp),
    acro_24 = acro*12/pi,
    Rpass = if_else(R[section=="DD"]>.5, "yes!", "no")
  )
  
smoothed.df_periods


# With cosinor
smoothed.df_periods_cos <- smoothed.df %>% 
  dplyr::filter(section %in% c("LD", "DD")) %>% 
  group_by(well, section) %>% 
  mutate(ZTTime = if_else(section == "LD", 
                          ZTTime - ZTcorte,
                          ZTTime - ZTLD)) %>%
  nest() %>% 
  left_join(periods, by = c("well", "section")) %>%
  mutate(
    cosinor = map(data, ~cosinor.lm(lumin_smoothed ~ time(ZTTime), period = period, data = .x)$fit),
    tidied = map(cosinor, broom::tidy),
    summ = map(cosinor, summary),
    R = map_dbl(summ, "r.squared") 
    ) %>% 
  unnest(tidied)  %>% 
  unnest(R) %>%
  select(all_of(c("well", "section", "period", "term", "estimate", "R"))) %>%
  pivot_wider(names_from = "term",
              values_from = "estimate") %>%
  mutate(alpha = `(Intercept)`,
         amp = sqrt(rrr^2 + sss^2),
         acro = atan(sss/rrr),
         acro_24 = acro*12/pi) %>%
  select(-c(`(Intercept)`, rrr, sss))


smoothed.df.predicted <- smoothed.df %>%
    left_join(smoothed.df_periods_cos %>% select(-c(R, acro_24)),
              by = c("well", "section")) %>%
    mutate(lumin_predicted = alpha + amp * cos(2*pi*ZTTime/period - acro)) %>%
    select(-c(period, alpha, amp, acro))
  
smoothed.df.predicted %>%
  dplyr::filter(well %in% "A01") %>%
  drop_na() %>%
  ggplot(aes(x = ZTTime-ZTcorte, color = section)) +
  annotate("rect",
           ymin = min(smoothed.df.predicted$lumin_smoothed[smoothed.df.predicted$well=="A01" & smoothed.df.predicted$section %in% c("LD", "DD")]),
           ymax = max(smoothed.df.predicted$lumin_smoothed[smoothed.df.predicted$well=="A01"& smoothed.df.predicted$section %in% c("LD", "DD")]),
           xmin = seq(12,ZTLD-ZTcorte-1,24),
           xmax = seq(24,ZTLD-ZTcorte,24),
           fill = "yellow", color = "yellow",
           alpha = .2) +
  annotate("rect",
           ymin = min(smoothed.df.predicted$lumin_smoothed[smoothed.df.predicted$well=="A01" & smoothed.df.predicted$section %in% c("LD", "DD")]),
           ymax = max(smoothed.df.predicted$lumin_smoothed[smoothed.df.predicted$well=="A01"& smoothed.df.predicted$section %in% c("LD", "DD")]),
           xmin = ZTLD-ZTcorte, 
           xmax = ZTDD-ZTcorte, 
           fill = "yellow", color = "yellow",
           alpha = .2) +
  geom_line(aes(y = lumin_predicted), size = 0.8, color = "grey50") +
  geom_line(aes(y = lumin_smoothed), size = 1) +
  geom_vline(xintercept = ZTLD-ZTcorte,
             linetype = "dashed",
             size = .5) +
  labs(x = "Time [hs]",
       y = "Detrended luminescence",
       title = "Detrended luminescence") +
  scale_colour_manual(values = c("#7373FF", "#FF7272")) +
  scale_x_continuous(breaks = seq(0,ZTDD-ZTcorte,12),
                     limits = c(0, ZTDD-ZTcorte)) +
  theme(legend.position = "none")
  

smoothed.df_periods%>% ggplot(aes(x = section,
                                 y = acro_24,
                                 color = section,
                                 fill = section)) +
  geom_jitter(width = .2,
              alpha = .2) + 
  stat_summary(geom = "bar", 
               alpha = .2) +
  stat_summary(geom = "errorbar", 
               alpha = 1,
               width = .2,
               position=position_nudge(-.2)) +
  geom_point(data = circ_means, 
             aes(y = mean_acro_24), 
             color = "black",
             position=position_nudge(.2)) +
  geom_errorbar(data = circ_means, 
                aes(y = mean_acro_24, ymin = mean_acro_24-sd_acro_24, ymax = mean_acro_24+sd_acro_24), 
                width = .2,
                color = "black",
                position=position_nudge(.2)) +
  geom_hline(yintercept = 24, color = "black", linetype = "dashed") +
  labs(x = NULL,
       y = "Fitted acrophase (24 hs)",
       caption = paste0("n=", nrow(smoothed.df_periods)/2)) +
  scale_x_discrete(limits = c("LD", "DD")) +
  scale_y_continuous(limits = c(0, 24), breaks = seq(0, 24, 6)) +
  scale_colour_manual(values = c("#7373FF", "#FF7272")) +
  scale_fill_manual(values = c("#7373FF", "#FF7272")) +
  theme(legend.position = "none")

## Circular
circ_means <- smoothed.df_periods %>% 
  group_by(section) %>% 
  nest() %>%
  mutate(circ_acro = map(data, ~circular(.x$acro_24, 
                                         units = "hours", 
                                         template = "clock24", 
                                         modulo = "2pi")),
         mean_acro_24 = map(circ_acro, ~mean(.x)),
         sd_acro_24 = map(circ_acro, ~sd(.x)),
         rayleigh = map(circ_acro, ~rayleigh.test(.x)),
         p.value = rayleigh[[1]]$p.value) %>%
  select(-c(data, circ_acro, rayleigh)) %>%
  unnest(mean_acro_24) %>%
  unnest(sd_acro_24)

smoothed.df_periods %>% ggplot(aes(x = acro_24,
                                   y = 1,
                                   color = section)) +
  geom_hline(yintercept = 1, color = "grey90") +
  geom_point(size = 3, alpha = .5) +
  geom_segment(
    data = circ_means,
    aes(x = mean_acro_24, xend = mean_acro_24, yend = exp(-sd_acro_24^2/2)),
    y = 0,
    size = 1,
    arrow = arrow(length = unit(0.05, "npc"))
  ) +
  labs(x = NULL,
       y = NULL,
       color = "Section") +
  scale_x_continuous(breaks = c(0,6,12,18,24),
                     limits = c(0, 24)) +
  scale_y_continuous(breaks = 1,
                     limits = c(0, 1)) +
  theme_void() +
  coord_polar() +
  scale_colour_manual(values = c("#7373FF", "#FF7272")) +
  theme(legend.position = "top")
  

