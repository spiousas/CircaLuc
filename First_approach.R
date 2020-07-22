library(tidyverse)

setwd("~/Dropbox/Estadistica/CircaLuc")
data.df <- read_csv("./data/data.csv") %>%
           gather(key = "Subject", 
                  value = "Lumin", 
                  -ZTTime , 
                  factor_key=TRUE) %>%
           group_by(Subject) %>%
           mutate(Z_Lumin = (Lumin-mean(Lumin, na.rm=TRUE))/sd(Lumin, na.rm=TRUE))

fig.mean.time <- data.df %>%
  filter(Subject == "A01") %>%
  ggplot(aes(x = ZTTime, y = Z_Lumin)) + 
                geom_line()
fig.mean.time


