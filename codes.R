rm(list = ls())
options(scipen = 999)

# libraries
library(fredr)
fredr_set_key # set API key
library(purrr)
library(ggplot2)
library(tidyr)
library(dplyr)

# load yield curve data and recession indicator from FRED #################
data_fred <-map_dfr(c("USREC", "GS10", "TB3MS"), fredr)

# filter sample
data_fred <- data_fred %>% spread(series_id, value) %>% filter(date >= "1950-01-01") %>% mutate(spread = GS10 - TB3MS)

 ggplot(data_fred, aes(date, spread, ))+
     geom_line()
