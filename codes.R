rm(list = ls())
options(scipen = 999)

# libraries
library(fredr)
fredr_set_key # set API key
library(purrr)
library(ggplot2)
library(tidyr)
library(dplyr)
library(xlsx)
library(lubridate)

# load yield curve data and recession indicator from FRED #################
data_fred_raw <-map_dfr(c("USREC", "GS10", "TB3MS"), fredr)

# filter sample
data_fred <- data_fred_raw %>% spread(series_id, value) %>% 
                           rename(i10y = GS10, i3m = TB3MS, nber_rec = USREC) %>%
                           filter(date >= "1950-01-01") %>% 
                           mutate(spr_10y_3m = i10y - i3m)

# plot spread
 ggplot(data_fred, aes(date, spread, ))+
     geom_line()
 
 # load term spread data from NY Fed
 data_nyfed_raw <- read.table("ACMTermPremium.csv", 
                         header = TRUE, 
                         sep = ";", 
                         blank.lines.skip = FALSE)
 
 data_nyfed_raw$DATE <- dmy(data_nyfed_raw$DATE)
 
 # select term premium on 10 year bond
 data_nyfed <- data_nyfed_raw %>% rename(date = DATE, term_prem10y_daily = ACMTP10) %>%
                                  select(date, term_prem10y_daily) %>% 
                                  mutate(yy = year(date), mm = month(date)) %>%
                                  group_by(yy, mm) %>%
                                  summarise(term_prem10y = round(mean(term_prem10y_daily),2)) %>%
                                  ungroup() %>% 
                                  mutate(date = make_date(year = yy, month = mm)) %>%
                                  select(-yy, -mm) %>%
                                  select(date, term_prem10y)

# merge two datasets and create term-premium adjusted spread ########
data <- merge(data_fred, data_nyfed, by = "date") %>% mutate(i10y_termprem = i10y - term_prem10y,
                                                             spr_10y_3m_termprem = i10y - term_prem10y - i3m)

ggplot(data, aes(x = date))+
    geom_line(aes(y = i10y), col = "blue")+
    geom_line(aes(y = i10y_termprem), col = "red")+
    geom_line(aes(y = i3m), col = "black")

ggplot(data, aes(x = date))+
    geom_line(aes(y = spr_10y_3m), col = "blue")+
    geom_line(aes(y = spr_10y_3m_termprem), col = "red")

ggplot(filter(data, date >= "1990-01-01"), aes(x = date, y = term_prem10y))+
    geom_line()

# convert to quarterly frequency ####################

dataQ <- data %>% gather(variable, value, -date) %>% 
                  mutate(yy = year(date), qq = ceiling(month(date)/3)) %>%
                  group_by(yy, qq, variable) %>%
                  summarise(tmp = mean(value)) %>%
                  spread(variable, tmp) %>%
                  ungroup() %>% 
                  mutate(date = make_date(year = yy, month = 3*qq),
                         nber_rec = round(nber_rec,0)) %>%
                  select(-yy, -qq) %>%
                  select(date, everything()) 


ggplot(dataQ, aes(x = date))+
    geom_line(aes(y = spr_10y_3m), col = "blue")+
    geom_line(aes(y = spr_10y_3m_termprem), col = "red")

# estimate probit model #################################

model_probit <- glm(nber_rec ~ spr_10y_3m, 
                    family = binomial(link = "probit"), 
                    data = dataQ)
coefficients(model_probit)

model_probit_termprem <- glm(nber_rec ~ spr_10y_3m_termprem, 
                             family = binomial(link = "probit"), 
                             data = dataQ)

coefficients(model_probit_termprem)



                        

