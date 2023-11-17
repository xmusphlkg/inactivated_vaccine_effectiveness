
# packages ----------------------------------------------------------------

library(tidyverse)
library(openxlsx)
library(lubridate)

fill_color <- paletteer::paletteer_d("ggsci::default_nejm")

# data clean --------------------------------------------------------------

datafile_cont_raw_1 <-read.xlsx('../data/vaccine_infections.xlsx', sheet = "quanzhou")
datafile_cont_raw_2 <-read.xlsx('../data/vaccine_infections.xlsx', sheet = "ningde")
datafile_cont_raw_3 <-read.xlsx('../data/vaccine_infections.xlsx', sheet = "xiamen")
datafile_cont_raw <- rbind(datafile_cont_raw_1, datafile_cont_raw_2, datafile_cont_raw_3)
remove(datafile_cont_raw_1, datafile_cont_raw_2, datafile_cont_raw_3)

## add var of vaccine product
datafile_cont <- datafile_cont_raw %>%
  mutate_at(vars(contains('date')), convertToDate) %>%
  mutate(vaccine = if_else(vaccine_accept > 0 &
                             lastvaccinedate >= as.Date('2022/3/10') - 13 &
                             !is.na(lastvaccinedate),
                           vaccine_accept - 1,
                           vaccine_accept),
         vaccine_produce = toupper(str_remove_all(vaccine_type, "_")),
         vaccine_produce = sapply(vaccine_produce, FUN = function(x){
           paste(sort(unique(strsplit(x, "")[[1]])), collapse = '')
         }),
         vaccine_inactivate = case_when(
           vaccine_produce == ''& vaccine == 0 ~ "Unvaccine",
           vaccine_produce %in% c("P", "V", "PV") ~ 'Inactivate',
           TRUE ~ 'Other'
         ))
table(datafile_cont$vaccine_produce)
table(datafile_cont$vaccine_inactivate)

## add age group and outcome values
datafile_cont_reg <-datafile_cont %>%
  mutate(vaccine_mix = if_else(nchar(vaccine_produce) < 2, '0', '1'),
         # gender = if_else(gender == 'Male', '1', '0'),
         age_g = if_else(age <18 & age >=0, 'c', 'a'),
         age_g = if_else(age >=65, 'o', age_g),
         age_g = factor(age_g, levels = c('c', 'a', 'o')),
         age_inf = round(age_inf),
         outcome_s = if_else(type %in% c('Mild', 'Moderate'), 1, 0),
         outcome_h = if_else(type %in% c('Moderate'), 1, 0)
  ) %>%
  filter(vaccine_inactivate != 'Other') %>%
  select(vaccine, vaccine_mix, vaccine_produce,vaccine_inactivate,vaccine_type,
         age_g, gender, age,
         age_inf,
         lastvaccinedate,
         firstvaccinedate, secondvaccinedate, thirdvaccinedate,
         outcome, outcome_s, outcome_h)
table(datafile_cont_reg$vaccine_produce)
table(datafile_cont_reg$vaccine_inactivate)

## fixed last vaccine dose date
datafile_cont_dose_0 <- datafile_cont_reg %>%
  filter(vaccine == 0) %>%
  mutate(lastvaccinedate = NA,
         firstvaccinedate = NA,
         secondvaccinedate = NA,
         thirdvaccinedate = NA)

datafile_cont_dose_1 <- datafile_cont_reg %>%
  filter(vaccine == 1) %>%
  mutate(lastvaccinedate = firstvaccinedate,
         secondvaccinedate = NA,
         thirdvaccinedate = NA)

datafile_cont_dose_2 <- datafile_cont_reg %>%
  filter(vaccine == 2) %>%
  mutate(lastvaccinedate = secondvaccinedate,
         thirdvaccinedate = NA)

datafile_cont_dose_3 <- datafile_cont_reg %>%
  filter(vaccine == 3) %>%
  mutate(lastvaccinedate = thirdvaccinedate)

datafile_cont_reg <-rbind(datafile_cont_dose_3,
                          datafile_cont_dose_2,
                          datafile_cont_dose_1,
                          datafile_cont_dose_0) %>%
  mutate(vaccine_d = as.Date('2022/3/17') - lastvaccinedate)

remove(datafile_cont_dose_3,
       datafile_cont_dose_2,
       datafile_cont_dose_1,
       datafile_cont_dose_0)

## remove negative date

datafile_cont_dose <- datafile_cont_reg %>%
  filter(vaccine_d < 14) %>%
  mutate(vaccine = vaccine - 1,
         lastvaccinedate = if_else(vaccine == 1,
                                   firstvaccinedate,
                                   thirdvaccinedate),
         vaccine_d = as.Date('2022/3/10') - lastvaccinedate)

datafile_cont_reg <- datafile_cont_dose %>%
  rbind(filter(datafile_cont_reg, vaccine_d >= 14| is.na(vaccine_d))) %>%
  mutate(vaccine_g = if_else(vaccine == 0, 0, 1),
         vaccine = as.factor(vaccine)) |>
  filter(age >= 3)

table(datafile_cont_reg$outcome)
table(datafile_cont_reg$outcome_s)
table(datafile_cont_reg$outcome_h)

table(datafile_cont_reg$vaccine_produce)
table(datafile_cont_reg$vaccine_inactivate)

datafile_cont_reg <- datafile_cont_reg |>
  mutate(vaccine_expire = case_when(vaccine_g == 1 & vaccine_d <= 4*7 ~ '1',
                                    vaccine_g == 1 & vaccine_d > 4*7 & vaccine_d <= 8*7 ~ '2',
                                    vaccine_g == 1 & vaccine_d > 8*7 & vaccine_d <= 12*7 ~ '3',
                                    vaccine_g == 1 & vaccine_d > 12*7 & vaccine_d <= 24*7 ~ '4',
                                    vaccine_g == 1 & vaccine_d > 24*7 & vaccine_d <= 52*7 ~ '5',
                                    vaccine_g == 1 & vaccine_d > 52*7 ~ '6',
                                    TRUE ~ 'U'),
         vaccine_expire = factor(vaccine_expire,
                                 levels = c('U', as.character(1:6)))
         )

save(datafile_cont_raw, datafile_cont_reg, datafile_cont, fill_color,
     file = '../data/omicron_ve.RData')
