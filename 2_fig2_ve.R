
## VE against moderate

# packages ----------------------------------------------------------------

library(tidyverse)
library(RColorBrewer)
library(patchwork)
library(survival)
library(Cairo)

remove(list = ls())

load('../data/omicron_ve.RData')

set.seed(202205)

# vaccine effectiveness OR subgroup ---------------------------------------

datafile_clog <- data.frame(
  var = character(),
  OR = numeric(),
  OR_1 = numeric(),
  OR_2 = numeric(),
  group = character(),
  adjust = character()
)

for (g in levels(datafile_cont_reg$age_g)) {
  res_clog <- clogit(formula = outcome_s ~ vaccine + strata(gender),
                     data = filter(datafile_cont_reg, age_g == g),
                     method = 'efron') %>%
    summary() %>%
    .[["conf.int"]] %>%
    as.data.frame() %>%
    select(-`exp(-coef)`) %>%
    rownames_to_column('var')
  names(res_clog)[2:4] <- c('OR', 'OR_1', 'OR_2')
  res_clog$group <- g
  res_clog$adjust <- 'age'
  datafile_clog <- rbind(datafile_clog, res_clog)
}

for (g in c('Male', 'Female')) {
  res_clog <- clogit(formula = outcome_s ~ vaccine  + strata(age_g),
                     data = filter(datafile_cont_reg, gender == g),
                     method = 'efron') %>%
    summary() %>%
    .[["conf.int"]] %>%
    as.data.frame() %>%
    select(-`exp(-coef)`) %>%
    rownames_to_column('var')
  names(res_clog)[2:4] <- c('OR', 'OR_1', 'OR_2')
  res_clog$group <- g
  res_clog$adjust <- 'gender'
  datafile_clog <- rbind(datafile_clog, res_clog)
}

for (g in c('V', 'P', 'PV')) {
  res_clog <- clogit(formula = outcome_s ~ vaccine  + strata(age_g) + strata(gender),
                     data = filter(datafile_cont_reg, vaccine_produce == g | vaccine_g == 0),
                     method = 'efron') %>%
    summary() %>%
    .[["conf.int"]] %>%
    as.data.frame() %>%
    select(-`exp(-coef)`) %>%
    rownames_to_column('var')
  names(res_clog)[2:4] <- c('OR', 'OR_1', 'OR_2')
  res_clog$group <- g
  res_clog$adjust <- 'produce'
  datafile_clog <- rbind(datafile_clog, res_clog)
}
datafile_clog$group <- factor(datafile_clog$group,
                              levels = c('c', 'a', 'o', 'Male', 'Female', 'V', 'P', 'PV'),
                              labels = c('3-17', '18-64', "≥65", "Male", 'Female', "SinoVac", "SinoPharm", "SinoVac&SinoPharm"))
write.csv(datafile_clog, file = './Table S4.csv', row.names = F)

datafile_clog$var <- factor(datafile_clog$var,
                            levels = paste('vaccine', 1:3, sep = ""),
                            labels = LETTERS[3:5])

# plot --------------------------------------------------------------------

group_labs <- c('3-17', '17-64', '≥65', 'Male', 'Female', 'SinoVac', 'SinoPharm', 'SinoVac&\nSinoPharm')

fig2 <- datafile_clog |>
  filter(!is.infinite(OR_2)) |>
  ggplot(mapping = aes(x = (1 - OR)*100,
                       y = group,
                       color = var))+
  geom_point()+
  geom_pointrange(mapping = aes(xmin = (1 - OR_2)*100,
                                xmax = (1 - OR_1)*100))+
  geom_vline(xintercept = 0,
             linetype = 'dashed',
             color = 'black')+
  coord_cartesian(xlim = c(-100, 100))+
  scale_y_discrete(limits = rev,
                   labels = rev(group_labs))+
  scale_color_manual(values = fill_color[c(1,3:4)],
                     labels = c('Received 1 dose', 'Received 2 doses', 'Received Booster dose'),
                     na.translate = F)+
  facet_grid(.~var)+
  theme_bw()+
  theme(legend.position = 'none',
        panel.grid.major.y = element_blank(),
        strip.background = element_blank(),
        strip.text = element_text(size = 14, hjust = 0),
        axis.ticks.x = element_blank(),
        axis.ticks.y = element_line(),
        axis.text = element_text(color = 'black'))+
  labs(y = NULL,
       x = 'Vaccine effectiveness(%)',
       color = NULL)

ggsave('./fig3.pdf',
       fig2,
       width = 9, height = 5.4,
       family = 'Times New Roman',
       device = cairo_pdf)

