
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

# data clean --------------------------------------------------------------
### analysis the distribution of vaccine days
datafile_cont_reg |>
  filter(vaccine_g == 1) |>
  ggplot()+
  geom_histogram(mapping = aes(x = vaccine_d,
                               fill = as.character(outcome_s)),
                 binwidth = 14)

quantile(datafile_cont_reg$vaccine_d, na.rm = T)

datafile_cont_group <- datafile_cont_reg |>
  mutate(group = case_when(
    vaccine_g == 0 ~ '0',
    vaccine == 1 ~ '1',
    vaccine == 2 & vaccine_d <= 24*7 ~ '2',
    vaccine == 2 & vaccine_d > 24*7 & vaccine_d <= 52*7 ~ '3',
    vaccine == 3 & vaccine_d <= 8*7 ~ '4',
    vaccine == 3 & vaccine_d > 8*7 & vaccine_d <= 12*7 ~ '5',
    vaccine == 3 & vaccine_d > 12*7 & vaccine_d <= 24*7 ~ '6'
  )
  )


# vaccine effectiveness OR ------------------------------------------------

datafile_clog <- data.frame(
  var = character(),
  OR = numeric(),
  OR_1 = numeric(),
  OR_2 = numeric(),
  group = character(),
  adjust = character()
)

### adjust by age group and gender
for (g in as.character(1:6)) {
  res_clog <- clogit(formula = outcome_s ~ group + strata(gender) + strata(age_g),
                     data = filter(datafile_cont_group, group == g | vaccine_g == 0),
                     method = 'efron') %>%
    summary() %>%
    .[["conf.int"]] %>%
    as.data.frame() %>%
    select(-`exp(-coef)`) %>%
    rownames_to_column('var')
  names(res_clog)[2:4] <- c('OR', 'OR_1', 'OR_2')
  res_clog$group <- g
  res_clog$adjust <- 'adjust'
  datafile_clog <- rbind(datafile_clog, res_clog)
}

## unadjust
for (g in as.character(1:6)) {
  res_clog <- clogit(formula = outcome_s ~ group,
                     data = filter(datafile_cont_group, group == g | vaccine_g == 0),
                     method = 'efron') %>%
    summary() %>%
    .[["conf.int"]] %>%
    as.data.frame() %>%
    select(-`exp(-coef)`) %>%
    rownames_to_column('var')
  names(res_clog)[2:4] <- c('OR', 'OR_1', 'OR_2')
  res_clog$group <- g
  res_clog$adjust <- 'unadjust'
  datafile_clog <- rbind(datafile_clog, res_clog)
}

datafile_clog$vaccine <- as.character(rep(c(1, 2, 2, 3, 3, 3), 2))
group_labs <- c('All', '2-24 weeks', '24-52 weeks', '2-8 weeks', '8-12 weeks', '12-24 weeks')

datafile_clog$var <- rep(group_labs, 2)
write.csv(datafile_clog, file = './Table S3.csv', row.names = F)

datafile_clog$adjust <- factor(datafile_clog$adjust,
                               levels = c('unadjust', 'adjust'),
                               labels = c('A', 'B'))


# plot --------------------------------------------------------------------

fig1 <- datafile_clog |>
  ggplot(mapping = aes(x = (1 - OR)*100,
                       y = group,
                       color = vaccine))+
  geom_point()+
  geom_pointrange(mapping = aes(xmin = (1 - OR_2)*100,
                                xmax = (1 - OR_1)*100))+
  geom_vline(xintercept = 0,
             linetype = 'dashed',
             color = 'black')+
  coord_cartesian(xlim = c(-100, 100))+
  facet_grid(.~adjust)+
  scale_y_discrete(limits = rev,
                   labels = rev(group_labs))+
  scale_color_manual(values = fill_color[c(1,3:4)],
                     labels = c('Received 1 dose', 'Received 2 doses', 'Received Booster dose'),
                     na.translate = F)+
  theme_bw()+
  theme(legend.position = 'top',
        panel.grid.major.y = element_blank(),
        strip.background = element_blank(),
        strip.text = element_text(size = 14, hjust = 0),
        axis.ticks.x = element_blank(),
        axis.ticks.y = element_line(),
        axis.text = element_text(color = 'black'))+
  labs(y = NULL,
       x = 'Vaccine effectiveness(%)',
       color = NULL)

ggsave(filename = './fig2.pdf',
       fig1,
       width = 6, height = 4.5, family = 'Times New Roman',
       device = cairo_pdf)


