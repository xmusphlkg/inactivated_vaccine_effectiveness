
# packages ----------------------------------------------------------------

library(tidyverse)
library(RColorBrewer)
library(patchwork)
library(paletteer)
library(DescrTab2)
# library(ggpubr)
library(gridExtra)
library(survival)
library(survminer)
library(ggforce)

remove(list = ls())

load('../data/omicron_ve.RData')

set.seed(202205)

# waning -----------------------------------------------------------------

datafile_cont_swap <- datafile_cont_reg |>
  mutate(vaccine_dw = as.numeric(vaccine_d)/7)

fig_1 <- datafile_cont_swap |>
  filter(vaccine %in% 1:3) |>
  ggplot(
    mapping = aes(x=vaccine_dw)
    ) +
  geom_histogram(
    mapping = aes(
      y = after_stat(density),
      fill = vaccine
      ),
    breaks=seq(0, 80, 4),
    color = 'white'
    ) +
  scale_fill_manual(
    values = fill_color[c(6:8)],
    na.translate = F
    )+
  facet_wrap(
    .~vaccine,
    scales = 'free',
    labeller = as_labeller(c('1' = 'A',
                             '2' = 'B',
                             '3' = 'C')),
    ncol = 1
    )+
  scale_y_continuous(
    limits = c(0, NA),
    expand = expansion(mult = c(0, 0.2))
    )+
  theme_classic()+
  theme(panel.spacing = unit(0.2, 'lines'),
        axis.text.x = element_text(size = 10, hjust = .5, vjust = 0.5, face = 'plain', color = 'black'),
        axis.text.y = element_text(size = 10, hjust = 1, vjust = 0.5, face = 'plain', color = 'black'),
        plot.title = element_text(hjust = 0, size = 16, face = 'bold'),
        strip.text = element_text(hjust = 0, size = 16, face = 'bold'),
        legend.position = 'right',
        strip.background = element_blank(),
        plot.margin = margin(2, 30, 2, 5))+
  labs(x = 'Weeks after last vaccine dose',
       y = 'Density of participants',
       fill = "Vaccine Dose")

fig_2 <- datafile_cont_swap |>
  filter(vaccine %in% 1:3 & outcome_s == 0) |>
  ggplot(
    mapping = aes(x=vaccine_dw)
  ) +
  geom_histogram(
    mapping = aes(
      y = after_stat(density),
      fill = vaccine
    ),
    breaks=seq(0, 80, 4),
    color = 'white'
  ) +
  scale_fill_manual(
    values = fill_color[c(6:8)],
    na.translate = F
  )+
  facet_wrap(
    .~vaccine,
    scales = 'free',
    labeller = as_labeller(c('1' = 'D',
                             '2' = 'E',
                             '3' = 'F')),
    ncol = 1
  )+
  scale_y_continuous(
    limits = c(0, NA),
    expand = expansion(mult = c(0, 0.2))
  )+
  theme_classic()+
  theme(panel.spacing = unit(0.2, 'lines'),
        axis.text.x = element_text(size = 10, hjust = .5, vjust = 0.5, face = 'plain', color = 'black'),
        axis.text.y = element_text(size = 10, hjust = 1, vjust = 0.5, face = 'plain', color = 'black'),
        plot.title = element_text(hjust = 0, size = 16, face = 'bold'),
        strip.text = element_text(hjust = 0, size = 16, face = 'bold'),
        legend.position = 'right',
        strip.background = element_blank(),
        plot.margin = margin(2, 30, 2, 5))+
  labs(x = 'Weeks after last vaccine dose',
       y = 'Density of participants',
       fill = "Vaccine Dose")

fig_3 <- datafile_cont_swap |>
  filter(vaccine %in% 1:3 & outcome_s == 1) |>
  ggplot(
    mapping = aes(x=vaccine_dw)
  ) +
  geom_histogram(
    mapping = aes(
      y = after_stat(density),
      fill = vaccine
    ),
    breaks=seq(0, 80, 4),
    color = 'white'
  ) +
  scale_fill_manual(
    values = fill_color[c(6:8)],
    na.translate = F
  )+
  facet_wrap(
    .~vaccine,
    scales = 'free',
    labeller = as_labeller(c('1' = 'G',
                             '2' = 'H',
                             '3' = 'I')),
    ncol = 1
  )+
  scale_y_continuous(
    limits = c(0, NA),
    expand = expansion(mult = c(0, 0.2))
  )+
  theme_classic()+
  theme(panel.spacing = unit(0.2, 'lines'),
        axis.text.x = element_text(size = 10, hjust = .5, vjust = 0.5, face = 'plain', color = 'black'),
        axis.text.y = element_text(size = 10, hjust = 1, vjust = 0.5, face = 'plain', color = 'black'),
        plot.title = element_text(hjust = 0, size = 16, face = 'bold'),
        strip.text = element_text(hjust = 0, size = 16, face = 'bold'),
        legend.position = 'right',
        strip.background = element_blank(),
        plot.margin = margin(2, 30, 2, 5))+
  labs(x = 'Weeks after last vaccine dose',
       y = 'Density of participants',
       fill = "Vaccine Dose")

fig_1 + fig_2 + fig_3 +
  plot_layout(ncol = 3, guides = 'collect')

ggsave(filename = './fig S1.pdf',
       width = 9, height = 6, family = 'Times New Roman',
       device = cairo_pdf)



