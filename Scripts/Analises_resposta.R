# Carregando pacotes
library(tidyverse)
library(ggplot2)
library(sjPlot)
library(patchwork)

# lendo a base de dados

data <- readRDS("Data_clean/df")

glimpse(data)

# Modelagem ---------------------------------------------------------------

# classification_fmd_0_low_1_normal_3_43_percent
# modelo 1

m1 <- glm(formula = composite_death_icu_or_vent ~ classification_fmd_0_low_1_normal_3_43_percent,
          family = "binomial",
          data = data)

summary(m1)

# plot

plot_grpfrq(data$composite_death_icu_or_vent,
            data$classification_fmd_0_low_1_normal_3_43_percent,
            geom.colors = "gs")

# table

p1 <- sjPlot::plot_model(m1,
                         colors = "bw",
                         dot.size = 1,
                         vline.color = "black",
                         vline.size = .1,
                         value.offset = .4,
                         show.values = TRUE,
                         value.size = 2,
                         title = "IPAq (reference = ativo)"
)

set_theme(base = theme_classic())  #To change y axis text size

p1

# resultados
parameters::model_parameters(m1,
                             exponentiate = TRUE)
# modelo 2
m1_1 <- glm(formula = composite_death_icu_or_vent ~ classification_fmd_0_low_1_normal_3_43_percent +
              x1_man_2_women + age + sat_o2_percent + smoking +
              obesity + pre_existing_condition,
          family = "binomial",
          data = data)

summary(m1_1)


# table

p1_1 <- sjPlot::plot_model(m1_1,
                         colors = "bw",
                         dot.size = 1,
                         vline.color = "black",
                         vline.size = .1,
                         value.offset = .4,
                         show.values = TRUE,
                         value.size = 2,
                         title = "IPAq (reference = ativo)"
)

set_theme(base = theme_classic())  #To change y axis text size

p1_1

# resultados
parameters::model_parameters(m1_1,
                             exponentiate = TRUE)

# modelo 3
m1_2 <- glm(formula = composite_death_icu_or_vent ~ classification_fmd_0_low_1_normal_3_43_percent +
              x1_man_2_women + age + sat_o2_percent + smoking +
              obesity + pre_existing_condition + d_dimer_ng_m_l_feu + as.numeric(crp),
            family = "binomial",
            data = data)

summary(m1_2)


# table

p1_2 <- sjPlot::plot_model(m1_2,
                           colors = "bw",
                           dot.size = 1,
                           vline.color = "black",
                           vline.size = .1,
                           value.offset = .4,
                           show.values = TRUE,
                           value.size = 2,
                           title = "IPAq (reference = ativo)"
)

set_theme(base = theme_classic())  #To change y axis text size

p1_2

# resultados
parameters::model_parameters(m1_2,
                             exponentiate = TRUE)
