library(parameters)
library(performance)
library(mice)

# lendo a base ------------------------------------------------------------

df <- readr::read_rds(file = "Data_clean/df")

# verificando missing -----------------------------------------------------

mice::md.pattern(x = df, plot = FALSE)


# Modelos e plot ----------------------------------------------------------
# Plot
df |>
  ggplot2::ggplot(mapping = ggplot2::aes(x = as.factor(icu_admission_0_no_1_yes),
                                         y = im_tmed_med)) +
  ggplot2::geom_boxplot(na.rm = TRUE) +
  ggplot2::geom_jitter(position = ggplot2::position_jitter(0.05))

#Excluindo os outliers

df2 <- df |> dplyr::filter(im_tmed_med < 1.2)

ggplot2::ggplot(data = df2, mapping = ggplot2::aes(x = as.factor(icu_admission_0_no_1_yes),
                                                   y = im_tmed_med)) +
  ggplot2::geom_boxplot(na.rm = TRUE) +
  ggplot2::geom_jitter(position = ggplot2::position_jitter(0.05))

# Modelo ajustado

model1 <- parameters::model_parameters(glm(formula =  icu_admission_0_no_1_yes ~ im_tmed_med +
                                             x18_39_0_40_64_1_60_3 +
                                             x1_man_2_women +
                                             sat_o2_percent +
                                             obesity +
                                             smoking +
                                             temp,
                                           family = binomial,
                                           data = df2))

model1

# Odds ratio

odds <- round(exp(coef(object = glm(formula =  icu_admission_0_no_1_yes ~ im_tmed_med +
                                      x1_man_2_women +
                                      sat_o2_percent +
                                      obesity +
                                      smoking +
                                      temp,
                                    family = binomial,
                                    data = df2))), digits = 2)

# IC95% do odds ratio

ic <- round(exp(confint(object = glm(formula =  icu_admission_0_no_1_yes ~ im_tmed_med +
                                       x1_man_2_women +
                                       sat_o2_percent +
                                       obesity +
                                       smoking +
                                       temp,
                                     family = binomial,
                                     data = df2))), digits = 2)

# data.frame com o odds ratio e IC95%

data.frame(odds, ic)

