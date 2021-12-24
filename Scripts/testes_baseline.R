# Lendo a base de dados ---------------------------------------------------

df <- readr::read_rds("Data_clean/df")

# Seperando os grupos (composite = yes or no) -----------------------------

comp_no <- df |>
  dplyr::filter(composite_death_icu_or_vent == 0) # grupo yes

comp_yes<- df |>
  dplyr::filter(composite_death_icu_or_vent == 1) # grupo no

# Comparação entre grupos - var cont --------------------------------------

# idade
t.test(x = comp_yes$age,
       y = comp_no$age,
       alternative = "two.sided",
       paired = FALSE,
       var.equal = TRUE,
       conf.level = TRUE)

# IMC
t.test(x = comp_yes$bmi,
       y = comp_no$bmi,
       alternative = "two.sided",
       paired = FALSE,
       var.equal = TRUE,
       conf.level = TRUE)

# temp
t.test(x = comp_yes$temp,
       y = comp_no$temp,
       alternative = "two.sided",
       paired = FALSE,
       var.equal = TRUE,
       conf.level = TRUE)

# SATO²
t.test(x = comp_yes$sat_o2_percent,
       y = comp_no$sat_o2_percent,
       alternative = "two.sided",
       paired = FALSE,
       var.equal = TRUE,
       conf.level = TRUE)

# creatinine
t.test(x = as.numeric(comp_yes$creatinine),
       y = as.numeric(comp_no$creatinine),
       alternative = "two.sided",
       paired = FALSE,
       var.equal = TRUE,
       conf.level = TRUE
       )

# CRP
t.test(x = as.numeric(comp_yes$crp),
       y = as.numeric(comp_no$crp),
       alternative = "two.sided",
       paired = FALSE,
       var.equal = TRUE,
       conf.level = TRUE)

# hemoglobin
t.test(x = as.numeric(comp_yes$hb_g_d_l),
       y = as.numeric(comp_no$hb_g_d_l),
       alternative = "two.sided",
       paired = FALSE,
       var.equal = TRUE,
       conf.level = TRUE)

# platelet
t.test(x = comp_yes$plat_mil_mm3,
       y = comp_no$plat_mil_mm3,
       alternative = "two.sided",
       paired = FALSE,
       var.equal = TRUE,
       conf.level = TRUE)

# dimero D
t.test(x = comp_yes$d_dimer_ng_m_l_feu,
       y = comp_no$d_dimer_ng_m_l_feu,
       alternative = "two.sided",
       paired = FALSE,
       var.equal = TRUE,
       conf.level = TRUE)

# troponina
t.test(x = comp_yes$trop,
       y = comp_no$trop,
       alternative = "two.sided",
       paired = FALSE,
       var.equal = TRUE,
       conf.level = TRUE,
       na.rm = TRUE)

# Comparação entre grupos - var cat ---------------------------------------

# sexo

comp_factor <- factor(x = df$composite_death_icu_or_vent, # fator composite
       levels = c("0","1"))

sexo_factor <- factor(x = df$x1_man_2_women,
                      levels = c("1","2"))

df_sexo <- table(comp_factor, sexo_factor)

chisq.test(df_sexo)

# fumo

smoking_factor <- factor(x = df$smoking,
                      levels = c("0","1"))

df_smoking <- table(comp_factor, smoking_factor)

chisq.test(df_smoking)

# obesidade

obesity_factor <- factor(x = df$obesity,
                         levels = c("0","1"))

df_obesity <- table(comp_factor, obesity_factor)

chisq.test(df_obesity)

# hipertensao

ht_factor <- factor(x = df$hypertension,
                    levels = c("0","1"))

df_ht <- table(comp_factor, ht_factor)

chisq.test(df_ht)

# diabetes

dm_factor <- factor(x = df$diabetes,
                         levels = c("0","1"))

df_dm <- table(comp_factor, dm_factor)

chisq.test(df_dm)

# asma

asma_factor <- factor(x = df$asma,
                    levels = c("0","1"))

df_asma <- table(comp_factor, asma_factor)

chisq.test(df_asma)

# DPOC

dpoc_factor <- factor(x = df$copd,
                      levels = c("0","1"))

df_dpoc <- table(comp_factor, dpoc_factor)

chisq.test(df_dpoc)

# DAC

dac_factor <- factor(x = df$congestive_arterial_disease,
                      levels = c("0","1"))

df_dac <- table(comp_factor, dac_factor)

chisq.test(df_dac)

# CKD

CKD_factor <- factor(x = df$chronic_renal_disease,
                     levels = c("0","1"))

df_ckd <- table(comp_factor, CKD_factor)

chisq.test(df_ckd)

# AMI

ami_factor <- factor(x = df$acute_myocardial_infarction,
                     levels = c("0","1"))

df_ami <- table(comp_factor, ami_factor)

chisq.test(df_ami)

# cancer

cancer_factor <- factor(x = df$cancer,
                     levels = c("0","1"))

df_cancer <- table(comp_factor, cancer_factor)

chisq.test(df_cancer)

# doença autoimune

dim_factor <- factor(x = df$autoimune,
                        levels = c("0","1"))

df_dim <- table(comp_factor, dim_factor)

chisq.test(df_dim)

# cancer

cancer_factor <- factor(x = df$cancer,
                        levels = c("0","1"))

df_cancer <- table(comp_factor, cancer_factor)

chisq.test(df_cancer)

# transplante figado

liver_factor <- factor(x = df$tranplante_figado,
                       levels = c("0","1"))

df_liver <- table(comp_factor, liver_factor)

chisq.test(df_liver)

# transplante renal

rim_factor <- factor(x = df$transplante_renal,
                       levels = c("0","1"))

df_rim <- table(comp_factor, rim_factor)

chisq.test(df_rim)

# transplante pulmão

lung_factor <- factor(x = df$transplante_pulmao,
                       levels = c("0","1"))

df_lung <- table(comp_factor, lung_factor)

chisq.test(df_lung)


# FMD, SAUC, IMTmean  e IMTmax --------------------------------------------

# FMD
t.test(x = comp_yes$fmd_percent_1s,
       y = comp_no$fmd_percent_1s,
       alternative = "two.sided",
       paired = FALSE,
       var.equal = TRUE,
       conf.level = TRUE,
       na.rm = TRUE)

# SAUC
t.test(x = comp_yes$srauc,
       y = comp_no$srauc,
       alternative = "two.sided",
       paired = FALSE,
       var.equal = TRUE,
       conf.level = TRUE,
       na.rm = TRUE)

# IMTmean
t.test(x = comp_yes$im_tmed_med,
       y = comp_no$im_tmed_med,
       alternative = "two.sided",
       paired = FALSE,
       var.equal = TRUE,
       conf.level = TRUE,
       na.rm = TRUE)

# IMTmax
t.test(x = comp_yes$im_tmax_med,
       y = comp_no$im_tmax_med,
       alternative = "two.sided",
       paired = FALSE,
       var.equal = TRUE,
       conf.level = TRUE,
       na.rm = TRUE)
