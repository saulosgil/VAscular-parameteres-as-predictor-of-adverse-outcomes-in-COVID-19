# Lendo e ajustando a base ------------------------------------------------

data <- readxl::read_excel("Data/28SARS-CoV-2_survival modelling_FINAL_lastJuly_SG (DECEMBER EDITION).xlsx",
                           sheet = "Spreadsheet_final_Saulo",
                           skip = 1)

data <- janitor::clean_names(data) # ajustando os labels

skimr::skim(data)
View(data)

# Criando um RDS file com a base ajusada ----------------------------------

readr::write_rds(x = data, file = "Data_clean/df")

View(readr::read_rds("Data_clean/df"))

