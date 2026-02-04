setwd("GitHub/DoingEconomics_Taller-1")

library(haven)
TenderosFU03_Publica <- read_dta("Data/Raw/TenderosFU03_Publica.dta")
View(TenderosFU03_Publica)

internet <- TenderosFU03_Publica %>%
  select(Munic_Dept, uso_internet, Municipio) %>%
  group_by(Munic_Dept, Municipio) %>%
  summarise(p_int = sum(uso_internet)/n() * 100)

View(internet)


