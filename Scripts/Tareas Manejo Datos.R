setwd("GitHub/DoingEconomics_Taller-1")

library(haven)
library(dplyr)
library(tidyr)
TenderosFU03_Publica <- read_dta("Data/Raw/TenderosFU03_Publica.dta")
View(TenderosFU03_Publica)

# Tarea 1

internet_muni <- TenderosFU03_Publica %>%
  select(Munic_Dept, uso_internet, Municipio) %>%
  group_by(Munic_Dept, Municipio) %>%
  summarise(p_int = sum(uso_internet)/n() * 100)

View(internet_muni)

# Tarea 2

lista_sec <- c(
  "actG1"  = "Tienda",
  "actG2"  = "Comida preparada",
  "actG3"  = "Peluquería y belleza",
  "actG4"  = "Ropa",
  "actG5"  = "Otras variedades",
  "actG6"  = "Papelería y comunicaciones",
  "actG7"  = "Vida nocturna",
  "actG8"  = "Productos bajo inventario",
  "actG9"  = "Salud",
  "actG10" = "Servicios",
  "actG11" = "Ferretería y afines"
)

internet_sectores <- TenderosFU03_Publica %>%
  select(uso_internet, actG1:actG11) %>%
  pivot_longer(cols = actG1:actG11, names_to = "sectores", values_to = "valor") %>%
  filter(valor == 1) %>%
  select(-valor) %>%
  mutate(sectores = recode(sectores, !!!lista_sec)) %>%
  group_by(sectores) %>%
  summarise(p_int = sum(uso_internet)/n() * 100) %>%
  arrange(desc(p_int))
  