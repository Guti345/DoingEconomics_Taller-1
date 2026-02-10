cat('\014') # Limpia la consola
rm(list=ls()) # Vacia la memoria
setwd("GitHub/DoingEconomics_Taller-1")

library(haven)
library(dplyr)
library(readxl)
library(tidyr)
library(ggplot2)
library(stringr)


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

View(internet_sectores)

# Tarea 3

internet_sec_muni <- TenderosFU03_Publica %>%
  select(uso_internet, actG1:actG11, Munic_Dept, Municipio) %>%
  pivot_longer(cols = actG1:actG11, names_to = "sectores") %>%
  arrange(Munic_Dept) %>%
  filter(value == 1) %>%
  select(-value) %>%
  group_by(sectores,Municipio, Munic_Dept) %>%
  summarise(p_int = (sum(uso_internet)/n())*100) %>%
  mutate(sectores = recode(sectores, !!!lista_sec)) %>%
  arrange(Municipio,desc(p_int))

# Tarea 4

TerriData_Dim2 <- read_excel("Data/Raw/TerriData_Dim2.xlsx")

poblacion <- TerriData_Dim2 %>%
  filter(Año == 2022) %>%
  select(Munic_Dept = `Código Entidad`, Municipio = Entidad, Poblacion = `Dato Numérico`) %>%
  mutate(Munic_Dept = as.numeric(Munic_Dept)) %>%
  mutate(Poblacion = as.numeric(str_replace_all(str_replace_all(Poblacion, "\\.", ""), ",", "."))) %>%
  group_by(Munic_Dept, Municipio) %>%
  summarise(Pob_Muni = sum(Poblacion))
  
internet_mun_pob <- merge(internet_muni, poblacion, by.x = "Munic_Dept", by.y = "Munic_Dept", all.x = TRUE)

internet_mun_pob <- internet_mun_pob %>%
  select(-Municipio.y)

corr_a <- cor(internet_mun_pob$p_int, internet_mun_pob$Pob_Muni)

ggplot(internet_mun_pob,
       aes(x = Pob_Muni, y = p_int)) +
  
  # Puntos por municipio
  geom_point(aes(color = Municipio.x), alpha = 0.7, size = 2) +
  
  # Línea de tendencia global
  geom_smooth(
    method = "lm",
    se = FALSE,
    color = "black",
    linewidth = 1
  ) +
  
  labs(
    x = "Población municipal",
    y = "Penetración de internet",
    color = "Municipio"
  ) +
  theme_minimal()

# Tarea 5 

internet_sec_mun_pob <- merge(internet_sec_muni, poblacion, by.x = "Munic_Dept", by.y = "Munic_Dept", all.x = TRUE)

internet_sec_mun_pob <- internet_sec_mun_pob %>%
  select(-Municipio.y)

wide_internet_sec_muni_pob <- internet_sec_mun_pob %>%
  select(Munic_Dept, Municipio.x, sectores, p_int, Pob_Muni) %>%
  pivot_wider(
    id_cols = c(Munic_Dept, Municipio.x, Pob_Muni),
    names_from = sectores,
    values_from = p_int,
    names_prefix = "p_int."
  )

large_internet_sec_muni_pob <- internet_sec_mun_pob


