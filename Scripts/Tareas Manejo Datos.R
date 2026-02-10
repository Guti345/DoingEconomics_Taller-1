setwd("GitHub/DoingEconomics_Taller-1")
cat('\014') # Limpia la consola
rm(list=ls()) # Vacia la memoria

library(haven)
library(dplyr)
library(readxl)
library(tidyr)
library(ggplot2)
library(stringr)

library(RColorBrewer)

n_sec <- n_distinct(internet_sec_muni$sectores)

pal_set2_ext <- colorRampPalette(brewer.pal(8, "Set2"))(n_sec)



TenderosFU03_Publica <- read_dta("Data/Raw/TenderosFU03_Publica.dta")
View(TenderosFU03_Publica)

# Tarea 1

internet_muni <- TenderosFU03_Publica %>%
  select(Munic_Dept, uso_internet, Municipio) %>%
  group_by(Munic_Dept, Municipio) %>%
  summarise(p_int = sum(uso_internet)/n() * 100)

View(internet_muni)

ggplot(internet_muni, aes(x = reorder(Municipio, p_int), y = p_int)) +
  geom_col(fill = "steelblue") +
  labs(
    x = "Municipio",
    y = "Penetración de internet",
    title = "Penetración de internet por municipio"
  ) +
  theme_minimal()
ggsave("Penetración x municipio.png", plot = last_plot(), path = "Outputs/Graphs")

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

ggplot(internet_sectores, aes(x = reorder(sectores, p_int), y = p_int)) +
  geom_col(fill = "orange") +
  labs(
    x = "Sector Económico",
    y = "Penetración de internet",
    title = "Penetración de internet por sector económico"
  ) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1)
  )
ggsave("Penetración x sector económico.png", plot = last_plot(), path = "Outputs/Graphs")

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

p_facets <- ggplot(
  internet_sec_muni,
  aes(x = sectores, y = p_int, fill = sectores)
) +
  geom_col(color = "grey30", linewidth = 0.2) +
  facet_wrap(~ Municipio) +
  scale_fill_manual(values = pal_set2_ext) +
  labs(
    x = NULL,
    y = "Penetración de internet (%)",
    fill = "Sector económico",
    title = "Penetración de internet por sector económico y municipio"
  ) +
  theme_minimal(base_size = 11) +
  theme(
    axis.text.x  = element_blank(),
    axis.ticks.x = element_blank(),
    axis.title.x = element_blank(),
    legend.position = "right",
    panel.grid.major.x = element_blank()
  )

p_facets
ggsave("Penetración x sector económico x Municipio.png", plot = p_facets, path = "Outputs/Graphs")


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

internet_sec_mun_pob <- internet_sec_muni %>%
  left_join(poblacion %>% select(Munic_Dept, Pob_Muni), by = "Munic_Dept") %>%
  filter(!is.na(Pob_Muni), !is.na(p_int)) %>%
  mutate(Pob_Muni = as.numeric(Pob_Muni))

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
ggsave("Penetración vs Población Municipio.png", plot = last_plot(), path = "Outputs/Graphs")


p_sec_facets <- ggplot(
  internet_sec_mun_pob,
  aes(x = Pob_Muni, y = p_int)
) +
  geom_point(aes(color = Municipio), alpha = 0.75, size = 2) +
  geom_smooth(method = "lm", se = FALSE, color = "black", linewidth = 0.9) +
  facet_wrap(~ sectores, scales = "free_y") +
  labs(
    x = "Población municipal",
    y = "Penetración de internet (%)",
    color = "Municipio",
    title = "Penetración vs Población por sector económico por ciudad",
    subtitle = "Cada facet muestra la relación dentro del sector; línea negra = tendencia lineal"
  ) +
  theme_minimal(base_size = 11)

p_sec_facets
ggsave("Penetración vs Población x Municipio y Sector.png", plot = p_sec_facets, path = "Outputs/Graphs")


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


