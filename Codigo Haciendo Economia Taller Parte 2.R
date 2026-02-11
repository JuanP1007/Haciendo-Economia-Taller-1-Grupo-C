
library(tidyverse)
library(haven)
library(readxl)
TerriData_Dim2 <- read_excel("D:/Jose Luis/Descargas/TerriData_Dim2.xlsx")
TenderosFU03_Publica <- read_dta("D:/Jose Luis/Descargas/TenderosFU03_Publica.dta")

#Punto 1

internet_municipio <- TenderosFU03_Publica %>%
  group_by(Munic_Dept) %>%
  summarise(
    total_tiendas = n(),
    tiendas_con_internet = sum(uso_internet, na.rm = TRUE),
    penetracion_internet = tiendas_con_internet / total_tiendas * 100
  )

municipios <- tibble(
  Munic_Dept = c(11001, 25899, 25307, 73001, 8001, 5088, 68001, 41001, 25754, 66001, 5001, 47001, 13001),
  municipio = c("Bogotá", "Zipaquirá", "Girardot", "Ibagué", "Barranquilla", "Bello",
                "Bucaramanga", "Neiva", "Soacha", "Pereira", "Medellín", "Santa Marta", "Cartagena")
)

internet_municipio <- internet_municipio %>%
  left_join(municipios, by = "Munic_Dept")

#Punto 2

base_act <- TenderosFU03_Publica %>%
  mutate(
    actividad = case_when(
      actG1  == 1 ~ "actG1",
      actG2  == 1 ~ "actG2",
      actG3  == 1 ~ "actG3",
      actG4  == 1 ~ "actG4",
      actG5  == 1 ~ "actG5",
      actG6  == 1 ~ "actG6",
      actG7  == 1 ~ "actG7",
      actG8  == 1 ~ "actG8",
      actG9  == 1 ~ "actG9",
      actG10 == 1 ~ "actG10",
      actG11 == 1 ~ "actG11",
      TRUE ~ NA_character_
    )
  )

internet_actividad <- base_act %>%
  group_by(actividad) %>%
  summarise(
    total_tiendas = n(),
    tiendas_con_internet = sum(uso_internet, na.rm = TRUE),
    penetracion_internet = tiendas_con_internet / total_tiendas * 100,
    .groups = "drop"
  )

actividades <- tibble(
  actividad = paste0("actG", 1:11),
  nombre_actividad = c(
    "Tienda",
    "Comida preparada",
    "Peluquería y belleza",
    "Ropa",
    "Otras variedades",
    "Papelería y comunicaciones",
    "Vida nocturna",
    "Productos bajo inventario",
    "Salud",
    "Servicios",
    "Ferretería y afines"
  )
)

internet_actividad <- internet_actividad %>%
  left_join(actividades, by = "actividad")

#Punto 3

base_act <- base_act %>%
  left_join(municipios, by = "Munic_Dept") %>%
  left_join(actividades, by = "actividad") %>%
  filter(!is.na(actividad), !is.na(municipio))

internet_act_ciudad <- base_act %>%
  group_by(Munic_Dept, municipio, actividad, nombre_actividad) %>%
  summarise(
    total_tiendas = n(),
    tiendas_con_internet = sum(uso_internet, na.rm = TRUE),
    penetracion_internet = tiendas_con_internet / total_tiendas * 100,
    .groups = "drop"
  ) %>%
  arrange(municipio, desc(penetracion_internet))

#Punto 4

poblacion_2022 <- TerriData_Dim2 %>%
  filter(Año == 2022) %>%
  rename(Munic_Dept = `Código Entidad`) %>%
  mutate(Munic_Dept = as.numeric(Munic_Dept))

internet_poblacion <- internet_municipio %>%
  left_join(poblacion_2022, by = "Munic_Dept")

