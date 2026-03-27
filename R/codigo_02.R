###############################################################################
# PROYECTO: Análisis de la variabilidad del cambio del bosque en el Chaco Seco
# Área de estudio: Provincia de Córdoba
# Descripción:
# Este script analiza la dinámica de:
# - Expansión del riego por pivote
# - Pérdida de cobertura arbórea
# - Evolución de coberturas del suelo
# a diferentes escalas (provincial y departamental)
###############################################################################

#------------------------------------------------------------------------------
# 0. Configuracion inicial
#------------------------------------------------------------------------------

# Directorio de trabajo
setwd("D:/Josefina/Proyectos/Bosques/codigo")

# Librerías principales
library(ggplot2)
library(dplyr)
library(tidyr)
library(gridExtra)
library(scales)

# Paleta base utilizada en gráficos
col_verde <- "#238b45"
col_azul  <- "#3182bd"

###############################################################################
# 1. Expansion del riego por pivote 
###############################################################################
# Dataset manual con:
# - Número total de pivotes
# - Superficie total bajo riego
# - Nuevos pivotes por año
# - Nueva superficie incorporada
# Período: 2022-2024

#Extraido pag web
data_riego <- data.frame(
  year = factor(c(2022, 2023, 2024), levels = c(2022, 2023, 2024)),
  puntos_total = c(1709, 1835, 1848),
  superficie_total = c(123370, 130180, 132514),
  puntos_nuevos = c(239, 249, 251),
  superficie_nueva = c(20579, 21727, 22241)
)

#------------------------------------------------------------------------------
# 1.1 Puntos totales de riego – Provincia de Córdoba

#Primer plot
puntos_total <- ggplot(data_riego, aes(x = year, y = puntos_total)) +
  geom_col(fill = col_verde, width = 0.6) +
  geom_text(aes(label = puntos_total),
            vjust = 5, color = "white", size = 4) +
  theme_classic() +
  labs(x = "Año",
       y = "Cantidad de pivotes",
       title = "Puntos de riego por pivote - Provincia de Córdoba")

#------------------------------------------------------------------------------
# 1.2 Superficie total bajo riego


superficie_total <- ggplot(data_riego, aes(x = year, y = superficie_total)) +
  geom_col(fill = col_verde, width = 0.6) +
  geom_text(aes(label = comma(superficie_total)),
            vjust = 5, color = "white", size = 4) +
  theme_classic() +
  scale_y_continuous(expand = c(0,0)) +
  labs(x = "Año",
       y = "Superficie (ha)",
       title = "Superficie total bajo riego - Provincia de Córdoba")

#------------------------------------------------------------------------------
# 1.3 Visualización conjunta


grid.arrange(puntos_total, superficie_total, ncol = 2)

###############################################################################
# 2. RIEGO POR PIVOTE EN CHACO SECO (ESCALA DEPARTAMENTAL)

# Se trabaja con base completa y se filtra:
# tipo == "CS" (Chaco Seco)

df <- read.csv("D:/Josefina/Proyectos/Bosques/data/Riego_pivot_resumen.csv",
               fileEncoding = "latin1")

df <- df %>% 
  filter(tipo == "CS")

#------------------------------------------------------------------------------
# 2.1 Resumen por departamento y año


df_sum <- df %>%
  group_by(dpto, year) %>%
  summarise(
    superficie_ha = sum(SupercieHa, na.rm = TRUE),
    cantidad_pivotes = n(),
    .groups = "drop"
  )

df_sum$year <- as.integer(df_sum$year)

#------------------------------------------------------------------------------
# 2.2 Evolución anual por departamento


ggplot(df_sum,
       aes(x = factor(year),
           y = superficie_ha,
           group = dpto)) +
  geom_line(color = "steelblue", linewidth = 0.9) +
  geom_point(color = "steelblue", size = 1.5) +
  facet_wrap(~ dpto, scales = "free_y") +
  theme_classic() +
  labs(x = "Año",
       y = "Superficie bajo riego (ha)",
       title = "Evolución anual de superficie de riego por departamento (Chaco Seco)")

###############################################################################
# 3. PÉRDIDA DE COBERTURA ARBÓREA POR INCENDIOS


df <- read.csv("D:/Josefina/Proyectos/Bosques/data/dataset.csv",
               fileEncoding = "latin1")

df_incendios <- df %>%
  filter(tipo == "perdida_cobertura_arborea_incendios")

ggplot(df_incendios,
       aes(x = year_2,
           y = valor,
           group = sitio)) +
  geom_line(color = "steelblue", linewidth = 0.9) +
  geom_point(color = "steelblue", size = 1.5) +
  facet_wrap(~ sitio, scales = "free_y") +
  scale_x_continuous(
    breaks = seq(min(df_incendios$year_2),
                 max(df_incendios$year_2),
                 by = 5)
  ) +
  theme_classic() +
  labs(x = "Año",
       y = "Superficie afectada (ha)",
       title = "Pérdida de cobertura arbórea por incendios")

###############################################################################
# 4. EVOLUCIÓN DE COBERTURAS DEL SUELO

# Dataset con categorías agregadas:
# Bosques, Agricultura, Urbano

df_cob <- read.csv("D:/Josefina/Proyectos/Bosques/data/cobertura.csv",
                   fileEncoding = "latin1")

df_cob <- df_cob %>%
  filter(year >= 2000)

# Transformación a formato largo (long format)
df_long <- df_cob %>%
  pivot_longer(
    cols = c(Bosques, Agricultura, Urbano),
    names_to = "categoria",
    values_to = "superficie_ha"
  )

#------------------------------------------------------------------------------
# 4.1 Evolución temporal por sitio


ggplot(df_long,
       aes(x = year,
           y = superficie_ha,
           color = categoria)) +
  geom_line(linewidth = 0.9) +
  geom_point(size = 1.6) +
  facet_wrap(~ sitio, scales = "free_y") +
  scale_x_continuous(
    breaks = seq(min(df_long$year),
                 max(df_long$year),
                 by = 3)
  ) +
  scale_y_continuous(expand = c(0,0)) +
  theme_classic() +
  theme(
    legend.title = element_blank(),
    axis.text.x = element_text(angle = 45, hjust = 1)
  ) +
  labs(x = "Año",
       y = "Superficie (ha)",
       title = "Evolución temporal de coberturas del suelo por sitio")

###############################################################################
# 5. PÉRDIDA FORESTAL POR CATEGORÍA DE ORDENAMIENTO (I, II, III)


df_forestal <- df %>%
  filter(resolucion_espacial == "dpto") %>%
  filter(tipo %in% c(
    "perdida_forestales_otras_cat_I ",
    "perdida_forestales_otras_cat_II ",
    "perdida_forestales_otras_cat_III"
  ))

df_sum <- df_forestal %>%
  group_by(sitio, year_2, tipo) %>%
  summarise(
    perdida_ha = sum(valor, na.rm = TRUE),
    .groups = "drop"
  )

ggplot(df_sum,
       aes(x = year_2,
           y = perdida_ha,
           color = tipo)) +
  geom_line(linewidth = 1) +
  geom_point(size = 1.6) +
  facet_wrap(~ sitio, scales = "free_y") +
  scale_color_manual(
    values = c(
      "perdida_forestales_otras_cat_I "  = "red",
      "perdida_forestales_otras_cat_II " = "goldenrod",
      "perdida_forestales_otras_cat_III" = "forestgreen"
    ),
    labels = c("Categoría I", "Categoría II", "Categoría III")
  ) +
  theme_classic() +
  theme(
    legend.title = element_blank(),
    axis.text.x = element_text(angle = 45, hjust = 1)
  ) +
  labs(x = "Año",
       y = "Pérdida forestal (ha)",
       title = "Evolución anual de pérdida forestal por categoría")