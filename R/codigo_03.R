###############################################################################
# PROYECTO: Dinámica de pérdida forestal en categorías de ordenamiento
# Área de estudio: Provincia de Córdoba
#
# OBJETIVO:
# Analizar la evolución temporal de la pérdida de superficie forestal
# en distintas categorías de ordenamiento territorial:
#   - Categoría I
#   - Categoría II
#   - Categoría III
#   - Sin categoría
#
# El análisis se realiza a dos escalas espaciales:
#   1. Provincial
#   2. Departamental
###############################################################################

#------------------------------------------------------------------------------
# 1. CARGA DE DATOS


# Se define el directorio de trabajo donde se encuentra la base
setwd("D:/Josefina/Proyectos/Bosques/data")

# Se carga el dataset con pérdidas forestales en otras categorías
# fileEncoding = "Latin1" se utiliza para evitar problemas con tildes
data <- read.csv("perdida_forestalesOtras_categorias.csv", fileEncoding = "Latin1")

#------------------------------------------------------------------------------
# 2. ANÁLISIS A ESCALA PROVINCIAL


# Se filtran únicamente los registros con resolución espacial provincial
data_provincial <- data[data$resolucion_espacial == "provincial", ]

# Exploración básica del dataset
unique(data_provincial$sitio)
names(data_provincial)

library(dplyr)
library(stringr)

#------------------------------------------------------------------------------
# 2.1 Transformación a formato largo (long format)
#
# Se pasan las columnas de categorías a una sola columna llamada "categoria",
# lo que permite graficar y analizar dinámicas comparativas entre categorías.


data_long <- data_provincial %>%
  pivot_longer(
    cols = c(
      perdida_forestales_otras_cat_I,
      perdida_forestales_otras_cat_II,
      perdida_forestales_otras_cat_III,
      perdida_forestales_otras_cat_Scat
    ),
    names_to = "categoria",
    values_to = "hectareas"
  ) %>%
  mutate(
    categoria = recode(
      categoria,
      "perdida_forestales_otras_cat_I"   = "Cat I",
      "perdida_forestales_otras_cat_II"  = "Cat II",
      "perdida_forestales_otras_cat_III" = "Cat III",
      "perdida_forestales_otras_cat_Scat"= "Sin categoría"
    )
  )

#------------------------------------------------------------------------------
# 2.2 Visualización: evolución anual de pérdida forestal (escala provincial)
#
# Se grafica:
# - Eje X: año
# - Eje Y: hectáreas perdidas
# - Color: categoría de ordenamiento
# - Facet: por sitio
#
# Se fija límite superior en 50.000 ha para homogeneizar la escala.

ggplot(data_long,
       aes(x = year_2,
           y = hectareas,
           color = categoria,
           group = categoria)) +
  
  geom_line(linewidth = 0.9) +
  geom_point(size = 1.6) +
  
  facet_wrap(~ sitio) +
  
  scale_x_continuous(
    breaks = sort(unique(data_long$year_2))
  ) +
  
  scale_y_continuous(
    limits = c(0, 50000),
    breaks = seq(0, 50000, by = 20000),
    expand = c(0, 0)
  )+
  
  scale_color_manual(
    values = c(
      "Cat I" = "red",
      "Cat II" = "yellow3",
      "Cat III" = "green4",
      "Sin categoría" = "grey60"
    )
  ) +
  
  theme_classic() +
  theme(
    strip.text = element_text(size = 9),
    axis.text.x = element_text(angle = 45, hjust = 1),
    legend.title = element_blank()
  ) +
  
  labs(
    x = "Año",
    y = "Pérdida forestal (ha)",
    title = "Evolución anual de pérdida forestal por provincia y categoría"
  )

###############################################################################
# 3. ANÁLISIS A ESCALA DEPARTAMENTAL


# Se filtran los registros con resolución espacial departamental
data_dpto <- data[data$resolucion_espacial == "dpto", ]

# Exploración básica
unique(data_dpto$sitio)
names(data_dpto)

library(ggplot2)

#------------------------------------------------------------------------------
# 3.1 Transformación a formato largo (idéntica lógica que escala provincial)


data_long_dpto <- data_dpto %>%
  pivot_longer(
    cols = c(
      perdida_forestales_otras_cat_I,
      perdida_forestales_otras_cat_II,
      perdida_forestales_otras_cat_III,
      perdida_forestales_otras_cat_Scat
    ),
    names_to = "categoria",
    values_to = "hectareas"
  ) %>%
  mutate(
    categoria = recode(
      categoria,
      "perdida_forestales_otras_cat_I"   = "Cat I",
      "perdida_forestales_otras_cat_II"  = "Cat II",
      "perdida_forestales_otras_cat_III" = "Cat III",
      "perdida_forestales_otras_cat_Scat"= "Sin categoría"
    )
  )

#------------------------------------------------------------------------------
# 3.2 Visualización: evolución anual por departamento
#
# En este caso se ajusta la escala Y a 10.000 ha para mejorar
# la lectura en escala departamental.


ggplot(data_long_dpto,
       aes(x = year,
           y = hectareas,
           color = categoria,
           group = categoria)) +
  
  geom_line(linewidth = 0.9) +
  geom_point(size = 1.6) +
  
  facet_wrap(~ sitio) +
  
  scale_x_continuous(
    breaks = sort(unique(data_long_dpto$year))
  ) +
  
  scale_y_continuous(
    breaks = seq(0, 10000, by = 3000),
    expand = c(0, 0)
  ) +
  coord_cartesian(ylim = c(0, 10000))+
  
  scale_color_manual(
    values = c(
      "Cat I" = "red",
      "Cat II" = "yellow3",
      "Cat III" = "green4",
      "Sin categoría" = "grey60"
    )
  ) +
  
  theme_classic() +
  theme(
    strip.text = element_text(size = 9),
    axis.text.x = element_text(angle = 45, hjust = 1),
    legend.title = element_blank()
  ) +
  
  labs(
    x = "Año",
    y = "Pérdida forestal (ha)",
    title = "Evolución anual de pérdida forestal por departamento y categoría"
  )

###############################################################################
# 4. LIMPIEZA DE AÑOS CON RANGOS (EJ: 2000-2001)

# En algunos registros, el año aparece como rango (ej: "2000-2001").
# Este bloque:
# 1. Separa año inicial y final.
# 2. Genera una secuencia intermedia.
# 3. Expande las filas para tener un año por fila.


library(dplyr)
library(tidyr)
library(stringr)

data_provincial <- data_provincial %>%
  mutate(
    start = as.numeric(str_sub(year, 1, 4)),
    end = ifelse(str_detect(year, "-"),
                 as.numeric(str_sub(year, 6, 9)),
                 start)
  ) %>%
  rowwise() %>%
  mutate(year_clean = list(seq(start, end))) %>%
  unnest(year_clean) %>%
  ungroup() %>%
  select(-start, -end)


# Se repite transformación a formato largo con año limpio


data_long <- data_provincial %>%
  pivot_longer(
    cols = c(
      perdida_forestales_otras_cat_I,
      perdida_forestales_otras_cat_II,
      perdida_forestales_otras_cat_III,
      perdida_forestales_otras_cat_Scat
    ),
    names_to = "categoria",
    values_to = "hectareas"
  ) %>%
  mutate(
    categoria = recode(
      categoria,
      "perdida_forestales_otras_cat_I"   = "Cat I",
      "perdida_forestales_otras_cat_II"  = "Cat II",
      "perdida_forestales_otras_cat_III" = "Cat III",
      "perdida_forestales_otras_cat_Scat"= "Sin categoría"
    )
  )

#------------------------------------------------------------------------------
# Visualización final con año expandido
#
# oob_squish permite mantener valores mayores al límite,
# pero "aplastados" en el máximo del eje.


ggplot(
  data_long,
  aes(
    x = year_clean,
    y = hectareas,
    color = categoria,
    group = categoria
  )
) +
  
  geom_line(linewidth = 0.9) +
  geom_point(size = 1.6) +
  
  facet_wrap(~ sitio) +
  
  scale_x_continuous(
    breaks = seq(min(data_long$year_clean),
                 max(data_long$year_clean),
                 by = 1)
  ) +
  
  scale_y_continuous(
    limits = c(0, 50000),
    breaks = seq(0, 50000, by = 10000),
    expand = c(0, 0),
    oob = scales::oob_squish
  ) +
  
  scale_color_manual(
    values = c(
      "Cat I" = "red",
      "Cat II" = "yellow3",
      "Cat III" = "green4",
      "Sin categoría" = "grey60"
    )
  ) +
  
  theme_classic() +
  theme(
    strip.text = element_text(size = 9),
    axis.text.x = element_text(angle = 45, hjust = 1),
    legend.title = element_blank()
  ) +
  
  labs(
    x = "Año",
    y = "Pérdida forestal (ha)",
    title = "Evolución anual de pérdida forestal por provincia y categoría"
  )