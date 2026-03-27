# =========================================================
# Analisis de cambio de bosques a usos agrícolas
# =========================================================
# Objetivo: analizar la conversión de bosques a agricultura
# en el Chaco Seco a partir de mapas de cobertura.
#
# Hipótesis:
# Existe una intensificación del cambio de uso del suelo
# respecto a periodos anteriores, asociada principalmente
# a la expansión del agronegocio.
# =========================================================

setwd("D:/Josefina/Proyectos/Bosques/data/cobertura_CS")

# Librerías utilizadas
library(raster)
library(terra)
library(ggplot2)
library(dplyr)

# ---------------------------------------------------------
# Lectura de rasters de cobertura
# ---------------------------------------------------------

r2000 <- rast("cobertura_2003_CS.tif")
r2001 <- rast("cobertura_2004_CS.tif")

for (i in 1:1){
  
  # ---------------------------------------------------------
  # Definición de clases de cobertura
  # ---------------------------------------------------------
  # IDs correspondientes a bosque nativo
  
  bosque_ids <- c(3, 4, 6,66,77,63)
  
  # Otros IDs posibles (no utilizados en este bloque)
  # agro_ids <- c(19,36, 15, 9)
  # urbano_ids <- c(24)
  # no_veg_ids <- c(25)
  # agua_ids <- c(33, 34)
  # herbaceas_ids <- c(12, 11,73)
  # Mosaico_usos <- c(21)
  # agricultura <- c(18)
  
  no_obs <- c(27)
  
  # compareGeom(r2000, r2001)
  
  # ---------------------------------------------------------
  # Identificación de cambio bosque → agricultura
  # ---------------------------------------------------------
  # Si un píxel es bosque en t0 y agricultura en t1
  # se considera cambio de uso del suelo.
  
  # Pixel = 30 m → 900 m2
  
  cambio <- (r2000 %in% bosque_ids) & (r2001 %in% agro_ids)
  
  # raster binario de cambio
  cambio_bin <- ifel(cambio, 1, NA)
  
  # plot(cambio_bin)
  
  # writeRaster(cambio_bin, 
  #  "./cambio/Bosque_a_Agro_2004_2005.tif",
  #             overwrite=TRUE)
  
  # ---------------------------------------------------------
  # Cálculo de superficie convertida
  # ---------------------------------------------------------
  
  area_m2 <- global(cambio_bin, "sum", na.rm=TRUE) * 900
  
  # conversión a hectáreas
  area_cambio <- area_m2 / 10000 
  
  print(area_cambio)
}

# ---------------------------------------------------------
# Superficie total del área de estudio
# ---------------------------------------------------------

n_pixeles <- ncell(r2000)
area_total_m2 <- n_pixeles * 900

area_total_ha <- area_total_m2 / 10000
area_total_km2 <- area_total_m2 / 1e6

# porcentaje del área total convertida
(area_cambio/area_total_ha)*100

# ---------------------------------------------------------
# Datos de cambio anual bosque → agricultura
# ---------------------------------------------------------

df <- data.frame(
  periodo= c(2000,2001,2002,2003,2004,2005,2006,2007,2008,200),
  ha_diferencia= c(
    54925,66171.24,75904.65,73800.45,66103.65,
    53456.58,41613.93
  )
)

# ---------------------------------------------------------
# Estimación de cobertura de bosque en un año
# ---------------------------------------------------------

bosque_ids <- c(3, 4, 6,66,77,63)

r2024 <- rast("cobertura_2003_CS.tif")

# máscara de bosque
bosque_2024 <- ifel(r2024 %in% bosque_ids, 1, NA)

# contar píxeles de bosque
n_pixeles_bosque <- global(bosque_2024, "sum", na.rm=TRUE)

# conversión a hectáreas
area_bosque_ha <- n_pixeles_bosque * 0.09

# en km2
# area_bosque_km2 <- area_bosque_ha / 100

area_bosque_ha
area_bosque_km2


###################
# Construcción del dataframe de deforestación
###################

deforestacion <- data.frame(
  Periodo = c(
    "2000-2001","2001-2002","2002-2003","2003-2004","2004-2005",
    "2005-2006","2006-2007","2007-2008","2008-2009","2009-2010",
    "2010-2011","2011-2012","2012-2013","2013-2014","2014-2015",
    "2015-2016","2016-2017","2017-2018","2018-2019","2019-2020",
    "2020-2021","2021-2022","2022-2023","2023-2024","2024"
  ),
  
  ha_diferencia_bosque_a_agro = c(
    54925, 66171.24, 75904.65, 73800.45, 66103.65,
    53456.58, 41613.93, 45796.5, 31958, 51932.52,
    45273.6, 43959.69, 45032.4, 56015.28, 53313.84,
    35083.53, 33146.55, 29752.92, 33610.41, 26846.01,
    30604.41, 34756.83, 36789.12, 60153.93, 0
  ),
  
  cobertura_bosque_inicial_ha = c(
    3783505, 3775279, 3724620, 3690558, 3645118,
    3612188, 3606359, 3597749, 3562450, 3561906,
    3565171, 3556384, 3554908, 3550256, 3538632,
    3528318, 3512542, 3498028, 3477117, 3471616,
    3504935, 3527330, 3512248, 3467283, 3364088
  )
)

# ---------------------------------------------------------
# Cálculo de tasa anual de deforestación
# ---------------------------------------------------------

deforestacion_plot <- deforestacion %>%
  mutate(
    tasa_porcentual_anual =
      (ha_diferencia_bosque_a_agro /
         cobertura_bosque_inicial_ha) * 100
  ) %>%
  filter(Periodo != "2024")

# ---------------------------------------------------------
# Gráfico de tasa anual de conversión
# ---------------------------------------------------------

ggplot(deforestacion_plot, 
       aes(x = Periodo, y = tasa_porcentual_anual, group = 1)) +
  geom_line() +
  geom_point() +
  labs(
    title = "Tasa anual de conversión de bosque a suelo agrícola",
    x = "Periodo",
    y = "Tasa anual de conversión (%)"
  ) +
  theme_classic()+
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1))
