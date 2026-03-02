
# Objetivo: analisis del cambio de bosques en campos agricola
#Hipótesis: Intensificación del cambio de uso de suelo: 
  #comparación con periodos anteriores. Agronegocio: principal uso del suelo cambiado.

("D:/Josefina/Proyectos/Bosques/data/cobertura_CS")

library(raster)
library(terra)
library(ggplot2)
library(dplyr)
bosque_ids <- c(3, 4, 6,66,77,63)

agro_ids <- c(19,36, 15, 9)


r2000 <- rast("cobertura_2004_CS.tif")
r2001 <- rast("cobertura_2005_CS.tif")

compareGeom(r2000, r2001)

#Idea Si pixel en 2000 ∈ bosque_ids
#Y pixel en 2001 ∈ agro_ids
# → entonces es cambio
#Pixel es de 30m
cambio <- (r2000 %in% bosque_ids) & (r2001 %in% agro_ids)
cambio_bin <- ifel(cambio, 1, NA)
plot(cambio_bin)
writeRaster(cambio_bin, 
 "./cambio/Bosque_a_Agro_2004_2005.tif",
            overwrite=TRUE)

area_m2 <- global(cambio_bin, "sum", na.rm=TRUE) * 900
#area_m2
#1 hectárea = 10,000 m²
area_cambio <- area_m2 / 10000 
area_cambio

n_pixeles <- ncell(r2000)
area_total_m2 <- n_pixeles * 900
area_total_ha <- area_total_m2 / 10000
area_total_km2 <- area_total_m2 / 1e6

(area_cambio/area_total_ha)*100

df <- data.frame(periodo= c(2000,2001,2002,2003,2004,2005,2006,2007,2008,200),
                 ha_diferencia= c(54925,66171.24,75904.65,73800.45,66103.65,
                                  53456.58,41613.93))

                 
                 

bosque_ids <- c(3, 4, 6,66,77,63)

r2024 <- rast("cobertura_2024_CS.tif")
bosque_2024 <- ifel(r2024 %in% bosque_ids, 1, NA)
# contar píxeles con valor 1
n_pixeles_bosque <- global(bosque_2024, "sum", na.rm=TRUE)

# convertir a hectáreas
area_bosque_ha <- n_pixeles_bosque * 0.09

# en km²
#area_bosque_km2 <- area_bosque_ha / 100

area_bosque_ha
area_bosque_km2



###################
# Crear dataframe de deforestación

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

# Ver dataframe
deforestacion_plot <- deforestacion %>%
  mutate(tasa_porcentual_anual = 
           (ha_diferencia_bosque_a_agro /
              cobertura_bosque_inicial_ha) * 100) %>%
  filter(Periodo != "2024")

library(ggplot2)
library(dplyr)

deforestacion_plot <- deforestacion %>%
  mutate(tasa_porcentual_anual = 
           (ha_diferencia_bosque_a_agro /
              cobertura_bosque_inicial_ha) * 100) %>%
  filter(Periodo != "2024")

ggplot(deforestacion_plot, 
       aes(x = Periodo, y = tasa_porcentual_anual, group = 1)) +
  geom_line() +
  geom_point() +
  labs(title = "Tasa anual de conversión de bosque a suelo agrícola",
       x = "Periodo",
       y = "Tasa anual de conversión (%)") +
  theme_classic()+
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1))



library(dplyr)

deforestacion_plot <- deforestacion %>%
  filter(Periodo != "2024") %>%
  mutate(
    tasa_porcentual_anual = 
      (ha_diferencia_bosque_a_agro /
         cobertura_bosque_inicial_ha) * 100,
    
    perdida_acumulada_ha = cumsum(ha_diferencia_bosque_a_agro)
  )

library(ggplot2)

ggplot(deforestacion_plot,
       aes(x = Periodo, y = perdida_acumulada_ha, group = 1)) +
  geom_line() +
  geom_point() +
  labs(title = "Conversión acumulada de bosque a suelo agrícola",
       x = "Periodo",
       y = "Pérdida acumulada (ha)") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1))


deforestacion_subt <- deforestacion[deforestacion$Periodo != 2024,]

ggplot(deforestacion_subt,
       aes(x = Periodo, y = ha_diferencia_bosque_a_agro, group = 1)) +
  geom_line() +
  geom_point() +
  labs(title = "Expansión agrícola sobre bosques nativos en el Chaco Seco",
       x = "Periodo",
       y = "Bosques convertidos en campos agricolas (ha/año)") +
  theme_classic() +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1))


############################

deforestacion_subt$tasa_relativa_anual <- round((deforestacion_subt$ha_diferencia_bosque_a_agro/
  deforestacion_subt$cobertura_bosque_inicial_ha)*100,2)

ggplot(deforestacion_subt,
       aes(x = Periodo, y = tasa_relativa_anual, group = 1)) +
  geom_line() +
  geom_point() +
  labs(title = "Tasa anual relativa de conversión de bosque a agricultura (2000-2024)",
       x = "Periodo",
       y = "Tasa de conversión (% del bosque existente)") +
  theme_classic() +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1))

min(deforestacion_subt$tasa_relativa_anual)
max(deforestacion_subt$tasa_relativa_anual)


View(deforestacion_subt)
