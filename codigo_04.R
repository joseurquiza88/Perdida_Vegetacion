
# Objetivo: analisis del cambio de bosques en campos agricola
#Hipótesis: Intensificación del cambio de uso de suelo: 
  #comparación con periodos anteriores. Agronegocio: principal uso del suelo cambiado.

setwd("D:/Josefina/Proyectos/Bosques/data/cobertura_CS")

library(raster)
library(terra)
library(ggplot2)
library(dplyr)


r2000 <- rast("cobertura_2003_CS.tif")
r2001 <- rast("cobertura_2004_CS.tif")
for (i in 1:1){
bosque_ids <- c(3, 4, 6,66,77,63)

#agro_ids <- c(19,36, 15, 9)
# 
#bosque_ids <- c(3, 4, 6, 66, 77, 63)
#urbano_ids <- c(24)
#no_veg_ids <- c(25)
#agua_ids <- c(33, 34)
#herbaceas_ids <- c(12, 11,73)
#Mosaico_usos <- c(21)
# agricultura <- c(18)
no_obs <- c(27)

#compareGeom(r2000, r2001)

#Idea Si pixel en 2000 ∈ bosque_ids
#Y pixel en 2001 ∈ agro_ids
# → entonces es cambio
#Pixel es de 30m
# cambio <- (r2000 %in% bosque_ids) & (r2001 %in% agro_ids)
cambio <- (r2000 %in% bosque_ids) & (r2001 %in% no_obs)
cambio_bin <- ifel(cambio, 1, NA)
#plot(cambio_bin)
# writeRaster(cambio_bin, 
#  "./cambio/Bosque_a_Agro_2004_2005.tif",
#             overwrite=TRUE)

area_m2 <- global(cambio_bin, "sum", na.rm=TRUE) * 900
#area_m2
#1 hectárea = 10,000 m²
area_cambio <- area_m2 / 10000 
print(area_cambio)
}


n_pixeles <- ncell(r2000)
area_total_m2 <- n_pixeles * 900
area_total_ha <- area_total_m2 / 10000
area_total_km2 <- area_total_m2 / 1e6

(area_cambio/area_total_ha)*100

df <- data.frame(periodo= c(2000,2001,2002,2003,2004,2005,2006,2007,2008,200),
                 ha_diferencia= c(54925,66171.24,75904.65,73800.45,66103.65,
                                  53456.58,41613.93))

                 
                 

bosque_ids <- c(3, 4, 6,66,77,63)

r2024 <- rast("cobertura_2003_CS.tif")
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


##############
#Sino se ocnvirtio en agricola en que se convirtio??

# ===============================
# ANALISIS DE TRANSICIONES BOSQUE → DESTINO
# ===============================

library(terra)
library(dplyr)
library(tidyr)

# -------------------------------
# 1. Definir carpeta
# -------------------------------
setwd("D:/Josefina/Proyectos/Bosques/data/cobertura_CS")


# -------------------------------
# IDs
# -------------------------------

bosque_ids <- c(3, 4, 6, 66, 77, 63)

agro_ids <- c(19, 36, 15, 9)
urbano_ids <- c(24)
no_veg_ids <- c(25)
agua_ids <- c(33, 34)
veg_no_forestal_ids <- c(12, 11, 73)

# -------------------------------
# Archivos
# -------------------------------

archivos <- list.files(pattern = "cobertura_.*_CS.tif$", full.names = TRUE)
archivos <- archivos[order(archivos)]
anios <- gsub("cobertura_|_CS.tif","", basename(archivos))

resultados <- list()

# -------------------------------
# Loop optimizado
# -------------------------------
i<-1
for(i in 1:(length(archivos)-1)){
  
  cat("Procesando:", anios[i], "-", anios[i+1], "\n")
  
  r1 <- rast(archivos[i])
  r2 <- rast(archivos[i+1])
  
  # 1️⃣ Crear máscara de bosque
  bosque_mask <- r1 %in% bosque_ids
  
  # 2️⃣ Extraer solo lo que era bosque
  r2_bosque <- mask(r2, bosque_mask)
  
  # 3️⃣ Clasificar destino
  # Crear matriz de reclasificación correcta
  rcl <- rbind(
    cbind(agro_ids, 1),
    cbind(urbano_ids, 2),
    cbind(no_veg_ids, 3),
    cbind(agua_ids, 4),
    cbind(veg_no_forestal_ids, 5)
  )
  
  destino <- classify(r2_bosque,
                      rcl = rcl,
                      others = NA)
  
  # Contar pixeles
  tabla <- freq(destino)
  
  if(!is.null(tabla)){
    
    df_temp <- as.data.frame(tabla)
    
    # pixel 30x30 = 900 m2 = 0.09 ha
    df_temp$ha <- df_temp$count * 0.09
    
    df_temp$Periodo <- paste(anios[i], anios[i+1], sep="-")
    
    resultados[[i]] <- df_temp
  }
}

# -------------------------------
# Unir resultados
# -------------------------------

df_transiciones <- bind_rows(resultados)

df_transiciones$Destino <- recode(df_transiciones$value,
                                  `1` = "Agricultura",
                                  `2` = "Urbano",
                                  `3` = "No vegetado",
                                  `4` = "Agua",
                                  `5` = "Vegetación no forestal")

df_transiciones <- df_transiciones %>%
  select(Periodo, Destino, ha)

head

df_transiciones_agricola <- df_transiciones[df_transiciones$Destino == "Agricultura", ]





# Metodo viejo
cambio1 <- ifel((r2000 %in% bosque_ids) & (r2001 %in% agro_ids), 1, NA)
area1 <- global(cambio1, "sum", na.rm=TRUE) * 0.09

# Metodo nuevo
cambio2 <- (r2000 %in% bosque_ids) & (r2001 %in% agro_ids)
area2 <- global(cambio2, "sum", na.rm=TRUE) * 0.09

area1
area2



r2000 <- rast("cobertura_2003_CS.tif")
r2001 <- rast("cobertura_2004_CS.tif")

bosque_ids <- c(3, 4, 6, 66, 77, 63)

# máscara de bosque 2015
bosque_2015 <- r2000 %in% bosque_ids

# extraer valores 2016 SOLO donde en 2015 era bosque
destinos <- mask(r2001, bosque_2015)

# tabla de frecuencias
tabla_destinos <- freq(destinos)

tabla_destinos


#####################################################
data <- read.csv("D:/Josefina/Proyectos/Bosques/data/transicion_bosques_a_otras.csv")

library(dplyr)
library(tidyr)
library(ggplot2)
names(data)

df_long <- data %>%
  pivot_longer(
    cols = c(bosques_a_agricultura, bosques_a_urbano, bosques_a_bosques, bosques_a_herbaceas, bosques_a_agua,                 
             bosques_a_otrasAreasNoVegetadas),
    names_to = "Destino",
    values_to = "Hectareas"
  )
df_long <- data %>%
  pivot_longer(
    cols = c(bosques_a_agricultura, bosques_a_urbano,  bosques_a_herbaceas, bosques_a_agua,                 
             bosques_a_otrasAreasNoVegetadas),
    names_to = "Destino",
    values_to = "Hectareas"
  )
df_long <- df_long %>%
  group_by(Destino) %>%
  mutate(total_destino = sum(Hectareas)) %>%
  ungroup() %>%
  mutate(Destino = fct_reorder(Destino, total_destino, .desc = FALSE))



ggplot(df_long, aes(x = Periodo, y = Hectareas, fill = Destino)) +
  geom_bar(stat = "identity") +
  scale_y_continuous(labels = scales::comma) +
  # scale_fill_manual(values = c(
  #   "bosques_a_bosques" = "#1B5E20",      # verde oscuro
  #   "bosques_a_agricultura" = "#D32F2F",  # rojo fuerte
  #   "bosques_a_urbano" = "#424242",       # gris urbano
  #   "bosques_a_herbaceas" = "#FBC02D",    # amarillo fuerte
  #   "bosques_a_agua" = "#1976D2",         # azul agua
  #   "bosques_a_otrasAreasNoVegetadas" = "#8D6E63" # marrón
  # )) +
  
  # 
  scale_fill_manual(values = c(
    "bosques_a_bosques" = "#A5D6A7",  # verde claro
    "bosques_a_agricultura" = "#E974ED",
    "bosques_a_urbano" = "#212121",
    "bosques_a_herbaceas" = "#d6bc74",
    "bosques_a_agua" = "#2532e4",
    "bosques_a_otrasAreasNoVegetadas" = "#db4d4f"
  ))+
  
  
  
  labs(
    title = "Transiciones de uso del suelo desde bosque nativo (2000-2024)",
    y = "Hectáreas",
    x = "Periodo"
  ) +
  theme_classic() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

######################################################
# tasa relativa (intensidad anual),

library(dplyr)
library(ggplot2)
library(scales)

df_tasa <- data %>%
  mutate(
    tasa_agricultura = (bosques_a_agricultura / bosque_inicial) * 100
  )

ggplot(df_tasa, aes(x = Periodo, y = tasa_agricultura, group = 1)) +
  geom_line(size = 1) +
  geom_point(size = 2) +
  scale_y_continuous(labels = function(x) paste0(round(x,2), "%")) +
  labs(
    title = "Tasa relativa anual de conversión de bosque a agricultura (2000–2024)",
    y = "Tasa (%) sobre bosque existente",
    x = "Periodo"
  ) +
  theme_classic() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

#####################################################
datos <- read.csv("D:/Josefina/Proyectos/Bosques/data/transicion_bosques_a_otras.csv")

library(dplyr)
library(tidyr)
library(ggplot2)

# Calcular el total de bosque que salió hacia otras categorías (flujo bruto)
datos <- datos %>%
  mutate(
    bosque_transformado = bosques_a_agricultura +
      bosques_a_urbano +
      bosques_a_herbaceas +
      bosques_a_agua +
      bosques_a_otrasAreasNoVegetadas
  )

# Totales acumulados 2000–2024
totales <- datos %>%
  summarise(
    total_agricultura = sum(bosques_a_agricultura),
    total_urbano = sum(bosques_a_urbano),
    total_herbaceas = sum(bosques_a_herbaceas),
    total_agua = sum(bosques_a_agua),
    total_otras = sum(bosques_a_otrasAreasNoVegetadas),
    total_transformado = sum(bosque_transformado)
  )

#Totales acumulados 2000–2024
totales <- datos %>%
  summarise(
    total_agricultura = sum(bosques_a_agricultura),
    total_urbano = sum(bosques_a_urbano),
    total_herbaceas = sum(bosques_a_herbaceas),
    total_agua = sum(bosques_a_agua),
    total_otras = sum(bosques_a_otrasAreasNoVegetadas),
    total_transformado = sum(bosque_transformado)
  )

#Calcular porcentajes reales de destino
estructura <- totales %>%
  mutate(
    pct_agricultura = total_agricultura / total_transformado * 100,
    pct_urbano = total_urbano / total_transformado * 100,
    pct_herbaceas = total_herbaceas / total_transformado * 100,
    pct_agua = total_agua / total_transformado * 100,
    pct_otras = total_otras / total_transformado * 100
  )



tabla_final <- estructura %>%
  select(starts_with("total_"), starts_with("pct_")) %>%
  round(2)

tabla_final


estructura_long <- datos %>%
  summarise(
    Agricultura = sum(bosques_a_agricultura),
    Urbano = sum(bosques_a_urbano),
    Herbaceas = sum(bosques_a_herbaceas),
    Agua = sum(bosques_a_agua),
    Otras = sum(bosques_a_otrasAreasNoVegetadas)
  ) %>%
  tidyr::pivot_longer(everything(),
                      names_to = "Destino",
                      values_to = "Hectareas") %>%
  mutate(
    Porcentaje = Hectareas / sum(Hectareas) * 100
  )
estructura_long


library(ggplot2)
library(dplyr)
library(forcats)

estructura_long %>%
  mutate(Destino = fct_reorder(Destino, Porcentaje, .desc = TRUE)) %>%
  ggplot(aes(x = Destino, y = Porcentaje, fill = Destino)) +
  geom_col() +
  geom_text(aes(label = paste0(round(Porcentaje, 1), "%"),
                y = Porcentaje+2),
            color = "black",
            size = 3.5)+
            #fontface = "bold") +
  scale_fill_manual(values = c(
    "Agricultura" = "#E974ED",   # rojo fuerte
    "Herbaceas"   = "#d6bc74",   # amarillo
    "Otras"       = "#db4d4f",   # marrón
    "Urbano"      = "black",   # negro
    "Agua"        = "#2532e4"    # azul
  )) +
  labs(
    title = "Estructura del cambio para el periodo 2000-2024",
    y = "Porcentaje de cambio (%)",
    x = "Categoria"
  ) +
  theme_classic() +
  theme(legend.position = "none")
