#Objetivo analisis de la perdida de vegetacion del Chaco seco
# en la provincia de cordoba

# ============================================================
# 1) Cargar datos y paquetes
# ============================================================

setwd("D:/Josefina/Proyectos/bosques")

df <- read.csv("perdida_vegetacion_CBA-ChacoS_V02.csv",
               stringsAsFactors = TRUE)
names (df)
library(dplyr)
library(tidyr)
library(ggplot2)
library(scales)

# ============================================================
# 2) Crear paleta de 18 colores con nombres exactos
# ============================================================

colores_ggplot <- hue_pal()(18)

departamentos <- c(
  "Calamuchita",
  "Capital",
  "Colon",
  "Cruz del Eje",
  "Ischilin",
  "Minas",
  "Pocho",
  "Punilla",
  "Rio Cuarto",
  "Rio Primero",
  "Rio Seco",
  "San Alberto",
  "San Javier",
  "Santa Maria",
  "Sobremonte",
  "Tercero Arriba",
  "Totoral",
  "Tulumba"
)

paleta_departamentos <- setNames(colores_ggplot, departamentos)

# ============================================================
# 3) Filtrar años 2000–2024
# ============================================================

df_filtrado <- df %>% 
  filter(year >= 2000 & year <= 2024)

# ============================================================
# 4) Transformar a formato largo + renombrar regiones
# ============================================================

df_long <- df_filtrado %>%
  pivot_longer(
    cols = c(
      ischilin_prim, tulumba_prim, calamuchita_prim, rioSeco_prim,
      totoral_prim, sobremonte_prim, punilla_prim, sanAlberto_prim,
      sanJavier_prim, pocho_prim, minas_prim, cruzEje_prim, capital_prim,
      colon_prim, rioCuarto_prim, rioPrimero_prim, SantaMaria_prim,
      terceroArriba_prim
    ),
    names_to = "region",
    values_to = "valor"
  ) %>%
  mutate(region = recode(region,
                         ischilin_prim      = "Ischilin",
                         tulumba_prim       = "Tulumba",
                         calamuchita_prim   = "Calamuchita",
                         rioSeco_prim       = "Rio Seco",
                         totoral_prim       = "Totoral",
                         sobremonte_prim    = "Sobremonte",
                         punilla_prim       = "Punilla",
                         sanAlberto_prim    = "San Alberto",
                         sanJavier_prim     = "San Javier",
                         pocho_prim         = "Pocho",
                         minas_prim         = "Minas",
                         cruzEje_prim       = "Cruz del Eje",
                         capital_prim       = "Capital",
                         colon_prim         = "Colon",
                         rioCuarto_prim     = "Rio Cuarto",
                         rioPrimero_prim    = "Rio Primero",
                         SantaMaria_prim    = "Santa Maria",
                         terceroArriba_prim = "Tercero Arriba"
  )) %>%
  mutate(valor = as.numeric(valor))

# ============================================================
# 5) Seleccionar los 4 valores más altos — FUNCIONA SIEMPRE
# ============================================================

df_top4 <- df_long %>%
  slice_max(order_by = valor, n = 4, with_ties = FALSE) %>%
  mutate(
    label = paste0(region, "\n", year, " – ", valor, " ha")
  )

# ============================================================
# 6) Plot final con tus colores
# ============================================================

plot <- ggplot() +
  
  # Líneas
  geom_line(
    data = df_long,
    aes(x = year, y = valor, color = region),
    size = 0.8,
    lineend = "round"
  ) +
  
  # Puntos de los top 4
  geom_point(
    data = df_top4,
    aes(x = year, y = valor, color = region),
    size = 3,
    show.legend = FALSE
  ) +
  
  # Segmentos
  geom_segment(
    data = df_top4,
    aes(
      x = year, xend = year + 1.8,
      y = valor, yend = valor + 300
    ),
    color = "black",
    linewidth = 0.6
  ) +
  
  # Etiquetas
  geom_text(
    data = df_top4,
    aes(
      x = year + 2,
      y = valor + 400,
      label = label,
      color = region
    ),
    size = 3.2,
    hjust = 0,
    show.legend = FALSE
  ) +
  
  # Escalas
  scale_x_continuous(
    limits = c(2000, 2024),
    breaks = seq(2000, 2024, by = 2)
  ) +
  scale_y_continuous(
    limits = c(0, 12000),
    breaks = seq(0, 12000, by = 2000)
  ) +
  
  # Paleta final aplicada aquí 🔥
  scale_color_manual(values = paleta_departamentos) +
  
  labs(
    x = "Año",
    y = "Pérdida de vegetación (ha)",
    color = "Departamento"
  ) +
  
  theme_classic() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1)
  )

plot

# ============================================================
# 7) Guardar imagen
# ============================================================

ggsave(
  filename = "plot_vegetacion.png",
  plot = plot,
  width = 12,
  height = 8,
  dpi = 300
)



#############################################################
#############################################################
library(dplyr)
library(tidyr)
library(ggplot2)

# Filtrar 2000–2024
df_filtrado <- df %>% 
  filter(year >= 2000 & year <= 2024)

# Formato largo + renombrar
df_long <- df_filtrado %>%
  pivot_longer(
    cols = c(ischilin_prim, tulumba_prim, calamuchita_prim, rioSeco_prim,
             totoral_prim, sobremonte_prim, punilla_prim, sanAlberto_prim,
             sanJavier_prim, pocho_prim, minas_prim,cruzEje_prim,	capital_prim,
             colon_prim,	rioCuarto_prim,	rioPrimero_prim	,SantaMaria_prim,
             terceroArriba_prim
    ),
    names_to = "region",
    values_to = "valor"
  ) %>%
  mutate(region = recode(region,
                         ischilin_prim = "Ischilín",
                         tulumba_prim = "Tulumba",
                         calamuchita_prim = "Calamuchita",
                         rioSeco_prim = "Río Seco",
                         totoral_prim = "Totoral",
                         sobremonte_prim = "Sobremonte",
                         punilla_prim = "Punilla",
                         sanAlberto_prim = "San Alberto",
                         sanJavier_prim = "San Javier",
                         pocho_prim = "Pocho",
                         minas_prim = "Minas",
                         cruzEje_prim  = "Cruz del Eje",
                         capital_prim	= "Capital",
                         colon_prim = "Colon",
                         rioCuarto_prim	 = "Rio Cuarto",
                         rioPrimero_prim = "Rio Primero",
                         SantaMaria_prim = "Santa Maria",	
                         terceroArriba_prim = "Tercero Arriba"
                         
  ))

# =============================
# Calcular pérdida acumulada por región con leynda afuera
# =============================
df_acum <- df_long %>%
  group_by(region) %>%
  arrange(year) %>%
  mutate(valor_acum = cumsum(valor)) %>%
  ungroup()

plot_acum <- ggplot() +
  
  # Línea acumulada por región
  geom_line(
    data = df_acum,
    aes(x = year, y = valor_acum, color = region),
    size = 0.8, lineend = "round"
  ) +
  
  scale_x_continuous(
    limits = c(2000, 2024),
    breaks = seq(2000, 2024, by = 2)
  ) +
  labs(
    x = "Año",
    y = "Pérdida acumulada de vegetación (ha)",
    color = "Departamento"
  ) +
  theme_classic() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1)
  )

plot_acum


####### Con leyenda con nombres
library(ggrepel)

#############################################################
# Filtrar 2000–2024
#############################################################

df_filtrado <- df %>% 
  filter(year >= 2000 & year <= 2024)

#############################################################
# Pasar a formato largo y renombrar departamentos
#############################################################

df_long <- df_filtrado %>%
  pivot_longer(
    cols = c(
      ischilin_prim, tulumba_prim, calamuchita_prim, rioSeco_prim,
      totoral_prim, sobremonte_prim, punilla_prim, sanAlberto_prim,
      sanJavier_prim, pocho_prim, minas_prim, cruzEje_prim, capital_prim,
      colon_prim, rioCuarto_prim, rioPrimero_prim, SantaMaria_prim,
      terceroArriba_prim
    ),
    names_to = "region",
    values_to = "valor"
  ) %>%
  mutate(region = recode(region,
                         ischilin_prim = "Ischilin",
                         tulumba_prim = "Tulumba",
                         calamuchita_prim = "Calamuchita",
                         rioSeco_prim = "Rio Seco",
                         totoral_prim = "Totoral",
                         sobremonte_prim = "Sobremonte",
                         punilla_prim = "Punilla",
                         sanAlberto_prim = "San Alberto",
                         sanJavier_prim = "San Javier",
                         pocho_prim = "Pocho",
                         minas_prim = "Minas",
                         cruzEje_prim  = "Cruz del Eje",
                         capital_prim	= "Capital",
                         colon_prim = "Colon",
                         rioCuarto_prim	= "Rio Cuarto",
                         rioPrimero_prim = "Rio Primero",
                         SantaMaria_prim = "Santa Maria",	
                         terceroArriba_prim = "Tercero Arriba"
  ))

#############################################################
# Calcular acumulados
#############################################################

df_acum <- df_long %>%
  group_by(region) %>%
  arrange(year) %>%
  mutate(valor_acum = cumsum(valor)) %>%
  ungroup()

#############################################################
# Extraer última observación por región (para etiquetas)
#############################################################

labels_final <- df_acum %>%
  group_by(region) %>%
  filter(year == max(year)) %>%
  ungroup()

#############################################################
# PLOT ACUMULADO — con colores personalizados
#############################################################

plot_acum <- ggplot() +
  
  geom_line(
    data = df_acum,
    aes(x = year, y = valor_acum, color = region),
    size = 0.8, lineend = "round"
  ) +
  
  geom_text_repel(
    data = labels_final,
    aes(
      x = year + 0.1,
      y = valor_acum,
      label = region,
      color = region
    ),
    direction = "y",
    hjust = 0,
    size = 3,
    segment.color = NA,
    min.segment.length = 0,
    max.overlaps = Inf
  ) +
  
  scale_color_manual(values = paleta_departamentos) +   # ⭐ MISMA PALETA EN TODO
  scale_x_continuous(
    limits = c(2000, 2026),
    breaks = seq(2000, 2024, by = 2)
  ) +
  scale_y_continuous(
    limits = c(0, 80000),
    breaks = seq(0, 80000, by = 20000)
  ) +
  
  labs(
    x = "Año",
    y = "Pérdida acumulada de vegetación (ha)"
  ) +
  
  theme_classic() +
  theme(
    legend.position = "none",
    axis.text.x = element_text(angle = 45, hjust = 1)
  )

plot_acum

ggsave(
  filename = "plot_vegetacion_acumulada.png",
  plot = plot_acum,
  width = 12,       # ancho en pulgadas
  height = 8,       # alto en pulgadas
  dpi = 300         # resolución alta
)

########################################################
########################################################
########################################################
#Analisis por hectarias
df_hectareas <- read.csv("hectareas_dptos_chacoSeco.csv")
df_hectareas <- df_hectareas[complete.cases(df_hectareas),]
names(df_hectareas)
library(ggplot2)
library(dplyr)

# Ordenar de menor a mayor
df_plot <- df_hectareas %>%
  arrange(porc_dpto_CS)
plot_ha_CS <-ggplot(df_plot, aes(x = reorder(TOPONIMIA, porc_dpto_CS),
                    y = porc_dpto_CS,
                    fill = TOPONIMIA)) +

  geom_bar(stat = "identity") +
  coord_flip() +
  
  scale_y_continuous(labels = scales::percent_format(accuracy = 1),
                     limits = c(0, 1)) +
  
  scale_fill_manual(values = paleta_departamentos) +   # 🔥 colores consistentes
  
  labs(
    x = "Departamento",
    y = "Porcentaje (%)"
  ) +
  theme_classic() +
  theme(
    axis.text.y = element_text(size = 11),
    legend.position = "none"
  )
  plot_ha_CS

  
  
####
  plot_ha_CS <- ggplot(df_plot, aes(
    x = reorder(TOPONIMIA, porc_dpto_Total_CS),
    y = porc_dpto_Total_CS,
    fill = TOPONIMIA
  )) +
    geom_bar(stat = "identity") +
    coord_flip() +
    
    # Escala del 0% al 105%
    scale_y_continuous(
      labels = scales::percent_format(accuracy = 1),
      limits = c(0, 1.05)
    ) +
    
    scale_fill_manual(values = paleta_departamentos) +
    
    # 🔥 Etiquetas del mismo color que su barra
    geom_text(
      aes(
        label = scales::percent(porc_dpto_Total_CS, accuracy = 1),
        y = porc_dpto_Total_CS,
        color = TOPONIMIA        # <— MISMO COLOR
      ),
      hjust = -0.1,
      size = 3.8
    ) +
    
    scale_color_manual(values = paleta_departamentos) +  # <— aplicar misma paleta
    guides(color = "none") +                             # <— NO mostrar 2da leyenda
    
    labs(
      x = "Departamento",
      y = "Porcentaje (%)"
    ) +
    theme_classic() +
    theme(
      axis.text.y = element_text(size = 11),
      legend.position = "none"
    )
  
  plot_ha_CS
  
  ggsave(
  filename = "plot_Porcentaje_Total_corresponde_CS.png",
  plot = plot_ha_CS,
  width = 12,       # ancho en pulgadas
  height = 8,       # alto en pulgadas
  dpi = 300         # resolución alta
)
  
################################################################
  ################################################################

  library(dplyr)
  library(tidyr)
  df <- df %>% 
    filter(year >= 2000 & year <= 2024)
  # DATAFRAME df: pérdida por año
  # Vector con nombres originales en df (sin _prim)
  originales <- c(
    "ischilin", "tulumba", "calamuchita", "rioSeco",
    "totoral", "sobremonte", "punilla", "sanAlberto",
    "sanJavier", "minas", "pocho", "cruzEje", "capital",
    "colon", "rioCuarto", "rioPrimero", "SantaMaria", "terceroArriba"
  )
  
  # Vector con nombres “bonitos” (coinciden con df_hectareas)
  bonitos <- c(
    "Ischilin", "Tulumba", "Calamuchita", "Rio Seco",
    "Totoral", "Sobremonte", "Punilla", "San Alberto",
    "San Javier", "Minas", "Pocho", "Cruz del Eje", "Capital",
    "Colon", "Rio Cuarto", "Rio Primero", "Santa Maria", "Tercero Arriba"
  )
  
  # Creamos un named vector para mapear
  nombres_map <- setNames(bonitos, originales)
  
  # Luego, cuando transformamos df a largo:
  df_long <- df %>%
    dplyr::select(-year, -total_prim) %>%
    tidyr::pivot_longer(
      cols = dplyr::everything(),
      names_to = "TOPONIMIA",
      values_to = "perdida_ha"
    ) %>%
    dplyr::mutate(
      TOPONIMIA = gsub("_prim", "", TOPONIMIA),        # quitamos sufijo
      TOPONIMIA = nombres_map[TOPONIMIA]              # renombramos
    )
  
  
  # sumar pérdida total por dpto
  df_perdida_total <- df_long %>%
    group_by(TOPONIMIA) %>%
    summarise(perdida_total_ha = sum(perdida_ha, na.rm = TRUE))
  
  df_merged <- df_perdida_total %>%
    left_join(df_hectareas, by = "TOPONIMIA")

  
  # Pérdida relativa considerando el tamaño dentro del Chaco Seco:
  # cuántas hectáreas perdió el departamento por cada 1% que ocupa del Chaco Seco.
  
  #Los valores altos indican departamentos más afectados proporcionalmente.
  #hectáreas ajustadas por el tamaño relativo del departamento dentro del Chaco Seco.
  # df_merged <- df_merged %>%
    # mutate(indice_intensidad = perdida_total_ha / porc_dpto_Total_CS)
    df_merged <- df_merged %>%
    mutate(indice_intensidad = perdida_total_ha / area_ha_ecoregion)
  
  
  
  # Ranking de departamentos más afectados
  ranking <- df_merged %>%
    arrange(desc(indice_intensidad))
  
  ranking
  
  
  df_merged <- df_merged %>%
    mutate(
      indice_percent = indice_intensidad / max(indice_intensidad) * 100
    )

  library(ggplot2)
  
  # Gráfico
severidad_perdida_acumulada<-  ggplot(df_merged, aes(x = reorder(TOPONIMIA, indice_percent), 
                        y = indice_percent, 
                        fill = TOPONIMIA)) +
    geom_col() +
    coord_flip() +
    # Texto al costado con hectáreas acumuladas
    geom_text(aes(label = round(perdida_total_ha, 0)), 
              hjust = -0.1, size = 3) +
    # Etiquetas y título
    labs(
      x = "Departamento",
      y = "Severidad relativa de pérdida (%)",
      title = ""
    ) +
    # Colores personalizados
    scale_fill_manual(values = paleta_departamentos) +
    theme_classic() +
    theme(legend.position = "none")
  
ggsave(
    filename = "Severidad relativa de pérdida (%)_2.png",
    plot = severidad_perdida_acumulada,
    width = 12,       # ancho en pulgadas
    height = 8,       # alto en pulgadas
    dpi = 300         # resolución alta
  )
  
  