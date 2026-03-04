##Objetivo
#Realizar diversos analisis sobre coebertura, bosques en el Chaco Seco
#en la provincia de Cordoba

setwd("D:/Josefina/Proyectos/Bosques/codigo")

#Librerias
library(ggplot2)
library(dplyr)


###############################################################################
# Dataset con el numero y superficie del riego por pivot en la zona de estudio
#Extraido de:
data_riego <- data.frame(
  year = factor(c(2022, 2023, 2024), levels = c(2022, 2023, 2024)),
  puntos_total = c(1709, 1835, 1848),
  superficie_total = c(123370, 130180, 132514),
  puntos_nuevos = c(239, 249, 251),
  superficie_nueva = c(20579, 21727, 22241)
)


col_barra <- "#238b45"   # verde prolijo

puntos_total<-ggplot(data_riego, aes(x = year, y = puntos_total)) +
  geom_col(fill = col_barra, width = 0.6) +
  
  geom_text(
    aes(label = puntos_total),
    vjust = 5,
    color = "white",
    size = 4
  ) +
  
  theme_classic() +
  labs(
    x = "Año",
    y = "Puntos de riego",
    title = "Puntos Pivot Prov. Córdoba"
  )





superficie_total<- ggplot(data_riego, aes(x = year, y = superficie_total)) +
  geom_col(fill = col_barra, width = 0.6) +
  
  geom_text(
    aes(label = scales::comma(superficie_total)),
    vjust = 5,
    color = "white",
    size = 4
  ) +
  
  theme_classic() +
  labs(
    x = "Año",
    y = "Superficie de riego (ha)",
    title = "Superficie de riego Prov. Córdoba"
  )+ scale_y_continuous(expand = c(0,0))



puntos_nuevos<-ggplot(data_riego, aes(x = year, y = puntos_nuevos)) +
  geom_col(fill = "#3182bd", width = 0.6) +
  theme_classic() +
  geom_text(
    aes(label = puntos_nuevos),
    vjust = 5,
    color = "white",
    size = 4
  ) +
  labs(
    x = "Año",
    y = "Puntos de riego",
    title = "Puntos Pivot Chaco Seco"
  )


superficie_nueva <- ggplot(data_riego, aes(x = year, y = superficie_nueva)) +
  geom_col(fill = "#3182bd", width = 0.6) +
  theme_classic() +
  geom_text(
    aes(label = superficie_nueva),
    vjust = 5,
    color = "white",
    size = 4
  ) +
  labs(
    x = "Año",
    y = "Superficie (ha)",
    title = "Superficie de riego Chaco Seco"
  ) + scale_y_continuous(expand = c(0,0))




library(gridExtra)

grid.arrange(
  puntos_total,superficie_total,puntos_nuevos, superficie_nueva,
  ncol = 2
)

df <- read.csv("D:/Josefina/Proyectos/Bosques/data/Riego_pivot_resumen.csv", fileEncoding = "latin1")


df <- df [df$tipo == "CS",]


library(ggplot2)
library(dplyr)

df_sum <- df %>%
  group_by(dpto, year) %>%
  summarise(
    superficie_ha = sum(SupercieHa, na.rm = TRUE),
    count = count(SupercieHa),
    .groups = "drop"
  )
df_sum$superficie_ha <- round(df_sum$superficie_ha,0)
df_sum$year <- as.integer(df_sum$year)

library(ggplot2)

ggplot(df_sum,
       #aes(x = factor(year), y = superficie_ha, group = dpto)) +
  aes(x = factor(year), y = superficie_ha, group = dpto)) +
  geom_line(color = "steelblue", linewidth = 0.9) +
  geom_point(color = "steelblue", size = 1.5) +
  
  facet_wrap(~ dpto,  scales = "free_y") + #scales = "fixed")+ #
  
  theme_classic() +
  
  labs(
    x = "Año",
    y = "Superficie de riego (ha)",
    title = "Evolución anual de superficie de riego por departamento dentro del Chaco Seco"
  )


####
df <- read.csv("D:/Josefina/Proyectos/Bosques/data/dataset.csv", fileEncoding = "latin1")
df <- df[df$tipo == "perdida_cobertura_arborea_incendios", ]

ggplot(df,
       
       aes(x = factor(year_2), y = valor, group = sitio)) +
  geom_line(color = "steelblue", linewidth = 0.9) +
  geom_point(color = "steelblue", size = 1.5) +
  
  facet_wrap(~ sitio,  scales = "free_y") + 
  
  theme_classic() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))+

  labs(
    x = "Año",
    y = "Superficie de riego (ha)",
    title = "perdida cobertura arborea incendios"
  )


ggplot(df,
       aes(x = year_2, y = valor, group = sitio)) +
  
  geom_line(color = "steelblue", linewidth = 0.9) +
  geom_point(color = "steelblue", size = 1.5) +
  
  facet_wrap(~ sitio, scales = "free_y") +
  
  scale_x_continuous(
    breaks = seq(min(df$year_2), max(df$year_2), by = 5)
  ) +
  
  theme_classic() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))+
  labs(
    x = "Año",
    y = "(ha)",
    title = "Pérdida de cobertura arbórea por incendios (ha)"
  )

########
df <- read.csv("D:/Josefina/Proyectos/Bosques/data/cobertura.csv", fileEncoding = "latin1")
df <- df[df$year >=2000,]

library(dplyr)
library(tidyr)

df_long <- df %>%
  pivot_longer(
    cols = c(Bosques,	Agricultura,	Urbano),
    names_to = "categoria",
    values_to = "superficie_ha"
  )


library(dplyr)
library(tidyr)

library(ggplot2)

ggplot(df_long,
       aes(x = year, y = superficie_ha, color = categoria)) +
  
  geom_line(linewidth = 0.9) +
  geom_point(size = 1.6) +
  
  facet_wrap(~ sitio, scales = "free_y") + #scales = "fixed")+
  
  scale_x_continuous(
    breaks = seq(min(df_long$year), max(df_long$year), by = 3)
  ) +
  scale_y_continuous(expand = c(0,0))+
  # scale_y_continuous(
  #   labels = scales::label_number(scale = 1e-5, suffix = " M"),
  #   expand = c(0,0)
  # )+

  
  
  theme_classic() +
  theme(
    strip.text = element_text(size = 9),
    legend.title = element_blank(),
    axis.text.x = element_text(angle = 45, hjust = 1)
  ) +   
  labs(
    x = "Año",
    y = "Superficie (ha)",
    title = "Evolución temporal de cobertura por sitio"
  )

###############################
df <- read.csv("D:/Josefina/Proyectos/Bosques/data/cobertura_completo.csv", fileEncoding = "latin1")
df <- df[df$year >=2000,]
sitios_filtrado <- c("Calamuchita", "Sobremonte", "Cruz del Eje", ,  "Ischilin",
                     "Minas", "Pocho", "Punilla", "Rio Seco",
                     "San Javier",  "San Alberto", "Tulumba"
  
)
df <- df[df$sitio %in% sitios_filtrado, ]

unique(df$sitio)
unique(df$)
library(dplyr)
library(tidyr)
names(df)
df_long <- df %>%
  pivot_longer(
    cols = c(Bosques.cerrados,Bosques.abiertos,Bosques.inundables,
             Arbustales.cerrados, Arbustales.abiertos, 
             Mosaicos.de.arbustos.y.herbáceas, Cultivos.temporarios,
             Cultivos.perennes,	Pasturas, Silvicultura,	Áreas.urbanas
), 
    names_to = "categoria",
    values_to = "superficie_ha"
  )

library(dplyr)
library(tidyr)

library(ggplot2)

ggplot(df_long,
       aes(x = year, y = superficie_ha, color = categoria)) +
  
  geom_line(linewidth = 0.9) +
  #geom_point(size = 1.6) +
  
  facet_wrap(~ sitio, scales = "free_y") + #scales = "fixed")+
  
  scale_x_continuous(
    breaks = seq(min(df_long$year), max(df_long$year), by = 3)
  ) +
  scale_y_continuous(expand = c(0,0))+
  # scale_y_continuous(
  #   labels = scales::label_number(scale = 1e-5, suffix = " M"),
  #   expand = c(0,0)
  # )+
  
  
  
  theme_classic() +
  theme(
    strip.text = element_text(size = 9),
    legend.title = element_blank(),
    axis.text.x = element_text(angle = 45, hjust = 1)
  ) +   
  labs(
    x = "Año",
    y = "Superficie (ha)",
    title = "Evolución temporal de cobertura por sitio"
  )

#############################
df_total_year <- df_long %>%
  group_by(year) %>%
  summarise(superficie_total = sum(superficie_ha, na.rm = TRUE))


ggplot(df_total_year, aes(x = year, y = superficie_total)) +
  geom_line(linewidth = 1) +
  geom_point() +
  scale_x_continuous(
    breaks = seq(min(df_total_year$year), max(df_total_year$year), by = 3)
  ) +
  theme_classic() +
  labs(
    x = "Año",
    y = "Superficie total (ha)",
    title = "Superficie total por año (todas las coberturas)"
  )



################
df_long <- df %>%
  pivot_longer(
    cols = c(
      Bosques.cerrados,
      Bosques.abiertos,
      Bosques.inundables,
      Arbustales.cerrados,
      Arbustales.abiertos,
      Mosaicos.de.arbustos.y.herbáceas,
      Cultivos.temporarios,
      Cultivos.perennes,
      Pasturas,
      Silvicultura,
      Áreas.urbanas
    ),
    names_to = "categoria",
    values_to = "superficie_ha"
  )



df_year_categoria <- df_long %>%
  group_by(year, categoria) %>%
  summarise(
    superficie_total = sum(superficie_ha, na.rm = TRUE),
    .groups = "drop"
  )




ggplot(df_year_categoria,
       aes(x = year, y = superficie_total, color = categoria)) +
  
  geom_line(linewidth = 1) +
  
  scale_x_continuous(
    breaks = seq(min(df_year_categoria$year),
                 max(df_year_categoria$year),
                 by = 3)
  ) +
  
  scale_y_continuous(expand = c(0,0)) +
  
  theme_classic() +
  theme(
    legend.title = element_blank(),
    axis.text.x = element_text(angle = 45, hjust = 1)
  ) +
  
  labs(
    x = "Año",
    y = "Superficie total (ha)",
    title = "Evolución temporal de coberturas por categoría Chaco Seco"
  )



library(scales)

ggplot(df_year_categoria,
       aes(x = year, y = superficie_total, color = categoria)) +
  
  geom_line(linewidth = 1) +
  
  scale_x_continuous(
    breaks = seq(min(df_year_categoria$year),
                 max(df_year_categoria$year),
                 by = 3)
  ) +
  
  scale_y_continuous(
    expand = c(0,0),
    labels = comma
  ) +
  
  theme_classic() +
  theme(
    legend.title = element_blank(),
    axis.text.x = element_text(angle = 45, hjust = 1)
  ) +
  
  labs(
    x = "Año",
    y = "Superficie total (ha)",
    title = "Evolución temporal de coberturas por categoría (suma total)"
  )

library(scales)

ggplot(df_year_categoria,
       aes(x = year, y = superficie_total, color = categoria)) +
  
  geom_line(linewidth = 1) +
  
  scale_x_continuous(
    breaks = seq(min(df_year_categoria$year),
                 max(df_year_categoria$year),
                 by = 3)
  ) +
  
  scale_y_continuous(
    expand = c(0,0),
    labels = comma
  ) +
  
  theme_classic() +
  theme(
    legend.title = element_blank(),
    axis.text.x = element_text(angle = 45, hjust = 1)
  ) +
  
  labs(
    x = "Año",
    y = "Superficie total (ha)",
    title = "Evolución temporal de coberturas por categoría Chaco Seco"
  )



######################
####
df <- read.csv("D:/Josefina/Proyectos/Bosques/data/dataset.csv", fileEncoding = "latin1")
unique(df$tipo)
library(dplyr)
library(ggplot2)


df <- df %>%
  filter(tipo %in% c(
    "perdida_forestales_otras_cat_I ",
    "perdida_forestales_otras_cat_II ",
    "perdida_forestales_otras_cat_III"
  ))

df_sum <- df %>%
  group_by(sitio, year, tipo) %>%
  summarise(
    perdida_ha = sum(valor, na.rm = TRUE),
    .groups = "drop"
  )

names(df)

"resolucion_espacial" "sitio"               "year"               
[4] "year_2"              "tipo"                "valor"              
[7] "unidad"              "tipoDato"            "fuenteDatos"

ggplot(df,
       
       aes(x = factor(year_2), y = valor, group = sitio)) +
  geom_line(color = "steelblue", linewidth = 0.9) +
  geom_point(color = "steelblue", size = 1.5) +
  
  facet_wrap(~ sitio,  scales = "free_y") + 
  
  theme_classic() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))+
  
  labs(
    x = "Año",
    y = "Superficie de riego (ha)",
    title = "perdida cobertura arborea incendios"
  )


ggplot(df,
       aes(x = year_2, y = valor, group = sitio)) +
  
  geom_line(color = "steelblue", linewidth = 0.9) +
  geom_point(color = "steelblue", size = 1.5) +
  
  facet_wrap(~ sitio, scales = "free_y") +
  
  scale_x_continuous(
    breaks = seq(min(df$year_2), max(df$year_2), by = 5)
  ) +
  
  theme_classic() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))+
  labs(
    x = "Año",
    y = "(ha)",
    title = "Pérdida de cobertura arbórea por incendios (ha)"
  )

df<- df[df$resolucion_espacial == "dpto",]
sitios_filtrado <- c("Calamuchita", "Sobremonte", "Cruz del Eje",  "Ischilín",
                     "Minas", "Pocho", "Punilla", "Rio Seco",
                     "San Javier",  "San Alberto", "Tulumba"
                     
)
df <- df[df$sitio %in% sitios_filtrado, ]

df <- df %>%
  filter(tipo %in% c(
    "perdida_forestales_otras_cat_I ",
    "perdida_forestales_otras_cat_II ",
    "perdida_forestales_otras_cat_III"
  ))


df_sum <- df %>%
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
  
  scale_x_continuous(
    breaks = seq(min(df_sum$year_2), max(df_sum$year_2), by = 2)
  ) +
  
  scale_color_manual(
    values = c(
      "perdida_forestales_otras_cat_I "   = "red",
      "perdida_forestales_otras_cat_II "  = "goldenrod",
      "perdida_forestales_otras_cat_III" = "forestgreen"
    ),
    labels = c("Cat I", "Cat II", "Cat III")
  ) +
  
  theme_classic() +
  theme(
    legend.title = element_blank(),
    strip.text = element_text(size = 9),
    axis.text.x = element_text(angle = 45, hjust = 1)
  ) +
  
  labs(
    x = "Año",
    y = "Pérdida forestal (ha)",
    title = "Evolución anual de pérdida forestal por sitio y categoría"
  )

