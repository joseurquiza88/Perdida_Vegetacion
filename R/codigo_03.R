setwd("D:/Josefina/Proyectos/Bosques/data")
data <- read.csv("perdida_forestalesOtras_categorias.csv", fileEncoding = "Latin1")

data_provincial <- data[data$resolucion_espacial == "provincial", ]
unique(data_provincial$sitio)
names(data_provincial)

library(dplyr)
library(stringr)


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

##########################################

data_dpto <- data[data$resolucion_espacial == "dpto", ]
unique(data_dpto$sitio)
names(data_dpto)

library(ggplot2)

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

##########################################
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
    oob = scales::oob_squish   # 👈 no borra valores grandes, los aplasta arriba
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

