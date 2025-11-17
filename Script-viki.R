#Tarea 2 Viki 

#Se comienza con instalar y cargar las librerías necesarias

library("dplyr")
library("ggplot2")
library("tidyverse")
library("ggrepel")
library("tidyr")
library("stringr")
library("showtext")
library("ggimage")
library("sysfonts")

install.packages("cowplot")


#Se sube la base de datos

viki_plataforma <- read_csv("titles.csv")

#Limpiamos la base de datos, eliminando las columnas que no se van a utilizar

viki_limpia <- viki_plataforma |> 
  select(-c(id, description, age_certification, runtime, seasons, imdb_id, imdb_score, imdb_votes))

#Se filtran los datos para solo tener los títulos de Corea del Sur, que sean del tipo SHOW estrenado el 2016, sacar todos los NA, quitar los realities y que tengan una popularidad mayor o igual a 10.000

viki_limpia <- viki_limpia |>
  filter(production_countries == "['KR']") |> 
  filter(type == "SHOW") |>
  filter(release_year == 2016) |>
  filter(genres != "['reality']") |> 
  filter(tmdb_popularity >= 10.000) 


#Ahora vamos a ordenar la base de datos segun el orden descendente del puntaje de TMDB

viki_limpia <- viki_limpia |> 
  arrange(desc(tmdb_score))

#Ahora seleccionamos todas las variables que no poseen repetición en sus valores y filtramos nuevamente para obtener solo los títulos con popularidad mayor o igual a 20.000 y asi reducir la base de datos funal.

viki_final <- viki_limpia |> 
  select(title, genres, tmdb_popularity, tmdb_score) |>
  filter(tmdb_popularity >= 20.000)

#Limpiare la columna "genres"

viki_final <- viki_final |>
  mutate(
    genres = str_remove_all(genres, "\\[|\\]|'"),       # Quitar corchetes y comillas
    genres = str_split(genres, ",\\s*"),                # Convertir a lista
    genres = sapply(genres, function(x) x[1])           # Tomar solo el primer género
  )

#Ahora empezaremos con el gráfico

font_add_google("Montserrat", "mont")
showtext_auto()

font_add_google("Poppins", "poppins")
showtext_auto()

font_add_google("Poppins", "poppins")
font_add_google("DM Sans", "dmsans")
font_add_google("Nanum Gothic", "nanumg")

colores_generos <- c(
  scifi = "#dodgerblue",
  comedy = "#CD1076",
  drama = "#CD5555",
  war = "#CDB79E"
)

  # Colores personalizados


viki_final <- viki_final |>
  arrange(desc(tmdb_score))

# Ordenar de mayor a menor para mantener el ranking
viki_final <- viki_final |>
  arrange(desc(tmdb_score))

#Este true

# ACTIVAR SHOWTEXT Y AÑADIR TIPOGRAFÍAS
showtext_auto()

font_add_google("Poppins", "poppins")
font_add_google("Nanum Gothic", "nanumg")
font_add_google("DM Sans", "dmsans")

# hACER UN GRÁFICO

colores_generos <- c(
  scifi = "#A7C7E7",
  comedy = "#F7A8B8",
  drama = "#F5CBA7",
  war = "#C9E4C5"
)

ggplot(viki_final,
       aes(x = tmdb_score,
           y = reorder(title, tmdb_score),
           fill = genres)) +
  
  # BARRAS
  geom_col() +
  
  # TEXTO DENTRO DE LA BARRA
  geom_text(
    aes(
      label = title,      # mitad exacta de la barra
    ),
    family = "nanumg",
    color = "white",
    size = 4,
    hjust = 1.1               # centrado dentro de la barra horizontal
  ) +
  
  # COLORES MANUALES
  scale_fill_manual(values = colores_generos) +
  
  labs(
    title = "2016: LOS K-DRAMAS DE ORO",
    x = "RANKING TMDB", 
    y = NULL
  ) +
  
  theme_minimal(base_family = "poppins") +
  theme(
    plot.title = element_text(size = 22, face = "bold", color = "#8B0A50"),
    axis.text.y = element_blank(),     # títulos ya están dentro de las barras
    axis.text.x = element_text(size = 9, color = "#8B3A62"),
    legend.text = element_text(family = "dmsans", color = "#8B3A62"),
    legend.title = element_text(face = "bold", family = "poppins", color = "#8B0A50"),
    panel.background = element_rect(fill = "#FDEDF4", color = NA),  # fondo suave
      plot.background  = element_rect(fill = "#FDEDF4", color = NA),  # fondo total
      panel.grid = element_blank(),        # borra todas las líneas de grilla
      axis.line = element_blank(),         # sin líneas del eje
      axis.ticks = element_blank(),        # sin ticks
  ) 
  
 coord_flip()
 

