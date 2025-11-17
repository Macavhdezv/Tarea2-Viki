# Tarea 2 
¡Hola a todxs! 

Para mi tarea 2 me inspiré en uno de mis mayores gustos: ✨K-Dramas✨. Los K-dramas o series surcoreanas son series normalmente de 16 capítulos que emiten 2 capítulos de aproximadamente una hora por semana. Tratan de diferentes cosas, en este caso y como "spoiler" los géneros principales son: romance, comedia, drama, acción u otros. 
¿Por qué me gustan? Por un lado considero que al ser series cortas van al hecho directamente, no hay tantas vueltas como en las series chilenas (como el Mega) y la forma de mostrar el amor es muy cute. También las actuaciones son buenas y el nivel de belleza es uff 💗
Para no salirme tanto del tema, me concentré en utilizar una base de datos de series punteadas en la plataforma Viki.com (especializada en dramas asiáticos) y como eran muchos datos, lo resumí en ciertas variables: El año 2016 💫 porque ese año para mí gusto salieron los mejores K-DRAMAS hasta la fecha, que fueran SHOW 🌸 porque son referidos al formato que busco (sacando Realities y Películas) y solo datos de Corea del Sur ya que buscaba la K de K-Dramas.

# Bases de Datos
Necesitamos primero subir y cargar las librerías necesarias para correr las funciones que necesitamos, tanto para limpiar los datos como para hacer el gráfico.

```{r}
 library("dplyr")   
library("ggplot2")
library("tidyverse")
library("tidyr")
library("showtext")
```
# Base de datos
Se sube la base de datos que encontré (me ayudó mi profe) en otra plataforma. 

```{r}
viki_plataforma <- read_csv("titles.csv")
```
Limpiamos la base de datos, eliminando las columnas que no se van a utilizar. Son varias y en realidad no requeridas para lo que buscaba finalmente hacer y creamos una nueva base de datos.

```{r}
viki_limpia <- viki_plataforma |> 
  select(-c(id, description, age_certification, runtime, seasons, imdb_id, imdb_score, imdb_votes))

```
## Nueva base de datos y filtración de datos
Se filtran los datos para solo tener los títulos de Corea del Sur, que sean del tipo SHOW estrenado el 2016, sacar todos los NA, quitar los realities y que tengan una popularidad mayor o igual a las 10.000 opiniones ranqueadas. Esto último lo hice con intención de reducir los valores y porque consideraba injusto que al tener menos opiniones podias tener mayores puntajes.

```{r}
viki_limpia <- viki_limpia |>
  filter(production_countries == "['KR']") |> 
  filter(type == "SHOW") |>
  filter(release_year == 2016) |>
  filter(genres != "['reality']") |> 
  filter(tmdb_popularity >= 10.000) 
```
Ahora se va a ordenar la base de datos segÚn el orden descendente del puntaje de TMDB

```{r}
viki_limpia <- viki_limpia |> 
  arrange(desc(tmdb_score))
```
## Base de datos final
No contabilizo ninguna variable que posea repetición en sus valores y filtro nuevamente para obtener solo los títulos con popularidad mayor o igual a 20.000 para reducir aún más los puntos de la base de datos funal.

```{r}
viki_final <- viki_limpia |> 
  select(title, genres, tmdb_popularity, tmdb_score) |>
  filter(tmdb_popularity >= 20.000)
```

Limpiaré la columna "genres" para que corra de mejor forma.

```{r}
viki_final <- viki_final |>
  mutate(
    genres = str_remove_all(genres, "\\[|\\]|'"),       # Quitar corchetes y comillas
    genres = str_split(genres, ",\\s*"),                # Convertir a lista
    genres = sapply(genres, function(x) x[1])           # Tomar solo el primer género
  )
```
# Gráfico

Ahora se comienza a trabajar con el gráfico. La primera etapa es subir las nuevas tipografías porque buscaba que se viera más bonito el gráfico final.

```{r}
showtext_auto()

font_add_google("Montserrat", "mont")
font_add_google("Poppins", "poppins")
font_add_google("DM Sans", "dmsans")
font_add_google("Nanum Gothic", "nanumg")

```
Vuelvo a ordenar de forma descencidente para mantener el ranking en la nueva base de datos

```{r}

viki_final <- viki_final |>
  arrange(desc(tmdb_score))

```

Comenzando con el gráfico, empezamos definiendo los colores para cada género. Aquí busqué colores pasteles que armonicen el color.

```{r}
colores_generos <- c(
  scifi = "#A7C7E7",
  comedy = "#F7A8B8",
  drama = "#F5CBA7",
  war = "#C9E4C5"
)
```
La siguiente parte es el código final, aqui se quería señalar el ranking en forma de gráfico de barras de forma horizontal. La variable x es el ranking que va desde 0 a 10 y la variable y es el título de la serie. A pesar de que todas las series tenían más de un tipo de género, es importante señalar que solo se ocupo el género principal.

```{r}
ggplot(viki_final,
       aes(x = tmdb_score,
           y = reorder(title, tmdb_score),
           fill = genres)) +
  
  # BARRAS
  geom_col() +
  
  # Texto dentro de la barra
  geom_text(
    aes(
      label = title,     
    ),
    family = "nanumg",
    color = "white",
    size = 4,
    hjust = 1.1               
  ) +
  
  # COLORES MANUALES
  scale_fill_manual(values = colores_generos) +
  
  #títulos de las variables y gráfico
  
  labs(
    title = "2016: LOS K-DRAMAS DE ORO",
    x = "RANKING TMDB", 
    y = NULL
  ) +
  
  theme_minimal(base_family = "poppins") +
  theme(
    plot.title = element_text(size = 22, face = "bold", color = "#8B0A50"),
    axis.text.y = element_blank(),                                         # títulos ya están dentro de las barras
    axis.text.x = element_text(size = 9, color = "#8B3A62"),
    legend.text = element_text(family = "dmsans", color = "#8B3A62"),
    legend.title = element_text(face = "bold", family = "poppins", color = "#8B0A50"),
    panel.background = element_rect(fill = "#FDEDF4", color = NA),                  # fondo suave
      plot.background  = element_rect(fill = "#FDEDF4", color = NA),                # fondo total
      panel.grid = element_blank(),                   # borra todas las líneas de grilla
      axis.line = element_blank(),                    # sin líneas del eje
      axis.ticks = element_blank(),                 # sin ticks
  ) 
  
 coord_flip()
```
¡Acá podemos ver la imagen! Estoy contenta con el resultado despúes de varios problemas que se tuvo.
 
<img width="493" height="355" alt="KDRAMA 2016" src="https://github.com/user-attachments/assets/a1b26336-6d6d-4880-9677-5c53cac6e8ee" />
