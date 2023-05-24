# ++ Librerias ---------------------------------------------------------------

if (!require(pacman)){install.packages("pacman")}
library(pacman)
p_load(tidyverse,
       funModeling, # Estadisticas descriptivas
       haven, # Lectura dta
       janitor,# Funciones adicionales
       survey, # Análisis muestral
       srvyr, # Paquete survey en lenguaje dplyr
       purrr, # Funcion map
       ggthemes)  #Temas gráficos



 




# ++ Funciones ---------------------------------------------------------------




# Razones no vacunacion ---------------------------------------------------


# Funcion especifica que sirve para obtener tabulados con las razones de no vacunación 
calculo_razones_no_vacunacion <- function(srvyr_object, variable) {
  variable <- rlang::sym(variable) # Permite que el elemento del vector pueda ser evaluado por la funcion. 
  df <- srvyr_object %>%
    filter(!is.na(!!variable)) %>%
    group_by(!!variable) %>%
    summarize(proportion = survey_mean(vartype = "ci"))
  return(df)
}


# Grafico bivariado -------------------------------------------------------

# Funcion que toma una salida bivariada de svyr y la transforma en un gráfico
graph_barras_bivariado <- function(df_original, titulo, label_eje_x = NULL, subtitulo = NULL){
  df <- df_original 
  # Preparando los datos para poder referirme a ellos en ggplot y transformandolos a porcentaje
  df$x <- as.factor(df[[1]])
  df$y <- round(df[[3]] * 100, 1)
  df$group_fill <- as.factor(df[[2]])
  df$ymax <- round(df[[5]] * 100, 1) 
  df$ymin <- round(df[[4]] * 100, 1)
  # Gráfico
  mi_grafico <- ggplot(data = df, 
         aes(x = x, 
             y = y, 
             group = group_fill, 
             fill = group_fill,
             ymax = ymax, 
             ymin = ymin)) +
    geom_col(stat = "identity", position = "dodge") + # identity permite trabajar con los datos agregados
    geom_errorbar(position = position_dodge(width = 0.9), width = 0.1) + # se añaden barras de error
    labs(title = titulo, # Titulo gráfico
         subtitle = if (is.null(subtitulo)){""} else {subtitulo}, # No se aplica por el tema del gráfico
         x = if (is.null(label_eje_x)){""} else {label_eje_x},  # Titulo eje x (con este tema no se usa así que lo dejé en nulo)
         y = "%") + # titulo eje y (no se visualiza en este tema)
    ylim(0,100) + # limites eje y 
    geom_text(aes(y = ymax, label = y), 
              position = position_dodge(width = 0.9), 
              vjust = -1,
              size = 3,
              fontface = 2) + # Argumento que agrega cursivas, negritas, etc
    scale_fill_discrete(name = "Leyenda") + # Titulo leyenda (no se usa en este tema)
    theme(legend.position = "bottom") +  # Posicion leyenda
    theme_excel_new() + # tema excel 
   scale_fill_excel_new(theme = "Badge") # paleta de colores para el tema excel
  return(mi_grafico)
}

# Prueba de la funcion
# graph_barras_bivariado(df_prueba,
#                        titulo = "Importancia pase de movilidad para vacunacion (d12.9) por tramo etario")
# graph_barras_bivariado(acata_normas_comparacion_anos,
#                        titulo = "Acata normas, comparacion año 2020 y 2022.",
#                        subtitulo = "prueba")



# Grafico univariado ------------------------------------------------------

# Funcion que toma una salida univariada de svyr y la transforma en un gráfico
graph_barras_univariado <- function(df_original, titulo, label_eje_x = NULL){
  df <- df_original 
  # Preparando los datos para poder referirme a ellos en ggplot y transformandolos a porcentaje
  df$x <- df[[1]]
  df$y <- round(df[[2]] * 100, 1)
  df$ymax <- round(df[[4]] * 100, 1) 
  df$ymin <- round(df[[3]] * 100, 1)
  # Gráfico
  mi_grafico <- ggplot(data = df, 
                       aes(x = x, 
                           y = y, 
                           fill = x, # Se dejó el argumento fill para que las barras conserven la paleta de colores
                           ymax = ymax, 
                           ymin = ymin)) +
    geom_col(stat = "identity", position = "dodge") + # identity permite trabajar con los datos agregados
    geom_errorbar(position = position_dodge(width = 0.9), width = 0.1) + # se añaden barras de error
    ylim(0,100) + # limites eje y 
    geom_text(aes(y = ymax, label = y), 
              position = position_dodge(width = 0.9), 
              vjust = -1,
              size = 3,
              fontface = 2) + # Argumento que agrega cursivas, negritas, etc
    scale_fill_discrete(name = "Leyenda") + # Titulo leyenda (no se usa en este tema)
    theme_excel_new() + # tema excel 
    scale_fill_excel_new(theme = "Badge") + # paleta de colores para el tema excel
    theme(legend.position = "none") # Posicion leyenda. Se pone none porque es univariado pero se quiere mantener los colores
  return(mi_grafico)
}

# Prueba de la funcion
# df_prueba_2 <- movid_2022_s |> 
#   filter(!d12_9_recoded == "NS/NR") |> 
#   group_by(d12_9_recoded) |> 
#   summarize(proportion = survey_mean(vartype = "ci"))
# graph_barras_univariado(df_prueba_2, titulo = "Importancia pase de movilidad para vacunacion (d12.9) por tramo etario")


# Moda --------------------------------------------------------------------

# Función moda (se utiliza para imputacion en indice de salud mental)

moda <- function(x) {
  tabla <- table(x)
  modas <- as.numeric(names(tabla)[tabla == max(tabla)])
  return(modas)
}


# Sin usar ----------------------------------------------------------------

proporcion_grupo <- 
  function(srvyr_object, variable_agrupada){
    df <- {{srvyr_object}} |> 
      group_by({{variable_agrupada}}) |> 
      summarize(proportion = survey_mean(vartype = "ci"),
                total = survey_total(vartype = "ci"))
    return(df)
    print(df)
  }
