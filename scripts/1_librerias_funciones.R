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
       ggthemes, #Temas gráficos
       kableExtra, # Tablas html para markdown
       extrafont)  # Para utilizar fuentes del sistema en ggplot

## Cambiar la fuente para los gráficos de ggplot

# Importa las fuentes del sistema
font_import()
# Registra las fuentes para su uso en PDF
loadfonts(device = "pdf")
# Cambia la fuente a "Times"
theme_set(theme_minimal(base_family = "Times"))

 




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
graph_barras_bivariado <- function(df_original, 
                                   titulo, 
                                   label_eje_x = NULL, 
                                   subtitulo = NULL, 
                                   angulo_text_eje_x = 0,
                                   hjust_text_eje_x = 0.5,
                                   ancho_texto_x = NULL){
  # EXPLICACION GENERAL 
  # SE CREARON TRES ARGUMENTOS PARA PODER PERSONALIZAR LA POSICIÓN DE LAS ETIQUETAS EN EL EJE X: 
  # angulo_text_eje_x = 45  pone el texto en 45 grados ( o cualquier angulo deseado),
  # hjust_text_eje_x = 0.8 alinea el texto cuando se gira. Se recomienda utilizar en 0.8 si el texto está en 45, si está en 0 grados, poner en 0.5. 
  # ancho_texto_x = 10 Limita el número de caracteres de la  etiqueta del eje X. Se usa para incluir saltos de linea
  #  cuando las etiquetas no caben bien. 
  df <- df_original 

  # Preparando los datos para poder referirme a ellos en ggplot y transformandolos a porcentaje
  df$x <- as.factor(df[[1]])
  
  if (!is.null(ancho_texto_x)){
    df$x <- str_wrap(df$x, width = ancho_texto_x)
  }
  
  
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
    scale_fill_discrete(name = "") + # Titulo leyenda 
    theme_minimal() +
    theme(
      legend.position = "bottom", # Posicion leyenda
      plot.title = element_text(size = rel(1.2), face = "bold", hjust = 0.5), # Ajusta el tamaño, tipo de letra y alineación del título del gráfico
      plot.subtitle = element_text(hjust = 0.5, size = rel(1)), # Subtítulo centrado
      legend.title = element_text(size = rel(1), face = "bold"), # Ajusta el tamaño y tipo de letra del título de la leyenda
      legend.text = element_text(size = rel(0.8)), # Ajusta el tamaño de la leyenda
      axis.title = element_text(size = rel(1), face = "bold"), # Ajusta el tamaño y tipo de letra de los títulos de los ejes
      axis.text = element_text(size = rel(0.8)),
      axis.text.x = element_text(angle = angulo_text_eje_x, hjust = hjust_text_eje_x))  # Rota las etiquetas del eje x 45 grados) # Ajusta el tamaño del texto de los ejes
  return(mi_grafico)
}

# Prueba de la funcion
# graph_barras_bivariado(df_prueba,
#                        titulo = "Importancia pase de movilidad para vacunacion (d12.9) por tramo etario")
# graph_barras_bivariado(acata_normas_comparacion_anos,
#                        titulo = "Acata normas, comparacion año 2020 y 2022.",
#                        subtitulo = "prueba",
#                        angulo_text_eje_x = 0,
#                        hjust_text_eje_x = 0.5,
#                        ancho_texto_x = NULL)



# Grafico univariado ------------------------------------------------------

# Funcion que toma una salida univariada de svyr y la transforma en un gráfico
graph_barras_univariado <- function(df_original, 
                                    titulo, 
                                    label_eje_x = NULL, 
                                    subtitulo = NULL, 
                                    angulo_text_eje_x = 0,
                                    hjust_text_eje_x = 0.5,
                                    ancho_texto_x = NULL){
  # EXPLICACION GENERAL 
  # SE CREARON TRES ARGUMENTOS PARA PODER PERSONALIZAR LA POSICIÓN DE LAS ETIQUETAS EN EL EJE X: 
  # angulo_text_eje_x = 45  pone el texto en 45 grados ( o cualquier angulo deseado),
  # hjust_text_eje_x = 0.8 alinea el texto cuando se gira. Se recomienda utilizar en 0.8 si el texto está en 45, si está en 0 grados, poner en 0.5. 
  # ancho_texto_x = 10 Limita el número de caracteres de la  etiqueta del eje X. Se usa para incluir saltos de linea
  #  cuando las etiquetas no caben bien. 
  
  df <- df_original 
  # Preparando los datos para poder referirme a ellos en ggplot y transformandolos a porcentaje
  df$x <- df[[1]]
  
  if (!is.null(ancho_texto_x)){
    df$x <- str_wrap(df$x, width = ancho_texto_x)
  }
  
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
    scale_fill_discrete(name = "") + # Titulo leyenda (no se usa en este tema)
    theme_minimal() +
    theme(
      legend.position = "none", # Posicion leyenda
      plot.title = element_text(size = rel(1.2), face = "bold", hjust = 0.5), # Ajusta el tamaño, tipo de letra y alineación del título del gráfico
      plot.subtitle = element_text(hjust = 0.5, size = rel(1)), # Subtítulo centrado
      legend.title = element_text(size = rel(1), face = "bold"), # Ajusta el tamaño y tipo de letra del título de la leyenda
      legend.text = element_text(size = rel(0.8)), # Ajusta el tamaño de la leyenda
      axis.title = element_text(size = rel(1), face = "bold"), # Ajusta el tamaño y tipo de letra de los títulos de los ejes
      axis.text = element_text(size = rel(0.8)),
      axis.text.x = element_text(angle = angulo_text_eje_x, hjust = hjust_text_eje_x)) # Ajusta el tamaño del texto de los ejes
  return(mi_grafico)
}

# Prueba de la funcion
# df_prueba_2 <- movid_2022_s |>
#   filter(!d12_9_recoded == "NS/NR") |>
#   group_by(d12_9_recoded) |>
#   summarize(proportion = survey_mean(vartype = "ci"))
# 
# 
# graph_barras_univariado(df_prueba_2,
#                         titulo = "Importancia pase de movilidad para vacunacion (d12.9) por tramo etario",
#                         subtitulo = "prueba",
#                         angulo_text_eje_x = 45,
#                         hjust_text_eje_x = 0.8)


# Ejemplo (ejecutar script 2 3 y 4 primero)
# razon_pospuso_tratamiento <- 
#   movid_2020_s |> 
#   filter(cronica_d == 1, pospuso_tratamiento == "Pospuso tratamiento") |> 
#   group_by(razon_pospuso_tratamiento) |> 
#   summarize(proportion = survey_mean(vartype = "ci")) |> 
#   mutate(razon_pospuso_tratamiento = recode(as.factor(razon_pospuso_tratamiento),
#                                             '1' = "Porque no le pareció importante controlarse",
#                                             '2' = "Por el costo económico",
#                                             '4' = "Porque tenía que esperar mucho tiempo",
#                                             '5' = "Por miedo a contagiarse de COVID-19",
#                                             '6' = "Porque el sistema de salud está muy lleno",
#                                             '7' = "No contó con trasporte para trasladarse al lugar de atención",
#                                             '8' = "No tuvo con quien dejar a personas a su cuidado",
#                                             '9' = "El consultorio, hospital o clínica le suspendió la hora y no le ha reagendado",
#                                             '10' = "Otro",
#                                             '98' = "No sabe"))
# graph_barras_univariado(razon_pospuso_tratamiento,
#                         titulo = "Razón por la que se pospuso el tratamiento",
#                         angulo_text_eje_x = 0,
#                         hjust_text_eje_x = 0.5,
#                         ancho_texto_x = 10)


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



# Funciones depreciadas (tema excel para gráficos) ------------------------

# 
# # Funcion que toma una salida bivariada de svyr y la transforma en un gráfico
# graph_barras_bivariado <- function(df_original, titulo, label_eje_x = NULL, subtitulo = NULL){
#   df <- df_original 
#   # Preparando los datos para poder referirme a ellos en ggplot y transformandolos a porcentaje
#   df$x <- as.factor(df[[1]])
#   df$y <- round(df[[3]] * 100, 1)
#   df$group_fill <- as.factor(df[[2]])
#   df$ymax <- round(df[[5]] * 100, 1) 
#   df$ymin <- round(df[[4]] * 100, 1)
#   # Gráfico
#   mi_grafico <- ggplot(data = df, 
#                        aes(x = x, 
#                            y = y, 
#                            group = group_fill, 
#                            fill = group_fill,
#                            ymax = ymax, 
#                            ymin = ymin)) +
#     geom_col(stat = "identity", position = "dodge") + # identity permite trabajar con los datos agregados
#     geom_errorbar(position = position_dodge(width = 0.9), width = 0.1) + # se añaden barras de error
#     labs(title = titulo, # Titulo gráfico
#          subtitle = if (is.null(subtitulo)){""} else {subtitulo}, # No se aplica por el tema del gráfico
#          x = if (is.null(label_eje_x)){""} else {label_eje_x},  # Titulo eje x (con este tema no se usa así que lo dejé en nulo)
#          y = "%") + # titulo eje y (no se visualiza en este tema)
#     ylim(0,100) + # limites eje y 
#     geom_text(aes(y = ymax, label = y), 
#               position = position_dodge(width = 0.9), 
#               vjust = -1,
#               size = 3,
#               fontface = 2) + # Argumento que agrega cursivas, negritas, etc
#     scale_fill_discrete(name = "Leyenda") + # Titulo leyenda (no se usa en este tema)
#     theme(legend.position = "bottom") +  # Posicion leyenda
#     theme_excel_new() + # tema excel 
#     scale_fill_excel_new(theme = "Badge") # paleta de colores para el tema excel
#   return(mi_grafico)
# }
# 
# # Prueba de la funcion
# # graph_barras_bivariado(df_prueba,
# #                        titulo = "Importancia pase de movilidad para vacunacion (d12.9) por tramo etario")
# # graph_barras_bivariado(acata_normas_comparacion_anos,
# #                        titulo = "Acata normas, comparacion año 2020 y 2022.",
# #                        subtitulo = "prueba")
# 
# 
# # Funcion que toma una salida univariada de svyr y la transforma en un gráfico
# graph_barras_univariado <- function(df_original, titulo, label_eje_x = NULL){
#   df <- df_original 
#   # Preparando los datos para poder referirme a ellos en ggplot y transformandolos a porcentaje
#   df$x <- df[[1]]
#   df$y <- round(df[[2]] * 100, 1)
#   df$ymax <- round(df[[4]] * 100, 1) 
#   df$ymin <- round(df[[3]] * 100, 1)
#   # Gráfico
#   mi_grafico <- ggplot(data = df, 
#                        aes(x = x, 
#                            y = y, 
#                            fill = x, # Se dejó el argumento fill para que las barras conserven la paleta de colores
#                            ymax = ymax, 
#                            ymin = ymin)) +
#     geom_col(stat = "identity", position = "dodge") + # identity permite trabajar con los datos agregados
#     geom_errorbar(position = position_dodge(width = 0.9), width = 0.1) + # se añaden barras de error
#     ylim(0,100) + # limites eje y 
#     geom_text(aes(y = ymax, label = y), 
#               position = position_dodge(width = 0.9), 
#               vjust = -1,
#               size = 3,
#               fontface = 2) + # Argumento que agrega cursivas, negritas, etc
#     scale_fill_discrete(name = "Leyenda") + # Titulo leyenda (no se usa en este tema)
#     theme_excel_new() + # tema excel 
#     scale_fill_excel_new(theme = "Badge") + # paleta de colores para el tema excel
#     theme(legend.position = "none") # Posicion leyenda. Se pone none porque es univariado pero se quiere mantener los colores
#   return(mi_grafico)
# }
# 
# # Prueba de la funcion
# # df_prueba_2 <- movid_2022_s |> 
# #   filter(!d12_9_recoded == "NS/NR") |> 
# #   group_by(d12_9_recoded) |> 
# #   summarize(proportion = survey_mean(vartype = "ci"))
# # graph_barras_univariado(df_prueba_2, titulo = "Importancia pase de movilidad para vacunacion (d12.9) por tramo etario")

