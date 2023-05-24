
# Eleccion de tema graficos -----------------------------------------------
# Es el script que se uso para escoger el color de los gráficos
# Se usa el theme de excel con las paletas de colores existentes.

# IMPORTANTE, EJECUTAR AL FINAL DE  LOS OTROS SCRIPT PARA PODER PRODUCIR EL DF DE PRUEBA

# Df de prueba

df_prueba <- 
  movid_2022_s |> 
  filter(!d12_9_recoded == "NS/NR") |> 
  group_by(edad_2, d12_9_recoded) |> 
  summarize(proportion = survey_mean(vartype = "ci")) 

# Todos los temas
temas <- c("Atlas", "Badge", "Berlin", "Celestial", "Crop", "Depth", "Droplet", "Facet", "Feathered", "Gallery", "Headlines", "Integral", "Ion Boardroom", "Ion", "Madison", "Main Event", "Mesh", "Office Theme", "Organic", "Parallax", "Parcel", "Retrospect", "Savon", "Slice", "Vapor Trail", "View", "Wisp", "Wood Type", "Aspect", "Blue Green", "Blue II", "Blue Warm", "Blue", "Grayscale", "Green Yellow", "Green", "Marquee", "Median", "Office 2007-2010", "Orange Red", "Orange", "Paper", "Red Orange", "Red Violet", "Red", "Slipstream", "Violet II", "Violet", "Yellow Orange", "Yellow")

# Funcion modificada para ver temas en el map

graph_barras_bivariado <- function(df_original, titulo, label_eje_x, tema_office){
  df <- df_original 
  # Preparando los datos para poder referirme a ellos en ggplot y transformandolos a porcentaje
  df$x <- df[[1]]
  df$y <- round(df[[3]] * 100, 1)
  df$group_fill <- df[[2]]
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
    labs(title = tema_office, # Titulo gráfico
         x = label_eje_x,  # Titulo eje x
         y = "%") + # titulo eje y
    ylim(0,100) + # limites eje y 
    geom_text(aes(y = ymax, label = y), 
              position = position_dodge(width = 0.9), 
              vjust = -1,
              size = 3,
              fontface = 2) + # Argumento que agrega cursivas, negritas, etc
    scale_fill_discrete(name = "Leyenda") + # Titulo leyenda (no se usa en este tema)
    theme(legend.position = "bottom") +  # Posicion leyenda
    theme_excel_new() + # tema excel 
    scale_fill_excel_new(theme = tema_office) # tema excel
  print(mi_grafico)
}


# Prueba con todos los gráficos
prueba <- walk(temas, ~graph_barras_bivariado(df_prueba,
                                              titulo = "Importancia pase de movilidad para vacunacion (d12.9) por tramo etario",
                                              label_eje_x = "Tramo etario",
                                              tema_office = .x))

# Prueba con la primera selección
temas_seleccionados <- c("Paper", "Office 2007-2010", "Median", "Marquee", "Aspect", "Wisp", "View", "Parcel", "Office Theme", "Headlines", "Feathered", "Crop", "Badge")
prueba <- walk(temas_seleccionados, ~graph_barras_bivariado(df_prueba,
                                                            titulo = "Importancia pase de movilidad para vacunacion (d12.9) por tramo etario",
                                                            label_eje_x = "Tramo etario",
                                                            tema_office = .x))

# Prueba con la selección final
temas_seleccionados_final <- c("View", "Median", "Badge")
prueba <- walk(temas_seleccionados_final, ~graph_barras_bivariado(df_prueba,
                                                                  titulo = "Importancia pase de movilidad para vacunacion (d12.9) por tramo etario",
                                                                  label_eje_x = "Tramo etario",
                                                                  tema_office = .x))