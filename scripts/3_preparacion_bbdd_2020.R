#  ++ Lectura BBDD ------------------------------------------------------------

## Lectura bases 
movid_2020 <- read_dta("./input/2020/MOVID-IMPACT.dta")

## Limpieza nombres columnas 
movid_2020 <- janitor::clean_names(movid_2020)

## Selecciona a las personas efectivamente entrevistadas de la bbdd
movid_2020 <- movid_2020 %>% filter(entrevistado == 1)





# ++ Variables independientes ------------------------------------------------


# Sexo --------------------------------------------------------------------

# 1 hombre, 2 mujer
movid_2020 |> group_by(sexo) |> tally()


# Nivel educacional -------------------------------------------------------

# Opcion 1
movid_2020 <- movid_2020%>% 
  mutate(educ_2 = as.factor(case_when(
    a8a == 1 | a8a == 2 ~ "Nunca asistió",
    a8a == 3 | a8a == 4 ~ "Básica",
    a8a == 5 | a8a == 6 ~ "Media",
    a8a == 7 | a8a == 8 ~ "Superior",
    a8a == 9 ~ "Postgrado",
    TRUE ~ "NS/NR")))

# Opcion 2: Se recomienda usar esta por IC
movid_2020 <- movid_2020 %>% 
  mutate(educ_3 = as.factor(case_when(
    a8a == 1 | a8a == 2 | a8a ==  3 |a8a == 4 ~ "Básica o nunca asistió",
    a8a == 5 | a8a ==6 ~ "Media",
    a8a == 7 | a8a ==8 | a8a == 9 ~ "Superior o postgrado",
    TRUE ~ "NS/NR")))
movid_2020 |> group_by(educ_3) |> tally()

# Prevision de salud ------------------------------------------------------

movid_2020 <- 
  movid_2020 %>% 
  mutate(prevision = as.factor(
    case_when(
      b2 == 1 ~ "Fonasa",
      b2 == 2 ~ "Isapre",
      b2 == 5 ~ "Ninguna", 
      b2 == 3 | b2 == 4 ~ "Otra",
      b2 == 8 | b2 == 9 ~ "NS/NR")))

movid_2020 |> group_by(prevision) |> tally()


# Ocupados ----------------------------------------------------------------

movid_2020 <- 
  movid_2020 %>% 
  mutate(
    ocupacion_all = as.factor(case_when(
      g1 == 1 | g2 == 1 | g3 != 10 ~ "ocupados", 
      g4 == 2 & g5 == 1 ~ "busca_trabajo_primera_vez",
      g1 == 2 & g2 == 2 & g3 == 10 & g4  == 1 & g5 == 1 ~ "cesante",
      TRUE ~ "fuera_fuerza_trabajo")),
    ocupacion_d = as.factor(case_when(
      g1 == 1 | g2 == 1 | g3 != 10 ~ "ocupados",
      (g4 == 2 & g5 == 1) | (g1 == 2 & g2 == 2 & g3 == 10 & g4  == 1 & g5 == 1) ~ "desocupados",
      TRUE ~ "fuera_fuerza_trabajo")))

# Importante: La desagregacion por cesante  y busca trabajo por primera vez no es 
# posible hacerla en la encuesta 2022. 


### Fuerza de trabajo (ocupados y personas que buscan trabajo mayores de 15 anos)
## Ocupados 
# g1 == 1 |(g1 == 2 & g2 == 1) | (g1 == 2 & g2 == 2 & g3 != 10) 
## Desocupados (Buscan trabajo por primera vez  y Cesantes)
# Buscan trabajo por primera vez
# g4 == 2 & g5 == 1
# Cesantes  
# g1 == 2 & g2 == 2 & g3 == 10 & g4  == 1 & g == 1



# Tramo etario ------------------------------------------------------------

# Opcion 1: Desagregacion completa
movid_2020$edad_rec <- cut(movid_2020$edad,
                           include.lowest = TRUE,
                           right = TRUE,
                           breaks = c(18, 24, 34, 44, 54, 64, 80, Inf)
)

movid_2020 |> group_by(edad_rec) |> tally()

# Opcion 2: Desagregacion reducida (recomendable por IC)
movid_2020 <- movid_2020 %>% 
  mutate(edad_2 = as.factor(case_when(
    edad < 30 ~ "18 a 29 años",
    edad >= 30 & edad < 60 ~ "30 a 59 años",
    edad >= 60 ~ "60 años o más"
  )))



# Nivel de ingresos -------------------------------------------------------

# Se pregunta por ingresos del hogar, pero no se sabe de cuantas personas es el hogar, por lo tanto
# No se puede obtener ingreso percapita. 
# Por lo tanto, no se recomienda usar esta variable para análisis. 

# g47 responde nivel de ingresos continuo del hogar ? si/no
movid_2020 |> group_by(g47) |> tally()

# g47 monto ingresos continuo
movid_2020 |> group_by(g47_monto) |> tally()

# g48 Tramo ingresos
movid_2020 |> group_by(g48) |> tally()

# Conversion de tramos de ingresos a media de ingresos (variable continua)
movid_2020 <- 
  movid_2020 |> 
  mutate(tramo_ingresos_continuo = case_when(
    g48 == 1 ~ 100000,
    g48 == 2 ~ 275000,
    g48 == 3 ~ 425000,
    g48 == 4 ~ 650000,
    g48 == 5 ~ 1000000,
    g48 == 6 ~ 1600000,
    g48 == 7 ~ 3500000,
    g48 == 8 ~ 6000000
  ))

# Ingreso hogar total (con estimacion de ingresos continuos)
movid_2020 <- 
  movid_2020 |> 
  mutate(ingreso_hogar_completo = ifelse(g47 == 1, g47_monto, tramo_ingresos_continuo))

# Ingresos hogar por tramo de ingreso 

movid_2020$tramo_ingresos_hogar_completo <- cut(movid_2020$ingreso_hogar_completo,
                                                include.lowest = TRUE,
                                                right = TRUE,
                                                ordered_result = TRUE,
                                                labels = levels(haven::as_factor(movid_2020$g48))[1:8],
                                                breaks = c(0,
                                                           200000, 
                                                           350000, 
                                                           500000, 
                                                           800000, 
                                                           1200000, 
                                                           2000000, 
                                                           5000000,
                                                           Inf))

# Se agrega categoria NS/NR a la clasificacion anterior
movid_2020 <- 
  movid_2020 |> 
  mutate(tramo_ingresos_hogar_completo = ifelse(is.na(tramo_ingresos_hogar_completo), 
                                                "NS/NR", 
                                                as.character(tramo_ingresos_hogar_completo)))
# Se transforma en factor
movid_2020$tramo_ingresos_hogar_completo <- 
  factor(movid_2020$tramo_ingresos_hogar_completo, 
         levels = c(levels(haven::as_factor(movid_2020$g48))[1:8], "NS/NR"))

# Variable final con el tramo de ingresos del hogar completo (visualizacion)
movid_2020 |> group_by(tramo_ingresos_hogar_completo) |>  tally()


# Enfermedad cronica ------------------------------------------------------

# IMPORTANTE: La pregunta de enfermedad crónica en año 2020 (c1) y 2022 (b2) difiere entre cuestionarios. 
# Se consideraron condiciones de salud mental como enfermedad crónica. 

# Reemplazar NA por 0 para que funcione bien ifelse

movid_2020 <- 
  movid_2020 |> 
  mutate(across(starts_with("c1"), ~ifelse(is.na(.), 0, .)))

# Personas con enfermedad cronica. 1 = 1  o más enfermedad cronicas / 0 = Sin enfermedad crónica. 
movid_2020 <- 
  movid_2020 |> 
  mutate(cronica_d = ifelse(c1_1 == 1 | c1_2 == 2 | c1_3 == 3  | c1_4 == 4 |
                            c1_5 == 5 | c1_6 == 6, 1, 0))
movid_2020 |> group_by(cronica_d) |> tally()

# Region ------------------------------------------------------------------

# Ordenando region (no funciona)
# movid_2020$region <- 
#   ordered(movid_2020$region, 
#           levels = c(15, 1, 2, 3, 4, 5 ,6 ,7, 16, 8 , 9, 14, 10, 11, 12, 13), 
#           labels=c("Región de Arica y Parinacota", "Región de Tarapacá", "Región de Antofagasta", 
#                    "Región de Atacama", "Región de Coquimbo","Región de Valparaíso", "Región de O'Higgins",                     
#                    "Región del Maule", "Región de Ñuble", "Región del Bío Bío", "Región de la Araucanía",                  
#                    "Región de los Ríos", "Región de los Lagos", "Región de Aysén",  "Región de Magallanes y Antártica Chilena",
#                    "Región Metropolitana de Santiago"))          

movid_2020 %>% group_by(region) %>% tally()

# Región Poblacion
movid_2020 <- movid_2020 %>% mutate(region_d = as.factor(ifelse(region == 13, "RM", "Otra region")))
movid_2020 %>% group_by(region_d) %>% tally()

#  ++ Creación variables dependientes ----------------------------------------

# F3: Modulo social -----------------------------------------------------------

# Ojo, las escalas estan con numeracion al reves que el año 2022.
movid_2020 <- 
  movid_2020 |> 
  mutate(across(starts_with("f3_"), 
                ~case_when(. %in% c(4:5) ~ "De acuerdo o muy de acuerdo",
                           . %in% c(3) ~ "Indiferente",
                           . %in% c(1:2) ~ "En desacuerdo o muy en desacuerdo",
                           . %in% c(8:9) ~ "NS/NR"),
                .names = "{.col}_recoded"))

# Comprobacion
movid_2020 |> select(starts_with("f3_") & ends_with("recoded"))




# Pospuso tratamiento -----------------------------------------------------

movid_2020 <- 
  movid_2020 |> 
  mutate(pospuso_tratamiento = case_when(
    e1_1 == 1 | e1_2 == 1 | e1_3 == 1 | e1_4 == 1 | e1_5 == 1 | e1_6 == 1 ~ "Pospuso tratamiento",
    e1_1 == 2 | e1_2 == 2 | e1_3 == 2 | e1_4 == 2 | e1_5 == 2 | e1_6 == 2 ~ "No  pospuso tratamiento",
    e1_1 >= 8 | e1_2 >= 8 | e1_3 >= 8 | e1_4 >= 8 | e1_5 >= 8 | e1_6 >= 8 ~ "NS/NR"
  ))


# Razon pospuso tratamiento -----------------------------------------------

# Razon por la que pospuso el tratamiento para el primer tratamiento que pospuso siguiendo el orden de la siguiente lista
# basada en C1:Diabetes, hipertension, infarto, enfermedad respiratorioa, depresión u otra. 

movid_2020 <- 
  movid_2020 |> 
  mutate(razon_pospuso_tratamiento = case_when(
    e2_1 >= 1 ~ e2_1,
    e2_2 >= 1 ~ e2_2,
    e2_3 >= 1 ~ e2_3,
    e2_4 >= 1 ~ e2_4,
    e2_5 >= 1 ~ e2_5,
    e2_6 >= 1 ~ e2_6
  ))

movid_2020 |> group_by(razon_pospuso_tratamiento) |> tally()


# Indice de salud mental --------------------------------------------------

# El PHQ-4, como una herramienta de detección rápida para la depresión y la ansiedad, se puede utilizar para construir un índice de salud mental rudimentario. Aquí está una manera de cómo se puede hacer esto:
#   
# El PHQ-4 consta de cuatro preguntas con cuatro opciones de respuesta: "Nada en absoluto", "Varios días", "Más de la mitad de los días", y "Casi todos los días", que se puntúan de 0 a 3 respectivamente.
# 
# Por lo tanto, el rango total de puntajes posibles para el PHQ-4 va de 0 (sin síntomas de ansiedad o depresión) a 12 (síntomas muy severos de ansiedad y depresión).
# 
# La interpretación generalmente aceptada de los puntajes del PHQ-4 es la siguiente:
#   
# Un puntaje de 0 a 2 indica no hay síntomas de ansiedad ni depresión.
# Un puntaje de 3 a 5 indica posibles síntomas de ansiedad o depresión.
# Un puntaje de 6 a 8 indica síntomas moderados de ansiedad y/o depresión.
# Un puntaje de 9 a 12 indica síntomas severos de ansiedad y/o depresión.
# 
# Entonces, este puntaje puede ser utilizado como un índice básico de salud mental. Sin embargo, es importante recordar 
# que este índice solo ofrece una visión general de la presencia y gravedad de los síntomas de ansiedad y depresión. 
# No aborda otros aspectos de la salud mental y no debe ser utilizado para diagnosticar trastornos de ansiedad o depresión. 
# Para un diagnóstico y evaluación completos de la salud mental, se necesita un profesional de salud mental.



# En este caso, se va a IMPUTAR la no respuesta con la moda para cada pregunta 


# Imputacion de NS/NR

# Obtener la moda sin considera NS/NR
moda_c2_1 <- moda(movid_2020$c2_1[movid_2020$c2_1 != 8 & movid_2020$c2_1 != 9])
moda_c2_2 <- moda(movid_2020$c2_2[movid_2020$c2_2 != 8 & movid_2020$c2_2 != 9])
moda_c2_3 <- moda(movid_2020$c2_3[movid_2020$c2_3 != 8 & movid_2020$c2_3 != 9])
moda_c2_4 <- moda(movid_2020$c2_4[movid_2020$c2_4 != 8 & movid_2020$c2_4 != 9])

# Reemplazar los valores 8 y 9 con la moda
movid_2020 <- movid_2020 |> 
  mutate(c2_1 = case_when(c2_1 == 8 ~ moda_c2_1,
                          c2_1 == 9 ~ moda_c2_1,
                          TRUE ~ c2_1),
         c2_2 = case_when(c2_2 == 8 ~ moda_c2_2,
                          c2_2 == 9 ~ moda_c2_2,
                          TRUE ~ c2_2),
         c2_3 = case_when(c2_3 == 8 ~ moda_c2_3,
                          c2_3 == 9 ~ moda_c2_3,
                          TRUE ~ c2_3),
         c2_4 = case_when(c2_4 == 8 ~ moda_c2_4,
                          c2_4 == 9 ~ moda_c2_4,
                          TRUE ~ c2_4))

# Calcular el indice con los valores imputados
movid_2020 <-
  movid_2020 |>
  mutate(phq_4 = as.numeric(c2_1+c2_2+c2_3+c2_4-4)) # Se resta 4 porque la escala original va de 0 a 3, no de 1 a 4 como en esta BBDD. 

print(movid_2020 |> group_by(phq_4) |> tally(), n = 13)



# Ahora se presenta en una escala categorica

movid_2020 <- 
  movid_2020 |> 
  mutate(phq_4_categorico = case_when(phq_4 <= 2 ~ "No hay síntomas de ansiedad ni depresión",
                                      phq_4 <= 5 ~ "Posibles síntomas de ansiedad o depresión",
                                      phq_4 <= 8 ~ "Síntomas moderados de ansiedad y/o depresión",
                                      phq_4 <= 12 ~ "Síntomas severos de ansiedad y/o depresión"))




#  ++ Creación de los objetos survey ------------------------------------------

## Usando paquete srvyr

movid_2020_s <- 
  movid_2020 %>% 
  as_survey_design(ids = 1, # No cluster
                   probs = NULL, # Probabilidad de los cluster
                   strata = NULL, # Formula con los estratos
                   weights = factor_expansion)

## Usando paquete survey

# movid_2020_s <- svydesign(id = ~1, 
#                          weights = ~fact_exp, 
#                          data = movid_2020,
#                          strata = NULL,
#                          fpc= NULL,
#                          probs = NULL)



