#  ++ Lectura BBDD ------------------------------------------------------------

## Lectura bases 

movid_2022 <- read_dta("./input/2022/MOVID-IMPACT-2022.dta")

## Limpieza nombres columnas 

movid_2022 <- janitor::clean_names(movid_2022)

## Selecciona a las personas efectivamente entrevistadas de la bbdd

movid_2022 <- movid_2022 %>% filter(entrevistado == 1) # Seleccionar a los entrevistados.




# ++ Variables independientes ------------------------------------------------


# Sexo --------------------------------------------------------------------

# 1 hombre, 2 mujer
movid_2022 |> group_by(sexo) |> tally()


# Nivel educacional -------------------------------------------------------

# Opcion 1
movid_2022 <- movid_2022 %>% 
  mutate(educ_2 = as.factor(case_when(
    a7 == 1 | a7 == 2 ~ "Nunca asistió",
    a7 == 3 | a7 == 4 ~ "Básica",
    a7 == 5 | a7 == 6 ~ "Media",
    a7 == 7 | a7 == 8 ~ "Superior",
    a7 == 9 ~ "Postgrado")))

# Opcion 2: Se recomienda usar esta por IC
movid_2022 <- movid_2022 %>% 
  mutate(educ_3 = as.factor(case_when(
    a7 == 1 | a7 == 2 | a7 ==  3 |a7 == 4 ~ "Básica o nunca asistió",
    a7 == 5 | a7 ==6 ~ "Media",
    a7 == 7 | a7 ==8 | a7 == 9 ~ "Superior o postgrado")))


# Prevision de salud ------------------------------------------------------

movid_2022 <- 
  movid_2022 %>% 
  mutate(prevision = as.factor(
    case_when(
      a11 == 1 ~ "Fonasa",
      a11 == 2 ~ "Isapre",
      a11 == 5 ~ "Ninguna", 
      a11 == 3 | a11 == 4 ~ "Otra",
      a11 == 8 | a11 == 9 ~ "NS/NR")))

movid_2022 |> group_by(prevision) |> tally()


# Vacunacion --------------------------------------------------------------

# Personas con esquema inicial de vacunación completo
# 1 = Si, 2 = No. 
movid_2022 <- movid_2022 %>% 
  mutate(vacunacion = as.factor(case_when(
    (d1 == 1 & (d2 >= 2 | d3 == 5)) ~ "Esquema vacunacion inicial completo",
    (d1 == 2 | ((d3 == 1 | d3 == 2 | d3== 3 | d3 == 4) & d2 < 2 )) ~ "Sin esquema vacunacion inicial completo",
    TRUE ~ "Sin esquema vacunacion inicial completo"
  )))


levels(movid_2022$vacunacion)
movid_2022 |>  filter(vacunacion == 1) |> group_by(vacunacion) |> tally()
# Ocupados ----------------------------------------------------------------

movid_2022 <- 
  movid_2022 %>% 
  mutate(
     ocupacion_d = as.factor(case_when(
      g1 == 1 | g2 == 1 | g3 != 10 ~ "ocupados",
      g4 == 2  ~ "desocupados",
      TRUE ~ "fuera_fuerza_trabajo")))


# Tramo etario ------------------------------------------------------------

# Opcion 1: Desagregacion completa
movid_2022$edad_rec <- cut(movid_2022$edad,
                           include.lowest = TRUE,
                           right = TRUE,
                           breaks = c(18, 24, 34, 44, 54, 64, 80, Inf)
)

movid_2022 |> group_by(edad_rec) |> tally()

# Opcion 2: Desagregacion reducida (recomendable por IC)
movid_2022 <- movid_2022 %>% 
  mutate(edad_2 = as.factor(case_when(
    edad < 30 ~ "18 a 29 años",
    edad >= 30 & edad < 60 ~ "30 a 59 años",
    edad >= 60 ~ "60 años o más"
  )))


# Nivel de ingresos -------------------------------------------------------

# Se pregunta por ingresos del hogar, pero no se sabe de cuantas personas es el hogar, por lo tanto
# No se puede obtener ingreso percapita. 
# Por lo tanto, no se recomienda usar esta variable para análisis. 

# G8 responde nivel de ingresos continuo del hogar ? si/no
movid_2022 |> group_by(g8) |> tally()

# G8 monto ingresos continuo
movid_2022 |> group_by(g8_monto) |> tally()

# G9 Tramo ingresos
movid_2022 |> group_by(g9) |> tally()

# Conversion de tramos de ingresos a media de ingresos (variable continua)
movid_2022 <- 
  movid_2022 |> 
  mutate(tramo_ingresos_continuo = case_when(
    g9 == 1 ~ 100000,
    g9 == 2 ~ 275000,
    g9 == 3 ~ 425000,
    g9 == 4 ~ 650000,
    g9 == 5 ~ 1000000,
    g9 == 6 ~ 1600000,
    g9 == 7 ~ 3500000,
    g9 == 8 ~ 6000000
  ))

# Ingreso hogar total (con estimacion de ingresos continuos)
movid_2022 <- 
  movid_2022 |> 
  mutate(ingreso_hogar_completo = ifelse(g8 == 1, g8_monto, tramo_ingresos_continuo))

# Ingresos hogar por tramo de ingreso 

movid_2022$tramo_ingresos_hogar_completo <- cut(movid_2022$ingreso_hogar_completo,
                           include.lowest = TRUE,
                           right = TRUE,
                           ordered_result = TRUE,
                           labels = levels(haven::as_factor(movid_2022$g9))[1:8],
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
movid_2022 <- 
  movid_2022 |> 
  mutate(tramo_ingresos_hogar_completo = ifelse(is.na(tramo_ingresos_hogar_completo), 
                                                "NS/NR", 
                                                as.character(tramo_ingresos_hogar_completo)))
# Se transforma en factor
movid_2022$tramo_ingresos_hogar_completo <- 
  factor(movid_2022$tramo_ingresos_hogar_completo, 
         levels = c(levels(haven::as_factor(movid_2022$g9))[1:8], "NS/NR"))

# Variable final con el tramo de ingresos del hogar completo (visualizacion)
movid_2022 |> group_by(tramo_ingresos_hogar_completo) |>  tally()


# Enfermedad cronica ------------------------------------------------------

# IMPORTANTE: La pregunta de enfermedad crónica en año 2020 (c1) y 2022 (b2) difiere entre cuestionarios. 
# Se consideraron condiciones de salud mental como enfermedad crónica. 

# Personas con enfermedad cronica. 1 = 1  o más enfermedad cronicas / 0 = Sin enfermedad crónica. 
movid_2022 <- 
  movid_2022 %>% 
  mutate(cronica_d = ifelse(b2_a == 1 | b2_b == 1 | b2_c == 1  | b2_d == 1 |
                            b2_e == 1 | b2_f == 1 | b2_g == 1  | b2_h == 1 |
                            b2_i == 1 | b2_j == 1 | b2_k == 1  | b2_i == 1, 1, 0))

movid_2022 |> group_by(cronica_d) |> tally()
# Region ------------------------------------------------------------------

# # Ordenando region (no funciona)
# movid_2022$region <- 
#   ordered(movid_2022$region, 
#           levels = c(15, 1, 2, 3, 4, 5 ,6 ,7, 16, 8 , 9, 14, 10, 11, 12, 13), 
#           labels=c("Región de Arica y Parinacota", "Región de Tarapacá", "Región de Antofagasta", 
#                    "Región de Atacama", "Región de Coquimbo","Región de Valparaíso", "Región de O'Higgins",                     
#                    "Región del Maule", "Región de Ñuble", "Región del Bío Bío", "Región de la Araucanía",                  
#                    "Región de los Ríos", "Región de los Lagos", "Región de Aysén",  "Región de Magallanes y Antártica Chilena",
#                    "Región Metropolitana de Santiago"))          

movid_2022 %>% group_by(region) %>% tally()

# Región Poblacion
movid_2022 <- movid_2022 %>% mutate(region_d = as.factor(ifelse(region == 13, "RM", "Otra region")))
movid_2022 %>% group_by(region_d) %>% tally()


# Medios de información ---------------------------------------------------

## Transformar d11  y desagregaciones a dummy de 0 y 1. 
movid_2022 <- 
  movid_2022 |> 
  mutate(across(starts_with("d11"), ~ifelse(is.na(.), 0, 1), .names = "{.col}_d"))

## Nuevas variables dummies para reconocer medios de informacion

# Medios tradicionales: 1. Radio, 2. Television abierta, 3. diarios impresos o digitales. 
# RRSS: 4. Redes sociales
# Sitios noticias de internet: 5. Sitios noticias de internet
# Otras paginas web: 6. Otras paginas web
# Otro : 7. Otro
# NS/NR: 8. No sabe, 9. No responde

movid_2022 <- 
  movid_2022 |> 
  mutate(mi_tradicionales = as.factor(ifelse(d11_1_d == 1 | d11_2_d == 1 | d11_3_d == 1, 1, 0)),
         mi_rss = as.factor(ifelse(d11_4_d == 1, 1, 0)),
         mi_web_noticias = as.factor(ifelse(d11_5_d == 1, 1, 0)),
         mi_web_otras = as.factor(ifelse(d11_6_d == 1, 1, 0)),
         mi_otras = as.factor(ifelse(d11_7_d == 1, 1, 0))) 

## Se crea variable que revisa si la persona se informa por medios tradicionales, por internet, o ambos

# Medios tradicionales: 1. Radio, 2. Television abierta, 3. diarios impresos o digitales. 
# Internet:  4. Redes sociales, 5. Sitios noticias de internet, 6. Otras paginas web
# Ambos: Medios tradicionales e internet
# Otro/NS/NR : 7. Otro, 8. No sabe, 9. No responde

movid_2022 <- 
  movid_2022 |> 
  mutate(mi_web_o_tradicionales = as.factor(case_when(
    mi_tradicionales == 1 & (mi_rss == 1 | mi_web_noticias == 1 | mi_web_otras == 1) ~ "Ambos",
    mi_tradicionales == 1 ~ "Tradicionales",
    mi_rss == 1 | mi_web_noticias == 1 | mi_web_otras == 1 ~ "Internet",
    TRUE ~ "Otros/NS/NR")))


#  ++ Creación variables dependientes ----------------------------------------


# Antivacunas -------------------------------------------------------------

## d8.h Personas que estan en contra las vacunas en general. 

# Recordar que esta pregunta solo se hizo a personas que no tienen su esquema de vacunación completo 
# y  que  es poco o nada probable que se vacunen en las próximas dos semanas (79 personas en toda la muestra)

movid_2022 |> group_by(d8_h) |> tally()

# Ahora bien, se puede identificar a este grupo de "anti-vacunas" como aquellos que
# 1. no se han vacunado contra el coronavirus y 2. manifestan estar en contra de las vacunas en general. 
# IMPORTANTE : SOLO 13 PERSONAS EN LA MUESTRA. 

movid_2022 |> filter(d8_h == 1) |> group_by(d8_h) |> tally()

# Si bien no será un grupo  representantivo estadisticamente, se puede construir la variable "antivacunas"

movid_2022 <- 
  movid_2022 |> 
  mutate(antivacunas = case_when(d8_h == 1  ~ "Antivacunas",
                                 TRUE ~ "Acepta vacunas"))
movid_2022 |> group_by(antivacunas) |> tally()

# D12  Opinion vacunacion -------------------------------------------------

# Se recodifica para disminuir los IC.
movid_2022 <- 
  movid_2022 |> 
  mutate(across(starts_with("d12_"), 
                ~case_when(. %in% c(1:2) ~ "De acuerdo o muy de acuerdo",
                           . %in% c(3) ~ "Indiferente",
                           . %in% c(4:5) ~ "En desacuerdo o muy en desacuerdo",
                           . %in% c(8:9) ~ "NS/NR"),
                .names = "{.col}_recoded"))

# Comprobacion
movid_2022 |> select(starts_with("d12_") & ends_with("recoded"))


# E1: Modulo social -----------------------------------------------------------

movid_2022 <- 
  movid_2022 |> 
  mutate(across(starts_with("e1_"), 
                ~case_when(. %in% c(1:2) ~ "De acuerdo o muy de acuerdo",
                           . %in% c(3) ~ "Indiferente",
                           . %in% c(4:5) ~ "En desacuerdo o muy en desacuerdo",
                           . %in% c(8:9) ~ "NS/NR"),
                .names = "{.col}_recoded"))

# Comprobacion
movid_2022 |> select(starts_with("e1_") & ends_with("recoded"))


# Disposición vacunación anual --------------------------------------------

movid_2022 <- 
  movid_2022 |> 
  mutate(d13_recoded = case_when(d13 %in% c(1:2) ~ "Muy o bastante dispuesto",
                                 d13 %in% c(3) ~ "Algo dispuesto",
                                 d13 %in% c(4:5) ~ "Poco o nada dispuesto",
                                 d13 %in% c(8:9) ~ "NS/NR"))
movid_2022 |> group_by(d13_recoded) |> tally()


# D14 Opinion vacunación en niños -----------------------------------------

# Se recodifica para disminuir los IC.
movid_2022 <- 
  movid_2022 |> 
  mutate(d14_recoded = case_when(d14 %in% c(1:2) ~ "De acuerdo o muy de acuerdo",
                                 d14 %in% c(3) ~ "Indiferente",
                                 d14 %in% c(4:5) ~ "En desacuerdo o muy en desacuerdo",
                                 d14 %in% c(8:9) ~ "NS/NR"))
movid_2022 |> group_by(d14_recoded) |> tally()



# C7 respuesta a contacto estrecho sin sintomas  ----------------------------------------


# Posibles respuestas (se recodifican para poder ser sumadas)
# *Tuve que usar NA como referencia para que funcionara el ifelse

movid_2022 <- 
  movid_2022 |> 
  mutate(c7_1 = if_else(is.na(c7_1), 0, 1), # No haría nada c7_1 == 1
         c7_2 = if_else(is.na(c7_2), 0, 1), # Se aislaría c7_2 == 2
         c7_3 = if_else(is.na(c7_3), 0, 1), # Utilizaría mascarilla c7_3 == 3
         c7_4 = if_else(is.na(c7_4), 0, 1),
         c7_5 = if_else(is.na(c7_5), 0, 1),
         c7_6 = if_else(is.na(c7_6), 0, 1)) 

# Se haría un un test (test rapido  auto aplicado, aplicado por otro o PCR)

movid_2022 <- 
  movid_2022 |> 
  mutate(c7_realizar_test = ifelse(c7_4 == 1 | c7_5 == 1 | c7_6 == 1, 1, 0))

# Comprobación
movid_2022 |> group_by(c7_1) |> tally()
movid_2022 |> group_by(c7_2) |> tally()
movid_2022 |> group_by(c7_3) |> tally()
movid_2022 |> group_by(c7_realizar_test) |> tally()

## Creación de un indice de respuesta a contacto estrecha
# Cada una de las acciones descrita anteriormente suma un punto al indice. 
movid_2022 <- 
  movid_2022 |> 
  mutate(indice_respuesta_contacto_estrecho_sin_sintomas = ifelse(c7_1 == 1, 0, c7_2+c7_3+c7_realizar_test))

# Comprobación
movid_2022 |> group_by(indice_respuesta_contacto_estrecho_sin_sintomas) |> tally()



# C8 respuesta a contacto estrecho con sintomas  ----------------------------------------


# Posibles respuestas (se recodifican para poder ser sumadas)
# *Tuve que usar NA como referencia para que funcionara el ifelse

movid_2022 <- 
  movid_2022 |> 
  mutate(c8_1 = if_else(is.na(c8_1), 0, 1), # No haría nada c8_1 == 1
         c8_2 = if_else(is.na(c8_2), 0, 1), # Se aislaría c8_2 == 2
         c8_3 = if_else(is.na(c8_3), 0, 1), # Utilizaría mascarilla c8_3 == 3
         c8_4 = if_else(is.na(c8_4), 0, 1),
         c8_5 = if_else(is.na(c8_5), 0, 1),
         c8_6 = if_else(is.na(c8_6), 0, 1)) 

# Se haría un un test (test rapido  auto aplicado, aplicado por otro o PCR)

movid_2022 <- 
  movid_2022 |> 
  mutate(c8_realizar_test = ifelse(c8_4 == 1 | c8_5 == 1 | c8_6 == 1, 1, 0))

# Comprobación
movid_2022 |> group_by(c8_1) |> tally()
movid_2022 |> group_by(c8_2) |> tally()
movid_2022 |> group_by(c8_3) |> tally()
movid_2022 |> group_by(c8_realizar_test) |> tally()

## Creación de un indice de respuesta a contacto estrecha
# Cada una de las acciones descrita anteriormente suma un punto al indice. 
movid_2022 <- 
  movid_2022 |> 
  mutate(indice_respuesta_contacto_estrecho_con_sintomas = ifelse(c8_1 == 1, 0, c8_2+c8_3+c8_realizar_test))

# Comprobación
movid_2022 |> group_by(indice_respuesta_contacto_estrecho_con_sintomas) |> tally()

#  ++ Creación de los objetos survey ------------------------------------------

## Usando paquete srvyr

movid_2022_s <- 
  movid_2022 %>% 
  as_survey_design(ids = 1, # No cluster
                   probs = NULL, # Probabilidad de los cluster
                   strata = NULL, # Formula con los estratos
                   weights = fact_exp)

## Usando paquete survey

# movid_2022_s <- svydesign(id = ~1, # No cluster
#                           probs = NULL, # Probabilidad de los cluster
#                           strata = NULL, # Formula con los estratos
#                           weights = ~fact_exp, 
#                           data = movid_2022)

