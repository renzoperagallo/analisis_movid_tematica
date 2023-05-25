# 1 Discontinuidad tratamientos pacientes cronicos ------------------------

# Personas que han tenido que descontinuar algun control de salud en los ultimos 3 meses
# Encuesta realizada diciembre 2020, por lo tanto, entre septiembre y diciembre. 

pospuso_tratamiento <- 
  movid_2020_s |> 
  filter(cronica_d == 1) |> 
  group_by(pospuso_tratamiento) |> 
  summarize(proportion = survey_mean(vartype = "ci"))

pospuso_tratamiento_sexo <- 
movid_2020_s |> 
  filter(cronica_d == 1) |> 
  group_by(sexo, pospuso_tratamiento) |> 
  summarize(proportion = survey_mean(vartype = "ci"))

pospuso_tratamiento_edad <- 
movid_2020_s |> 
  filter(cronica_d == 1) |> 
  group_by(edad_2, pospuso_tratamiento) |> 
  summarize(proportion = survey_mean(vartype = "ci"))

# Razon por la que pospuso el tratamiento entre personas con enfermedad cronica y que pospusieron el tratamiento
razon_pospuso_tratamiento <- 
  movid_2020_s |> 
  filter(cronica_d == 1, pospuso_tratamiento == "Pospuso tratamiento", razon_pospuso_tratamiento %in%(1:9)) |> 
  group_by(razon_pospuso_tratamiento) |> 
  summarize(proportion = survey_mean(vartype = "ci"))


# Pacientes cronicos que tuvieron que posponer alguna de las sigientes atenciones de salud planificadas
# en los últimos 3 meses. 

# Consultas médicas

postergacion_consultas_medicas <- 
movid_2020_s |> 
  filter(cronica_d == 1, !is.na(e5_1), e5_1 %in% c(1:2)) |> 
  group_by(e5_1) |> 
  summarize(proportion = survey_mean(vartype = "ci"))

# Toma de examenes de laboratorio o imagenológicos
postergacion_examenes <- 
movid_2020_s |> 
  filter(cronica_d == 1, !is.na(e5_4), e5_4 %in% c(1:2)) |> 
  group_by(e5_4) |> 
  summarize(proportion = survey_mean(vartype = "ci"))

# Cirugías electivas
postergacion_cirugias_electivas <- 
movid_2020_s |> 
  filter(cronica_d == 1, !is.na(e5_8), e5_8 %in% c(1:2)) |> 
  group_by(e5_8) |> 
  summarize(proportion = survey_mean(vartype = "ci"))


# Consultas médicas segun fonasa  o isapre en pacientes crónicos

postergacion_consultas_medicas_prevision <- 
movid_2020_s |> 
  filter(cronica_d == 1, !is.na(e5_1), e5_1 %in% c(1:2), prevision %in% c("Fonasa", "Isapre")) |> 
  group_by(prevision, e5_1) |> 
  summarize(proportion = survey_mean(vartype = "ci"))

# Toma de examenes de laboratorio o imagenológicos segun fonasa  o isapre en pacientes crónicos
postergacion_examenes_prevision <- 
movid_2020_s |> 
  filter(cronica_d == 1, !is.na(e5_4), e5_4 %in% c(1:2), prevision %in% c("Fonasa", "Isapre")) |> 
  group_by(prevision, e5_4) |> 
  summarize(proportion = survey_mean(vartype = "ci"))

# Cirugías electivas segun fonasa  o isapre en pacientes crónicos
postergacion_cirugias_electivas_prevision <- 
movid_2020_s |> 
  filter(cronica_d == 1, !is.na(e5_8), e5_8 %in% c(1:2), prevision %in% c("Fonasa", "Isapre")) |> 
  group_by(prevision, e5_8) |> 
  summarize(proportion = survey_mean(vartype = "ci"))



# 2. Discontinuidad a exámenes diagnósticos asociados a cancer -----------
# (pap-mamo)

# Disontiuidad  papanicolau, mamografías u otros (solo mujeres)
examen_cancer_pospuesto_mujeres <- 
  movid_2020_s |> 
  filter(!is.na(e5_5), e5_5 %in% c(1:2), sexo == 2) |> 
  group_by(e5_5) |> 
  summarize(proportion = survey_mean(vartype = "ci"))

# Razon de la discontinuidad 
razon_examen_cancer_pospuesto_mujeres <- 
movid_2020_s |> 
  filter(e5_5 == 1, sexo == 2, !e6==98) |> 
  group_by(e6) |> 
  summarize(proportion = survey_mean(vartype = "ci"))

# Disontiuidad  antigeno prostaico, colonoscopía u otros (solo hombres)
examen_cancer_pospuesto_hombres <- 
  movid_2020_s |> 
  filter(!is.na(e5_6), e5_6 %in% c(1:2), sexo == 1) |> 
  group_by(e5_6) |>   
  summarize(proportion = survey_mean(vartype = "ci"))

# Razon de la discontinuidad 
razon_examen_cancer_pospuesto_hombres <- 
movid_2020_s |> 
  filter(e5_6 == 1, sexo == 1) |> 
  group_by(e6) |> 
  summarize(proportion = survey_mean(vartype = "ci"))


movid_2020 |> group_by(sexo) |> tally()

# 3. Discontinuidad del programa de inmunizaciones y control de nino sano --------







# 4. Disposición vacunal en adultos (mayores de 18 años)  -----------------

## Personas con esquema inicial completo según la encuesta
esquema_vacunacion_incial_completo <- 
movid_2022_s |> 
  group_by(vacunacion) |> 
  summarize(proportion = survey_mean(vartype = "ci"))



## ¿Por qué razon no se han vacunado?
# Solo aquellas personas que en d7 respondiendo que "4. Es poco probable que me vacune" o 
# "5. No me vacunaría por ningún motivo". 

razones_no_vacunacion <- c("d8_a", "d8_b","d8_c", "d8_d", "d8_e", "d8_f","d8_g", "d8_h", "d8_i")

# Tabulados razones de no vacunacion. OJO, la funcion utilizada esta en script 1
tabulados_razones_no_vacunacion <- map(razones_no_vacunacion, ~calculo_razones_no_vacunacion(movid_2022_s, .x))

# Seleccion de los principales (principales razones mencioandas) (d, e, f, h)
tabulados_razones_no_vacunacion_main <- c(tabulados_razones_no_vacunacion[4:6], tabulados_razones_no_vacunacion[8])

# Renombrar columnas 
for (i in c(1:4)){
  names(tabulados_razones_no_vacunacion_main[[i]]) <-  c("si_no", "proportion", "proportion_low", "proportion_upp")
  tabulados_razones_no_vacunacion_main[[i]] <- 
    tabulados_razones_no_vacunacion_main[[i]] |> 
    mutate(si_no = recode(as.factor(si_no), 
                          '1' = "Si",
                          '2' = "No"))
}


tabulados_razones_no_vacunacion_main[[1]] <- 
  tabulados_razones_no_vacunacion_main[[1]] |> 
  mutate(razon = "Preocupación posibles efectos adversos de la vacuna") |> 
  select(razon, everything())

tabulados_razones_no_vacunacion_main[[2]] <- 
  tabulados_razones_no_vacunacion_main[[2]] |> 
  mutate(razon = "No está seguro que la vacuna funcione contra el coronavirus") |> 
  select(razon, everything())

tabulados_razones_no_vacunacion_main[[3]] <- 
  tabulados_razones_no_vacunacion_main[[3]] |> 
  mutate(razon = "Prefiere esperar a ver que la vacuna sea segura") |> 
  select(razon, everything())

tabulados_razones_no_vacunacion_main[[4]] <- 
  tabulados_razones_no_vacunacion_main[[4]] |> 
  mutate(razon = "Está en contra de las vacunas en general") |> 
  select(razon, everything())

tabulados_razones_no_vacunacion_df <- rbind(tabulados_razones_no_vacunacion_main[[1]], 
                                            tabulados_razones_no_vacunacion_main[[2]],
                                            tabulados_razones_no_vacunacion_main[[3]],
                                            tabulados_razones_no_vacunacion_main[[4]])



## Evaluación preguntas likert

# Confianza en la seguridad de las vacunas
confianza_vacunas <- 
movid_2022_s |> 
  filter(!d12_1_recoded == "NS/NR") |> 
  group_by(d12_1_recoded) |> 
  summarize(proportion = survey_mean(vartype = "ci"))

# Confianza en las autoridades sanitarias
confianza_autoridades_sanitarias <- 
movid_2022_s |> 
  filter(!d12_2_recoded == "NS/NR") |> 
  group_by(d12_2_recoded) |> 
  summarize(proportion = survey_mean(vartype = "ci"))

# Importancia de la vacunación
importancia_vacunacion <- 
movid_2022_s |> 
  filter(!d12_3_recoded == "NS/NR") |> 
  group_by(d12_3_recoded) |> 
  summarize(proportion = survey_mean(vartype = "ci"))

# Importancia de la vacunación según tramo etario
importancia_vacunacion_edad <- 
movid_2022_s |> 
  filter(!d12_3_recoded == "NS/NR") |> 
  group_by(edad_2, d12_3_recoded) |> 
  summarize(proportion = survey_mean(vartype = "ci"))


# Importancia de la vacunación según sexo
importancia_vacunacion_sexo <-
movid_2022_s |> 
  filter(!d12_3_recoded == "NS/NR") |> 
  group_by(sexo, d12_3_recoded) |> 
  summarize(proportion = survey_mean(vartype = "ci"))


# Importancia de la vacunación según medios de información
importancia_vacunacion_medios <- 
movid_2022_s |> 
  filter(!d12_3_recoded %in% c("NS/NR","Indiferente"), !mi_web_o_tradicionales == "Otros/NS/NR") |> 
  group_by(mi_web_o_tradicionales, d12_3_recoded) |> 
  summarize(proportion = survey_mean(vartype = "ci"))

# Mis amigos y familiares creen que es importante vacunarse
movid_2022_s |> 
  filter(!d12_4_recoded == "NS/NR") |> 
  group_by(d12_4_recoded) |> 
  summarize(proportion = survey_mean(vartype = "ci"))



## Disposición a vacunación anual 

# General

disposicion_vacunacion_anual <- 
  movid_2022_s |> 
  filter(!d13_recoded == "NS/NR") |> 
  group_by(d13_recoded) |> 
  summarize(proportion = survey_mean(vartype = "ci"))


# Por sexo
disposicion_vacunacion_anual_sexo <- 
movid_2022_s |> 
  filter(!d13_recoded == "NS/NR") |> 
  group_by(sexo, d13_recoded) |> 
  summarize(proportion = survey_mean(vartype = "ci"))



# Por edad
disposicion_vacunacion_anual_edad <- 
movid_2022_s |> 
  filter(!d13_recoded == "NS/NR") |> 
  group_by(edad_2, d13_recoded) |> 
  summarize(proportion = survey_mean(vartype = "ci"))


# 5. Disposicion vacunal en ninos -----------------------------------------
# (desde los 6 meses considerando grupos de edad y diferentes vacunas) 

 
# Importancia de la vacunación en ninos 
vacunacion_ninos <- 
movid_2022_s |> 
  filter(!d14_recoded %in% c("NS/NR")) |> 
  group_by( d14_recoded) |> 
  summarize(proportion = survey_mean(vartype = "ci"))



# Importancia de la vacunación en ninos según medios de información (se filtran neutrales)
vacunacion_ninos_medios <- 
movid_2022_s |> 
  filter(!d14_recoded %in% c("NS/NR","Indiferente"), !mi_web_o_tradicionales == "Otros/NS/NR") |> 
  group_by(mi_web_o_tradicionales, d14_recoded) |> 
  summarize(proportion = survey_mean(vartype = "ci"))

# Importancia de la vacunación en ninos según edad (se filtran neutrales)
vacunacion_ninos_edad <- 
movid_2022_s |> 
  filter(!d14_recoded %in% c("NS/NR","Indiferente"),  ) |> 
  group_by(edad_2, d14_recoded) |> 
  summarize(proportion = survey_mean(vartype = "ci"))

# Importancia de la vacunación en ninos según sexo (se filtran neutrales)
vacunacion_ninos_sexo <- 
movid_2022_s |> 
  filter(!d14_recoded %in% c("NS/NR","Indiferente") ) |> 
  group_by(sexo, d14_recoded) |> 
  summarize(proportion = survey_mean(vartype = "ci"))


## Disposición vacunal en niños y adultos 
# Se exckuye kla categiría algo dispuesto (neutra) en d13 para simplificar el gráfico. 
disposicion_vacunacion_ninos_adultos <- 
  movid_2022_s |> 
  filter(!d13_recoded %in% c("NS/NR", "Algo dispuesto"), !d14_recoded %in% c("NS/NR")) |> 
  group_by(d13_recoded, d14_recoded) |> 
  summarize(proportion = survey_mean(vartype = "ci"))


# 6. Medidas de autocuidado y protección ante sospecha de COVID-19 ---------


## Indice de acciones realizadas si la persona es contacto estrecho
# Donde  se evaluan tres acciones (se aislaría, utilizaría mascarilla al interior del hogar o 
# en lugares cerrados y se  realizaría un test rapido o examen pcr). El número indica cuantas de dichas 
# acciones realizaría la persona. 


indice_respuesta_contacto_estrecho_sin_sintomas <- 
  movid_2022_s |> 
  filter() |> 
  group_by(as.factor(indice_respuesta_contacto_estrecho_sin_sintomas)) |> 
  summarize(proportion = survey_mean(vartype = "ci")) |> 
  mutate(sintomas = "sin sintomas") |> 
  select(sintomas, indice_respuesta_contacto_estrecho =`as.factor(indice_respuesta_contacto_estrecho_sin_sintomas)`, everything())

indice_respuesta_contacto_estrecho_con_sintomas <- 
  movid_2022_s |> 
  filter() |> 
  group_by(as.factor(indice_respuesta_contacto_estrecho_con_sintomas)) |> 
  summarize(proportion = survey_mean(vartype = "ci")) |> 
  mutate(sintomas = "con sintomas") |> 
  select(sintomas, indice_respuesta_contacto_estrecho = `as.factor(indice_respuesta_contacto_estrecho_con_sintomas)`, everything())


indice_respuesta_contacto_estrecho_comparacion <- rbind(indice_respuesta_contacto_estrecho_sin_sintomas, 
                                                        indice_respuesta_contacto_estrecho_con_sintomas)
# Orden de la variable para gráfico
indice_respuesta_contacto_estrecho_comparacion$sintomas <- 
  factor(indice_respuesta_contacto_estrecho_comparacion$sintomas,
         levels = c("sin sintomas", "con sintomas"))



## Detalle y comparación de acciones mencionadas 

# mascarilla
utilizar_mascarilla_contacto_estrecho_con_sintomas <- 
movid_2022_s |> 
  filter() |> 
  group_by(as.factor(c8_3)) |> 
  summarize(proportion = survey_mean(vartype = "ci")) |> 
  mutate(accion = "Utilizar mascarilla en lugares cerrados") |> 
  select(accion, si_no = `as.factor(c8_3)`, everything()) |> 
  mutate(si_no = recode(si_no, `0` = "No se menciona espontaneamente", `1` = "Se menciona espontaneamente"))

# Aislamiento
aislamiento_contacto_estrecho_con_sintomas <- 
  movid_2022_s |> 
  filter() |> 
  group_by(as.factor(c8_2)) |> 
  summarize(proportion = survey_mean(vartype = "ci")) |> 
  mutate(accion = "Aislamiento") |> 
  select(accion, si_no = `as.factor(c8_2)`, everything()) |> 
  mutate(si_no = recode(si_no, `0` = "No se menciona espontaneamente", `1` = "Se menciona espontaneamente"))

# Realización test
test_contacto_estrecho_con_sintomas <- 
  movid_2022_s |> 
  filter() |> 
  group_by(as.factor(c8_realizar_test)) |> 
  summarize(proportion = survey_mean(vartype = "ci")) |> 
  mutate(accion = "Realizar test COVID") |> 
  select(accion, si_no = `as.factor(c8_realizar_test)`, everything()) |> 
  mutate(si_no = recode(si_no, `0` = "No se menciona espontaneamente", `1` = "Se menciona espontaneamente"))


# Comparacion

comparacion_acciones_contacto_estrecho_con_sintomas <- rbind(utilizar_mascarilla_contacto_estrecho_con_sintomas,
                                                             aislamiento_contacto_estrecho_con_sintomas,
                                                             test_contacto_estrecho_con_sintomas)

graph_barras_bivariado(comparacion_acciones_contacto_estrecho_con_sintomas,
                       titulo = "Acciones a realizar en caso de contacto estrecho y presentar sintomas")



## Desagregaciones 

# Medidas de autocuidado y proteccion antes sospecha covid según medios de información 
indice_respuesta_contacto_estrecho_con_sintomas_medios <- 
  movid_2022_s |> 
  filter(!mi_web_o_tradicionales == "Otros/NS/NR") |> 
  group_by(mi_web_o_tradicionales, as.factor(indice_respuesta_contacto_estrecho_con_sintomas)) |> 
  summarize(proportion = survey_mean(vartype = "ci"))

# Medidas de autocuidado y proteccion antes sospecha covid según edad 
indice_respuesta_contacto_estrecho_con_sintomas_edad <- 
  movid_2022_s |> 
  filter() |> 
  group_by(edad_2, as.factor(indice_respuesta_contacto_estrecho_con_sintomas)) |> 
  summarize(proportion = survey_mean(vartype = "ci"))

# Medidas de autocuidado y proteccion antes sospecha covid según sexo
indice_respuesta_contacto_estrecho_con_sintomas_sexo <- 
  movid_2022_s |> 
  filter() |> 
  group_by(sexo, as.factor(indice_respuesta_contacto_estrecho_con_sintomas)) |> 
  summarize(proportion = survey_mean(vartype = "ci"))

# Medidas de autocuidado y proteccion antes sospecha covid según si la persona es enferma crónica 
indice_respuesta_contacto_estrecho_con_sintomas_cronica <- 
  movid_2022_s |> 
  filter() |> 
  group_by(cronica_d, as.factor(indice_respuesta_contacto_estrecho_con_sintomas)) |> 
  summarize(proportion = survey_mean(vartype = "ci"))




# 7. Aceptabilidad de estrategias para aumentar tasa de vacunación  --------
# ante COVID-19 (percepción que tengan las personas de estas estrategias)

## d12.9 Considera que el pase de movilidad es una motivación importante para que las personas se vacunen?

motivacion_pase <- 
  movid_2022_s |> 
  filter(!d12_9_recoded == "NS/NR") |> 
  group_by(d12_9_recoded) |> 
  summarize(proportion = survey_mean(vartype = "ci"))

# Desagergada por sexo
motivacion_pase_sexo <-
movid_2022_s |> 
  filter(!d12_9_recoded == "NS/NR") |> 
  group_by(sexo, d12_9_recoded) |> 
  summarize(proportion = survey_mean(vartype = "ci"))

# Desagregada por medios de informacion
motivacion_pase_medios <-
movid_2022_s |> 
  filter(!d12_9_recoded == "NS/NR", !mi_web_o_tradicionales == "Otros/NS/NR") |> 
  group_by(mi_web_o_tradicionales, d12_9_recoded) |> 
  summarize(proportion = survey_mean(vartype = "ci"))

# Desagregada por tramo etario
motivacion_pase_edad <-
movid_2022_s |> 
  filter(!d12_9_recoded == "NS/NR") |> 
  group_by(edad_2, d12_9_recoded) |> 
  summarize(proportion = survey_mean(vartype = "ci"))

## Acatación normas  autoridades sanitarias, comparacion año 2020 (f3_3) y 2022 (e1_4)

#Año 2020
acata_normas_2020 <- 
movid_2020_s |> 
  filter(!f3_3_recoded == "NS/NR") |> 
  group_by(f3_3_recoded) |> 
  summarize(proportion = survey_mean(vartype = "ci")) |> 
  mutate(ano = "2020") |> 
  select(ano, valoracion = f3_3_recoded,  everything())
# Año 2022
acata_normas_2022 <- 
movid_2022_s |> 
  filter(!e1_4_recoded == "NS/NR") |> 
  group_by(e1_4_recoded) |> 
  summarize(proportion = survey_mean(vartype = "ci")) |> 
  mutate(ano = "2022") |> 
  select(ano, valoracion = e1_4_recoded,  everything())

# Tabulado con comparacion de los anos

acata_normas_comparacion_anos <- rbind(acata_normas_2020, acata_normas_2022)
acata_normas_comparacion_anos


## Aprobación del gobierno y sus medidas sanitarias, comparacion año 2020 (f3_4) y 2022 (e1_5)

#Año 2020
aprobacion_gobierno_2020 <- 
  movid_2020_s |> 
  filter(!f3_4_recoded == "NS/NR") |> 
  group_by(f3_4_recoded) |> 
  summarize(proportion = survey_mean(vartype = "ci")) |> 
  mutate(ano = "2020") |> 
  select(ano, valoracion = f3_4_recoded,  everything())
# Año 2022
aprobacion_gobierno_2022 <- 
  movid_2022_s |> 
  filter(!e1_5_recoded == "NS/NR") |> 
  group_by(e1_5_recoded) |> 
  summarize(proportion = survey_mean(vartype = "ci")) |> 
  mutate(ano = "2022") |> 
  select(ano, valoracion = e1_5_recoded,  everything())

# Tabulado con comparacion de los anos

aprobacion_gobierno_comparacion_anos <- rbind(aprobacion_gobierno_2020, aprobacion_gobierno_2022)
aprobacion_gobierno_comparacion_anos


# 8. Aceptabilidad inmunizaciones en general post COVID-19  ----------------
# (reticencia vacunal) 

## d8.h Personas que estan en contra las vacunas en general. 

# Recordar que esta pregunta solo se hizo a personas que no tienen su esquema de vacunación completo 
# y  que  es poco o nada probable que se vacunen en las próximas dos semanas 

# Muestra
muestra_antivacuna <- 
movid_2022 |> 
  filter() |> 
  group_by(d8_h) |> 
  tally()

#Estimacion poblacional
antivacunas <- 
movid_2022_s |> 
  filter() |> 
  group_by(d8_h) |> 
  summarize(proportion = survey_mean(vartype = "ci"))



# Ahora bien, se puede identificar a este grupo de "anti-vacunas" como aquellos que
# 1. no se han vacunado contra el coronavirus y 2. manifestan estar en contra de las vacunas en general. 

# En base a los resultados, se observa que alrededor del 1% (IC95% 0.27% - 1.74%)  de la población objetivo de la encuesta  
# estaría encontra de las vacunas en general (antivacunas)

movid_2022_s |> 
  filter() |> 
  group_by(antivacunas) |> 
  summarize(proportion = survey_mean(vartype = "ci"))

# Si bien no se recomienda hacer mayores desagragaciones ya que la muestra es muy pequeña y podría no
# ser representativa (solo 13 casos), se entrega una descripciòn mas detallada de este grupo

# IMPORTANTE: DADO QUE ESTE ANÁLISIS ES DISTINTO POR EL BAJO N DE LOS ANTIVACUNAS, SE PROCEDIO A DESCRIBIR ESA MUESTRA
# POR ESO SE USA FUNCION FILTER Y NO GROUP_BY PARA LAS DESAGREGACIONES

# Sexo
muestra_antivacunas_sexo <- 
  movid_2022 |> 
  filter(antivacunas == "Antivacunas") |> 
  group_by(sexo) |> 
  tally()|> 
  rename(Sexo = sexo) |> 
  mutate(Sexo = recode(as.factor(Sexo), '1' = "Hombre", '2' = "Mujer"))


# Tramo etario
muestra_antivacunas_edad <- 
  movid_2022 |> 
  filter(antivacunas == "Antivacunas") |> 
  group_by(edad_2) |> 
  tally()|> 
  rename(`Tramo etario` = edad_2) 

# Medios de información 
muestra_antivacunas_medios <- 
  movid_2022 |> 
  filter(antivacunas == "Antivacunas") |> 
  group_by(mi_web_o_tradicionales) |> 
  tally()|> 
  rename(`Principal medio de información` = mi_web_o_tradicionales) 

# e1_1 Medicina alternativa
muestra_antivacunas_medicina_alternativa <- 
movid_2022 |> 
  filter(antivacunas == "Antivacunas") |> 
  group_by(e1_1_recoded) |> 
  tally() |> 
  rename(`La medicina alternativa puede ser más efectiva que la convencional para enfrentar la pandemia` = e1_1_recoded) 

# e1_2 Conspiracionismo
muestra_antivacunas_conspiracionismo <- 
movid_2022 |> 
  filter(antivacunas == "Antivacunas") |> 
  group_by(e1_2_recoded) |> 
  tally()|> 
  rename(`La pandemia COVID19 fue creada intencionalmente, con fines políticos y económicos` = e1_2_recoded) 


muestra_antivacunas_sexo
muestra_antivacunas_edad
muestra_antivacunas_medios
muestra_antivacunas_medicina_alternativa
muestra_antivacunas_conspiracionismo

# 9. Prevalencia de sintomatología post COVID-19 (Long COVID)  -------------

## Año 2022

# c6. Personas que tuvieron sintomas que duraron màs de un mes por covid
long_covid_2022 <-
  movid_2022_s |> 
  filter(c1 == 1, !c6 == 8, !c6 == 9) |> 
  group_by(c6) |> 
  summarize(proportion = survey_mean(vartype = "ci"))|> 
  mutate(ano = "2022") |> 
  select(ano, long_covid = c6, everything())

# Sexo
long_covid_2022_sexo <- 
movid_2022_s |> 
  filter(c1 == 1, !c6 == 8, !c6 == 9) |> 
  group_by(sexo, c6) |> 
  summarize(proportion = survey_mean(vartype = "ci"))

# Tramo etario
long_covid_2022_edad <- 
movid_2022_s |> 
  filter(c1 == 1, !c6 == 8, !c6 == 9) |> 
  group_by(edad_2, c6) |> 
  summarize(proportion = survey_mean(vartype = "ci"))

# Pacientes cronicos
long_covid_2022_cronicos <- 
movid_2022_s |> 
  filter(c1 == 1, !c6 == 8, !c6 == 9) |> 
  group_by(cronica_d, c6) |> 
  summarize(proportion = survey_mean(vartype = "ci"))

# Prevision de salud
long_covid_2022_prevision <- 
movid_2022_s |> 
  filter(c1 == 1, !c6 == 8, !c6 == 9, prevision %in% c("Fonasa", "Isapre")) |> 
  group_by(prevision, c6) |> 
  summarize(proportion = survey_mean(vartype = "ci"))

# Hospitalizados 
# c5 = Hospitalizados, c6 = Sintomas que duraron màs de un mes.
long_covid_2022_hospitalizados <- 
movid_2022_s |> 
  filter(c1 == 1, !c6 == 8, !c6 == 9, c5 %in% c(1:2)) |> 
  group_by(c5, c6) |> 
  summarize(proportion = survey_mean(vartype = "ci"))


## Año 2020

# d9. Personas que tuvieron sintomas que duraron màs de un mes por covid 
long_covid_2020 <- 
  movid_2020_s |> 
  filter(d6 == 1, !d9 == 8, !d9 == 9) |> 
  group_by(d9) |> 
  summarize(proportion = survey_mean(vartype = "ci")) |> 
  mutate(ano = "2020") |> 
  select(ano, long_covid = d9, everything())

# Sexo

movid_2020_s |> 
  filter(d6 == 1, !d9 == 8, !d9 == 9) |> 
  group_by(sexo, d9) |> 
  summarize(proportion = survey_mean(vartype = "ci"))

# Tramo etario

movid_2020_s |> 
  filter(d6 == 1, !d9 == 8, !d9 == 9) |> 
  group_by(edad_2, d9) |> 
  summarize(proportion = survey_mean(vartype = "ci"))

# Pacientes cronicos

movid_2020_s |> 
  filter(d6 == 1, !d9 == 8, !d9 == 9) |> 
  group_by(cronica_d, d9) |> 
  summarize(proportion = survey_mean(vartype = "ci"))

# Prevision de salud

movid_2020_s |> 
  filter(d6 == 1, !d9 == 8, !d9 == 9, prevision %in% c("Fonasa", "Isapre")) |> 
  group_by(prevision, d9) |> 
  summarize(proportion = survey_mean(vartype = "ci"))

# Hospitalizados 
# d8 = Hospitalizados, d9 = Sintomas que duraron màs de un mes
movid_2020_s |> 
  filter(d6 == 1, !d9 == 8, !d9 == 9, d8 %in% c(1:2)) |> 
  group_by(d8, d9) |> 
  summarize(proportion = survey_mean(vartype = "ci"))


## Comparacion años 2020 y 2022

long_covid_comparacion_anos <- rbind(long_covid_2020, long_covid_2022)
long_covid_comparacion_anos <- 
  long_covid_comparacion_anos |> 
  mutate(long_covid = recode(as.factor(long_covid), 
                             '1' = "Sintomas por más de un mes", 
                             '2' = "Sin sintomas por más de un mes"))

graph_barras_bivariado(long_covid_comparacion_anos, 
                       titulo = "Personas con sintomas que duraron más de un mes tras COVID-19")

# 10. Prevalencia de patologías de salud mental post pandemia por COVID ----
# Se utiliza indice PHQ-4 (ver script 2 para más detalles sobre su construcción)


## Indice salud mental año 2020.
salud_mental_2020 <- 
  movid_2020_s |> 
  filter() |> 
  group_by(phq_4_categorico) |> 
  summarize(proportion = survey_mean(vartype = "ci"))

# Sexo
salud_mental_2020_sexo <- 
movid_2020_s |> 
  filter() |> 
  group_by(sexo, phq_4_categorico) |> 
  summarize(proportion = survey_mean(vartype = "ci"))
# Tramo etario
salud_mental_2020_edad <- 
movid_2020_s |> 
  filter() |> 
  group_by(edad_2, phq_4_categorico) |> 
  summarize(proportion = survey_mean(vartype = "ci"))
# Region
salud_mental_2020_region <- 
movid_2020_s |> 
  filter() |> 
  group_by(region_d, phq_4_categorico) |> 
  summarize(proportion = survey_mean(vartype = "ci"))



# Region completa (solo categoria  sin  sintomas depresion o ansiedad) CI MUY ALTOS
salud_mental_region_completa_prop <- 
  movid_2020_s |> 
  filter() |> 
  group_by(region, phq_4_categorico) |> 
  summarize(proportion = survey_mean(vartype = "ci")) |> 
  filter(phq_4_categorico == "No hay síntomas de ansiedad ni depresión") |> 
  arrange(proportion)

# Indice cuantitativo CI MUY ALTOS
salud_mental_region_completa_mean <- 
  movid_2020_s |> 
  filter() |> 
  group_by(region) |> 
  summarize(promedio = survey_mean(phq_4, vartype = "ci")) |> 
  arrange(promedio)



graph_barras_univariado(salud_mental_2020, 
                       titulo = "Personas con sintomas que duraron más de un mes tras COVID-19")

## Prevalencia condiciones de salud mental diagnosticadas, comparacion año 2020 y 2022


#Año 2020
diagnostico_salud_mental_2020 <- 
  movid_2020_s |> 
  filter() |> 
  group_by(c1_5) |> 
  summarize(   proportion = survey_mean(vartype = "ci")) |> 
  mutate(ano = "2020") |> 
  select(ano, valoracion = c1_5,  everything()) |> 
  mutate(valoracion = ifelse(valoracion == 0, "Sin condición de salud mental", "Con condición de salud mental diagnosticada"))
# Año 2022
diagnostico_salud_mental_2022 <- 
  movid_2022_s |> 
  filter() |> 
  group_by(b2_l) |> 
  summarize(proportion = survey_mean(vartype = "ci")) |> 
  mutate(ano = "2022") |> 
  select(ano, valoracion = b2_l,  everything()) |> 
  mutate(valoracion = ifelse(valoracion == 2, "Sin condición de salud mental", "Con condición de salud mental diagnosticada"))


# Tabulado con comparacion de los anos

diagnostico_salud_mental_comparacion_anos <- rbind(diagnostico_salud_mental_2020, diagnostico_salud_mental_2022)
diagnostico_salud_mental_comparacion_anos

graph_barras_bivariado(diagnostico_salud_mental_comparacion_anos, 
                        titulo = "Prevalencia condiciones de salud mental diagnosticada")
