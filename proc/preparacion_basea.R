# 0. Identificación  -----------------------------------------------------

#Título: Código de procesamiento  de base de datos  para artículo sobre jóvenes de Centroamérica
#Institución: Centro de Estudios de Conflicto y Cohesión Social (COES)

# Resumen ejectuvi: Este script de procesamiento contiene el código que permite elaborar la Base de Datos A, siendo esta la que se ocupará para los análisis del articulo principal.

# La Base de Datos A incluye las variables de autoritarismo (AC) y las variables de confianza en las instituciones (CONF) como indices.

# 1. Cargar librerías --------------------------------------------------------

#install.packages("pacman") #Instalamos el package manager para cargar eficientemente las librerías
#library(pacman) #Cargamos el package manager

#Cargamos las librerías a utilizar en el procesamiento
pacman::p_load(haven, #Para cargar los datos .sav
               tidyverse,#Para manipulación de datos
               car,#Para transformación de variables 
               sjmisc,
               summarytools) #Para explorar datos y hacer tablas

# 2. Cargar datos ------------------------------------------------------------

datos <- read_sav("input/data/Centroamerica_desgarrada_ SandovalR.sav")


# 3. Manipulación de los datos --------------------------------------------
datos_proc_a <- datos %>%
  
  # 3.1 Selección de variables a utilizar ----------------------------------

dplyr::select( 
  # Sociodemographic
  pais = PAIS, # Categorical (5) -> Los números entreparentesis corresponden a la cantidad de categorías
  sexo = P02_SEXO, # Categorical (2)
  edad = P03_EDAD, # Numeric
  trabajo_remunerado = P04_TRABAJO_REMUNERADO, # Categorical (2)
  horas_trabajo = P06_HORAS_DE_TRABAJO, # Numeric
  personas_casa = P49_NUMERO_PERSONAS_CASA, # Numeric
  seguro_social = P07_SEGURO_SOCIAL, # Categorical (2)
  
  # Dinero
  
  nivel_educ = P12_ULTIMO_ANO_DE_ESTUDIO, # Categorical (11)
  dinero_comida = P23_DINERO_PARA_COMIDA, # Categorical (7) # Reverse coding # Done 
  dinero_casa = P24_DINERO_PARA_CASA, # Categorical (7) # Reverse coding # Done
  dinero_buses = P25_DINERO_PARA_BUSES, # Categorical (7) # Reverse coding
  
  # Victima de violencia
  
  victimas_violencia = P27_VICTIMAS_DE_VIOLECIA, # Categorical (5) # Reverse coding
  
  # Situaciones encuestado
  
  sit_personal_pas= P34_1SITUACION_PERSONAL_5_ANOS_B, # Categorical (3)
  sit_comunidad_pas = P34_2SITUACION_COMUNIDAD_5_ANOT, # Categorical (3)
  sit_municipio_pas = P34_3SITUACION_MUNICIPIO_5_ANOT, # Categorical(3)  
  sit_pais_pas = P34_4SITUACION_PAIS_5_ANOS_ATRAT, # Categorical (3)
  sit_personal = P35_1SITUACION_PERSONAL, # Categorical (3)
  sit_comunidad = P35_2SITUACION_COMUNIDAD, # Categorical (3)
  sit_municipio = P35_3SITUACION_MUNICIPIO, # Categorical (3)
  sit_pais = P35_4SITUACION_PAIS, # Categorical (3)
  sit_personal_fut = P36_1SITUACION_PERSONAL_5_ANOS_G, # Categorical (3)
  sit_comunidad_fut = P36_2SITUACION_COMUNIDAD_5_ANOT, # Categorical (3)
  sit_municipio_fut = P36_3SITUACION_MUNICIPIO_5_ANOT, # Categorical (3)
  sit_pais_fut = P36_4SITUACION_PAIS_5_ANOS_FUTUS, # Categorical (3)
  
  # Distribución de la riqueza (variable dependiente)
  
  distribucion_de_riqueza = P22_DISTRIBUCION_DE_RIQUEZA, # Categorical (6)
  
  # Confianza en instituciones
  
  conf_ig_catolica = P39_1IGLESIA_CATOLICA, # Numeric (1-10)
  conf_ig_no_catolica = P39_2IGLESIA_NO_CATOLICA, # Numeric (1-10)
  conf_gob_central = P39_3GOBIERNO_CENTRAL, # Numeric (1-10)
  conf_alcaldia = P39_4ALCALDIA_DE_SU_MUNICIPIP, # Numeric (1-10)
  conf_asamblea_dip = P39_5ASAMBLE_DE_DIPUTADOS, # Numeric (1-10)
  conf_universidades = P39_6UNIVERSIDADES, # Numeric (1-10)
  conf_empresa_priv = P39_7EMPRESA_PRIVADA, # Numeric (1-10)
  conf_medios_comunicacion = P39_8MEDIOS_DE_COMUNICACION, # Numeric (1-10)
  conf_sindicatos = P39_9SINDICATOS, # Numeric (1-10)
  conf_policia_mun = P39_10POLICIA_MUNICIPAL, # Numeric (1-10)
  conf_policia_nac = P39_11POLICIA_NACIONAL, # Numeric (1-10)
  conf_ejercito = P39_12EJERCITO, # Numeric (1-10)
  conf_partidos = P39_13PARTIDOS_POLITICOS, # Numeric (1-10)
  conf_alcalde = P39_14ALCALDE_DE_SU_MUNICIPIP, # Numeric (1-10)
  conf_jueces = P39_15JUECES, # Numeric (1-10)
  conf_centros_educ = P39_16CENTROS_EDUCATIVOS, # Numeric (1-10)
  
  # Autoritarismo
  
  ac_obedecer_autoridad_gob = P41_1OBEDECER_AUTORIDADES_DE_GOC, # Categorical (5) # Revese coding # Not ordered
  ac_obedecer_padres = P41_2OBEDECER_A_PADRES, # Categorical (5) # Revese coding # Not ordered
  ac_destino = P41_3DESTINO_NO_DEPENDE_NOSOTROT, # Categorical (5) # Revese coding # Not ordered
  ac_mano_dura = P41_4MANO_DURA, # Categorical (5) # Revese coding # Not ordered
  
  # Se requieren organizaciones en:
  
  org_ddhh = P45_1DERECHOS_HUMANOS, # Numeric (1-10)
  org_medio_amb = P45_2MEDIO_AMBIENTE, # Numeric (1-10)
  org_religion = P45_3RELIGION, # Numeric (1-10)
  org_dd_sexuales = P45_4DERECHOS_SEXUALES, # Numeric (1-10)
  org_prev_drogas = P45_5PREVENCION_DE_CONSUMO_DROGB, # Numeric (1-10)
  org_empleo = P45_6CREACION_DE_EMPLEO, # Numeric (1-10)
  org_microcredito = P45_7MICROCREDITO, # Numeric (1-10)
  
  # Creación organizaciones
  
  cr_org_adultos_estudio = P46_1ADULTAS_CON_ESTUDIO, # Numeric (1-10)
  cr_org_gobs = P46_2CREAN_LOS_GOBIERNOS, # Numeric (1-10)
  cr_org_iglesias = P46_3CREAN_LAS_IGLESIAS,  # Numeric (1-10)
  cr_org_pers_ud = P46_4PERSONAS_COMO_USTED, # Numeric (1-10)
  cr_org_partidos = P46_5CREAN_LOS_PARTIDOS_POLITICP, # Numeric (1-10)
  
  # Valoración organizaciones
  
  val_org_resuelvan = P47_1DIRIGENTES_RESUELVAN_PROBLF, # Numeric (1-10)
  val_org_escuchan = P47_2DIRIGENTES_ESCUCHEN_OPINIOO, # Numeric (1-10)
  val_org_comuniquen = P47_3DIRIGENTES_COMUNIQUEN_PROBM, # Numeric (1-10)
  val_org_recolecten = P47_4DIRIGENTES_FONDOS_PARA_AYUE, # Numeric (1-10)
  val_org_consejos = P47_5DIRIGENTES_CONSEJOS_PRACTID, # Numeric (1-10)
  val_org_lideres = P47_6DIRIGENTES_SEAN_LIDERES_DEN) # Numeric (1-10)
  
  
  # 3.2 Recodificación de variables -----------------------------------------

# Arreglar variables de autoritarismo que tienen reverse coding

## ac_destino

datos_proc_a$ac_destino <- car::recode(datos_proc_a$ac_destino,
                                       as.numeric = T,
  "1 = 5;
   2 = 4;
   5 = 3;
   3 = 2;
   4 = 1"
) %>% 
  
  sjlabelled::set_labels(
  labels = c("Muy en desacuerdo" = 1,
             "En desacuerdo" = 2,
             "Ni de acuerdo ni en desacuerdo" = 3,
             "De acuerdo" = 4,
             "Muy de acuerdo" = 5,
             "NS/NR" = 99
             )
)

# Revisar que se hayan recodificado bien
frq(datos$P41_3DESTINO_NO_DEPENDE_NOSOTROT) # Codificación original
frq(datos_proc_a$ac_destino) # Recodificación

## ac_mano_dura

datos_proc_a$ac_mano_dura <- car::recode(datos_proc_a$ac_mano_dura,
                                       as.numeric = T,
                                       "1 = 5;
   2 = 4;
   5 = 3;
   3 = 2;
   4 = 1"
) %>% 
  
  sjlabelled::set_labels(
    labels = c("Muy en desacuerdo" = 1,
               "En desacuerdo" = 2,
               "Ni de acuerdo ni en desacuerdo" = 3,
               "De acuerdo" = 4,
               "Muy de acuerdo" = 5,
               "NS/NR" = 99
    )
  )

# Revisar que se hayan recodificado bien
frq(datos$P41_4MANO_DURA) # Codificación original
frq(datos_proc_a$ac_mano_dura) # Recodificación

## ac_obedecer_padres

datos_proc_a$ac_obedecer_padres <- car::recode(datos_proc_a$ac_obedecer_padres,
                                       as.numeric = T,
                                       "1 = 5;
   2 = 4;
   5 = 3;
   3 = 2;
   4 = 1"
) %>% 
  
  sjlabelled::set_labels(
    labels = c("Muy en desacuerdo" = 1,
               "En desacuerdo" = 2,
               "Ni de acuerdo ni en desacuerdo" = 3,
               "De acuerdo" = 4,
               "Muy de acuerdo" = 5,
               "NS/NR" = 99
    )
  )

# Revisar que se hayan recodificado bien
frq(datos$P41_2OBEDECER_A_PADRES) # Codificación original
frq(datos_proc_a$ac_obedecer_padres) # Recodificación

## ac_obedecer_autoridades_gob

datos_proc_a$ac_obedecer_autoridad_gob <- car::recode(datos_proc_a$ac_obedecer_autoridad_gob,
                                       as.numeric = T,
                                       "1 = 5;
   2 = 4;
   5 = 3;
   3 = 2;
   4 = 1"
) %>% 
  
  sjlabelled::set_labels(
    labels = c("Muy en desacuerdo" = 1,
               "En desacuerdo" = 2,
               "Ni de acuerdo ni en desacuerdo" = 3,
               "De acuerdo" = 4,
               "Muy de acuerdo" = 5,
               "NS/NR" = 99
    )
  )

# Revisar que se hayan recodificado bien
frq(datos$P41_1OBEDECER_AUTORIDADES_DE_GOC) # Codificación original
frq(datos_proc_a$ac_obedecer_autoridad_gob) # Recodificación


# Transformación de variables cuyas categorías van del 1 al 10 a variables
#numéricas

datos_proc_a <- datos_proc_a %>%
  
mutate_at(vars(starts_with("org")|
                 starts_with("cr")|
                 starts_with("val")|
                 starts_with("sit")|
                 starts_with("conf")|
                 starts_with("ac")),
          ~as.numeric(.))  %>%
  
  #De esas mismas variables, se realiza la recodificación de 99 
  #(correspondientes a NS/NR) a NA
  mutate_at(vars(starts_with("org")|
                   starts_with("cr")|
                   starts_with("val")|
                   starts_with("sit")|
                   starts_with("conf")|
                   starts_with("ac")),
            ~ car::recode(.,"99 = NA")) %>% 
  
  #Transformación de variable horas_trabajo a numérica y recodificación 99 a NA
  mutate(horas_trabajo = as.numeric(horas_trabajo)) %>%
  mutate(horas_trabajo = car::recode(.$horas_trabajo,
                                     recodes = c("99 = NA"))) %>%
  
  #Transformación de variable personas_casa a numérica y recodificación 99 a NA
  mutate(personas_casa = as.numeric(personas_casa)) %>%
  mutate(personas_casa = car::recode(.$personas_casa,
                                     recodes = c("99 = NA"))) %>%
  
  #Transformación de variable distribución de la riqueza a numérica y recodificación 99 a NA
  mutate(distribucion_de_riqueza = as.numeric(distribucion_de_riqueza)) %>%
  mutate(distribucion_de_riqueza = car::recode(.$distribucion_de_riqueza,
                                               recodes = c("99 = NA"))) %>%
  
  
  
  #Transformación de variables de clase labelled a factor
  mutate_if(is.labelled, ~ (forcats::as_factor(.))) %>% 
  
  #Recodificación de NS/NR a NA en las variables que presentan esta categoría
  mutate_at(vars(starts_with("dinero")|
                   victimas_violencia|
                   seguro_social),
            ~ (car::recode(., recodes = "'NS/NR' = NA"))) %>%
  
  #Recodificación variable nivel_educ
  mutate(nivel_educ = car::recode(.$nivel_educ,
                                  #Se elimina la categoría Educación especial
                                  recodes = c("'Educación especial' = NA"),
                                  #Se reordenan los niveles de respuesta
                                  levels = c('Ninguno',
                                             'Primaria incompleta',
                                             'Primaria completa',
                                             'Secundaria incompleta',
                                             'Secundaria completa',
                                             'Educación técnica incompleta',
                                             'Educación técnica completa',
                                             'Universidad incompleta',
                                             'Universidad completa',
                                             'Posgrado universitario'))) %>%
  
  # 3. 4. Ajuste del orden de los niveles de respuesta  ---------------------

#de las variables que comienzan por dinero
mutate_at(vars(starts_with("dinero")),
          ~(factor(.,levels = c("Nunca",
                                "Muy pocas veces",
                                "Pocas veces",
                                "Algunas veces",
                                "Con frecuencia",
                                "Con mucha frecuencia")))) %>%
  
  # de la variable victimas_violencia
  mutate(victimas_violencia = factor(.$victimas_violencia,
                                     levels = c("Nada de miedo",
                                                "Poco miedo",
                                                "Miedo",
                                                "Mucho miedo")))

# 3. 5. Etiquetado de variables -------------------------------------------

datos_proc_a <- datos_proc_a %>%
  mutate(horas_trabajo = sjlabelled::set_label(.$horas_trabajo,
                                               "Cuántas horas a la semana trabaja"),
         
         personas_casa = sjlabelled::set_label(.$personas_casa,
                                               "Cuántas personas viven en su casa"),
         
         seguro_social = sjlabelled::set_label(.$seguro_social,
                                               "Tenencia de seguro social"),
         
         nivel_educ = sjlabelled::set_label(.$nivel_educ, 
                                            "Nivel educacional"),
         
         dinero_comida = sjlabelled::set_label(.$dinero_comida,
                                               "Quedarse sin dinero para comprar la comida? "),
         
         dinero_casa = sjlabelled::set_label(.$dinero_casa,
                                             "Quedarse sin dinero para pagar la casa"),
         
         dinero_buses = sjlabelled::set_label(.$dinero_buses,
                                              "Quedarse sin dinero para pagar autobuses?"),
         
         victimas_violencia = sjlabelled::set_label(.$victimas_violencia, 
                                                    "Victimas de violencia"),
         
         sit_personal_pas = sjlabelled::set_label(.$sit_personal_pas,
                                                  "Situación personal hace 5 años era"),
         
         sit_comunidad_pas = sjlabelled::set_label(.$sit_comunidad_pas,
                                                   "Situación de su comunidad hace 5 años era"),
         
         sit_municipio_pas = sjlabelled::set_label(.$sit_municipio_pas,
                                                   "Situación de su municipio hace 5 años era"),
         
         sit_pais_pas = sjlabelled::set_label(.$sit_pais_pas,
                                              "Situación del país hace 5 años era"),
         
         sit_personal = sjlabelled::set_label(.$sit_personal,
                                              "Situación personal es"),
         
         sit_comunidad = sjlabelled::set_label(.$sit_comunidad,
                                               "Situación de su comunidad es"),
         
         sit_municipio = sjlabelled::set_label(.$sit_municipio,
                                               "Situación de su municipio es"),
         
         sit_pais = sjlabelled::set_label(.$sit_pais,
                                          "Situación del país es"),
         
         sit_personal_fut = sjlabelled::set_label(.$sit_personal_fut,
                                                  "Situación personal en 5 años será"),
         
         sit_comunidad_fut = sjlabelled::set_label(.$sit_comunidad_fut,
                                                   "Situación de su comunidad en 5 años será"),
         
         sit_municipio_fut = sjlabelled::set_label(.$sit_municipio_fut,
                                                   "Situación de su municipio en 5 años será"),
         
         distribucion_de_riqueza = sjlabelled::set_label(.$distribucion_de_riqueza,
                                                         "Evaluación de justicia de la distribución de la riqueza en su país"),
         
         conf_ig_catolica = sjlabelled::set_label(.$conf_ig_catolica,
                                                  "Nivel de confianza en instituciones:Iglesia católica"),
         
         conf_ig_no_catolica = sjlabelled::set_label(.$conf_ig_no_catolica,
                                                     "Nivel de confianza en instituciones: Iglesia no católica"),
         
         conf_gob_central = sjlabelled::set_label(.$conf_gob_central,
                                                  "Nivel de confianza en instituciones: Gobierno central"),
         
         conf_alcaldia = sjlabelled::set_label(.$conf_alcaldia,
                                               "Nivel de confianza en instituciones: Alcaldía de su municipio"),
         
         conf_asamblea_dip = sjlabelled::set_label(.$conf_asamblea_dip,
                                                   "Nivel de confianza en instituciones: Asamblea de diputados"),
         
         conf_universidades = sjlabelled::set_label(.$conf_universidades,
                                                    "Nivel de confianza en instituciones: Universidades"),
         
         conf_empresa_priv = sjlabelled::set_label(.$conf_empresa_priv,
                                                   "Nivel de confianza en instituciones: Empresa privada"),
         
         conf_medios_comunicacion = sjlabelled::set_label(.$conf_medios_comunicacion,
                                                          "Nivel de confianza en instituciones: Medios de comunicación"),
         
         conf_sindicatos = sjlabelled::set_label(.$conf_sindicatos,
                                                 "Nivel de confianza en instituciones: Sindicatos"),
         
         conf_policia_mun = sjlabelled::set_label(.$conf_policia_mun,
                                                  "Nivel de confianza en instituciones: Policía municipal"),
         
         conf_policia_nac = sjlabelled::set_label(.$conf_policia_nac,
                                                  "Nivel de confianza en instituciones: Policía Nacional"),
         
         conf_ejercito = sjlabelled::set_label(.$conf_ejercito,
                                               "Nivel de confianza en instituciones: Ejército"),
         
         conf_partidos = sjlabelled::set_label(.$conf_partidos,
                                               "Nivel de confianza en instituciones: Partidos políticos"),
         
         conf_alcalde = sjlabelled::set_label(.$conf_alcalde,
                                              "p39.14.Nivel de confianza en instituciones: Alcalde de su municipio"),
         
         conf_jueces = sjlabelled::set_label(.$conf_jueces,
                                             "Nivel de confianza en instituciones: Jueces"),
         
         conf_centros_educ = sjlabelled::set_label(.$conf_centros_educ,
                                                   "Nivel de confianza en instituciones: Centros educativos"),
         
         ac_obedecer_autoridad_gob = sjlabelled::set_label(.$ac_obedecer_autoridad_gob,
                                                           "Grado de acuerdo:Obedecer autoridades del gobierno, aunque no tengan razón"),
         
         ac_obedecer_padres = sjlabelled::set_label(.$ac_obedecer_padres,
                                                    "Grado de acuerdo:Obedecer a los padres, aunque no se hayan ganado el respeto"),
         
         ac_destino = sjlabelled::set_label(.$ac_destino,
                                            "Grado de acuerdo:Todos tenemos un destino que no depende de nosotros"),
         
         ac_mano_dura = sjlabelled::set_label(.$ac_mano_dura,
                                              "Grado de acuerdo:La mano dura va a acabar con los problemas del país "),
         
         org_ddhh = sjlabelled::set_label(.$org_ddhh,
                                          "Percepción requerimiento organizaciones: Derechos Humanos"),
         
         org_medio_amb = sjlabelled::set_label(.$org_ddhh,
                                               "Percepción requerimiento organizaciones: Medio ambiente"),
         
         org_religion = sjlabelled::set_label(.$org_religion,
                                              "Percepción requerimiento organizaciones: Religión"),
         
         org_dd_sexuales = sjlabelled::set_label(.$org_dd_sexuales,
                                                 "Percepción requerimiento organizaciones: Derechos sexuales"),
         
         org_prev_drogas = sjlabelled::set_label(.$org_prev_drogas,
                                                 "Percepción requerimiento organizaciones: Prevención de consumo de drogas"),
         
         org_empleo = sjlabelled::set_label(.$org_empleo,
                                            "Percepción requerimiento organizaciones: Creación de empleo"),
         
         org_microcredito = sjlabelled::set_label(.$org_microcredito,
                                                  "Percepción requerimiento organizaciones: Microcrédito"),
         
         cr_org_adultos_estudio = sjlabelled::set_label(.$cr_org_adultos_estudio,
                                                        "Grado de acuerdo: Organizaciones creadas por personas adultas con estudios"),
         
         cr_org_gobs = sjlabelled::set_label(.$cr_org_gobs,
                                             "Grado de acuerdo: Organizaciones creadas por gobiernos"),
         
         cr_org_iglesias = sjlabelled::set_label(.$cr_org_iglesias,
                                                 "Grado de acuerdo: Organizaciones creadas por iglesias"),
         
         cr_org_pers_ud = sjlabelled::set_label(.$cr_org_pers_ud,
                                                "Grado de acuerdo: Persona como Ud. puede crear organización"),
         
         cr_org_partidos = sjlabelled::set_label(.$cr_org_partidos,
                                                 "Grado de acuerdo: Organizaciones creadas por partidos"),
         
         val_org_resuelvan = sjlabelled::set_label(.$val_org_resuelvan,
                                                   "Valoración: Dirigentes resuelvan problemas"),
         
         val_org_escuchan = sjlabelled::set_label(.$val_org_escuchan,
                                                  "Valoración: Dirigentes escuchen opiniones"),
         
         val_org_comuniquen = sjlabelled::set_label(.$val_org_comuniquen,
                                                    "Valoración: Dirigentes comuniquen problemas a autoridades"),
         
         val_org_recolecten = sjlabelled::set_label(.$val_org_recolecten,
                                                    "Valoración: Dirigentes recolecten fondos para ayudar"),
         
         val_org_consejos = sjlabelled::set_label(.$val_org_consejos,
                                                  "Valoración: Dirigentes den consejos prácticos"),
         
         val_org_lideres = sjlabelled::set_label(.$val_org_lideres,
                                                 "Valoración: Dirigentes sean líderes democráticos"))

datos_proc_a$distribucion_de_riqueza <- datos_proc_a$distribucion_de_riqueza <- sjlabelled::set_labels(datos_proc_a$distribucion_de_riqueza, labels = c( "Muy Justa" = 5, 
                                                                                                   "Un poco justa" = 4, 
                                                                                                   "Ni justa ni injusta" = 3, 
                                                                                                   "Un poco injusta" = 2,
                                                                                                   "Muy injusta" = 1))

# 3. 6. Creación de índices --------------------------------------------

# 3. 6. 1. Crear índice percepción  pasado
datos_proc_a <- datos_proc_a %>% 
  rowwise() %>% #Agrupar por filas
  mutate(ind_perc_pas = sum(sit_personal_pas,
                            sit_comunidad_pas,
                            sit_municipio_pas,
                            sit_pais_pas, na.rm = T)/4) %>%
  ungroup() 
#Considerar que esta operación asume que NA representa 0

datos_proc_a$ind_perc_pas <- sjlabelled::set_label(datos_proc_a$ind_perc_pas, label = "Indice percepción situación en el pasado") # Etiquetar nueva variable


#3. 6. 2. Crear índice percepción futuro
datos_proc_a <- datos_proc_a %>% 
  rowwise() %>% #Agrupar por filas
  mutate(ind_perc_fut = sum(sit_personal_fut,
                            sit_comunidad_fut,
                            sit_municipio_fut,
                            sit_pais_fut, na.rm = T)/4) %>%
  ungroup()

datos_proc_a$ind_perc_fut <- sjlabelled::set_label(datos_proc_a$ind_perc_fut, label = "Indice percepción situación en el futuro") # Etiquetar nueva variable

#3. 6. 3. Crear índice percepción

datos_proc_a <- datos_proc_a %>% 
  rowwise() %>% #Agrupar por filas
  mutate(ind_perc = sum(sit_personal,
                        sit_comunidad,
                        sit_municipio,
                        sit_pais, na.rm = T)/4)%>%
  ungroup()

datos_proc_a$ind_perc <- sjlabelled::set_label(datos_proc_a$ind_perc, label = "Indice percepción situación actual") # Etiquetar nueva variable


#3. 6. 4. Crear índice confianza institucional 
datos_proc_a <- datos_proc_a %>% 
  rowwise() %>% #Agrupar por filas
  mutate(ind_conf_inst = sum(conf_ig_catolica,
                             conf_ig_no_catolica,
                             conf_gob_central,
                             conf_alcaldia,
                             conf_asamblea_dip,
                             conf_universidades,
                             conf_empresa_priv,
                             conf_medios_comunicacion,
                             conf_sindicatos,
                             conf_policia_mun,
                             conf_policia_nac,
                             conf_ejercito,
                             conf_partidos,
                             conf_alcalde,
                             conf_jueces,
                             conf_centros_educ, na.rm = T)/16) %>%
  ungroup()  

datos_proc_a$ind_conf_inst <- sjlabelled::set_label(datos_proc_a$ind_conf_inst, label = "Indice confianza en instituciones") # Etiquetar nueva variable


#3. 6. 5. Crear índice de autoritarismo 
datos_proc_a <- datos_proc_a %>% 
  rowwise() %>% #Agrupar por filas
  mutate(ind_aut = sum(ac_obedecer_autoridad_gob,
                       ac_obedecer_padres,
                       ac_destino,
                       ac_mano_dura, na.rm = T)/4) %>%
  ungroup() 

datos_proc_a$ind_aut <- sjlabelled::set_label(datos_proc_a$ind_aut, label = "Indice autoritarismo") # Etiquetar nueva variable


# 3. 7. Analizar los descriptivos de todas las variables -------------------

df_gen_a <- dfSummary(datos_proc_a,
                   plain.ascii = FALSE,
                   style = "grid",
                   tmp.img.dir = "/tmp",
                   graph.magnif = TRUE,
                   headings = F,  # encabezado
                   varnumbers = F, # num variable
                   labels.col = T, # etiquetas
                   na.col = T,    # missing
                   graph.col = T, # plot
                   valid.col = T, # n valido
                   col.widths = c(1000,10,10,10,10,10)
)
df_gen_a$Variable <- NULL # delete variable column
summarytools::view(df_gen_a, file = "output/tables/df_gen_a.html") # Ver tabla en un archivo HTML

# Efectivamente horas_trabajo es la única variable con más de mil NA
# seguro_social es la segunda variable con más NA

datos_proc_a %>% VIM::aggr() # Ver patrones de NA

table1::table1(~horas_trabajo | pais, data = datos_proc_a)


#3. 8. Eliminar variables que no serán utilizadas posteriormente

## NOTA: Cómo se podrá notas, se dejan fuera las variables que componen los indices, asi como también algunas variables que presentan demasiados NA.

datos_proc_a <- datos_proc_a  %>% 
  dplyr::select(-c(sit_personal_pas,
                   sit_comunidad_pas,
                   sit_municipio_pas,
                   sit_pais_pas,
                   sit_personal_fut,
                   sit_comunidad_fut,
                   sit_municipio_fut,
                   sit_pais_fut,
                   sit_personal,
                   sit_comunidad,
                   sit_municipio,
                   sit_pais,
                   conf_ig_catolica,
                   conf_ig_no_catolica,
                   conf_gob_central,
                   conf_alcaldia,
                   conf_asamblea_dip,
                   conf_universidades,
                   conf_empresa_priv,
                   conf_medios_comunicacion,
                   conf_sindicatos,
                   conf_policia_mun,
                   conf_policia_nac,
                   conf_ejercito,
                   conf_partidos,
                   conf_alcalde,
                   conf_jueces,
                   conf_centros_educ,
                   ac_destino,
                   ac_mano_dura,
                   ac_obedecer_padres,
                   ac_obedecer_autoridad_gob,
                   horas_trabajo,
                   seguro_social)) #excluyendo variables no utilizadas en modelamiento (se excluye horas_trabajo y seguro social por su cantidad de NA)



# 3. 9. Crear subsets por país --------------------------------------------

#Costa Rica

costa_rica_a <- datos_proc_a %>%
  filter(., pais == 'Costa Rica') %>% #filtrados por país
  dplyr::select(-pais) #excluyendo variables no utilizadas en modelamiento

#Nicaragua

nicaragua_a <- datos_proc_a %>%
  filter(., pais == 'Nicaragua') %>% #filtrados por país
  dplyr::select(-pais) #excluyendo variables no utilizadas en modelamiento

#El Salvador

el_salvador_a <- datos_proc_a %>%
  filter(., pais == 'Salvador') %>% #filtrados por país
  dplyr::select(-pais) #excluyendo variables no utilizadas en modelamiento

#Guatemala

guatemala_a <- datos_proc_a %>%
  filter(., pais == 'Guatemala') %>% #filtrados por país
  dplyr::select(-pais) #excluyendo variables no utilizadas en modelamiento

#Honduras

honduras_a <- datos_proc_a %>%
  filter(., pais == 'Honduras') %>% #filtrados por país
  dplyr::select(-pais) #excluyendo variables no utilizadas en modelamiento

# Eliminar casos perdidos --------------------------------------------

#datos_proc_a_original <- datos_proc_a
#dim(datos_proc_a)
#sum(is.na(datos_proc_a))
#datos_proc_a <-na.omit(datos_proc_a)
#dim(datos_proc_a)
#datos_proc_a <-sjlabelled::copy_labels(datos_proc_a,datos_proc_a_original)

#Crear variable muestras a partir de la variable pais para posterior análisis  
#datos_proc_a <- datos_proc_a %>% 
# mutate(muestras = case_when((pais == "Salvador" | pais =="Guatemala")~"Salvador-Guatemala",
#                          (pais == "Salvador" | pais == "Honduras")~"Salvador-Honduras",
#                          (pais == "Salvador" | pais == "Costa Rica")~"Salvador-Costa Rica",
#                          (pais == "Salvador" | pais == "Nicaragua")~ "Salvador-Nicaragua",
#                          (pais == "Guatemala"| pais == "Honduras")~"Guatemala-Honduras",
#                          (pais == "Guatemala"| pais == "Costa Rica")~"Guatemala-Costa Rica",
#                          (pais == "Guatemala"| pais == "Nicaragua")~"Guatemala-Nicaragua",
#                          (pais == "Honduras"| pais == "Costa Rica")~"Guatemala-Costa Rica",
#                          (pais == "Honduras"| pais == "Nicaragua")~"Honduras-Nicaragua",
#                          (pais == "Costa Rica"| pais == "Nicaragua")~"Costa Rica-Nicaragua"))  



# 4. Exportar datos procesados --------------------------------------------

save(datos_proc_a, file = "input/data/datos_proc_a.RData") 

saveRDS(datos_proc_a, file = "input/data/datos_proc_a.rds") 
saveRDS(costa_rica_a, file = "input/data/costa_rica_a.rds") 
saveRDS(nicaragua_a, file = "input/data/nicaragua_a.rds") 
saveRDS(el_salvador_a, file = "input/data/el_salvador_a.rds") 
saveRDS(guatemala_a, file = "input/data/guatemala_a.rds") 
saveRDS(honduras_a, file = "input/data/honduras_a.rds") 
