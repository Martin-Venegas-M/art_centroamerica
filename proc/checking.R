Cheking

all(datos_proc$pais == datos$PAIS, na.rm = TRUE) # Categorical
all(datos_proc$sexo == datos$P02_SEXO, na.rm = TRUE) # Categorical
all(datos_proc$edad == datos$P03_EDAD, na.rm = TRUE) # Numeric
all(datos_proc$trabajo_remunerado == datos$P04_TRABAJO_REMUNERADO, na.rm = TRUE) # Categoric
all(datos_proc$horas_trabajo == datos$P06_HORAS_DE_TRABAJO, na.rm = TRUE) # Numeric
all(datos_proc$personas_casa == datos$P49_NUMERO_PERSONAS_CASA, na.rm = TRUE) # Numeric
all(datos_proc$seguro_social == datos$P07_SEGURO_SOCIAL, na.rm = TRUE) # Categoric
all(datos_proc$nivel_educ == datos$P12_ULTIMO_ANO_DE_ESTUDIO, na.rm = TRUE) # Categoric
all(datos_proc$dinero_comida == datos$P23_DINERO_PARA_COMIDA, na.rm = TRUE) # Categoric # Reverse coding # Done 
all(datos_proc$dinero_casa == datos$P24_DINERO_PARA_CASA, na.rm = TRUE) # Categoric # Reverse coding # Done
all(datos_proc$dinero_buses == datos$P25_DINERO_PARA_BUSES, na.rm = TRUE) # Categoric # Reverse coding
all(datos_proc$victimas_violencia == datos$P27_VICTIMAS_DE_VIOLECIA, na.rm = TRUE) # Categoric # Reverse coding
all(datos_proc$sit_personal_pas== datos$P34_1SITUACION_PERSONAL_5_ANOS_B, na.rm = TRUE) # Categoric
all(datos_proc$sit_comunidad_pas == datos$P34_2SITUACION_COMUNIDAD_5_ANOT, na.rm = TRUE)
all(datos_proc$sit_municipio_pas == datos$P34_3SITUACION_MUNICIPIO_5_ANOT, na.rm = TRUE)
all(datos_proc$sit_pais_pas == datos$P34_4SITUACION_PAIS_5_ANOS_ATRAT, na.rm = TRUE)
all(datos_proc$sit_personal == datos$P35_1SITUACION_PERSONAL, na.rm = TRUE)
all(datos_proc$sit_comunidad == datos$P35_2SITUACION_COMUNIDAD, na.rm = TRUE)
all(datos_proc$sit_municipio == datos$P35_3SITUACION_MUNICIPIO, na.rm = TRUE)
all(datos_proc$sit_pais == datos$P35_4SITUACION_PAIS, na.rm = TRUE)
all(datos_proc$sit_personal_fut == datos$P36_1SITUACION_PERSONAL_5_ANOS_G, na.rm = TRUE)
all(datos_proc$sit_comunidad_fut == datos$P36_2SITUACION_COMUNIDAD_5_ANOT, na.rm = TRUE)
all(datos_proc$sit_municipio_fut == datos$P36_3SITUACION_MUNICIPIO_5_ANOT , na.rm = TRUE)
all(datos_proc$sit_pais_fut == datos$P36_4SITUACION_PAIS_5_ANOS_FUTUS, na.rm = TRUE)
all(datos_proc$distribucion_de_riqueza == datos$P22_DISTRIBUCION_DE_RIQUEZA, na.rm = TRUE)
all(datos_proc$conf_ig_catolica == datos$P39_1IGLESIA_CATOLICA, na.rm = TRUE)
all(datos_proc$conf_ig_no_catolica == datos$P39_2IGLESIA_NO_CATOLICA, na.rm = TRUE)
all(datos_proc$conf_gob_central == datos$P39_3GOBIERNO_CENTRAL, na.rm = TRUE)
all(datos_proc$conf_alcaldia == datos$P39_4ALCALDIA_DE_SU_MUNICIPIP, na.rm = TRUE)
all(datos_proc$conf_asamblea_dip == datos$P39_5ASAMBLE_DE_DIPUTADOS, na.rm = TRUE)
all(datos_proc$conf_universidades == datos$P39_6UNIVERSIDADES, na.rm = TRUE)
all(datos_proc$conf_empresa_priv == datos$P39_7EMPRESA_PRIVADA, na.rm = TRUE)
all(datos_proc$conf_medios_comunicacion == datos$P39_8MEDIOS_DE_COMUNICACION, na.rm = TRUE)
all(datos_proc$conf_sindicatos == datos$P39_9SINDICATOS, na.rm = TRUE)
all(datos_proc$conf_policia_mun == datos$P39_10POLICIA_MUNICIPAL, na.rm = TRUE)
all(datos_proc$conf_policia_nac == datos$P39_11POLICIA_NACIONAL, na.rm = TRUE)
all(datos_proc$conf_ejercito == datos$P39_12EJERCITO, na.rm = TRUE)
all(datos_proc$conf_partidos == datos$P39_13PARTIDOS_POLITICOS, na.rm = TRUE)
all(datos_proc$conf_alcalde == datos$P39_14ALCALDE_DE_SU_MUNICIPIP, na.rm = TRUE)
all(datos_proc$conf_jueces == datos$P39_15JUECES, na.rm = TRUE)
all(datos_proc$conf_centros_educ == datos$P39_16CENTROS_EDUCATIVOS, na.rm = TRUE)
all(datos_proc$ac_obedecer_autoridad_gob == datos$P41_1OBEDECER_AUTORIDADES_DE_GOC, na.rm = TRUE)
all(datos_proc$ac_obedecer_padres == datos$P41_2OBEDECER_A_PADRES, na.rm = TRUE)
all(datos_proc$ac_destino == datos$P41_3DESTINO_NO_DEPENDE_NOSOTROT, na.rm = TRUE)
all(datos_proc$ac_mano_dura == datos$P41_4MANO_DURA, na.rm = TRUE)
all(datos_proc$org_ddhh == datos$P45_1DERECHOS_HUMANOS, na.rm = TRUE)
all(datos_proc$org_medio_amb == datos$P45_2MEDIO_AMBIENTE, na.rm = TRUE)
all(datos_proc$org_religion == datos$P45_3RELIGION, na.rm = TRUE)
all(datos_proc$org_dd_sexuales == datos$P45_4DERECHOS_SEXUALES, na.rm = TRUE)
all(datos_proc$org_prev_drogas == datos$P45_5PREVENCION_DE_CONSUMO_DROGB, na.rm = TRUE)
all(datos_proc$org_empleo == datos$P45_6CREACION_DE_EMPLEO, na.rm = TRUE)
all(datos_proc$org_microcredito == datos$P45_7MICROCREDITO, na.rm = TRUE)
all(datos_proc$cr_org_adultos_estudio == datos$P46_1ADULTAS_CON_ESTUDIO, na.rm = TRUE)
all(datos_proc$cr_org_gobs == datos$P46_2CREAN_LOS_GOBIERNOS, na.rm = TRUE)
all(datos_proc$cr_org_iglesias == datos$P46_3CREAN_LAS_IGLESIAS, na.rm = TRUE)
all(datos_proc$cr_org_pers_ud == datos$P46_4PERSONAS_COMO_USTED, na.rm = TRUE)
all(datos_proc$cr_org_partidos == datos$P46_5CREAN_LOS_PARTIDOS_POLITICP, na.rm = TRUE)
all(datos_proc$val_org_resuelvan == datos$P47_1DIRIGENTES_RESUELVAN_PROBLF, na.rm = TRUE)
all(datos_proc$val_org_escuchan == datos$P47_2DIRIGENTES_ESCUCHEN_OPINIOO, na.rm = TRUE)
all(datos_proc$val_org_comuniquen == datos$P47_3DIRIGENTES_COMUNIQUEN_PROBM, na.rm = TRUE)
all(datos_proc$val_org_recolecten == datos$P47_4DIRIGENTES_FONDOS_PARA_AYUE, na.rm = TRUE)
all(datos_proc$val_org_consejos == datos$P47_5DIRIGENTES_CONSEJOS_PRACTID, na.rm = TRUE)
all(datos_proc$val_org_lideres == datos$P47_6DIRIGENTES_SEAN_LIDERES_DEN, na.rm = TRUE)


