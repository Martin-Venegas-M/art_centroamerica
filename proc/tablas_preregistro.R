# 0. Identificación -------------------------------------------------------
#Título: Código de elaboración de tablas para pre-registro
#de artículo sobre jóvenes de Centroamérica
#Institución: Centro de Estudios de Conflicto y Cohesión Social (COES)

# 1. Cargar librerías -----------------------------------------------------
library(pacman)
pacman::p_load(sjmisc)

# 2. Cargar datos ---------------------------------------------------------
datos_proc <- readRDS("input/data/datos_proc.rds")

# 3. Crear exportar tablas ---------------------------------------------------

#3.1 Variable dependiente
tabla_dependiente <- datos_proc %>% 
  select(distribucion_de_riqueza) 

sjPlot::view_df(tabla_dependiente,
                encoding = "Latin-1",
                file = "output/tables/dependiente.doc") 
#3.2 Variables independientes 1er conjunto
tabla_independientes1 <- datos_proc %>% 
  select(dinero_comida,
         dinero_casa,
         dinero_buses,
         victimas_violencia) 

sjPlot::view_df(tabla_independientes1,
                encoding = "Latin-1",
                file = "output/tables/independientes1.doc") 

#3.2 Variables independientes 2ndo conjunto
tabla_independientes2 <- datos_proc %>% 
  select(sit_personal_pas,
         sit_comunidad_pas,
         sit_municipio_pas,
         sit_pais_pas,
         sit_personal,
         sit_comunidad,
         sit_municipio,
         sit_pais,
         sit_personal_fut,
         sit_comunidad_fut,
         sit_municipio_fut,
         sit_pais_fut) 

sjPlot::view_df(tabla_independientes2,
                encoding = "Latin-1",
                file = "output/tables/independientes2.doc") 

#3.3 Variables independientes 3er conjunto
tabla_independientes3 <- datos_proc %>% 
  select(conf_ig_catolica,
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
         conf_centros_educ) 

sjPlot::view_df(tabla_independientes3,
                encoding = "Latin-1",
                file = "output/tables/independientes3.doc") 

#3.4 Variables independientes 4to conjunto
tabla_independientes4 <- datos_proc %>% 
  select(ac_obedecer_autoridad_gob,
         ac_obedecer_padres,
         ac_destino,
         ac_mano_dura) 

sjPlot::view_df(tabla_independientes4,
                encoding = "Latin-1",
                file = "output/tables/independientes4.doc") 

#3.5 Variables independientes 5to conjunto
tabla_independientes5 <- datos_proc %>% 
  select(org_ddhh,
         org_medio_amb,
         org_religion,
         org_dd_sexuales,
         org_prev_drogas,
         org_empleo,
         org_microcredito) 

sjPlot::view_df(tabla_independientes5,
                encoding = "Latin-1",
                file = "output/tables/independientes5.doc") 

#3.6 Variables independientes 6to conjunto
tabla_independientes6 <- datos_proc %>% 
  select(cr_org_adultos_estudio,
         cr_org_gobs,
         cr_org_iglesias,
         cr_org_pers_ud,
         cr_org_partidos) 

sjPlot::view_df(tabla_independientes6,
                encoding = "Latin-1",
                file = "output/tables/independientes6.doc") 

#3.7 Variables independientes 7mo conjunto
tabla_independientes7 <- datos_proc %>% 
  select(val_org_resuelvan,
         val_org_escuchan,
         val_org_comuniquen,
         val_org_recolecten,
         val_org_consejos,
         val_org_lideres) 

sjPlot::view_df(tabla_independientes7,
                encoding = "Latin-1",
                file = "output/tables/independientes7.doc") 

#3.8 Variables de control
tabla_control <- datos_proc %>% 
  select(sexo,
         edad,
         trabajo_remunerado,
         horas_trabajo,
         seguro_social,
         nivel_educ,
         personas_casa) 

sjPlot::view_df(tabla_control,
                encoding = "Latin-1",
                file = "output/tables/control.doc")



