
#Tablas preregistro

library(pacman)
pacman::p_load("tidyverse")

#Dependiente
dependiente <- datos_proc %>% 
  select(distribucion_de_riqueza)

sjPlot::view_df(dependiente)

#Independientes

independientes <- datos_proc %>% select(dinero_comida,
                                        dinero_casa,
                                        dinero_buses,
                                        victimas_violencia,
                                        sit_personal_pas,
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
                                        sit_pais_fut, 
                                        distribucion_de_riqueza,
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
                                        ac_obedecer_autoridad_gob,
                                        ac_obedecer_padres,
                                        ac_destino,
                                        ac_mano_dura,
                                        org_ddhh,
                                        org_medio_amb,
                                        org_religion,
                                        org_dd_sexuales,
                                        org_prev_drogas,
                                        org_empleo,
                                        org_microcredito)
sjPlot::view_df(independientes)
#Control
