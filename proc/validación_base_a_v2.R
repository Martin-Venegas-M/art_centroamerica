#---------------- Validación de datos --------

# Explicación:
# En el análisis principal del artículo (Base A), los coeficientes de mayor magnitud presentan un sentido que se apega poco al sentido común. En detalle, se halla que
# quienes "Con mucha frecuencia" se quedan sin dinero para pagar la casa, evalúan más justicia en la distribución de riqueza que quienes que a quienes nunca les pasa.

# Al anañozar los bivariados entre ambas variables, se encuentra que efectivamente esa es la tendencia, pero que los promedios altos de justicia distributiva en
# la categoría "Con mucha frecuencia" probablemente tienen ese valor por el bajo N.

# A raíz de lo anterior, se prueban dos cambios para evaluar el comportamiento de dichos resultados.

# 1) Invertir la escala de la variable
# 2) Agrupar las categorías de la variable "dinero_casa"

###------------- 1. Librerías -------------

library(pacman)
pacman::p_load(tidyverse,
               sjmisc,
               sjPlot, #Para realizar tablas
               stats, # Test de Kruskal-Wallis
               ggplot2, #Para realizar gráficos
               rstatix, #Revisar normalidad
               lattice,
               olsrr,
               nortest, #test Lilliefors
               MASS,
               summarytools,
               VIM,
               table1,
               car,
               jtools,
               ggstance,
               broom.mixed,
               huxtable,
               officer,
               flextable)

###------------2. Cargar datos --------------

datos_proc_a <- readRDS("input/data/datos_proc_a.rds")
costa_rica_a <- readRDS("input/data/costa_rica_a.rds")
nicaragua_a <- readRDS("input/data/nicaragua_a.rds") 
el_salvador_a <- readRDS("input/data/el_salvador_a.rds") 
guatemala_a <- readRDS("input/data/guatemala_a.rds") 
honduras_a <- readRDS("input/data/honduras_a.rds") 

###------------------- 3. Preparar datoos ------------------------

# Recodificar las variables de dinero 

datos_proc_a$dinero_casa <- datos_proc_a$dinero_casa %>% recode("'Nunca' = 'Nunca'; c('Muy pocas veces', 'Pocas veces', 'Algunas veces', 'Con frecuencia', 'Con mucha frecuencia') = 'Ocurrido aunque sea una vez'", as.factor = T )
datos_proc_a$dinero_comida <- datos_proc_a$dinero_comida %>% recode("'Nunca' = 'Nunca'; c('Muy pocas veces', 'Pocas veces', 'Algunas veces', 'Con frecuencia', 'Con mucha frecuencia') = 'Ocurrido aunque sea una vez'", as.factor = T )
datos_proc_a$dinero_buses <- datos_proc_a$dinero_buses %>% recode("'Nunca' = 'Nunca'; c('Muy pocas veces', 'Pocas veces', 'Algunas veces', 'Con frecuencia', 'Con mucha frecuencia') = 'Ocurrido aunque sea una vez'", as.factor = T )

costa_rica_a$dinero_casa <- costa_rica_a$dinero_casa %>% recode("'Nunca' = 'Nunca'; c('Muy pocas veces', 'Pocas veces', 'Algunas veces', 'Con frecuencia', 'Con mucha frecuencia') = 'Ocurrido aunque sea una vez'", as.factor = T )
costa_rica_a$dinero_comida <- costa_rica_a$dinero_comida %>% recode("'Nunca' = 'Nunca'; c('Muy pocas veces', 'Pocas veces', 'Algunas veces', 'Con frecuencia', 'Con mucha frecuencia') = 'Ocurrido aunque sea una vez'", as.factor = T )
costa_rica_a$dinero_buses <- costa_rica_a$dinero_buses %>% recode("'Nunca' = 'Nunca'; c('Muy pocas veces', 'Pocas veces', 'Algunas veces', 'Con frecuencia', 'Con mucha frecuencia') = 'Ocurrido aunque sea una vez'", as.factor = T )

nicaragua_a$dinero_casa <- nicaragua_a$dinero_casa %>% recode("'Nunca' = 'Nunca'; c('Muy pocas veces', 'Pocas veces', 'Algunas veces', 'Con frecuencia', 'Con mucha frecuencia') = 'Ocurrido aunque sea una vez'", as.factor = T )
nicaragua_a$dinero_comida <- nicaragua_a$dinero_comida %>% recode("'Nunca' = 'Nunca'; c('Muy pocas veces', 'Pocas veces', 'Algunas veces', 'Con frecuencia', 'Con mucha frecuencia') = 'Ocurrido aunque sea una vez'", as.factor = T )
nicaragua_a$dinero_buses <- nicaragua_a$dinero_buses %>% recode("'Nunca' = 'Nunca'; c('Muy pocas veces', 'Pocas veces', 'Algunas veces', 'Con frecuencia', 'Con mucha frecuencia') = 'Ocurrido aunque sea una vez'", as.factor = T )

el_salvador_a$dinero_casa <- el_salvador_a$dinero_casa %>% recode("'Nunca' = 'Nunca'; c('Muy pocas veces', 'Pocas veces', 'Algunas veces', 'Con frecuencia', 'Con mucha frecuencia') = 'Ocurrido aunque sea una vez'", as.factor = T )
el_salvador_a$dinero_comida <- el_salvador_a$dinero_comida %>% recode("'Nunca' = 'Nunca'; c('Muy pocas veces', 'Pocas veces', 'Algunas veces', 'Con frecuencia', 'Con mucha frecuencia') = 'Ocurrido aunque sea una vez'", as.factor = T )
el_salvador_a$dinero_buses <- el_salvador_a$dinero_buses %>% recode("'Nunca' = 'Nunca'; c('Muy pocas veces', 'Pocas veces', 'Algunas veces', 'Con frecuencia', 'Con mucha frecuencia') = 'Ocurrido aunque sea una vez'", as.factor = T )

guatemala_a$dinero_casa <- guatemala_a$dinero_casa %>% recode("'Nunca' = 'Nunca'; c('Muy pocas veces', 'Pocas veces', 'Algunas veces', 'Con frecuencia', 'Con mucha frecuencia') = 'Ocurrido aunque sea una vez'", as.factor = T )
guatemala_a$dinero_comida <- guatemala_a$dinero_comida %>% recode("'Nunca' = 'Nunca'; c('Muy pocas veces', 'Pocas veces', 'Algunas veces', 'Con frecuencia', 'Con mucha frecuencia') = 'Ocurrido aunque sea una vez'", as.factor = T )
guatemala_a$dinero_buses <- guatemala_a$dinero_buses %>% recode("'Nunca' = 'Nunca'; c('Muy pocas veces', 'Pocas veces', 'Algunas veces', 'Con frecuencia', 'Con mucha frecuencia') = 'Ocurrido aunque sea una vez'", as.factor = T )

honduras_a$dinero_casa <- honduras_a$dinero_casa %>% recode("'Nunca' = 'Nunca'; c('Muy pocas veces', 'Pocas veces', 'Algunas veces', 'Con frecuencia', 'Con mucha frecuencia') = 'Ocurrido aunque sea una vez'", as.factor = T )
honduras_a$dinero_comida <- honduras_a$dinero_comida %>% recode("'Nunca' = 'Nunca'; c('Muy pocas veces', 'Pocas veces', 'Algunas veces', 'Con frecuencia', 'Con mucha frecuencia') = 'Ocurrido aunque sea una vez'", as.factor = T )
honduras_a$dinero_buses <- honduras_a$dinero_buses %>% recode("'Nunca' = 'Nunca'; c('Muy pocas veces', 'Pocas veces', 'Algunas veces', 'Con frecuencia', 'Con mucha frecuencia') = 'Ocurrido aunque sea una vez'", as.factor = T )



# Ver cambios
frq(datos_proc_a$distribucion_de_riqueza)

frq(datos_proc_a$dinero_casa)
frq(datos_proc_a$dinero_comida)
frq(datos_proc_a$dinero_buses)

frq(costa_rica_a$dinero_casa)
frq(costa_rica_a$dinero_comida)
frq(costa_rica_a$dinero_buses)

frq(nicaragua_a$dinero_casa)
frq(nicaragua_a$dinero_comida)
frq(nicaragua_a$dinero_buses)

frq(el_salvador_a$dinero_casa)
frq(el_salvador_a$dinero_comida)
frq(el_salvador_a$dinero_buses)

frq(guatemala_a$dinero_casa)
frq(guatemala_a$dinero_comida)
frq(guatemala_a$dinero_buses)

frq(honduras_a$dinero_casa)
frq(honduras_a$dinero_comida)
frq(honduras_a$dinero_buses)


#--- 4. Análisis ----

# 3. 5. Regresión lineal stepwise backwards -------------------------------

# 3. 5. 1. Modelo general (toda la muestra) -----------------------------------------------

general_reg_val2 <- na.omit(datos_proc_a) # hacer una base sin NA para las regresiones

intercept_only_general <- lm(distribucion_de_riqueza ~ 1, data = general_reg_val2) # crear base solo con interceptio

all_general<- lm(distribucion_de_riqueza ~., data = general_reg_val2) # crear modelo de regresión con todos los predictores para hacer backward

# NOTA: Quitando horas_trabajo el modelo funciona. Queda con un n de 105. Quitando seguro_social queda con un n de 226. OJO: esas variables las excluí en el script de procesamiento, pero aquí igual apliqué un na-omit para las otras variables que contenían NA.

stargazer::stargazer(all_general, type = "text") # mostrar tabla para modelo con todos los predictores


# Elaborar modelos por backard step
backward_general<- step(all_general, direction = "backward") # crear moidelo
bgen_val2 <- summary(backward_general) # Esto muestra el modelo con mejor ajuste





# 3. 5. 2. Modelo para Costa Rica -----------------------------------------

costa_rica_reg_val2 <- na.omit(costa_rica_a) # hacer una base sin NA para las regresiones

intercept_only_costa_rica <- lm(distribucion_de_riqueza ~ 1, data = costa_rica_reg_val2) # crear base solo con interceptio

all_costa_rica <- lm(distribucion_de_riqueza ~., data = costa_rica_reg_val2) # crear modelo de regresión con todos los predictores para hacer backward

# NOTA: Quitando horas_trabajo el modelo funciona. Queda con un n de 105. Quitando seguro_social queda con un n de 226. OJO: esas variables las excluí en el script de procesamiento, pero aquí igual apliqué un na-omit para las otras variables que contenían NA.

stargazer::stargazer(all_costa_rica, type = "text") # mostrar tabla para modelo con todos los predictores


# Elaborar modelos por backard step
backward_costa_rica <- step(all_costa_rica, direction = "backward") # crear moidelo
bcr_val2 <- summary(backward_costa_rica) # Esto muestra el modelo con mejor ajuste







# 3. 5. 3. Modelo para Nicaragua ------------------------------------------

nicaragua_reg_val2 <- na.omit(nicaragua_a) # hacer una base sin NA para las regresiones

intercept_only_nicaragua <- lm(distribucion_de_riqueza ~ 1, data = nicaragua_reg_val2) # hacer una base con solo el intercepto como prueba

all_nicaragua <- lm(distribucion_de_riqueza ~., data = nicaragua_reg_val2) # crear modelo de regresión con todos los predicores para hacer backward

stargazer::stargazer(all_nicaragua, type = "text") # ver modelo de regresión con todos los predictores

# Calcular modelos por backward step
backward_nicaragua <- step(all_nicaragua, direction = "backward") # crear modelo backward
bn_val2 <- summary(backward_nicaragua) # Esto muestra el modelo con mejor ajuste




# 3. 5. 4. Modelo para El Salvador ----------------------------------------

el_salvador_reg_val2 <- na.omit(el_salvador_a) # hacer una base sin NA para las regresiones

intercept_only_el_salvador <- lm(distribucion_de_riqueza ~ 1, data = el_salvador_reg_val2) # hacer una base con solo el intercepto como prueba

all_el_salvador <- lm(distribucion_de_riqueza ~., data = el_salvador_reg_val2) # crear modelo de regresión con todos los predictores para hacer backward

stargazer::stargazer(all_el_salvador, type = "text") # ver modelo de regresión con todos los predictores

# Calcular modelos por backward step
backward_el_salvador <- step(all_el_salvador, direction = "backward") # crear modelo backward
bes_val2 <- summary(backward_el_salvador) # Esto muestra el modelo con mejor ajuste




# 3. 5. 5. Modelo para Guatemala ------------------------------------------
guatemala_reg_val2 <- na.omit(guatemala_a) # hacer una base sin NA para las regresiones

intercept_only_guatemala <- lm(distribucion_de_riqueza ~ 1, data = guatemala_reg_val2) # hacer una base con solo el intercepto como prueba

all_guatemala <- lm(distribucion_de_riqueza ~., data = guatemala_reg_val2) # crear modelo de regresión con todos los predicores para hacer backward

stargazer::stargazer(all_guatemala, type = "text") # ver modelo de regresión con todos los predictores

# Calcular modelos por backward step
backward_guatemala <- step(all_guatemala, direction = "backward") # crear modelo backward
bg_val2 <- summary(backward_guatemala) # Esto muestra el modelo con mejor ajuste





# 3. 5. 6. Modelo para Honduras -------------------------------------------

honduras_reg_val2 <- na.omit(honduras_a) # hacer una base sin NA para las regresiones

intercept_only_honduras <- lm(distribucion_de_riqueza ~ 1, data = honduras_reg_val2) # hacer una base con solo el intercepto como prueba

all_honduras <- lm(distribucion_de_riqueza ~., data = honduras_reg_val2) # crear modelo de regresión con todos los predicores para hacer backward

stargazer::stargazer(all_honduras, type = "text") # ver modelo de regresión con todos los predictores

# Calcular modelos por backward step
backward_honduras <- step(all_honduras, direction = "backward") # crear modelo backward
bh_val2 <- summary(backward_honduras) # Esto muestra el modelo con mejor ajuste



## Visualizaciones


plot_summs(bgen_val2, bcr_val2, bn_val2, bes_val2, bg_val2, bh_val2,
           model.names = c("General",
                           "Costa Rica",
                           "Nicaragua",
                           "El Salvador",
                           "Guatemala",
                           "Honduras"),
           omit.coefs = c("trabajo_remuneradoSí",
                          "victimas_violenciaPoco miedo",
                          "val_org_lideres",
                          "val_org_resuelvan",
                          "ind_aut"),
           
           coefs = c("Pais|Nicaragua" = "paisNicaragua",
                     "Pais|Salvador" = "paisSalvador",
                     "Pais|Guatemala" = "paisGuatemala",
                     "Pais|Honduras" = "paisHonduras",
                     "Edad" = "edad",
                     # "Trabajo remmunerado|Si" = "trabajo_remuneradoSí",
                     "Número de personas en la casa" = "personas_casa",
                     "Dinero Comida|Ocurrido aunque sea una vez" = "dinero_comidaOcurrido aunque sea una vez",
                     "Dinero Casa|Ocurrido aunque sea una vez" = "dinero_casaOcurrido aunque sea una vez",
                     "Dinero Buses|Ocurrido aunque sea una vez" = "dinero_busesOcurrido aunque sea una vez",
                     "Victimas Violencia|Poco miedo" = "victimas_violenciaPoco miedo",
                     "Victimas Violencia|Miedo" = "victimas_violenciaMiedo",
                     "Victimas Violencia|Mucho miedo" = "victimas_violenciaMucho miedo",
                     "Se necesitan Org. Derechos Sexuales" = "org_dd_sexuales",
                     "Se necesitan Org. DDHH" = "org_ddhh",
                     "Se necesitan Org. Religión" = "org_religion",
                     "Se necesitan Org. Prevención Drogas" = "org_prev_drogas",
                     "Se necesitan Org. Empleo" = "org_empleo",
                     "Orgs. se crean por adultos con estudios" = "cr_org_adultos_estudio",
                     "Orgs. se crean por personas como ud." = "cr_org_pers_ud",
                     "Orgs. se crean por el gobierno" = "cr_org_gobs",
                     "Orgs se crean por las iglesas" = "cr_org_iglesias",
                     "Se valora que dirigentes recolecten fondos" = "val_org_recolecten",
                     # "Se valora que dirigentes sean lideres democraticos" = "val_org_lideres",
                     "Se valora que dirigentes escuchen" = "val_org_escuchan",
                     # "Se valora que dirigentes resuelvan problemas" = "val_org_resuelvan",
                     "Indice percepción situación actual" = "ind_perc",
                     "Indice percepción pasado" = "ind_perc_pas",
                     "Indice confianza instituciones" = "ind_conf_inst"),
           # "Indice autoritarismo" = "ind_aut"),
           scale = TRUE)

saveRDS(bgen_val2, "output/bgn_val2.rds")
saveRDS(bcr_val2, "output/bcr_val2.rds")
saveRDS(bn_val2, "output/bn_val2.rds")
saveRDS(bes_val2, "output/bes_val2.rds")
saveRDS(bg_val2, "output/bg_val2.rds")
saveRDS(bh_val2, "output/bh_val2.rds")

saveRDS(general_reg_val2, "output/general_reg_val2.rds")
saveRDS(costa_rica_reg_val2, "output/costa_rica_reg_val2.rds")
saveRDS(nicaragua_reg_val2, "output/nicaragua_reg_val2.rds")
saveRDS(guatemala_reg_val2, "output/guatemala_reg_val2.rds")
saveRDS(honduras_reg_val2, "output/honduras_reg_val2.rds")
saveRDS(el_salvador_reg_val2, "output/el_salvador_reg_val2.rds")



export_summs(bgen_val2, bcr_val2, bn_val2, bes_val2, bg_val2, bh_val2,
             model.names = c("General",
                             "Costa Rica",
                             "Nicaragua",
                             "El Salvador",
                             "Guatemala",
                             "Honduras"),
             coefs = c("Pais|Nicaragua" = "paisNicaragua",
                       "Pais|Salvador" = "paisSalvador",
                       "Pais|Guatemala" = "paisGuatemala",
                       "Pais|Honduras" = "paisHonduras",
                       "Edad" = "edad",
                       "Sexo|Hombre" = "sexoHombre",
                       "Trabajo remmunerado|Si" = "trabajo_remuneradoSí",
                       "Número de personas en la casa" = "personas_casa",
                       "Dinero Comida|Ocurrido aunque sea una vez" = "dinero_comidaOcurrido aunque sea una vez",
                       "Dinero Casa|Ocurrido aunque sea una vez" = "dinero_casaOcurrido aunque sea una vez",
                       "Dinero Buses|Ocurrido aunque sea una vez" = "dinero_busesOcurrido aunque sea una vez",
                       "Victimas Violencia|Poco miedo" = "victimas_violenciaPoco miedo",
                       "Victimas Violencia|Miedo" = "victimas_violenciaMiedo",
                       "Victimas Violencia|Mucho miedo" = "victimas_violenciaMucho miedo",
                       "Se necesitan Org. Derechos Sexuales" = "org_dd_sexuales",
                       "Se necesitan Org. DDHH" = "org_ddhh",
                       "Se necesitan Org. Religión" = "org_religion",
                       "Se necesitan Org. Prevención Drogas" = "org_prev_drogas",
                       "Se necesitan Org. Empleo" = "org_empleo",
                       "Orgs. se crean por adultos con estudios" = "cr_org_adultos_estudio",
                       "Orgs. se crean por personas como ud." = "cr_org_pers_ud",
                       "Orgs. se crean por el gobierno" = "cr_org_gobs",
                       "Orgs se crean por las iglesas" = "cr_org_iglesias",
                       "Se valora que dirigentes recoleten fondos" = "val_org_recolecten",
                       "Se valora que dirigentes sean lideres democraticos" = "val_org_lideres",
                       "Se valora que dirigentes escuhen" = "val_org_escuchan",
                       "Se valora que dirigentes resuelvan problemas" = "val_org_resuelvan",
                       "Indice percepción situación actual" = "ind_perc",
                       "Indice percepción pasado" = "ind_perc_pas",
                       "Indice confianza instituciones" = "ind_conf_inst",
                       "Indice autoritarismo" = "ind_aut"),
             scale = TRUE,
             to.file = "html",
             file.name = "output/tables/reg_tab_val2.html"
)

frq(costa_rica_reg_val2$distribucion_de_riqueza)
table1::table1(~distribucion_de_riqueza | dinero_casa, data = costa_rica_reg_val2)
table1::table1(~distribucion_de_riqueza | dinero_casa, data = nicaragua_reg_val2)
table1::table1(~distribucion_de_riqueza | dinero_casa, data = guatemala_reg_val2)

table1::table1(~distribucion_de_riqueza | dinero_casa, data = honduras_reg_val2)
table1::table1(~distribucion_de_riqueza | dinero_casa, data = el_salvador_reg_val2)
# Resultado: 