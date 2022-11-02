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

# Invertir escala de la variable dependiente

## Muestra general
datos_proc_a$distribucion_de_riqueza <- car::recode(datos_proc_a$distribucion_de_riqueza, " 1 = 5; 2 = 4; 3 = 3; 4 = 2; 5 = 1")

datos_proc_a$distribucion_de_riqueza <- set_labels(datos_proc_a$distribucion_de_riqueza, labels = c( "Muy Justa" = 1, 
                                                                                                     "Un poco justa" = 2, 
                                                                                                     "Ni justa ni injusta" = 3, 
                                                                                                     "Un poco injusta" = 4,
                                                                                                     "Muy injusta" = 5))



costa_rica_a$distribucion_de_riqueza <- car::recode(costa_rica_a$distribucion_de_riqueza, " 1 = 5; 2 = 4; 3 = 3; 4 = 2; 5 = 1")

costa_rica_a$distribucion_de_riqueza <- set_labels(costa_rica_a$distribucion_de_riqueza, labels = c( "Muy Justa" = 1, 
                                                                                                     "Un poco justa" = 2, 
                                                                                                     "Ni justa ni injusta" = 3, 
                                                                                                     "Un poco injusta" = 4,
                                                                                                     "Muy injusta" = 5))

nicaragua_a$distribucion_de_riqueza <- car::recode(nicaragua_a$distribucion_de_riqueza, " 1 = 5; 2 = 4; 3 = 3; 4 = 2; 5 = 1")

nicaragua_a$distribucion_de_riqueza <- set_labels(nicaragua_a$distribucion_de_riqueza, labels = c( "Muy Justa" = 1, 
                                                                                                     "Un poco justa" = 2, 
                                                                                                     "Ni justa ni injusta" = 3, 
                                                                                                     "Un poco injusta" = 4,
                                                                                                     "Muy injusta" = 5))

el_salvador_a$distribucion_de_riqueza <- car::recode(el_salvador_a$distribucion_de_riqueza, " 1 = 5; 2 = 4; 3 = 3; 4 = 2; 5 = 1")

el_salvador_a$distribucion_de_riqueza <- set_labels(el_salvador_a$distribucion_de_riqueza, labels = c( "Muy Justa" = 1, 
                                                                                                     "Un poco justa" = 2, 
                                                                                                     "Ni justa ni injusta" = 3, 
                                                                                                     "Un poco injusta" = 4,
                                                                                                     "Muy injusta" = 5))

guatemala_a$distribucion_de_riqueza <- car::recode(guatemala_a$distribucion_de_riqueza, " 1 = 5; 2 = 4; 3 = 3; 4 = 2; 5 = 1")

guatemala_a$distribucion_de_riqueza <- set_labels(guatemala_a$distribucion_de_riqueza, labels = c( "Muy Justa" = 1, 
                                                                                                     "Un poco justa" = 2, 
                                                                                                     "Ni justa ni injusta" = 3, 
                                                                                                     "Un poco injusta" = 4,
                                                                                                     "Muy injusta" = 5))


honduras_a$distribucion_de_riqueza <- car::recode(honduras_a$distribucion_de_riqueza, " 1 = 5; 2 = 4; 3 = 3; 4 = 2; 5 = 1")

honduras_a$distribucion_de_riqueza <- set_labels(honduras_a$distribucion_de_riqueza, labels = c( "Muy Justa" = 1, 
                                                                                                     "Un poco justa" = 2, 
                                                                                                     "Ni justa ni injusta" = 3, 
                                                                                                     "Un poco injusta" = 4,
                                                                                                     "Muy injusta" = 5))



# Ver cambios

frq(datos_proc_a$distribucion_de_riqueza)
frq(costa_rica_a$distribucion_de_riqueza)
frq(nicaragua_a$distribucion_de_riqueza)
frq(el_salvador_a$distribucion_de_riqueza)
frq(guatemala_a$distribucion_de_riqueza)
frq(honduras_a$distribucion_de_riqueza)


#--- 4. Análisis ----

# 3. 5. Regresión lineal stepwise backwards -------------------------------

# 3. 5. 1. Modelo general (toda la muestra) -----------------------------------------------

general_reg <- na.omit(datos_proc_a) # hacer una base sin NA para las regresiones

intercept_only_general <- lm(distribucion_de_riqueza ~ 1, data = general_reg) # crear base solo con interceptio

all_general<- lm(distribucion_de_riqueza ~., data = general_reg) # crear modelo de regresión con todos los predictores para hacer backward

# NOTA: Quitando horas_trabajo el modelo funciona. Queda con un n de 105. Quitando seguro_social queda con un n de 226. OJO: esas variables las excluí en el script de procesamiento, pero aquí igual apliqué un na-omit para las otras variables que contenían NA.

stargazer::stargazer(all_general, type = "text") # mostrar tabla para modelo con todos los predictores


# Elaborar modelos por backard step
backward_general<- step(all_general, direction = "backward") # crear moidelo
bgen <- summary(backward_general) # Esto muestra el modelo con mejor ajuste





# 3. 5. 2. Modelo para Costa Rica -----------------------------------------

costa_rica_reg <- na.omit(costa_rica_a) # hacer una base sin NA para las regresiones

intercept_only_costa_rica <- lm(distribucion_de_riqueza ~ 1, data = costa_rica_reg) # crear base solo con interceptio

all_costa_rica <- lm(distribucion_de_riqueza ~., data = costa_rica_reg) # crear modelo de regresión con todos los predictores para hacer backward

# NOTA: Quitando horas_trabajo el modelo funciona. Queda con un n de 105. Quitando seguro_social queda con un n de 226. OJO: esas variables las excluí en el script de procesamiento, pero aquí igual apliqué un na-omit para las otras variables que contenían NA.

stargazer::stargazer(all_costa_rica, type = "text") # mostrar tabla para modelo con todos los predictores


# Elaborar modelos por backard step
backward_costa_rica <- step(all_costa_rica, direction = "backward") # crear moidelo
bcr <- summary(backward_costa_rica) # Esto muestra el modelo con mejor ajuste







# 3. 5. 3. Modelo para Nicaragua ------------------------------------------

nicaragua_reg <- na.omit(nicaragua_a) # hacer una base sin NA para las regresiones

intercept_only_nicaragua <- lm(distribucion_de_riqueza ~ 1, data = nicaragua_reg) # hacer una base con solo el intercepto como prueba

all_nicaragua <- lm(distribucion_de_riqueza ~., data = nicaragua_reg) # crear modelo de regresión con todos los predicores para hacer backward

stargazer::stargazer(all_nicaragua, type = "text") # ver modelo de regresión con todos los predictores

# Calcular modelos por backward step
backward_nicaragua <- step(all_nicaragua, direction = "backward") # crear modelo backward
bn <- summary(backward_nicaragua) # Esto muestra el modelo con mejor ajuste




# 3. 5. 4. Modelo para El Salvador ----------------------------------------

el_salvador_reg <- na.omit(el_salvador_a) # hacer una base sin NA para las regresiones

intercept_only_el_salvador <- lm(distribucion_de_riqueza ~ 1, data = el_salvador_reg) # hacer una base con solo el intercepto como prueba

all_el_salvador <- lm(distribucion_de_riqueza ~., data = el_salvador_reg) # crear modelo de regresión con todos los predictores para hacer backward

stargazer::stargazer(all_el_salvador, type = "text") # ver modelo de regresión con todos los predictores

# Calcular modelos por backward step
backward_el_salvador <- step(all_el_salvador, direction = "backward") # crear modelo backward
bes <- summary(backward_el_salvador) # Esto muestra el modelo con mejor ajuste




# 3. 5. 5. Modelo para Guatemala ------------------------------------------
guatemala_reg <- na.omit(guatemala_a) # hacer una base sin NA para las regresiones

intercept_only_guatemala <- lm(distribucion_de_riqueza ~ 1, data = guatemala_reg) # hacer una base con solo el intercepto como prueba

all_guatemala <- lm(distribucion_de_riqueza ~., data = guatemala_reg) # crear modelo de regresión con todos los predicores para hacer backward

stargazer::stargazer(all_guatemala, type = "text") # ver modelo de regresión con todos los predictores

# Calcular modelos por backward step
backward_guatemala <- step(all_guatemala, direction = "backward") # crear modelo backward
bg <- summary(backward_guatemala) # Esto muestra el modelo con mejor ajuste





# 3. 5. 6. Modelo para Honduras -------------------------------------------

honduras_reg <- na.omit(honduras_a) # hacer una base sin NA para las regresiones

intercept_only_honduras <- lm(distribucion_de_riqueza ~ 1, data = honduras_reg) # hacer una base con solo el intercepto como prueba

all_honduras <- lm(distribucion_de_riqueza ~., data = honduras_reg) # crear modelo de regresión con todos los predicores para hacer backward

stargazer::stargazer(all_honduras, type = "text") # ver modelo de regresión con todos los predictores

# Calcular modelos por backward step
backward_honduras <- step(all_honduras, direction = "backward") # crear modelo backward
bh <- summary(backward_honduras) # Esto muestra el modelo con mejor ajuste



## Visualizaciones


plot_summs(bgen, bcr, bn, bes, bg, bh,
           model.names = c("General",
                           "Costa Rica",
                           "Nicaragua",
                           "El Salvador",
                           "Guatemala",
                           "Honduras"),
           omit.coefs = c("trabajo_remuneradoSí",
                          "dinero_comidaPocas veces",
                          "dinero_casaMuy pocas veces",
                          "dinero_casaPocas veces",
                          "dinero_casaAlgunas veces",
                          "dinero_casaCon frecuencia",
                          "dinero_busesMuy pocas veces",
                          "dinero_busesPocas veces",
                          "dinero_busesAlgunas veces",
                          "dinero_busesCon frecuencia",
                          "dinero_busesCon mucha frecuencia",
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
                     "Dinero Comida|Muy pocas veces" = "dinero_comidaMuy pocas veces",
                     # "Dinero Comida|Pocas veces" = "dinero_comidaPocas veces",
                     "Dinero Comida|Algunas veces" = "dinero_comidaAlgunas veces",
                     "Dinero Comida|Con frecuencia" = "dinero_comidaCon frecuencia",
                     "Dinero Comida|Con mucha frecuencia" = "dinero_comidaCon mucha frecuencia",
                     # "Dinero Casa|Muy pocas veces" = "dinero_casaMuy pocas veces",
                     # "Dinero Casa|Pocas veces" = "dinero_casaPocas veces",
                     # "Dinero Casa|Algunas veces" = "dinero_casaAlgunas veces",
                     # "Dinero Casa|Con frecuencia" = "dinero_casaCon frecuencia",
                     "Dinero Casa|Con mucha frecuencia" = "dinero_casaCon mucha frecuencia",
                     # "Dinero Buses|Muy pocas veces" = "dinero_busesMuy pocas veces",
                     # "Dinero Buses|Pocas veces" = "dinero_busesPocas veces",
                     # "Dinero Buses|Algunas veces" = "dinero_busesAlgunas veces",
                     # "Dinero Buses|Con frecuencia" = "dinero_busesCon frecuencia",
                     # "Dinero Buses|Con mucha frecuencia" = "dinero_busesCon mucha frecuencia",
                     # "Victimas Violencia|Poco miedo" = "victimas_violenciaPoco miedo",
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


export_summs(bgen, bcr, bn, bes, bg, bh,
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
                       "Trabajo remmunerado|Si" = "trabajo_remuneradoSí",
                       "Número de personas en la casa" = "personas_casa",
                       "Dinero Comida|Muy pocas veces" = "dinero_comidaMuy pocas veces",
                       "Dinero Comida|Pocas veces" = "dinero_comidaPocas veces",
                       "Dinero Comida|Algunas veces" = "dinero_comidaAlgunas veces",
                       "Dinero Comida|Con frecuencia" = "dinero_comidaCon frecuencia",
                       "Dinero Comida|Con mucha frecuencia" = "dinero_comidaCon mucha frecuencia",
                       "Dinero Casa|Muy pocas veces" = "dinero_casaMuy pocas veces",
                       "Dinero Casa|Pocas veces" = "dinero_casaPocas veces",
                       "Dinero Casa|Algunas veces" = "dinero_casaAlgunas veces",
                       "Dinero Casa|Con frecuencia" = "dinero_casaCon frecuencia",
                       "Dinero Casa|Con mucha frecuencia" = "dinero_casaCon mucha frecuencia",
                       "Dinero Buses|Muy pocas veces" = "dinero_busesMuy pocas veces",
                       "Dinero Buses|Pocas veces" = "dinero_busesPocas veces",
                       "Dinero Buses|Algunas veces" = "dinero_busesAlgunas veces",
                       "Dinero Buses|Con frecuencia" = "dinero_busesCon frecuencia",
                       "Dinero Buses|Con mucha frecuencia" = "dinero_busesCon mucha frecuencia",
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
             to.file = "xlsx",
             file.name = "output/tables/reg_tab_val1.xlsx"
)

# Resultado: el análisis muestra los mismos resultados, pero con el signo contrario. Eso significa que la escala en la versióin original está bien.