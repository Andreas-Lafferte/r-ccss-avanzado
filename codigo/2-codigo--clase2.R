# ANÁLISIS AVANZADO DE DATOS SOCIALES USANDO R
# ESTACIÓN LASTARRIA - MAYO/JUNIO 2020
# PROFESOR: FELIPE RUIZ

# ---- 0. PAQUETES A UTILIZAR ---- 

library(haven)
library(survey)
library(dplyr)

# ---- 1. CARGA DE BASE DE DATOS (DESDE SPSS)  ----

casen_2017 <- read_sav("datos/Casen 2017.sav")

casen_2017 <- readRDS("datos/casen.RDS") # Alternativa ante no lectura SPSS

# ---- 2. CÁLCULO INDICADOR POBREZA MULTIDIMENSIONAL: NIVEL MUESTRAL ----

table(casen_2017$pobreza_multi_5d) # 1 = 44.972 // Variable original, coincide con libro de códigos

# ¿Nos sirve este valor para conocer la situación nacional?

# ---- 3. CÁLCULO INDICADOR POBREZA MULTIDIMENSIONAL: NIVEL POBLACIONAL ----

#DEFINIR MUESTRA COMPLEJA PARA DATOS DE NIVEL NACIONAL/REGIONAL
casen_pond <- svydesign(data = casen_2017, id=~1, strata = ~varstrat, weights = ~expr)

#Definir variables CASEN: expr (factor de expansión regional), varstrat (estratos de varianza), varunit (conglomerados de varianza)
#Definir argmentos svy: id (Formula or data frame specifying cluster ids from largest level to smallest level, ~0 or ~1 is a formula for no clusters), 
#strata (Formula or vector specifying strata), weights (Formula or vector specifying sampling weights as an alternative to prob),
#fpc (Finite population correction).

#CÁLCULO POBREZA MULTI5D NACIONAL
# Coincide número oficial con cálculo propio: tasa es 20,7 y absoluto 3.530.889
svytotal(~pobreza_multi_5d, casen_pond, na.rm=T) # Variable original

#CÁLCULO POBREZA MULTI5D SEGÚN REGIONES
#Coincide número oficial con cálculo propio: RM = 1.387.116
svyby(~pobreza_multi_5d, ~region, casen_pond, svytotal, na.rm=T)

# ----- 4. CONSTRUIR BASE PARA UTILIZAR OFF LINE

casen_seleccion <- select(casen_2017, expr, expc, varstrat, region, comuna)

# Guardar CASEN completa en formato RDS
saveRDS(casen_2017, file = "datos/casen.RDS")

# Guardar CASEN sólo con variables de interés para análisis en formato RDS
saveRDS(casen_seleccion, file = "datos/casen_pobreza.RDS")