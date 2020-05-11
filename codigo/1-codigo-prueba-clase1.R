
# ANÁLISIS AVANZADO DE DATOS SOCIALES USANDO R
# ESTACIÓN LASTARRIA - MAYO/JUNIO 2020
# PROFESOR: FELIPE RUIZ

# CARGAR BASE DE DATOS
datos <- read.csv2("datos/paraguay.csv")

# GUARDAR BASE DE DATOS EN FORMATO R (EN CARPETA DATOS)

saveRDS(datos, file = "datos/paraguay_rds.rds")


# GUARDAR RESULTADO DE UN ANÁLISIS (EN CARPETA RESULTADOS)
resultado1 <- summary(datos$edad)

install.packages("openxlsx")
library(openxlsx)
write.xlsx(resultado1, file = "resultados/impresora.xlsx")
