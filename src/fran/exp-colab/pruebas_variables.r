# Este script genera graficos que muestra que para algunos meses,
#  ciertas variables #  fueron pisadas con CEROS por el sector de
#  IT que genera el DataWarehouse

# limpio la memoria
rm(list = ls()) # remove all objects
gc() # garbage collection


require("data.table")

# Parametros del script
PARAM <- list()
PARAM$dataset <- "./datasets/competencia_2024.csv.gz"
PARAM$experimento <- "prueba_variables"
# FIN Parametros del script

#------------------------------------------------------------------------------
#------------------------------------------------------------------------------
# Aqui comienza el programa

# copio si hace falta el dataset

setwd("~/buckets/b1/")

# cargo el dataset
dataset <- fread(PARAM$dataset) # donde entreno


# creo la carpeta donde va el experimento
dir.create(paste0("./exp-flow/", PARAM$experimento, "/"), showWarnings = FALSE)
# Establezco el Working Directory DEL EXPERIMENTO
setwd(paste0("./exp-flow/", PARAM$experimento, "/"))


# ordeno el dataset
setorder(dataset, foto_mes, numero_de_cliente)

campos_buenos <- setdiff(
  colnames(dataset),
  c("numero_de_cliente", "foto_mes", "clase_ternaria")
)

#------------------------------------------------------------------------------
# Para cada variable ,
# grafico para cada mes el ratio de ceros que tiene esa variable
# el zeroes_ratio de una variable para un mes dado
# es el cociente entre
#   la cantidad de veces que la variable toma el valor cero ese mes
#   y la cantidad total de registros para ese mes




