# limpio la memoria
rm( list=ls() )  # remove all objects
gc()             # garbage collection

require("data.table")
require("rpart")
require("rpart.plot")

setwd("~/buckets/b1/" )  # establezco la carpeta donde voy a trabajar
# cargo el dataset
dataset <- fread( "./datasets/dataset_pequeno.csv")

dir.create("./exp/", showWarnings = FALSE)
dir.create("./exp/CN4110/", showWarnings = FALSE)
# Establezco el Working Directory DEL EXPERIMENTO
setwd("./exp/CN4110/")

# uso esta semilla para los canaritos
set.seed(102191)

print(names(dataset))
print(dataset[1:20,Visa_mlimitecompra])

dataset[,"prop_consumo_visa" := Visa_mconsumospesos/Visa_mlimitecompra]

print(dataset[1:20,prop_consumo_visa])
# agrego los siguientes canaritos
for( i in 1:154 ) dataset[ , paste0("canarito", i ) :=  runif( nrow(dataset)) ]


# Usted utilice sus mejores hiperparamatros
# yo utilizo los encontrados por Elizabeth Murano
 modelo  <- rpart(formula= "clase_ternaria ~ .",
               data= dataset[ foto_mes==202107,],
               model = TRUE,
               xval = 0,
               cp = -0.5,
               minsplit =  600,
               minbucket = 150,
               maxdepth = 6)


#pdf(file = "./arbol_canaritos_consumo_visa.pdf", width=28, height=4)
prp(modelo, extra=101, digits=5, branch=1, type=4, varlen=0, faclen=0)
dev.off()
importancia <- modelo$variable.importance

plot(importancia, xlab="variable", 
     ylab="Importance", xaxt = "n", pch=20)
axis(1, at=1:310, labels=names(dataset))
library(vip)
vip(modelo,num_features = 30)
plot(vi(modelo))
print(vi(modelo),n=100)
