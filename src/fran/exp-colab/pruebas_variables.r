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
dataset[, vm_mfinanciacion_limite := rowSums(cbind(Master_mfinanciacion_limite, Visa_mfinanciacion_limite), na.rm = TRUE)]

dataset[, vm_Fvencimiento := pmin(Master_Fvencimiento, Visa_Fvencimiento, na.rm = TRUE)]
dataset[, vm_Finiciomora := pmin(Master_Finiciomora, Visa_Finiciomora, na.rm = TRUE)]
dataset[, vm_msaldototal := rowSums(cbind(Master_msaldototal, Visa_msaldototal), na.rm = TRUE)]
dataset[, vm_msaldopesos := rowSums(cbind(Master_msaldopesos, Visa_msaldopesos), na.rm = TRUE)]
dataset[, vm_msaldodolares := rowSums(cbind(Master_msaldodolares, Visa_msaldodolares), na.rm = TRUE)]
dataset[, vm_mconsumospesos := rowSums(cbind(Master_mconsumospesos, Visa_mconsumospesos), na.rm = TRUE)]
dataset[, vm_mconsumosdolares := rowSums(cbind(Master_mconsumosdolares, Visa_mconsumosdolares), na.rm = TRUE)]
dataset[, vm_mlimitecompra := rowSums(cbind(Master_mlimitecompra, Visa_mlimitecompra), na.rm = TRUE)]
dataset[, vm_madelantopesos := rowSums(cbind(Master_madelantopesos, Visa_madelantopesos), na.rm = TRUE)]
dataset[, vm_madelantodolares := rowSums(cbind(Master_madelantodolares, Visa_madelantodolares), na.rm = TRUE)]
dataset[, vm_fultimo_cierre := pmax(Master_fultimo_cierre, Visa_fultimo_cierre, na.rm = TRUE)]
dataset[, vm_mpagado := rowSums(cbind(Master_mpagado, Visa_mpagado), na.rm = TRUE)]
dataset[, vm_mpagospesos := rowSums(cbind(Master_mpagospesos, Visa_mpagospesos), na.rm = TRUE)]
dataset[, vm_mpagosdolares := rowSums(cbind(Master_mpagosdolares, Visa_mpagosdolares), na.rm = TRUE)]
dataset[, vm_fechaalta := pmax(Master_fechaalta, Visa_fechaalta, na.rm = TRUE)]
dataset[, vm_mconsumototal := rowSums(cbind(Master_mconsumototal, Visa_mconsumototal), na.rm = TRUE)]
dataset[, vm_cconsumos := rowSums(cbind(Master_cconsumos, Visa_cconsumos), na.rm = TRUE)]
dataset[, vm_cadelantosefectivo := rowSums(cbind(Master_cadelantosefectivo, Visa_cadelantosefectivo), na.rm = TRUE)]
dataset[, vm_mpagominimo := rowSums(cbind(Master_mpagominimo, Visa_mpagominimo), na.rm = TRUE)]

# a partir de aqui juego con la suma de Mastercard y Visa
dataset[, vmr_Master_mlimitecompra := Master_mlimitecompra / vm_mlimitecompra]
dataset[, vmr_Visa_mlimitecompra := Visa_mlimitecompra / vm_mlimitecompra]
dataset[, vmr_msaldototal := vm_msaldototal / vm_mlimitecompra]
dataset[, vmr_msaldopesos := vm_msaldopesos / vm_mlimitecompra]
dataset[, vmr_msaldopesos2 := vm_msaldopesos / vm_msaldototal]
dataset[, vmr_msaldodolares := vm_msaldodolares / vm_mlimitecompra]
dataset[, vmr_msaldodolares2 := vm_msaldodolares / vm_msaldototal]
dataset[, vmr_mconsumospesos := vm_mconsumospesos / vm_mlimitecompra]
dataset[, vmr_mconsumosdolares := vm_mconsumosdolares / vm_mlimitecompra]
dataset[, vmr_madelantopesos := vm_madelantopesos / vm_mlimitecompra]
dataset[, vmr_madelantodolares := vm_madelantodolares / vm_mlimitecompra]
dataset[, vmr_mpagado := vm_mpagado / vm_mlimitecompra]
dataset[, vmr_mpagospesos := vm_mpagospesos / vm_mlimitecompra]
dataset[, vmr_mpagosdolares := vm_mpagosdolares / vm_mlimitecompra]
dataset[, vmr_mconsumototal := vm_mconsumototal / vm_mlimitecompra]
dataset[, vmr_mpagominimo := vm_mpagominimo / vm_mlimitecompra]



dataset[,tactivo_corriente := mcuentas_saldo+
            mplazo_fijo_dolares+
            mplazo_fijo_pesos+
            minversion1_pesos+
            minversion1_dolares+
            minversion2]
dataset[,tpasivo_corriente := vm_mconsumospesos+
            mprestamos_personales+
            mprestamos_prendarios+
            mprestamos_hipotecarios+
            mcuenta_debitos_automaticos+
            mttarjeta_master_debitos_automaticos+
            mpagodeservicios+
            mpagomiscuentas+
            mcomisiones_mantenimiento+
            mcomisiones_otras]
dataset[,iliquidez := tactivo_corriente/tpasivo_corriente]
dataset[,psaldo_cc := mcuentas_saldo/ccuenta_corriente]
dataset[,psaldo_ca := mcuentas_saldo/ccaja_ahorro]
dataset[,psaldo_ctas := mcuentas_saldo/(ccaja_ahorro + ccuenta_corriente)]
dataset[,isaldo_debito := mcuentas_saldo/ctarjeta_debito]
dataset[,iconsumo_payroll := (vmr_mconsumospesos + vmr_mconsumosdolares)/mpayroll]
dataset[,ifidelidad1 := cliente_antiguedad * cliente_edad]
dataset[,ipayroll_chq := cpayroll_trx/mcheques_emitidos]
dataset[,pcons_trans_m := mtarjeta_master_consumo / ctarjeta_master_transacciones]
dataset[,pcons_trans_v := mtarjeta_visa_consumo / ctarjeta_visa_transacciones]
dataset[,pcons_trans_vm := (pcons_trans_m+pcons_trans_v)/2]
dataset[,irent_prod := mrentabilidad / cproductos]
dataset[,tprestamos := mprestamos_personales+ mprestamos_prendarios +
            mprestamos_hipotecarios]
dataset[,cprestamos := cprestamos_personales+ cprestamos_prendarios +
            cprestamos_hipotecarios]
dataset[,pprestamos := tprestamos/cprestamos]
dataset[,cinversiones := cplazo_fijo + cinversion1 + cinversion2
]
dataset[,tinversiones := mplazo_fijo_pesos + mplazo_fijo_dolares + minversion1_pesos + 
minversion1_dolares + minversion2]
dataset[,cseguros := cseguro_vida + cseguro_auto + cseguro_vivienda + 
cseguro_accidentes_personales]
dataset[,cacred_haberes := cpayroll_trx + cpayroll2_trx
]
dataset[,tacred_haberes := mpayroll + mpayroll2]
dataset[,cctransferencias := ctransferencias_recibidas + ctransferencias_emitidas]
dataset[,tmtransferencias := mtransferencias_recibidas + mtransferencias_emitidas]
dataset[,ptransferencias_recibidas := mtransferencias_recibidas / ctransferencias_recibidas]
dataset[,ptransferencias_emitidas := mtransferencias_emitidas / ctransferencias_emitidas]
dataset[,itransferencias := (mtransferencias_emitidas + mtransferencias_recibidas) / (ctransferencias_recibidas + ctransferencias_emitidas)]
# Verificar si los denominadores son diferentes de cero
dataset$denominador <- dataset$ctransferencias_recibidas + dataset$ctransferencias_emitidas

# Calcular pponderado_transeferencias
dataset$pponderado_transeferencias <- ifelse(dataset$denominador != 0,
                                             (dataset$mtransferencias_recibidas * dataset$ctransferencias_recibidas +
                                                dataset$mtransferencias_emitidas * dataset$ctransferencias_emitidas) /
                                               dataset$denominador,
                                             0)

# Eliminar la columna de auxiliar de denominador si ya no la necesitas
dataset <- subset(dataset, select = -c(denominador))

dataset[,ccheques := ccheques_depositados + ccheques_emitidos]  
dataset[,tcheques := mcheques_depositados + mcheques_emitidos] 
dataset[, ratio_ccheques := ifelse(ccheques_emitidos != 0, ccheques_depositados / ccheques_emitidos, NA)]
dataset[, ratio_mcheques := ifelse(mcheques_emitidos != 0, mcheques_depositados / mcheques_emitidos, NA)]
dataset[, pcheques_depositados := ifelse(ccheques_depositados != 0, mcheques_depositados / ccheques_depositados, NA)]
dataset[, pcheques_emitidos := ifelse(ccheques_emitidos != 0, mcheques_emitidos / ccheques_emitidos, NA)]
dataset[,toperaciones_sucursal := ccajas_consultas + ccajas_depositos + ccajas_extracciones + ccajas_otras]
dataset[, edad_bin := cut(cliente_edad, breaks = c(0, 30, 60, Inf), labels = c(0, 1, 2), right = FALSE)]
dataset[,trentabilidad_mensual := mrentabilidad + mcomisiones + mactivos_margen + mpasivos_margen ]
dataset[,prentabilidad_mensual := trentabilidad_mensual / mrentabilidad_annual     ]  
dataset[,prentabilidad_mensual := mrentabilidad / mrentabilidad_annual ]
dataset[,icomisiones := (mcomisiones - mean(mcomisiones)) / sd(mcomisiones)     ]  
dataset[,iactivos := (mactivos_margen - mean(mactivos_margen)) / sd(mactivos_margen)     ]  
dataset[,ipasivos := (mpasivos_margen  - mean(mpasivos_margen )) / sd(mpasivos_margen )     ]  
dataset[, ratio_movimiento_capital := ifelse((mpayroll + mpayroll2 ) != 0, (mtransferencias_emitidas -  ccajas_extracciones) / (mpayroll + mpayroll2 ) , NA)]
dataset[ ,ratio_endeudamiento :=   ifelse( (mcuentas_saldo + 
                                              mtransferencias_recibidas + mpayroll + mpayroll2 + mcheques_depositados   )!=0, (Visa_madelantopesos + Visa_madelantodolares + Master_madelantopesos +
                                                                                                                                 Master_madelantodolares + mpagomiscuentas + mpagodeservicios + mactivos_margen + 
                                                                                                                                 cdescubierto_preacordado + mtarjeta_visa_consumo + mtarjeta_master_consumo) / (mcuentas_saldo + 
                                                                                                                                                                                                                  mtransferencias_recibidas + mpayroll + mpayroll2 + mcheques_depositados   ), NA)
       ]
dataset[ ,ratio_ahorro :=   ifelse( (mtransferencias_recibidas + mpayroll + mpayroll2 + mcheques_depositados   )!=0, (mcuentas_saldo + mplazo_fijo_dolares + mplazo_fijo_pesos + minversion1_pesos + minversion1_dolares + minversion2) / ( mtransferencias_recibidas + mpayroll + mpayroll2 + mcheques_depositados   ), NA) ]  
dataset[,patm_other := matm_other / catm_trx_other]
dataset[,patm := matm / catm_trx]
dataset[,pforex_buy := mforex_buy / cforex_buy]
dataset[,pforex_sell := mforex_sell / cforex_sell]
dataset[,ratio_cforex_buysell := cforex_buy / cforex_sell]
dataset[,ratio_mforex_buysell := mforex_buy / mforex_sell]
dataset[,p_mextraccion_autoservicio := mextraccion_autoservicio / matm]
dataset[,p_cextraccion_autoservicio := cextraccion_autoservicio / catm_trx]
dataset[ ,dprestamos :=   ifelse( (cprestamos_personales + cprestamos_prendarios + cprestamos_hipotecarios) > 0 ,1, 0)]  
dataset[ ,dseguros :=   ifelse( (cseguro_vida + cseguro_auto + cseguro_vivienda + cseguro_accidentes_personales) > 0 ,1, 0)]  
dataset[ ,dcajas_ahorro :=   ifelse( (ccaja_ahorro) > 0 ,1, 0)]  
dataset[ ,dcuenta_corriente :=   ifelse( (ccuenta_corriente) > 0 ,1, 0)] 
dataset[ ,ddebitos_automaticos :=   ifelse( (ccuenta_debitos_automaticos) > 0 ,1, 0)]  
dataset[ ,dpagodeservicios :=   ifelse( (cpagodeservicios) > 0 ,1, 0)]  
dataset[ ,dpagomiscuentas :=   ifelse( (cpagomiscuentas) > 0 ,1, 0)]  
dataset[ ,dforex :=   ifelse( (cforex) > 0 ,1, 0)]  
dataset[ ,dforex_buy :=   ifelse( (cforex_buy) > 0 ,1, 0)]  
dataset[ ,dforex_sell :=   ifelse( (cforex_sell) > 0 ,1, 0)]  
dataset[ ,dtransferencias_emitidas :=   ifelse( (ctransferencias_emitidas) > 0 ,1, 0)]  
dataset[ ,duso_atm :=   ifelse( (catm_trx+catm_trx_other) > 0 ,1, 0)]  
dataset[ ,dcheques_emitidos :=   ifelse( ccheques_emitidos > 0 ,1, 0)]  
dataset[ ,dcheques_depositados :=   ifelse( ccheques_depositados > 0 ,1, 0)]  
dataset[ ,doperaciones_en_sucursal :=   ifelse( (
  ccajas_transacciones +
  ccajas_consultas +
  ccajas_depositos +
  ccajas_extracciones +
  ccajas_otras

) > 0 ,1, 0)]  
dataset[,tmontos := mrentabilidad+mrentabilidad_annual+mcomisiones+mactivos_margen+mpasivos_margen+mcuenta_corriente_adicional+mcuenta_corriente+mcaja_ahorro+mcaja_ahorro_adicional+mcaja_ahorro_dolares+mcuentas_saldo+mautoservicio+mtarjeta_visa_consumo+mtarjeta_master_consumo+mprestamos_personales+mprestamos_prendarios+mprestamos_hipotecarios+mplazo_fijo_dolares+mplazo_fijo_pesos+minversion1_pesos+minversion1_dolares+minversion2+mpayroll+mpayroll2+mcuenta_debitos_automaticos+mttarjeta_master_debitos_automaticos+mpagodeservicios+mpagomiscuentas+mcajeros_propios_descuentos+mtarjeta_visa_descuentos+mtarjeta_master_descuentos+mcomisiones_mantenimiento+mcomisiones_otras+mforex_buy+mforex_sell+mtransferencias_recibidas+mtransferencias_emitidas+mextraccion_autoservicio+mcheques_depositados+mcheques_emitidos+mcheques_depositados_rechazados+mcheques_emitidos_rechazados+matm+Master_mfinanciacion_limite+Master_msaldototal+Master_msaldopesos+Master_msaldodolares+Master_mconsumospesos+Master_mconsumosdolares+Master_mlimitecompra+Master_madelantopesos+Master_madelantodolares+Master_mpagado+Master_mpagospesos+Master_mpagosdolares+Master_mconsumototal+Master_mpagominimo]
dataset[,pond_montos := tmontos/sum(dataset$tmontos)]
dataset[,pond_rentabilidad := trentabilidad_mensual/sum(dataset$trentabilidad_mensual)]

dataset[,d_trentabilidad_mensual_neg := ifelse( (trentabilidad_mensual) < 0 ,1, 0)]
dataset[,d_iliquidez_negativa := ifelse( (iliquidez) < 0 ,1, 0)]
dataset[,dca_negativa := ifelse( (mcaja_ahorro) > 0 ,1, 0)]
dataset[,dcc_negativa := ifelse( (mcuenta_corriente ) > 0 ,1, 0)]
dataset[,indice_dummy := dca_negativa-dcc_negativa-dcajas_ahorro+dcuenta_corriente+ddebitos_automaticos+
          dpagodeservicios+dpagomiscuentas+dforex+dforex_buy+dforex_sell+dtransferencias_emitidas+duso_atm+
          dcheques_emitidos+dprestamos+dseguros+d_iliquidez_negativa-d_trentabilidad_mensual_neg]


