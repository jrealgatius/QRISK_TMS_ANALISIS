## Generació d'informe

# 1. Lectura
rm(list = ls())
test_param<-F
rmarkdown::render(here::here("codi","1_lectura_qrisk.Rmd"),params = list(fitxers_test= test_param))


# 2. Preparació
rm(list = ls())
gc()
test_param<-F
rmarkdown::render(here::here("codi","2_preparacio_qrisk.Rmd"),params = list(fitxers_test= test_param))

# 3. Analisis total
gc()
rm(list = ls())
test_param<-F
rmarkdown::render(here::here("codi","3_Analisis_qrisk_global.Rmd"),
                  params = list(fitxers_test= test_param),
                  output_file = here::here("resultats",paste0("Informe_resultats",Sys.Date())))



# 3. Analisis total PER SUBGRUPS
# "DG.ALTRES_PSICOTICS" "DG.ESQUIZO" "DG.BIPOLAR_MANIA"

gc()
rm(list = ls())
library(dplyr)
test_param<-F
if (test_param) {subnom<-"test"} else subnom<-"global"
subgrupTMS<-"DG.ALTRES_PSICOTICS"
rmarkdown::render(here::here("codi","3_Analisis_qrisk_global.Rmd"),
                  params = list(fitxers_test= test_param,subgrupTMS=subgrupTMS),
                  output_file = here::here("resultats",paste0("Informe_resultats",subgrupTMS,"_",Sys.Date())))

gc()
rm(list = ls())
library(dplyr)
test_param<-F
if (test_param) {subnom<-"test"} else subnom<-"global"
subgrupTMS<-"DG.ESQUIZO"
rmarkdown::render(here::here("codi","3_Analisis_qrisk_global.Rmd"),
                  params = list(fitxers_test= test_param,subgrupTMS=subgrupTMS),
                  output_file = here::here("resultats",paste0("Informe_resultats",subgrupTMS,"_",Sys.Date())))

gc()
rm(list = ls())
library(dplyr)
test_param<-F
if (test_param) {subnom<-"test"} else subnom<-"global"
subgrupTMS<-"DG.BIPOLAR_MANIA"
rmarkdown::render(here::here("codi","3_Analisis_qrisk_global.Rmd"),
                  params = list(fitxers_test= test_param,subgrupTMS=subgrupTMS),
                  output_file = here::here("resultats",paste0("Informe_resultats",subgrupTMS,"_",Sys.Date())))



gc()
rm(list = ls())
library(dplyr)
test_param<-F
if (test_param) {subnom<-"test"} else subnom<-"global"
subgrupTMS<-"DG.DEPRESSIO_GREU"
rmarkdown::render(here::here("codi","3_Analisis_qrisk_global.Rmd"),
                  params = list(fitxers_test= test_param,subgrupTMS=subgrupTMS),
                  output_file = here::here("resultats",paste0("Informe_resultats",subgrupTMS,"_",Sys.Date())))





################################    CASOS INCIDENTS

# 3. Analisis total
gc()
rm(list = ls())
test_param<-F
subgrupTMS<-"NA"
subgrupINCIDENT<- T
rmarkdown::render(here::here("codi","3_Analisis_qrisk_global.Rmd"),
                  params = list(fitxers_test= test_param,subgrupTMS=subgrupTMS,subgrupINCIDENT=subgrupINCIDENT),
                  output_file = here::here("resultats",paste0("Informe_resultats_inc",subgrupTMS,"_",Sys.Date())))

# 3. Analisis total PER SUBGRUPS
# "DG.ALTRES_PSICOTICS" "DG.ESQUIZO" "DG.BIPOLAR_MANIA"

gc()
rm(list = ls())
library(dplyr)
test_param<-F
if (test_param) {subnom<-"test"} else subnom<-"global"
subgrupTMS<-"DG.ALTRES_PSICOTICS"
subgrupINCIDENT<- TRUE
rmarkdown::render(here::here("codi","3_Analisis_qrisk_global.Rmd"),
                  params = list(fitxers_test= test_param,subgrupTMS=subgrupTMS,subgrupINCIDENT=subgrupINCIDENT),
                  output_file = here::here("resultats",paste0("Informe_resultats_inc",subgrupTMS,"_",Sys.Date())))

gc()
rm(list = ls())
library(dplyr)
test_param<-F
if (test_param) {subnom<-"test"} else subnom<-"global"
subgrupTMS<-"DG.ESQUIZO"
subgrupINCIDENT<- TRUE
rmarkdown::render(here::here("codi","3_Analisis_qrisk_global.Rmd"),
                  params = list(fitxers_test= test_param,subgrupTMS=subgrupTMS,subgrupINCIDENT=subgrupINCIDENT),
                  output_file = here::here("resultats",paste0("Informe_resultats_inc",subgrupTMS,"_",Sys.Date())))

gc()
rm(list = ls())
library(dplyr)
test_param<-F
if (test_param) {subnom<-"test"} else subnom<-"global"
subgrupTMS<-"DG.BIPOLAR_MANIA"
subgrupINCIDENT<- TRUE
rmarkdown::render(here::here("codi","3_Analisis_qrisk_global.Rmd"),
                  params = list(fitxers_test= test_param,subgrupTMS=subgrupTMS,subgrupINCIDENT=subgrupINCIDENT),
                  output_file = here::here("resultats",paste0("Informe_resultats_inc",subgrupTMS,"_",Sys.Date())))


gc()
rm(list = ls())
library(dplyr)
test_param<-F
if (test_param) {subnom<-"test"} else subnom<-"global"
subgrupTMS<-"DG.DEPRESSIO_GREU"
subgrupINCIDENT<- TRUE
rmarkdown::render(here::here("codi","3_Analisis_qrisk_global.Rmd"),
                  params = list(fitxers_test= test_param,subgrupTMS=subgrupTMS,subgrupINCIDENT=subgrupINCIDENT),
                  output_file = here::here("resultats",paste0("Informe_resultats_inc",subgrupTMS,"_",Sys.Date())))




