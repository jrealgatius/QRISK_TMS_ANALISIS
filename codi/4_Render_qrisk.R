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
rmarkdown::render(here::here("codi","3_Analisis_qrisk.Rmd"),
                  params = list(fitxers_test= test_param),
                  output_file = here::here("resultats",paste0("Informe_resultats",Sys.Date())))


# 3.2 Anàlisis per parts (Per problemes de memoria no fa tots els chuncks)

# # Exemple 

gc()
rm(list = ls())
library(dplyr)
test_param<-F
if (test_param) {subnom<-"test"} else subnom<-"global"
subgrup<-"NA"

rmarkdown::render(here::here("codi","3_Analisis_qrisk_I.Rmd"),
                  params = list(fitxers_test= test_param,subgrup=subgrup),
                  output_file = here::here("resultats",paste0("Informe_I",Sys.Date())))

rmarkdown::render(here::here("codi","3_Analisis_qriskII.Rmd"),
                  params = list(fitxers_test= test_param,subgrup=subgrup),
                  output_file = here::here("resultats",paste0("Informe_II",Sys.Date())))

rmarkdown::render(here::here("codi","3_Analisis_qriskIII.Rmd"),
                  params = list(fitxers_test= test_param,subgrup=subgrup),
                  output_file = here::here("resultats",paste0("Informe_III",Sys.Date())))

rmarkdown::render(here::here("codi","3_Analisis_qriskIV.Rmd"),
                  params = list(fitxers_test= test_param,subgrup=subgrup),
                  output_file = here::here("resultats",paste0("Informe_IV",Sys.Date())))

rmarkdown::render(here::here("codi","3_Analisis_qriskV.Rmd"),
                  params = list(fitxers_test= test_param,subgrup=subgrup),
                  output_file = here::here("resultats",paste0("Informe_V",Sys.Date())))


# ----------------------



#### Generació d'informes #######

# 1 Global i per subgrups 

# DG.ALTRES_PSICOTICS
# DG.BIPOLAR_MANIA
# DG.DEPRESSIO_GREU (Exclosos)
# DG.ESQUIZO

# --------------------------------------------------------

gc()
rm(list = ls())
library(dplyr)
test_param<-F
if (test_param) {subnom<-"test"} else subnom<-"global"
subgrup<-"DG.ALTRES_PSICOTICS"

dir_results<-paste0("resultats/",subgrup)

rmarkdown::render(here::here("codi","3_Analisis_qrisk_I.Rmd"),
                  params = list(fitxers_test= test_param,subgrup=subgrup),
                  output_file = here::here(dir_results,paste0("Informe_I",Sys.Date())))

rmarkdown::render(here::here("codi","3_Analisis_qriskII.Rmd"),
                  params = list(fitxers_test= test_param,subgrup=subgrup),
                  output_file = here::here(dir_results,paste0("Informe_II",Sys.Date())))

rmarkdown::render(here::here("codi","3_Analisis_qriskIII.Rmd"),
                  params = list(fitxers_test= test_param,subgrup=subgrup),
                  output_file = here::here(dir_results,paste0("Informe_III",Sys.Date())))

rmarkdown::render(here::here("codi","3_Analisis_qriskIV.Rmd"),
                  params = list(fitxers_test= test_param,subgrup=subgrup),
                  output_file = here::here(dir_results,paste0("Informe_IV",Sys.Date())))

rmarkdown::render(here::here("codi","3_Analisis_qriskV.Rmd"),
                  params = list(fitxers_test= test_param,subgrup=subgrup),
                  output_file = here::here(dir_results,paste0("Informe_V",Sys.Date())))




# 1 Global i per subgrups 

# DG.ALTRES_PSICOTICS
# DG.BIPOLAR_MANIA
# DG.DEPRESSIO_GREU (Exclosos)
# DG.ESQUIZO

# --------------------------------------------------------

gc()
rm(list = ls())
library(dplyr)
test_param<-F
if (test_param) {subnom<-"test"} else subnom<-"global"
subgrup<-"DG.BIPOLAR_MANIA"

dir_results<-paste0("resultats/",subgrup)

rmarkdown::render(here::here("codi","3_Analisis_qrisk_I.Rmd"),
                  params = list(fitxers_test= test_param,subgrup=subgrup),
                  output_file = here::here(dir_results,paste0("Informe_I",Sys.Date())))

rmarkdown::render(here::here("codi","3_Analisis_qriskII.Rmd"),
                  params = list(fitxers_test= test_param,subgrup=subgrup),
                  output_file = here::here(dir_results,paste0("Informe_II",Sys.Date())))

rmarkdown::render(here::here("codi","3_Analisis_qriskIII.Rmd"),
                  params = list(fitxers_test= test_param,subgrup=subgrup),
                  output_file = here::here(dir_results,paste0("Informe_III",Sys.Date())))

rmarkdown::render(here::here("codi","3_Analisis_qriskIV.Rmd"),
                  params = list(fitxers_test= test_param,subgrup=subgrup),
                  output_file = here::here(dir_results,paste0("Informe_IV",Sys.Date())))

rmarkdown::render(here::here("codi","3_Analisis_qriskV.Rmd"),
                  params = list(fitxers_test= test_param,subgrup=subgrup),
                  output_file = here::here(dir_results,paste0("Informe_V",Sys.Date())))


# 1 Global i per subgrups 

# DG.ALTRES_PSICOTICS
# DG.BIPOLAR_MANIA
# DG.DEPRESSIO_GREU (Exclosos)
# DG.ESQUIZO

# --------------------------------------------------------


gc()
rm(list = ls())
library(dplyr)
test_param<-F
if (test_param) {subnom<-"test"} else subnom<-"global"
subgrup<-"DG.ESQUIZO"

dir_results<-paste0("resultats/",subgrup)

rmarkdown::render(here::here("codi","3_Analisis_qrisk_I.Rmd"),
                  params = list(fitxers_test= test_param,subgrup=subgrup),
                  output_file = here::here(dir_results,paste0("Informe_I",Sys.Date())))

rmarkdown::render(here::here("codi","3_Analisis_qriskII.Rmd"),
                  params = list(fitxers_test= test_param,subgrup=subgrup),
                  output_file = here::here(dir_results,paste0("Informe_II",Sys.Date())))

rmarkdown::render(here::here("codi","3_Analisis_qriskIII.Rmd"),
                  params = list(fitxers_test= test_param,subgrup=subgrup),
                  output_file = here::here(dir_results,paste0("Informe_III",Sys.Date())))

rmarkdown::render(here::here("codi","3_Analisis_qriskIV.Rmd"),
                  params = list(fitxers_test= test_param,subgrup=subgrup),
                  output_file = here::here(dir_results,paste0("Informe_IV",Sys.Date())))

rmarkdown::render(here::here("codi","3_Analisis_qriskV.Rmd"),
                  params = list(fitxers_test= test_param,subgrup=subgrup),
                  output_file = here::here(dir_results,paste0("Informe_V",Sys.Date())))
# 1 Global i per subgrups 

# DG.ALTRES_PSICOTICS
# DG.BIPOLAR_MANIA
# DG.DEPRESSIO_GREU
# DG.ESQUIZO

# --------------------------------------------------------





































# c("DG.ALTRES_PSICOTICS","DG.BIPOLAR_MANIA","DG.DEPRESSIO_GREU","DG.ESQUIZO") %>% 
#   

#   purrr::map(~rmarkdown::render(here::here("codi","3_Analisis_qrisk.Rmd"), 
#       
# params = list(fitxers_test= T,subgrup=.x),
#                          output_file = here::here("resultats",paste0("Informe_resultats_test",.x))
#                          )
#       )



