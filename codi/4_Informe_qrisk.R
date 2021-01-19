## Generació d'informe


# 1. Lectura
rm(list = ls())
test_param<-F
rmarkdown::render(here::here("codi","1_lectura_qrisk.Rmd"),params = list(fitxers_test= test_param))


# 2. Preparació
rm(list = ls())
test_param<-T
rmarkdown::render(here::here("codi","2_preparacio_qrisk.Rmd"),params = list(fitxers_test= test_param))

# 3. Analisis
rm(list = ls())
test_param<-T
rmarkdown::render(here::here("codi","3_Analisis_qrisk.Rmd"),
                  params = list(fitxers_test= test_param),output_file = here::here("resultats",paste0("Informe_resultats_test",Sys.Date())))




#### Generació d'informes #######

# 1 Global i per subgrups 

# DG.ALTRES_PSICOTICS
# DG.BIPOLAR_MANIA
# DG.DEPRESSIO_GREU
# DG.ESQUIZO

# # Exemple 

library(dplyr)

test_param<-F
if (test_param) subnom<-"test" else subnom<-"global"

subgrup<-"NA"
rmarkdown::render(here::here("codi","3_Analisis_qrisk.Rmd"),
                  params = list(fitxers_test= test_param,subgrup=subgrup),
                  output_file = here::here("resultats",paste0("Informe",Sys.Date(),"_resultats_",subnom,subgrup))
                  )

# # Exemple 
subgrup<-"DG.ESQUIZO"
rmarkdown::render(here::here("codi","3_Analisis_qrisk.Rmd"),
                  params = list(fitxers_test= test_param,subgrup=subgrup),
                  output_file = here::here("resultats",paste0("Informe",Sys.Date(),"_resultats_",subnom,subgrup))
                  )

# # Exemple 
subgrup<-"DG.BIPOLAR_MANIA"
rmarkdown::render(here::here("codi","3_Analisis_qrisk.Rmd"),
                  params = list(fitxers_test= test_param,subgrup=subgrup),
                  output_file = here::here("resultats",paste0("Informe",Sys.Date(),"_resultats_",subnom,subgrup))
                  )

# # Exemple 
subgrup<-"DG.DEPRESSIO_GREU"
rmarkdown::render(here::here("codi","3_Analisis_qrisk.Rmd"),
                  params = list(fitxers_test= test_param,subgrup=subgrup),
                  output_file = here::here("resultats",paste0("Informe,",Sys.Date(),"_resultats_",subnom,subgrup))
                  )

# # Exemple 
subgrup<-"DG.ALTRES_PSICOTICS"
rmarkdown::render(here::here("codi","3_Analisis_qrisk.Rmd"),
                  params = list(fitxers_test= test_param,subgrup=subgrup),
                  output_file = here::here("resultats",paste0("Informe,",Sys.Date(),"_resultats_",subnom,subgrup))
)




# c("DG.ALTRES_PSICOTICS","DG.BIPOLAR_MANIA","DG.DEPRESSIO_GREU","DG.ESQUIZO") %>% 
#   

#   purrr::map(~rmarkdown::render(here::here("codi","3_Analisis_qrisk.Rmd"), 
#       
# params = list(fitxers_test= T,subgrup=.x),
#                          output_file = here::here("resultats",paste0("Informe_resultats_test",.x))
#                          )
#       )



