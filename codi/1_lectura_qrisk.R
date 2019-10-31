# 1. Lectura de fitxers 
#
gc()
rm(list=ls())

# 0. Inicialització de parametres  -----------------------------

# N test mostra a seleccionar  (Nmostra=Inf)

# Nmostra=Inf  # Seria tota la mostra
nmostra=Inf
fitxers_test<-T

# Carregar funcions de lectura de dades ------------------

link_source<-paste0("https://github.com/jrealgatius/Stat_codis/blob/master/funcions_propies.R","?raw=T")
devtools::source_url(link_source)

source("codi/global_lectura.R")

# Si fitxers_test  carrego fitxers mostra 
funcions_lectura_dades(mostra=fitxers_test)

# Lectura de fitxers  -------------
# Població
poblacio_dt<-nmostra %>% LLEGIR.POBLACIO() %>% 
  left_join(nmostra %>% LLEGIR.VAR.GEOSANITARIES()) %>% 
  left_join(nmostra %>% LLEGIR.SOCIOECONOMIQUES())

# Llegir historics 
cliniques_dt<-nmostra %>% LLEGIR.CLINIQUES()
analitiqu_dt<-nmostra %>% LLEGIR.ANALITIQUES()
facturats_dt<-nmostra %>% LLEGIR.FX.FACTURATS()
prescrits_dt<-nmostra %>% LLEGIR.FX.PRESCRITS()
problemes_dt<-nmostra %>% LLEGIR.PROBLEMES()
tabaquism_dt<-nmostra %>% LLEGIR.TABAC()
visites_dt<-nmostra %>% LLEGIR.VISITES()
visitesagr_dt<-nmostra %>% LLEGIR.VISITES_AGR()

# Generar data d'inclusio (data index)      ----------------

# Primera data de visita en periode 

# visites_dt capurar data minima 
temp_dt<-agregar_visites(visites_dt,bd.dindex = 20161231,finestra.dies = c(-Inf,0),data="dat")


# Recode 0->NA
temp_dt<-temp_dt %>% mutate_all(funs(ifelse(.==0,NA,.)))

temp_dt<-temp_dt %>% mutate(dtindex=pmin(visites_10999,visites_30999,na.rm = T))

# 





