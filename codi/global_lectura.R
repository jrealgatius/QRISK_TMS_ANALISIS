
#  Global lectura ---------------------


funcions_lectura_dades<-function(mostra) {
  
  # Si mostra --> Llegir fitxers mostra
  if (mostra==T) {

    LLEGIR.POBLACIO<<-function(n=Nmostra) {
      readRDS(here::here("dades/mostra","poblacio_dt.rds"))%>% as_tibble() %>% head(n) }
    
    LLEGIR.PROBLEMES<<-function(n=Nmostra) {
      readRDS(here::here("dades/mostra","problemes_dt.rds"))%>% as_tibble() %>% head(n) }
    
    LLEGIR.FX.PRESCRITS<<-function(n=Nmostra) {
      readRDS(here::here("dades/mostra","fx_prescrits_dt.rds"))%>% as_tibble() %>% head(n) }
    
    LLEGIR.FX.FACTURATS<<-function(n=Nmostra) {
      readRDS(here::here("dades/mostra","fx_facturats_dt.rds"))%>% as_tibble() %>% head(n) }
    
    LLEGIR.TABAC<<-function(n=Nmostra) {
      readRDS(here::here("dades/mostra","tabac_dt.rds"))%>% as_tibble() %>% head(n) }
    
    LLEGIR.ANALITIQUES<<-function(n=Nmostra) {
      readRDS(here::here("dades/mostra","analitiques_dt.rds"))%>% as_tibble() %>% head(n) }
    
    LLEGIR.CLINIQUES<<-function(n=Nmostra) {
      readRDS(here::here("dades/mostra","cliniques_dt.rds"))%>% as_tibble() %>% head(n) }
    
    LLEGIR.VAR.GEOSANITARIES<<-function(n=Nmostra) {
      readRDS(here::here("dades/mostra","var_geosanitaries_dt.rds"))%>% as_tibble() %>% head(n) }
    
    LLEGIR.SOCIOECONOMIQUES<<-function(n=Nmostra) {
      readRDS(here::here("dades/mostra","socioeconomiques_dt.rds"))%>% as_tibble() %>% head(n) }
    
    LLEGIR.VISITES<<-function(n=Nmostra) {
      readRDS(here::here("dades/mostra","visites_dt.Rds"))%>% as_tibble() %>% head(n) }
    
    LLEGIR.VISITES_AGR<<-function(n=Nmostra) {
      readRDS(here::here("dades/mostra","visitesagregades_dt.rds"))%>% as_tibble() %>% head(n) }
    
    
  }
  
  # Llegir fitxers globals
  
  if (mostra==F) {
    
    LLEGIR.POBLACIO<<-function(n=Nmostra) {
      data.table::fread("dades/sidiap" %>% here::here("QRISKTMS_entregable_poblacio_20190521_101138.txt")) %>% as_tibble() %>% head(n)}
    
    LLEGIR.PROBLEMES<<-function(n=Nmostra) {
      data.table::fread("dades/sidiap" %>% here::here("QRISKTMS_entregable_diagnostics_20190521_101138.txt")) %>% as_tibble() %>% head(n)}
    
    LLEGIR.FX.PRESCRITS<<-function(n=Nmostra) {
      data.table::fread("dades/sidiap" %>% here::here("QRISKTMS_entregable_farmacs_prescrits_20190521_101138.txt")) %>% as_tibble() %>% head(n)}
    
    LLEGIR.FX.FACTURATS<<-function(n=Nmostra) {
      data.table::fread("dades/sidiap" %>% here::here("QRISKTMS_entregable_farmacs_facturats_20190711_151825.txt")) %>% as_tibble() %>% head(n)}
    
    LLEGIR.TABAC<<-function(n=Nmostra) {
      data.table::fread("dades/sidiap" %>% here::here("QRISKTMS_entregable_tabaquisme_20190521_101138.txt")) %>% as_tibble() %>% head(n)}
    
    LLEGIR.ANALITIQUES<<-function(n=Nmostra) {
      data.table::fread("dades/sidiap" %>% here::here("QRISKTMS_entregable_variables_analitiques_20190521_101138.txt")) %>% as_tibble() %>% head(n)}
    
    LLEGIR.CLINIQUES<<-function(n=Nmostra) {
      data.table::fread("dades/sidiap" %>% here::here("QRISKTMS_entregable_variables_cliniques_20190521_101138.txt")) %>% as_tibble() %>% head(n)}
    
    LLEGIR.VAR.GEOSANITARIES<<-function(n=Nmostra) {
      data.table::fread("dades/sidiap" %>% here::here("QRISKTMS_entregable_variables_geo_sanitaries_20190521_101138.txt")) %>% as_tibble() %>% head(n)}
    
    LLEGIR.VAR.GEOSANITARIES<<-function(n=Nmostra) {
      data.table::fread("dades/sidiap" %>% here::here("QRISKTMS_entregable_variables_geo_sanitaries_20190521_101138.txt")) %>% as_tibble() %>% head(n)}
    
    LLEGIR.SOCIOECONOMIQUES<<-function(n=Nmostra) {
      data.table::fread("dades/sidiap" %>% here::here("QRISKTMS_entregable_variables_socioeconomiques_20190521_101138.txt")) %>% as_tibble() %>% head(n)}
    
    LLEGIR.VISITES<<-function(n=Nmostra) {
      data.table::fread("dades/sidiap" %>% here::here("QRISKTMS_entregable_visites_20190521_101138.txt")) %>% as_tibble() %>% head(n)}
    
    LLEGIR.VISITES_AGR<<-function(n=Nmostra) {
      data.table::fread("dades/sidiap" %>% here::here("QRISKTMS_entregable_visites_agregades_20190711_151825.txt")) %>% as_tibble() %>% head(n)}
    
    
  }
  
}










