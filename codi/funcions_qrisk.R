
resum_events_v3<-function(dt=dadestotal,evento="RD",temps="temps",valorevent="Si") {
  
  # evento="EV.CVD"
  # temps="EV.CVD_temps"
  # valorevent=1
  # dt=dades
  
  est.rate<-function(ncas=evento,ntar=P_Years) {
    pp<-epiR::epi.conf(as.matrix(cbind(ncas, ntar)), ctype = "inc.rate", method = "exact", N = 1000, design = 1, conf.level = 0.95) 
    pp$est}
  
  est.Linf<-function(ncas=evento,ntar=P_Years) {
    pp<-epiR::epi.conf(as.matrix(cbind(ncas, ntar)), ctype = "inc.rate", method = "exact", N = 1000, design = 1, conf.level = 0.95) 
    pp$lower}
  
  est.Lsup<-function(ncas=evento,ntar=P_Years) {
    pp<-epiR::epi.conf(as.matrix(cbind(ncas, ntar)), ctype = "inc.rate", method = "exact", N = 1000, design = 1, conf.level = 0.95) 
    pp$upper}
  
  
  dt %>% summarise(Patients=n(),
                   P_Years=sum(!!sym(temps)),
                   Years_free_event_mean=mean(!!sym(temps)),
                   Years_free_event_median=median(!!sym(temps) %>% as.numeric),
                   N_events=sum(!!sym(evento)==valorevent),
                   Event_rate_1000=((N_events/P_Years)*1000),
                   Event_rate_IC95_inf=est.Linf(N_events,P_Years)*1000,
                   Event_rate_IC95_sup=est.Lsup(N_events,P_Years)*1000,
                   IA_100=(N_events/Patients)*100,
                   IA_100_IC95_inf=est.Linf(N_events,Patients)*100,
                   IA_100_IC95_sup=est.Lsup(N_events,Patients)*100
                   
  )
}



genera_output_HR<-function(x_event="EV.CVD",x_temps="EV.CVD_temps",dt=dt_temp) {
  
  # x_event="EV.CVD"
  # x_temps="EV.CVD_temps"
  # dt=dt_temp
  
  # x_event=llistaevents[1]
  # x_temps=llista_temps[1]
  
  sym_event=sym(x_event)
  sym_temps=sym(x_temps)
  
  label_outcome<-read_conductor(conductor_variables) %>% filter(camp==x_event) %>% pull(descripcio)
  
  cat("\n")
  cat("\n## OUTCOME: ",label_outcome,"\n")
  
  formu1<-paste0("Surv(",x_temps,",",x_event,") ~ QRISK3_2017") %>% as.formula()
  formu2<-paste0("Surv(",x_temps,",",x_event,") ~ QRISK3_2017.cat") %>% as.formula()
  model1<-try(coxph(formu1,data=dt))
  model2<-try(coxph(formu2,data=dt))
  
  SI_events=!(summary(model1)["nevent"]==0)
  
  # Extreure i imprimir R2, 
  R2<-CoxR2::coxr2(model1)
  R2<-psychometric::CI.Rsq(R2$rsq, summary(model1)$n, 1) 
  R2_text<-paste0("R Squared: ",round(R2$Rsq,3), "  95%IC:( ",round(R2$LCL,3),"-",round(R2$UCL,3)," )")
  
  # sjPlot::tab_model(model2,show.aic=T,dv.labels=label_outcome)
  if (SI_events) {cat(sjPlot::tab_model(model1,model2,show.aic=T,show.r2=T,title = label_outcome,dv.labels=R2_text)$knitr,"\n------\n")}
  
  cat("\n")
  formu3<-paste0("Surv(",x_temps,",",x_event,") ~ regicor") %>% as.formula()
  formu4<-paste0("Surv(",x_temps,",",x_event,") ~ regicor.cat") %>% as.formula()
  model1 <- coxph(formu3,data=dt) 
  model2 <- coxph(formu4,data=dt) 
  
  # Extreure i imprimir R2, 
  R2<-CoxR2::coxr2(model1)
  R2<-psychometric::CI.Rsq(R2$rsq, summary(model1)$n, 1) 
  R2_text<-paste0("R Squared: ",round(R2$Rsq,3), "  95%IC:(",round(R2$LCL,3),"-",round(R2$UCL,3),")")
  
  if (SI_events) cat(sjPlot::tab_model(model1,model2,show.aic=T,title = label_outcome,dv.labels=R2_text)$knitr,"\n------\n")
  
  # Genero curva ROC (1 calculo indicadors i plotejo)
  # Només si te dos nivells l'event
  
  cat("\n")
  
  if (dt %>% select(!!sym_event) %>% distinct() %>% nrow()==2) {
    
    # g_qrisk <- try(pROC::roc(as.formula(paste0(x_event,"~QRISK3_2017")), data = dt %>% filter(!is.na(QRISK3_2017))))
    # auc1=try(pROC::auc(g_qrisk))
    # auc1_ci=try(pROC::ci(g_qrisk))
    # g_regicor <- try(pROC::roc(as.formula(paste0(x_event,"~regicor")), data = dt %>% filter(!is.na(regicor))))
    # auc2=try(pROC::auc(g_regicor))
    # auc2_ci=try(pROC::ci(g_regicor))
    
    fig<-fig_curva_roc(dt,event=x_event)
    print(fig)
    
  }
  
  cat("\n### Estratificat per sexe\n")
  
  # Estratificat per sexe
  models<-
    dt %>% split(.$sexe) %>% 
    map(~coxph(formu1,data=.x),.id="grup") %>% try()

  # Actualitzar models valids  
  models_events=models %>% map(~summary(.x)) %>% map(~.x["nevent"]) %>% unlist
  models<-models[which(models_events!=0)]
  
  models_events=models %>% map(~summary(.x)) %>% map(~.x["nevent"]) %>% unlist
  SI_events<-0 %in% models_events
  
  # Extreure i imprimir R2, 
  R2<-models %>% purrr::map(~CoxR2::coxr2(.x))
  R2<-map2(R2,models,~psychometric::CI.Rsq(.x$rsq, summary(.y)$n, 1))
  
  if (SI_events) {
    cat(sjPlot::tab_model(models,show.aic=T,auto.label = T,dv.labels = c(names(models)[[1]], names(models)[[2]]),title=label_outcome)$knitr,"\n-------\n",
      paste0("R Squared (Home): ",round(R2[[1]]$Rsq,3), "  95%IC:(",round(R2[[1]]$LCL,3),"-",round(R2[[1]]$UCL,3),")"),"\n",
      paste0("R Squared (Dona): ",round(R2[[2]]$Rsq,3), "  95%IC:(",round(R2[[2]]$LCL,3),"-",round(R2[[2]]$UCL,3),")") )
    }
    
  models<-
    dt %>% split(.$sexe) %>% 
    map(~coxph(formu3,data=.x),.id="grup") %>% try()
  
  # Actualitzar models valids  
  models_events=models %>% map(~summary(.x)) %>% map(~.x["nevent"]) %>% unlist
  models<-models[which(models_events!=0)]
  models_events=models %>% map(~summary(.x)) %>% map(~.x["nevent"]) %>% unlist
  SI_events<-0 %in% models_events
  
  if (SI_events) {
    cat(sjPlot::tab_model(models,show.aic=T,auto.label = T,dv.labels = c(names(models)[[1]], names(models)[[2]]),title=label_outcome)$knitr,"\n-------\n",
        paste0("R Squared (Home): ",round(R2[[1]]$Rsq,3), "  95%IC:(",round(R2[[1]]$LCL,3),"-",round(R2[[1]]$UCL,3),")"),"\n",
        paste0("R Squared (Dona): ",round(R2[[2]]$Rsq,3), "  95%IC:(",round(R2[[2]]$LCL,3),"-",round(R2[[2]]$UCL,3),")") )
    }
  
  cat("\n")  
  
  # plot roc per sexe
  fig<-dt %>% split(.$sexe) %>% map(~fig_curva_roc(dt=.x,event=x_event))
  
  if (SI_events) print(fig$Home + ggtitle("ROC Curve (Males)"))
  
  cat("\n")  
  if (SI_events) print (fig$Dona + ggtitle("ROC Curve (Females)"))
  
  
  # Estratificat per Grups d'edat
  models<-
    dt %>% split(.$edat.cat7,drop = T) %>% 
    map(~coxph(formu1,data=.x),.id="grup") %>% try() %>% 
    purrr::discard(models,.p=~is.na(.x$coefficients))
  
  # Actualitzar models valids  
  models_events=models %>% map(~summary(.x)) %>% map(~.x["nevent"]) %>% unlist
  models<-models[which(models_events!=0)]
  models_events=models %>% map(~summary(.x)) %>% map(~.x["nevent"]) %>% unlist
  SI_events<-0 %in% models_events
  
  if (SI_events) {
    cat(
    sjPlot::tab_model(models,show.aic=T,auto.label = T,dv.labels = c(names(models)[[1]], names(models)[[2]]),title=label_outcome)$knitr,"\n-------\n")
    }
    
  cat("\n")
  
  models<-
    dt %>% split(.$edat.cat7,drop = T) %>% 
    map(~coxph(formu3,data=.x),.id="grup") %>% try() %>% 
    purrr::discard(models,.p=~is.na(.x$coefficients))
  
  if (SI_events) {
    cat(sjPlot::tab_model(models,show.aic=T,auto.label = T,dv.labels = c(names(models)[[1]], names(models)[[2]]),title=label_outcome)$knitr) 
    }
    
  cat("\n")  
  
  # plot roc per grup d'edat
  
  porca<-dt %>% split(.$edat.cat7,drop = T)
  porca_noms<-names(porca)
  figs<-map2(.x=porca,
             .y=porca_noms,
             ~fig_curva_roc(dt=.x,event=x_event,titul = paste0("ROC curve: ", .y))
  ) %>% 
    purrr::discard(figs,  .p=~ is.null(.x))
  
  print(figs)
  
  cat("\n")
  # plot roc per grup d'edat + Sexe 
  
  porca<-dt %>% split(list(.$sexe,.$edat.cat7),drop = T)
  porca_noms<-names(porca)
  figs<-map2(.x=porca,
             .y=porca_noms,
             ~fig_curva_roc(dt=.x,event=x_event,titul = paste0("ROC curve: ", .y))
  ) %>% 
    purrr::discard(figs,  .p=~ is.null(.x))
  
  print(figs)
  
}



fig_curva_roc<-function(dt=dt,event=x_event, titul="ROC Curve") {
  
  # dt=dades
  # event=x_event
  
  if((dt[[event]] %>% unique()) %>% length()==2) {
    
    g_qrisk <- try(pROC::roc(as.formula(paste0(event,"~QRISK3_2017")), data = dt %>% filter(!is.na(QRISK3_2017)),
                             levels=c(0, 1),direction="<"))
    
    # g_qrisk <- try(pROC::roc(as.formula(paste0(event,"~QRISK3_2017")), data = dt %>% filter(!is.na(QRISK3_2017))))
    
    auc1=try(pROC::auc(g_qrisk))
    auc1_ci=try(pROC::ci(g_qrisk))
    g_regicor <- try(pROC::roc(as.formula(paste0(event,"~regicor")), data = dt %>% filter(!is.na(regicor)),
                               levels=c(0, 1),direction="<"))
    auc2=try(pROC::auc(g_regicor))
    auc2_ci=try(pROC::ci(g_regicor))
    
    dt %>% transmute(D:=!!sym(event) %>% as.numeric(),QRISK=QRISK3_2017 %>% as.numeric(),Regicor=as.numeric(regicor)) %>% 
      plotROC::melt_roc(d="D", m=c("QRISK", "Regicor")) %>% 
      ggplot(aes(d = D.D, m = M, color = name)) + 
      plotROC::geom_roc() + 
      plotROC::style_roc() +
      annotate("text", x = .75, y = .25, label = paste("AUC:",round(auc1,2),"   [95CI%:",round(auc1_ci[2],2),"-",round(auc1_ci[3],2),"]"),colour = "red") +
      
      annotate("text", x = .75, y = .18, label = paste("AUC:",round(auc2,2),"  [95CI%:",round(auc2_ci[2],2),"-",round(auc2_ci[3],2),"]"),colour = "skyblue2") + 
      ggtitle(titul) + 
      labs(colour="Risk scale")+ ggtitle(titul)
    
  } else { NULL }
  
}

taula_HR<-function(x_event="EV.CVD",x_temps="EV.CVD_temps",dt=dt_temp) {
  
  # x_event="EV.AIT"
  # x_temps="EV.AIT_temps"
  # dt=dt_temp %>% filter(regicor>=0 & QRISK3_2017>=0)
  
  sym_event=sym(x_event)
  sym_temps=sym(x_temps)
  
  formu1<-paste0("Surv(",x_temps,",",x_event,") ~ QRISK3_2017") %>% as.formula()
  modelQrisk<-try(coxph(formu1,data=dt))
  
  if (is.na(modelQrisk$coefficients)) {
    modelQrisk<-tibble(Parameter="QRISK3_2017",SE=NA,Coefficient=NA,CI_low=NA,CI_high=NA,p=NA,z=NA,df_error=NA) } else {
      modelQrisk<-modelQrisk %>% parameters::parameters(exponentiate =T) %>% as_tibble() }
  
  
  formu2<-paste0("Surv(",x_temps,",",x_event,") ~ regicor") %>% as.formula()
  modelRegicor <- try(coxph(formu2,data=dt)) 
  
  if (is.na(modelRegicor$coefficients)) {
    modelRegicor<-tibble(Parameter="regicor",SE=NA,Coefficient=NA,CI_low=NA,CI_high=NA,p=NA,z=NA,df_error=NA) } else {
      modelRegicor<-modelRegicor %>% parameters::parameters(exponentiate =T) %>% as_tibble() }
  
  bind_cols(event=x_event,bind_rows(modelQrisk,modelRegicor)) %>% 
    select(-c(SE,z,df_error)) %>% rename(HR=Coefficient)
  
}


taula_AUC_R2<-function(x_event="EV.CVD",dt=dt_temp) {
  
  # x_event="EV.CVD"
  # dt=dades %>% filter(regicor>=0 & QRISK3_2017>=0)
  
  sym_event=sym(x_event)
  
  # Calcul de AUC
  
  if (length(table(dt[x_event]))==2 ) {
    
    g_qrisk <- try(pROC::roc(as.formula(paste0(x_event,"~QRISK3_2017")), data = dt %>% 
                               filter(!is.na(QRISK3_2017))))
    auc1_ci=try(pROC::ci(g_qrisk)) %>% as.numeric()
    
    g_regicor <- try(pROC::roc(as.formula(paste0(x_event,"~regicor")), data = dt %>% 
                                 filter(!is.na(regicor))))
    auc2_ci=try(pROC::ci(g_regicor)) %>% as.numeric()
    
  } else {
    
    auc1_ci=c(NA,NA,NA)
    auc2_ci=c(NA,NA,NA)
    
  }
  
  
  
  tibble(AUC.qrisk=auc1_ci[2],AUC.qrisk_linf=auc1_ci[1],AUC.qrisk_lsup=auc1_ci[3], 
         AUC.regicor=auc2_ci[2],AUC.regicor_linf=auc2_ci[1],AUC.regicor_lsup=auc2_ci[3])
  
  # Calcul de R2
  # formu1<-paste0("Surv(",x_temps,",",x_event,") ~ QRISK3_2017") %>% as.formula()
  # model<-try(coxph(formu1,data=dt))
  # pp<-summary(model)
  # pp$concordance
  
}

taula_AUC_R2_grup<-function(x_event="EV.CVD",dt=dt_temp,grup="sexe") {
  
  split(dt,dt[grup]) %>% map_df(~taula_AUC_R2(x_event=x_event,dt=.x),.id="grup")
  
}


funcio_kappa<-function(dt=dades) {
  
  # dt<-llistat_dades[[8]]
  # dt=dades
  
  result=tryCatch({
    k=table(dt$regicor.cat,dt$QRISK3_2017.cat) %>% psych::cohen.kappa()
    IC1=k$confid[1,1]
    IC2=k$confid[1,3]
    k=k$kappa
    r=cor(dt$regicor,dt$QRISK3_2017, use = "na.or.complete")
    tibble::tibble(kappa=k,IC95inf=IC1,IC95sup=IC2,coef_pearson=r,N=as.numeric(count(dt)))
    }
    
    ,error= function (e) {
      r=cor(dt$regicor,dt$QRISK3_2017, use = "na.or.complete")
      tibble::tibble(kappa=NA,IC95inf=NA,IC95sup=NA,coef_pearson=r,N=as.numeric(count(dt)))}
    )
  
  }




