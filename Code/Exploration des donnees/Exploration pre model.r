
explore_prepare_before_model<-function()
{
  DATA_FILE_IN<-"DataCorrelated.csv"
  DATA_FILE_OUT<-"data_model_restant_trous.csv"
  fichier_donnees<-paste(CT_PATH_DATA_PREP,DATA_FILE_IN,sep="/")
  df<-read.csv(fichier_donnees)
  
  df
  
  head(df)
  
  nrow(df)
  
  summary(df)
  
  #labl_fac1<-c("1","2","3","4","5","6","7","8","9","10","11","12","13","14","15","16","17","")
  #levl_fac1 <- c("ete","hiver","hiver","hiver","hiver","hiver","hiver","hiver","Toussaint","Noel","printemps","printemps","printemps","printemps","printemps","printemps","printemps","")
  #df$Conge_scolaire_raison <- factor(df$Conge_scolaire_raison, levels = levl_fac1, labels=labl_fac1)
  #  c("ete","hiver","hiver","hiver","hiver","hiver","hiver","hiver","Toussaint","Noel","printemps","printemps","printemps","printemps","printemps","printemps","printemps","")
  
  df$temps_present <- fct_collapse(df$temps_present,
                                          Clairs = c('<c9>clairs visibles, tonnerre non perceptible','<c9>tat du ciel inchang<e9> dans l<U+0092>ensemble', '<c9>tat du ciel inchang<e9> dans l<U+0092>ensemble'),
                                          Averse = c('Averse(s) de gr<ea>le*, ou de pluie et de gr<ea>le*', 'Averse(s) de neige, faible(s)','Averse(s) de neige, ou de pluie et de neige', 'Averse(s) de pluie', 'Averse(s) de pluie et neige m<ea>l<e9>es, faible(s)', 'Averse(s) de pluie, faible(s)', 'Averse(s) de pluie, mod<e9>r<e9>e(s) ou forte(s)', 'Averse(s) de pluie, violente(s)'),
                                          Neige = c('Chute continue de flocons de neige, faible au moment de l<U+0092>observation','Chute continue de flocons de neige, mod<e9>r<e9>e au moment de l<U+0092>observation', 'Chute intermittente de flocons de neige, faible au moment de l<U+0092>observation','Neige', 'Neige en grains (avec ou sans brouillard)'),
                                          Brouillard = c('Brouillard ou brouillard glac<e9>', 'Brouillard ou brouillard glac<e9> <e0> distance au moment de' ,'Brouillard ou brouillard glac<e9> en bancs', 'Brouillard ou brouillard glac<e9>, ciel invisible, a d<e9>but<e9> ou est devenu plus <e9>pais au cours del\'heure pr<e9>c<e9>dente', 'Brouillard ou brouillard glac<e9>, ciel invisible, s<U+0092>est aminci au cours de l<U+0092>heure pr<e9>c<e9>dente', 'Brouillard ou brouillard glac<e9>, ciel invisible, sans changement appr<e9>ciable au cours de l<U+0092>heure pr<e9>c<e9>dente' ,'Brouillard ou brouillard glac<e9>, ciel visible, a d<e9>but<e9> ou est devenu plus <e9>pais au cours del\'heure pr<e9>c<e9>dente', 'Brouillard ou brouillard glac<e9>, ciel visible, s<U+0092>est aminci au cours de l<U+0092>heure pr<e9>c<e9>dente', 'Brouillard ou brouillard glac<e9>, ciel visible, sans changement appr<e9>ciable au cours de l<U+0092>heure pr<e9>c<e9>dente', 'Brouillard, d<e9>posant du givre, ciel invisible' ,'Brouillard, d<e9>posant du givre, ciel visible','Mince couche de brouillard ou de brouillard glac<e9> <e0> la station, qu<U+0092>il s<U+0092>agisse d<U+0092>une station terrestre ou d<U+0092>une station en mer, d<U+0092>une <e9>paisseur n<U+0092>exc<e9>dant pas 2 m<e8>tres sur terre ou 10 m<e8>tres en mer'),
                                          Bruine = c('Bruine (ne se congelant pas) ou neige en grains', 'Bruine et pluie, faibles', 'Bruine et pluie, mod<e9>r<e9>es ou fortes', 'Bruine ou pluie se congelant', 'Bruine, sans cong<e9>lation, continue, faible au moment de l<U+0092>observation', 'Bruine, sans cong<e9>lation, continue, mod<e9>r<e9>e au moment de l<U+0092>observation','Bruine, sans cong<e9>lation, intermittente, faible au moment de l<U+0092>observation','Bruine, sans cong<e9>lation, intermittente, mod<e9>r<e9>e au moment de l<U+0092>observation'),
                                          Brume = c('Brume', 'Brume s<e8>che'),
                                          Nuages = c('Dans l<U+0092>ensemble, nuages se dissipant ou devenant moins <e9>pais','Nuages en formation ou en train de se d<e9>velopper', 'On n<U+0092>a pas observ<e9> d<U+0092><e9>volution des nuages ou on n<U+0092>a pas pu suivre cette <e9>volution'),
                                          Orage = c('Orage (avec ou sans pr<e9>cipitations)' ,'Orage faible ou mod<e9>r<e9>, sans gr<ea>le*, mais avec pluie ou neige ou pluie et neige m<ea>l<e9>es au moment de l<U+0092>observation', 'Orage, mais pas de pr<e9>cipitations au moment de l<U+0092>observation','Pluie faible au moment de l<U+0092>observation, Orage durant l<U+0092>heure pr<e9>c<e9>dente mais non au moment de l<U+0092>observation', 'Pluie mod<e9>r<e9>e ou forte au moment de l<U+0092>observation, Orage durant l<U+0092>heure pr<e9>c<e9>dente mais non au moment de l<U+0092>observation'),
                                          Pluie = c('Pluie (ne se congelant pas)','Pluie (ou bruine) et neige, faibles', 'Pluie (ou bruine) et neige, mod<e9>r<e9>es ou fortes','Pluie et neige m<ea>l<e9>es ou granules de glace','Pluie, sans cong<e9>lation, continue, faible au moment de l<U+0092>observation', 'Pluie, sans cong<e9>lation, continue, forte au moment de l<U+0092>observation','Pluie, sans cong<e9>lation, continue, mod<e9>r<e9>e au moment de l<U+0092>observation', 'Pluie, sans cong<e9>lation, intermittente, faible au moment de l<U+0092>observation', 'Pluie, sans cong<e9>lation, intermittente, mod<e9>r<e9>e au moment de l<U+0092>observation', 'Pluie, se congelant, faible' ,'Pr<e9>cipitations en vue, atteignant le sol ou la surface de la mer, mais distantes (c<U+0092>est-<e0>-dire <e0> plus de 5 km <e0> l<U+0092>estime) de la station', 'Pr<e9>cipitations en vue, atteignant le sol ou la surface de la mer, pr<e8>s de la station mais pas <e0> la station m<ea>me')  
  )
  
  #lvel_fac2<-c('1','2','3','4','5','6','7','8','9','10','11','12','13','14','15','16','17','18','19','20','21','22','23','24','25','26','27','28','29','30','31','32','33','34','35','36','37','38','39','40','41','42','43','44','45','46','47','48','49','50','51','52','53','54','55','56','57')
  
  #labl_fac2<-c('Clairs','Clairs','Averse','Neige','Averse','Averse','Averse','Averse','Averse','Averse','Brouillard','Brouillard','Brouillard','Brouillard','Brouillard','Brouillard','Brouillard','Brouillard','Brouillard','Brouillard','Brouillard','Bruine','Bruine','Bruine','Bruine','Bruine','Bruine','Bruine','Bruine','Brume','Brume','Neige','Neige','Neige','Nuages','Brouillard','Neige','Neige','Nuages','Nuages','Orage','Orage','Orage','Pluie','Pluie','Pluie','Pluie','Pluie','Pluie','Pluie','Pluie','Pluie','Pluie','Pluie','Pluie','Pluie','Pluie')
  
  #levels(df$temps_present)<-factor(df$temps_present,labels=lvel_fac2,levels=labl_fac2)
  #df$temps_present<-factor(df$temps_present,labels=labl_fac2)
  
  names(df)[11]<-"Ferie"
  names(df)[12]<-"Ferie_raison"
  
  #On aura Paris et ailleurs comme congés escolaire
  df[,"Conges_Escolaire_Zone_AB"]<-0
  df[df["Conges_Escolaire_Zone_A"]==1 | df["Conges_Escolaire_Zone_B"]==1,"Conges_Escolaire_Zone_AB"]<-1
  
  #Nous supprimons les colonnes dont nous n'avons plus besoin
  df$Conge_zones<-NULL
  df$Conges_Escolaire<-NULL
  df$Conges_Escolaire_Zone_A<-NULL
  df$Conges_Escolaire_Zone_B<-NULL
  #df$Conges_Escolaire_Zone_C<-NULL
  
  #Jour d'activité ?
  #df["jour_activite"]<-(df["Ferie"]==0 & df["Jour_de_la_Semaine"]<=5)
  df[,"jour_activite"]<-0
  df[df["Ferie"]==0 & df["Jour_de_la_Semaine"]<=5,"jour_activite"]<-1
  #On supprime ces variables 
  df["Ferie"]<-NULL
  df["Conge_hebdomadaire"]<-NULL
  #df["Jour_de_la_Semaine"]<-NULL
  
  #"Feriée raison", il est déjà pris en compte par feriée + moi. Pareil pour "Congés raison". 
  df$Ferie_raison<-NULL
  df$Conge_scolaire_raison<-NULL
  df$Conge_scolaire<-NULL
  
  df$ext_so2<-NULL
  df$ext_co<-NULL
  
  df$X<-NULL
  
  summary(df)
  
  fichier_donnees=paste(CT_PATH_DATA_PREP,DATA_FILE_OUT,sep="/")
  write.csv(df,fichier_donnees)
  print(fichier_donnees)
}