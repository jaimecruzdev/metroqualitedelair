trouver_trous <- function(df,var_name,sup_24=FALSE,sup_N=0) 
{    
  #matrix support
  C02trousMatrix<-data.frame(date_heure=NA,date=NA,heure=NA,taille=NA)   
  #identifier les trous
  co2trous<-df[is.na(df[var_name]),c("Date_Heure_Locale",var_name)]
  #index de registres et son prédécesseur des trous
  inds<-seq_len(nrow(co2trous))
  inds2<-c(inds[-1],inds[1])
  #Valeur différent avec le registre consecutive dans l'ensemble de trous?
  co2trous$dist_next<-as.numeric(row.names(co2trous[inds2,]))-as.numeric(row.names(co2trous[inds,]))
  co2trous[co2trous["dist_next"]!=1,"dist_next"]<-0
  #Identification des premiers registres de chaque bloque de NAs
  dfaux=data.frame(co2trous[inds,"dist_next"],co2trous[inds2,"dist_next"])
  co2trous[inds2,"first_NA_block"]<-(co2trous[inds2,"dist_next"]==1 & co2trous[inds,"dist_next"]==0)
  #Taille de nombre de registres consecutives ayant de NA's 
  a<-rle(co2trous$dist_next)$lengths
  a<-a[rle(co2trous$dist_next)$values==1]
  co2trous[co2trous["first_NA_block"]==TRUE,"size_trou"]<-a
  co2trous[co2trous["first_NA_block"]==TRUE,"size_trou_day"]<-co2trous[co2trous["first_NA_block"]==TRUE,"size_trou"] / 24
  co2trous<-co2trous[co2trous["first_NA_block"]==TRUE,]
  if (sup_24==TRUE)
  {
    co2trous<-co2trous[co2trous["first_NA_block"]==TRUE & co2trous["size_trou"]>24,]
  }
  else (sup_N>0)
  {        
    co2trous<-co2trous[co2trous["first_NA_block"]==TRUE & co2trous["size_trou"]>sup_N,]
  }
  return (co2trous[,!names(co2trous) %in% c("dist_next","first_NA_block")])
}

exploration_des_trous<-function()
{
  DATA_FILE_IN<-"data_model_restant_trous.csv"
  fichier_donnees<-paste(CT_PATH_DATA_PREP,DATA_FILE_IN,sep="/")
  df<-read.csv(fichier_donnees)
  
  data_f<-trouver_trous(df,"co2",FALSE)
  print(data_f)
  
  data_f<-trouver_trous(df,"co2",TRUE)
  print(data_f)
  
  data_f<-trouver_trous(df,"nebulosite_totale",FALSE)
  print(data_f)
  
  data_f<-trouver_trous(df,"temperature",FALSE)
  print(data_f)
  
  data_f<-trouver_trous(df,"temperature",TRUE)
  print(data_f)
  
  data_f<-trouver_trous(df,"no",FALSE)
  print(data_f)
  
  data_f<-trouver_trous(df,"no",TRUE)
  print(data_f)
  
  data_f<-trouver_trous(df,"no2",FALSE)
  print(data_f)
  
  data_f<-trouver_trous(df,"no2",TRUE)
  print(data_f)
  
  data_f<-trouver_trous(df,"humidite",FALSE)
  print(data_f)
  
  data_f<-trouver_trous(df,"humidite",TRUE)
  print(data_f)
  
  data_f<-trouver_trous(df,"particules_fines",FALSE)
  print(data_f)
  
  data_f<-trouver_trous(df,"particules_fines",TRUE)
  print(data_f)
  
  data_f<-trouver_trous(df,"ext_pm25",FALSE)
  print(data_f)
  
  data_f<-trouver_trous(df,"ext_pm10",FALSE)
  print(data_f)
  
  data_f<-trouver_trous(df,"ext_o3",FALSE)
  print(data_f)
  
  data_f<-trouver_trous(df,"ext_no2",FALSE)
  print(data_f)
  
  vart_trous_cible<-c("co2")
  vars_trous_RATP<-c("temperature","no","no2","humidite","particules_fines")
  vars_trous_ext<-c("ext_pm25","ext_pm10","ext_o3","ext_no2")
  #vars_trous<-c(vart_trous_cible,vars_trous_RATP,vars_trous_ext)
  vars_trous<-c(vars_trous_RATP,vars_trous_ext)
  list_vars<-c("temperature","no","no2","humidite","particules_fines","ext_pm25" , "ext_pm10", "ext_o3", "ext_no2")
  
  df_filt1<-df[df$Annee>2013 & df$Annee<2020,]
  data_f_co2<-df_filt1%>%filter(!is.na(co2))
  df_filt2<-df_filt1[which(!is.na(df_filt1$co2)),]
  
  for (arg in list_vars)
  {
    print("***************")
    print(arg)    
    print("***************")
    print(trouver_trous(df_filt2,arg,TRUE))
  }
  
  df_trous<-df_filt2[,vars_trous]
  
  matrixplot(df_trous,sortby = 3)
  
  marginplot(df_trous[,c("temperature","humidite")])
  
  marginplot(df_trous[,c("temperature","particules_fines")])
  
  marginplot(df_trous[,c("no2","particules_fines")])
  
  marginplot(df_trous[,c("ext_o3","particules_fines")])
  
  marginplot(df_trous[,c("ext_o3","temperature")])
  
  tabNA<-matrix("p",nrow=nrow(df_trous),ncol=ncol(df_trous))
  tabNA[is.na(df_trous)]<-"a"
  dimnames(tabNA)<-dimnames(df_trous)
  
  res.mca<-MCA(tabNA,graph=FALSE)
  plot(res.mca,invisible="ind")
}

imputation_des_trous<-function(par_imput_RF=FALSE,par_imput_PCA=TRUE)
{
  OPT_IMPUT_RF=par_imput_RF
  OPT_IMPUT_PCA=par_imput_PCA
  
  DATA_FILE_IN<-"data_model_restant_trous.csv"
  DATA_FILE_OUT<-"data_model.csv"
  DATA_FILE_OUT_RF<-"data_model_RF.csv"
  DATA_FILE_OUT_PCA<-"data_model_PCA.csv"
  DATA_FILE_OUT_CLEAN<-"data_model_CLEAN.csv"
  
  fichier_donnees<-paste(CT_PATH_DATA_PREP,DATA_FILE_IN,sep="/")
  df<-read.csv(fichier_donnees)
  
  df
  
  df$X<-NULL
  
  df_filt1<-df[df$Annee>2013 & df$Annee<2020,]
  
  data_f_co2<-df_filt1%>%filter(!is.na(co2))
  
  df_filt2<-df_filt1[which(!is.na(df_filt1$co2)),]
  
  is.na(df_filt2$co2)
  
  df_filt2%>%select(Date_Heure_Locale,co2)%>%filter(is.na(co2))
  
  vart_trous_cible<-c("co2")
  vars_trous_RATP<-c("temperature","no","no2","humidite","particules_fines")
  vars_trous_ext<-c("ext_pm25","ext_pm10","ext_o3","ext_no2")
  #vars_trous<-c(vart_trous_cible,vars_trous_RATP,vars_trous_ext)
  vars_trous<-c(vars_trous_RATP,vars_trous_ext)
  vars_trous
  
  list_vars<-c("temperature","no","no2","humidite","particules_fines","ext_pm25" , "ext_pm10", "ext_o3", "ext_no2")
  
  rownames(df_filt2) <- NULL
  
  df_trous<-df_filt2[,vars_trous]
  df_filt2[,c("Date_Heure_Locale",vars_trous)]
  
  res<-summary(aggr(df_trous,sortVar=TRUE))$combinations
  
  df_clean<-drop_na(df_filt2)
  nrow(df_clean)*100/nrow(df_filt2)
  
  if (OPT_IMPUT_RF==TRUE)
  {
      df_impt_RF<-missForest(df_filt2[vars_trous])
      df_impt_RF_clean<-df_filt2
      df_impt_RF_clean[vars_trous]<-df_impt_RF
  }
  
  if (OPT_IMPUT_PCA==TRUE)
  {
      nb<-estim_ncpPCA(df_filt2[vars_trous])
      df_impt_PCA<-imputePCA(df_filt2[vars_trous],ncp=nb$ncp)
      df_impt_PCA_clean<-df_filt2
      df_impt_PCA_clean[vars_trous]<-df_impt_PCA_clean
      df_impt_PCA$completeObs[290:300,]
  }
  
  write.csv(df_clean,paste(CT_PATH_DATA_PREP,DATA_FILE_OUT_CLEAN,sep="/"))
  
  if (OPT_IMPUT_RF==TRUE)
  {
      write.csv(df_impt_RF_clean,paste(CT_PATH_DATA_PREP,DATA_FILE_OUT_RF,sep="/"))
  }
  
  if (OPT_IMPUT_PCA==TRUE)
  {
      write.csv(df_impt_PCA_clean,paste(CT_PATH_DATA_PREP,DATA_FILE_OUT_PCA,sep="/"))
  }
}
