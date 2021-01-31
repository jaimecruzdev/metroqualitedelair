############################################
# Etendres valeurs journalières aux heures #
############################################

################################################################################
#   Nous disposons que des valeurs journalières pour la qualité de l'air de    #
#   Paris extérieur, dans cette fonction nous allons préparer les données pour #
#   se adapter aux format horaire des autres sources des données               #
################################################################################

etendreHeuresQairExt<-function()
{
  #Lecture des données du fichier source
  df<-lecture_qairExt()
  #preparation
  df<-ext_qairExt(df)
  #Ecriture des données préparés dans le fichier correspondant
  ecriture_qairExt(df)
}

#Lire le fichier meteo
lecture_qairExt<-function()
{
  #### Variables globales ####
  #PREP_DATA_PATH<-"C://Formation IA//CEPE//Projet//QAIR//Data//"
  #PREP_DATA_FILE<-"RATP_qair_2013_2020.csv"
  PREP_DATA_PATH<-CT_PATH_DATA
  PREP_DATA_FILE<-"paris-air-quality.csv"
  
  #### Lecture du fichier correspondant ####
  fichier_donnees=paste(PREP_DATA_PATH,PREP_DATA_FILE,sep="/")
  print(fichier_donnees)
  df<-read.csv(fichier_donnees,sep=",")
  
  return (df)
}

#Extensions des valeurs
ext_qairExt<-function(df_qair_ext)
{
  #Nous allons prendre le calendrier qui contient les heures dont il faut s'adapter
  PREP_DATA_PATH<-CT_PATH_DATA
  PREP_DATA_FILE<-"calendrier.csv"
  
  #### Lecture du fichier correspondant ####
  fichier_donnees=paste(PREP_DATA_PATH,PREP_DATA_FILE,sep="/")
  print(fichier_donnees)
  df_cal<-read.csv(fichier_donnees,sep=",")["Date_Heure"]
  
  #changer les nommes des colonnes
  colnames(df_qair_ext)<-c("la_date","ext_pm25","ext_pm10","ext_o3","ext_no2","ext_so2","ext_co")
  
  #print(head(df_cal))
  #print(date(df_cal["Date_Heure"], "%m/%d/%Y"))
  
  #Ajouer une colonne
  v1 <- as.POSIXct(df_cal[,"Date_Heure"], format="%Y-%m-%dT%H:%M:%OS",tz="GMT")
  v2 <- as.Date(v1,format='%m/%d/%Y')
  df_cal$la_date<-v2
  df_cal<-df_cal[,c("la_date","Date_Heure")]
  
  #Changer le format de la date
  v3<-as.Date(df_qair_ext[,"la_date"])
  df_qair_ext$la_date<-v3
  
  #Merge des données
  #joined_df <- merge(df_qair_ext, df_cal, by.x = "la_date", 
  #                   by.y = "la_date", all.x = TRUE, all.y = TRUE)
  
  df_qair_ext <-left_join(df_qair_ext, df_cal, by="la_date")
  df_qair_ext$la_date <- NULL
  
  arrange(df_qair_ext,Date_Heure)

  return (df_qair_ext)
}

#Ecriture dans le fichier
ecriture_qairExt<-function(df)
{
  CT_PATH_DATA_WT_PREP<-CT_PATH_DATA_PREP
  CT_FILE_RATP_PREP<-"paris_qair_ext.csv"
  
  #### Ecriture du fichier correspondant ####
  fichier_donnees=paste(CT_PATH_DATA_WT_PREP,CT_FILE_RATP_PREP,sep="/")
  #write.csv(df,fichier_donnees,row.names=F)
  write.csv(df, fichier_donnees,row.names=F)
}
