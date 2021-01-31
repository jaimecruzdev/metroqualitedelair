
#Merge des toutes les sources des données
join_all_sources<-function()
{
  print("**** Start Merge ... ")
  
  #Lire les sources
  PREP_DATA_FILE1<-"calendrier.csv"
  fichier_donnees1=paste(CT_PATH_DATA,PREP_DATA_FILE1,sep="/")
  
  PREP_DATA_FILE2<-"meteo_interpole.csv"
  fichier_donnees2=paste(CT_PATH_DATA_PREP,PREP_DATA_FILE2,sep="/")
  
  PREP_DATA_FILE3<-"RATP_qair_2013_2020_Prep.csv"
  fichier_donnees3=paste(CT_PATH_DATA_PREP,PREP_DATA_FILE3,sep="/")
  
  PREP_DATA_FILE4<-"paris_qair_ext.csv"
  fichier_donnees4=paste(CT_PATH_DATA_PREP,PREP_DATA_FILE4,sep="/")
  
  #le fichier calendrier sert de base sur laquelle seront ajoutées les colonnes des fichiers RATP,    Météo et PollutionExterne
  dfFinal <- read_csv(fichier_donnees1,locale = locale(encoding = "UTF-8"))
  dfMeteo <- read_csv(fichier_donnees2,locale = locale(encoding = "UTF-8"))
  dfRATP <- read_csv(fichier_donnees3,locale = locale(encoding = "UTF-8"))
  #dfPolExt <- read_csv(fichier_donnees4,locale = locale(encoding = "UTF-8"))
  dfPolExt <- read.csv(fichier_donnees4)
  
  dfPolExt$Date_Heure <- as.POSIXct(dfPolExt[,"Date_Heure"], format="%Y-%m-%dT%H:%M:%OS",tz="GMT")
  
  #print(head(dfPolExt))

  
  #Nom du fichier de sortie où les données seront mergées
  PREP_DATA_FILE_OUT<-"DataCorrelated.csv"
  fichier_donnees_out=paste(CT_PATH_DATA_PREP,PREP_DATA_FILE_OUT,sep="/")
  
  #Changer le nom de colonnes où nous ferons le join
  colnames(dfFinal)[colnames(dfFinal)=="Date_Heure"]<-"Timestamp"
  colnames(dfMeteo)[colnames(dfMeteo)=="date"]<-"Timestamp"
  colnames(dfRATP)[colnames(dfRATP)=="date_heure"]<-"Timestamp"
  colnames(dfPolExt)[colnames(dfPolExt)=="Date_Heure"]<-"Timestamp"
  
  # Joins 
  dfFinal <- left_join(dfFinal, dfMeteo, by="Timestamp")
  rm(dfMeteo)
  
  dfFinal <- left_join(dfFinal, dfRATP, by="Timestamp")
  rm(dfRATP)
  
  dfFinal <- left_join(dfFinal, dfPolExt, by="Timestamp")
  rm(dfPolExt)
  
  #Changer noms des variables dans le fichier
  # on renomme les colonnes meteo
  setnames(dfFinal, c("rr3", "tc", "pres", "dd", "ww", "cod_tend", "ff", "n"),
           c("pluie_3_heures", "temperature_celsius", "pression", "direction_vent_10mn", "temps_present_num",
             "type_tendance_barometrique", "vitesse_vent_10mn", "nebulosite_totale"))
  
  ### savegarde du résultat dans un fichier
  #write.csv(dfFinal,fichier_donnees_out,fileEncoding="UTF-8")
  write.csv(dfFinal,fichier_donnees_out)
  
  print("**** End Merge ... ")
}