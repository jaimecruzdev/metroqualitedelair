##################################################################
#               Préparation des données qair RATP               #
#################################################################

####################################################################
# Dans cette fonction nous allons préparer les données             #
# de qualité de l'air avant de les merger avec les autres sources. #
#                                                                  #
# Etapes:                                                          #
#                                                                  #
# 1. Supprimmer dates dupliquées                                   #
# 2. Renommer les colonnes                                         #
#                                                                  #
####################################################################

prepar_qair_ratp<-function()
{
  #Lecture des données du fichier source
  df<-lecture_qair_ratp()
  #preparation
  df<-pr_qair_ratp(df)
  #Ecriture des données préparés dans le fichier correspondant
  ecriture_qair_ratp(df)
}

lecture_qair_ratp<-function()
{
  #### Variables globales ####
  #PREP_DATA_PATH<-"C://Formation IA//CEPE//Projet//QAIR//Data//"
  #PREP_DATA_FILE<-"RATP_qair_2013_2020.csv"
  PREP_DATA_PATH<-CT_PATH_DATA
  PREP_DATA_FILE<-"RATP_qair_2013_2020.csv"
  
  #### Lecture du fichier correspondant ####
  fichier_donnees=paste(PREP_DATA_PATH,PREP_DATA_FILE,sep="/")
  print(fichier_donnees)
  df<-read.csv(fichier_donnees)
  
  return (df)
}

pr_qair_ratp<-function(df)
{
  #supprimer registre dupliqué: 59705
  df<-df[-59705,]
  
  #changer les noms des colonnes
  names(df)<-c("index","particules_fines","date_heure","temperature","no2","humidite","no","co2")
  
  #supprimer colonnes avec les index
  df<-df[,c("co2","date_heure","temperature","no","no2","humidite","particules_fines")]
  
  return (df)
}

ecriture_qair_ratp<-function(df)
{
  #### Variables globales ####
  #PREP_DATA_PATH<-"C://Formation IA//CEPE//Projet//QAIR//Data//"
  #PREP_DATA_FILE<-"RATP_qair_2013_2020.csv"
  CT_PATH_DATA_WT_PREP<-CT_PATH_DATA_PREP
  CT_FILE_RATP_PREP<-"RATP_qair_2013_2020_Prep.csv"
  
  #### Ecriture du fichier correspondant ####
  fichier_donnees=paste(CT_PATH_DATA_WT_PREP,CT_FILE_RATP_PREP,sep="/")
  write.csv(df,fichier_donnees,row.names=F)
}
