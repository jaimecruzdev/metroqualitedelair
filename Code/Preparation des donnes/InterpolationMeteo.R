#######################################
# Interpolation trihoraire des heures #
#######################################

################################################################################
#   Ayant disponible de la source seulement des valeurs trihoraires,           #
#   nous allons interpoler les heures au millieu, afin de "remplir les trous"  #
################################################################################
CT_METEO_VAR_INTERPOL<<-c("rr3","tc","pres","dd","ww","cod_tend","ff","n")

interpolation_meteo<-function()
{
  #Lecture des données du fichier source
  df<-lecture_meteo()
  #preparation
  df<-inter_meteo(df)
  #Ecriture des données préparés dans le fichier correspondant
  ecriture_meteo(df)
}

#Lire le fichier meteo
lecture_meteo<-function()
{
  #### Variables globales ####
  #PREP_DATA_PATH<-"C://Formation IA//CEPE//Projet//QAIR//Data//"
  #PREP_DATA_FILE<-"RATP_qair_2013_2020.csv"
  PREP_DATA_PATH<-CT_PATH_DATA
  PREP_DATA_FILE<-"meteo_final_2013_2019.csv"
  
  #### Lecture du fichier correspondant ####
  fichier_donnees=paste(PREP_DATA_PATH,PREP_DATA_FILE,sep="/")
  print(fichier_donnees)
  df<-read.csv(fichier_donnees,sep=";")
  
  return (df)
}

#Interpolation des valeurs NA
inter_meteo<-function(df)
{
  #trie par date
  df$date<-as.POSIXct(df$date,format="%Y-%m-%d %H:%M:%OS",tz="GMT")
  df<-df[order(df$date),]
  
  #reindex
  rownames(df)<-NULL
  
  #lire calendrier sans trous qu'on prendre comme référence
  PREP_DATA_CAL_FILE<-"calendrier.csv"
  fic_cal=paste(CT_PATH_DATA,PREP_DATA_CAL_FILE,sep="/")
  df_cal<-read.csv(fic_cal,sep=",")[c("Date_Heure")]
  df_cal$Date_Heure<-as.POSIXct(df_cal$Date_Heure,format="%Y-%m-%dT%H:%M:%OS",tz="GMT")
  df_cal<-subset(df_cal, Date_Heure> "2012-12-31" & Date_Heure < "2020-07-01")
  
  #et merge les deux
  joined_df <- merge(df, df_cal, by.x = "date", 
                     by.y = "Date_Heure", all.x = TRUE, all.y = TRUE)
  
  #Intepolation des champs numériques
  joined_df[,CT_METEO_VAR_INTERPOL]<-na.approx(joined_df[,CT_METEO_VAR_INTERPOL])
  
  ############################################################################
  #             Interpolation de la description du temps :                   #
  #                                                                          #
  #  Il y a toujours de base deux valeurs NA entre deux vraie valeurs.       #
  #  Nous allons assigner à chaque fois la valeur la plus proche :           #
  #  Si on a : "A" NA NA "B"                                                 #
  #  Nous allons généré : "A" "A" "B" "B"                                    #
  #                                                                          #
  ############################################################################
  
  #On enleve les premières des valeurs NA
  def_aux_1 <- joined_df[-seq(2,nrow(joined_df),by=3),]
  #On prendre la dernière vraie valeur comme référence
  def_aux_1$temps_present <- na.locf(def_aux_1$temps_present,fromLast=TRUE)
  #On merge
  joined_df[-seq(2,nrow(joined_df),by=3),] <- def_aux_1
  #On prendre la première vraie valeur comme référence 
  joined_df$temps_present <- na.locf(joined_df$temps_present,fromLast=FALSE)
  
  return (joined_df)
}

#Ecriture dans le fichier
ecriture_meteo<-function(df)
{
  #### Variables globales ####
  #PREP_DATA_PATH<-"C://Formation IA//CEPE//Projet//QAIR//Data//"
  #PREP_DATA_FILE<-"RATP_qair_2013_2020.csv"
  CT_PATH_DATA_WT_PREP<-CT_PATH_DATA_PREP
  CT_FILE_RATP_PREP<-"meteo_interpole.csv"
  
  #### Ecriture du fichier correspondant ####
  fichier_donnees=paste(CT_PATH_DATA_WT_PREP,CT_FILE_RATP_PREP,sep="/")
  write.csv(df,fichier_donnees,row.names=F)
}
