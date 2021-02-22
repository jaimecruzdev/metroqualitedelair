modeling<-function()
{
  #read data
  DATA_FILE_IN_PCA<-"data_model_PCA.csv"
  DATA_FILE_IN_RF<-"data_model_RF.csv"
  DATA_FILE_IN<-"data_model_CLEAN.csv"
  
  fichier_donnees<-paste(CT_PATH_DATA_PREP,DATA_FILE_IN,sep="/")
  df<-read.csv(fichier_donnees)
  
  #organisation des variables
  var_mod_cible<-c("co2")
  var_mod_date<-c("Date_Heure_Locale","Timestamp")
  var_mod_quant<-c("Heure_Local","Annee","Mois","Semaine_de_lannee","Jour_de_la_Semaine","pluie_3_heures","temperature_celsius","pression","direction_vent_10mn","temps_present_num","type_tendance_barometrique","vitesse_vent_10mn","nebulosite_totale","temperature","no","no2","humidite","particules_fines","ext_pm25","ext_pm10","ext_o3","ext_no2")
  var_mod2_ext<-c("ext_pm25","ext_pm10","ext_o3","ext_no2")
  var_mod_qual<-c("temps_present")
  var_mod_bool<-c("Conges_Escolaire_Zone_C","Conges_Escolaire_Zone_AB","jour_activite")
  
  #transform type of data
  df<-prepare_data(df,var_mod_date,var_mod_quant,var_mod_qual,var_mod_bool)
  
  #Rajouter les variables cibles à prédire en amont.
  #On aura 12 heures, 24 heures et une semaine
  list_other_cibles<-c(12,24,7*24)
  var_noms_other_cibles<-lapply(list_other_cibles,generer_var_nom)
  df<-rajoute_cibles(df,var_mod_cible,list_other_cibles)
    
  #Rajouter variables "lagées" 
  
  #split données d'entrainement et test
  deux_dfs<- split_X_Y(df,var_mod_cible,var_mod_quant,var_mod_qual,var_mod_bool)
  
  #"standardisation" des données
  df_norm<-deux_dfs
  df_norm$train<-scale_data(deux_dfs$train,var_mod_quant,var_mod_cible)
  df_norm$test<-scale_data(deux_dfs$test,var_mod_quant,var_mod_cible)
  
  #entrainement et prédictions
  train_and_predict(var_mode_cible,df_train,df_test)
  
  print("au revoir")
  
  #return (df)
}

#A partir d'une variable cible et un integer il donne le nom de la variable.
generer_var_nom<-function(par_raj,par_var_cible="co2")
{
  paste(par_var_cible,par_raj,sep="_")
}  


#Preparer chacune des variables pour le modèle
prepare_data<-function(par_df,par_var_mod_date,par_var_mod_quant,par_var_mod_qual,par_var_mod_bool)
{
  ret_df =par_df #variables à retourner
  
  #Pas besoin de X, nous avons Index
  ret_df$X<-NULL
  
  #Conversion des dates
  ret_df[par_var_mod_date]<-lapply(ret_df[par_var_mod_date],as.POSIXct)
  
  #Conversion de variables qualitatives
  ret_df[par_var_mod_qual]<-lapply(ret_df[par_var_mod_qual],factor)
  
  #Conversion de variables booleans
  ret_df[par_var_mod_bool]<-lapply(ret_df[par_var_mod_bool],as.logical)
  
  return (ret_df)
}

#Rajouter variables à prédire dans notre dataframe
rajoute_cibles<-function(par_df,par_var_cible="co2",par_list_cibles)
{
  ret_df<-par_df
  
  var_noms_cibles<-lapply(list_cibles,generer_var_nom)
  
  #Nous nous assurons de l'ordre par date d'abord
  ret_df <-ret_df[order(ret_df$Date_Heure_Local),]
  
  #on lag !
  for (ind_lead in par_list_cibles)
  {
    var_nom_col=generer_var_nom(ind_lead)
    ret_df[,var_nom_col]<-lead(ret_df[,par_var_cible],ind_lead)
  }    
  
  #on doit supprimer les NA qu'on pourra pas prédire en tout cas
  #print(tail(ret_df[,c("Date_Heure_Locale","co2",unlist(var_noms_cibles))]))
  
  ret_df<-na.omit(ret_df)
  
  return (ret_df)
}

#Séparer les jeux de données d'entrainement et test avec les variables à modèliser
split_X_Y<-function(par_df,par_var_mod_cible,par_var_mod_quant,par_var_mod_qual,par_var_mod_bool)
{
  #données avec les variables à modéliser
  df_vars<-par_df[c(par_var_mod_cible,par_var_mod_quant,par_var_mod_qual,par_var_mod_bool)]
  
  #condition de split pour le train
  cond_TRAIN=df_vars$Annee<2019
  
  #train data
  ret_df_train<-df_vars[cond_TRAIN,]
  
  #test data
  ret_df_test<-df_vars[-cond_TRAIN,]
  
  return (list(train=ret_df_train,test=ret_df_test))
}

#standardise data
scale_data<-function(par_df,par_var_mod_quant,par_var_mod_cible)
{
  ret_df<-par_df
  
  #variable cible
  #ret_df[par_var_mod_cible]<-scale(ret_df[par_var_mod_cible])
  ret_df$co2<-scale(ret_df$co2)
  
  #variables quantitatives
  ret_df[par_var_mod_quant]<-scale(ret_df[par_var_mod_quant])
  
  #This is how we'll got back unscale data
  #Unscaled_Pred <- predict(Mod, cars) * sd(cars$speed) + mean(cars$speed)
  
  return (ret_df)
  
}

#Créer le modèles et prédire
train_and_predict<-function(par_df_train,par_df_test,par_var_mod_cible)
{
  #Modèle linéaire sans normalisation des données
  #mod_reg<-lm(~.,df_mod)
}

