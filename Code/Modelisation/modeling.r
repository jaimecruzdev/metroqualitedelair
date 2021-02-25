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
  #Par exemple, on aura 12 heures, 24 heures et une semaine
  list_other_cibles<-c(12,24,7*24)
  var_noms_other_cibles<-lapply(list_other_cibles,generer_var_nom)
  df<-add_Y_vars(df,var_mod_cible,list_other_cibles)
    
  #Rajouter variables "lagées" horaire 
  list_deepness<-c(1,5,9,13,17,21)
  lag_order<-order(df$Date_Heure_Local,df$Heure_Local)
  df<-add_lagged_vars(df,var_mod_cible,list_deepness,with(df,lag_order),"_hr_")

  #Rajouter variables "lagées" des jours précédents ouvrés / non-ouvrés à la même heure. 
  list_deepness<-c(1,2,3,5)
  lag_order<-order(df$jour_activite,df$Date_Heure_Local,df$Heure_Local)
  df<-add_lagged_vars(df,var_mod_cible,list_deepness,with(df,lag_order),"_jr_act_")
  
  #on doit supprimer les NA générés suite aux "lagges et leads" dont on ne pourra pas s'en servir
  df<-na.omit(df)
  #need to reorder ?
  df<-df[with(df,order(Date_Heure_Locale)),]
  
  #split données d'entrainement et test
  deux_dfs<- split_X_Y(df,var_mod_cible,var_mod_quant,var_mod_qual,var_mod_bool)
  
  #"standardisation" des données
  df_norm<-deux_dfs
  df_norm$train<-scale_data(deux_dfs$train,var_mod_quant,var_mod_cible)
  df_norm$test<-scale_data(deux_dfs$test,var_mod_quant,var_mod_cible)
  
  #entrainement et prédictions
  df_results<-train_and_predict(c(var_mod_cible,var_noms_other_cibles),deux_dfs$train,deux_dfs$test)
  
  #Rajoute les prédictions
  eval<-evaluate("RLM",df_results)
  
  write.csv(eval,paste(CT_PATH_DATA_OUT,"evaluations.csv",sep="/"),row.names=FALSE)
  
  print("au revoir")
  
  return (df_results)
}

#A partir d'une variable cible et un integer il donne le nom de la variable.
generer_var_nom<-function(par_raj,par_var_cible="co2",separ="_")
{
  paste(par_var_cible,par_raj,sep=separ)
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
add_Y_vars<-function(par_df,par_var_cible="co2",par_list_cibles)
{
  ret_df<-par_df
  
  #var_noms_cibles<-lapply(par_list_cibles,generer_var_nom)
  
  #Nous nous assurons de l'ordre par date d'abord
  ret_df <-ret_df[order(ret_df$Date_Heure_Local),]
  
  #on lag !
  for (ind_lead in par_list_cibles)
  {
    var_nom_col=generer_var_nom(ind_lead)
    ret_df[,var_nom_col]<-lead(ret_df[,par_var_cible],ind_lead)
  }    
  
  return (ret_df)
}

#Rajouter des variables laggées dont le model s'appuiera pour s'en entrainer 
add_lagged_vars<-function(df,par_var_cible,list_deepness,order_index,ident="_hr_")
{
  ret_df<-df[order_index,]
  
  for (ind_lead in list_deepness)
  {
    var_nom_col=generer_var_nom(ind_lead,separ=paste0("_lag",ident))
    ret_df[,var_nom_col]<-lag(ret_df[,par_var_cible],ind_lead)
  }  
  
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
train_and_predict<-function(par_list_Y,par_df_train,par_df_test)
{
  #fist Y
  the_Y<-unlist(par_list_Y[1])
  
  #########  train ##############
  
  print(paste0("Training for ",the_Y))
  
  #work
  #mod_reg<-lm(co2~.,par_df_train)
  formRL<-paste0(the_Y,"~.")
  mod_reg<-lm(formRL,par_df_train)
  #show
  sink(paste(CT_PATH_DATA_OUT,"mod_rl_co2.txt",sep="/"))
  print(summary(mod_reg))
  sink()
  #and save
  saveRDS(mod_reg, file = paste(CT_PATH_DATA_OUT,"mod_rl_co2.rds",sep="/"))
  
  #########  train ##############
  
  ###########  predicting  ############
  
  predict_Y<-predict(mod_reg,par_df_test)
  
  res<-cbind(real=par_df_test[the_Y],predicted=predict_Y,error=round(get_error(par_df_test[the_Y],predict_Y),2))
  names(res)<-c("real","predicted","error")
  
  write.csv(res,paste(CT_PATH_DATA_OUT,"pred_rl_co2.csv",sep="/"),row.names=FALSE)
  
  ###########  predicting  ############
  
  return (res)
}

#Obtient l'errerur pour l'ensemble de prédictions
get_error<-function(par_real,par_predicted)
{
  ret_err<-abs(par_real-par_predicted)/par_real
  return (ret_err)
}

#Evaluations des prédictions
evaluate<-function(par_meth="RLM",par_res)
{
  mape_res<-get_MAPE(par_res)
  rmse_res<-get_RMSE(par_res)
  
  ret_eval<-cbind(Methode=par_meth,MAPE=mape_res,RMSE=rmse_res)
  
  return (ret_eval)
}

#MAPE
get_MAPE<-function(res)
{
  ret_MAPE<-sum(res$error)/nrow(res)
  
  return (ret_MAPE)
}

#RMSE
get_RMSE<-function(res)
{
  ret_RMSE<-sqrt((sum((res$error)^2))/nrow(res))
  
  return (ret_RMSE)
}