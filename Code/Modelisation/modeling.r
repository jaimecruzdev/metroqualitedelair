###########################################################
# Ce module va entrainer l'ensemble des modèles choisis. #
# Comme sortie il va générer plusieurs fichiers :        #
#                                                        #
#     1. Summary description                             #
#     2. Prédictions                                     #
#     3. Evaluations des modèles                         #
#     4. rds des modèles afin de pouvoir les reprendre   # 
#                                                        #
# Variantes à travailler :                               #
#                                                        #
#     1. Variable à prédire.                             #
#     2. Sources des variables, écarter les var. ext ?   #
#     3. Variables laggées ?                             #
#     4. Type de Modèle.                                 #
#     5. Normalisation des variables                     #
#     6. Selection des variables selon p-critiques       #
#                                                        #
##########################################################

modeling<-function(var_mod_cible="co2")
{
  print("Go modeling !")
  
  #read data
  DATA_FILE_IN_PCA<-"data_model_PCA.csv"
  DATA_FILE_IN_RF<-"data_model_RF.csv"
  DATA_FILE_IN<-"data_model_CLEAN.csv"
  
  fichier_donnees<-paste(CT_PATH_DATA_PREP,DATA_FILE_IN,sep="/")
  df<-read.csv(fichier_donnees)
  
  #organisation des variables
  var_mod_idx<-"Date_Heure_Locale"
  var_mod_date<-c("Date_Heure_Locale","Timestamp")
  var_mod_quant<-c("co2","Heure_Local","Annee","Mois","Semaine_de_lannee","Jour_de_la_Semaine","pluie_3_heures","temperature_celsius","pression","direction_vent_10mn","temps_present_num","type_tendance_barometrique","vitesse_vent_10mn","nebulosite_totale","temperature","no","no2","humidite","particules_fines","ext_pm25","ext_pm10","ext_o3","ext_no2")
  var_mod2_ext<-c("ext_pm25","ext_pm10","ext_o3","ext_no2")
  var_mod_qual<-c("temps_present")
  var_mod_bool<-c("Conges_Escolaire_Zone_C","Conges_Escolaire_Zone_AB","jour_activite")
  
  #Enleve la variable cible choisie des variables quantitatives
  var_mod_quant<-setdiff(var_mod_quant,var_mod_cible)
  
  #transform type of data
  df<-prepare_data(df,var_mod_date,var_mod_quant,var_mod_qual,var_mod_bool)
  
  #Rajouter les variables cibles à prédire en amont.
  #Par exemple, on aura 12 heures, 24 heures et une semaine
  list_other_cibles<-c(12,24,7*24)
  var_noms_other_cibles<-unlist(lapply(list_other_cibles,generer_var_nom,par_var_cible=var_mod_cible))
  df<-add_Y_vars(df,var_mod_cible,list_other_cibles)
  
  #Rajouter variables "lagées" horaire 
  list_deepness<-c(1,5,9,13,17,21)
  lag_order<-order(df$Date_Heure_Local,df$Heure_Local)
  ret_lag<-add_lagged_vars(df,var_mod_cible,list_deepness,with(df,lag_order),"_jr_")
  df<-ret_lag$df_lagged
  var_mod_lag_x_jr<-ret_lag$lag_col_names #variables to add to the model
  
  #Rajouter variables "lagées" des jours précédents ouvrés / non-ouvrés à la même heure. 
  list_deepness<-c(1,2,3,5)
  lag_order<-order(df$jour_activite,df$Date_Heure_Local,df$Heure_Local)
  ret_lag<-add_lagged_vars(df,var_mod_cible,list_deepness,with(df,lag_order),"_jr_act_")
  df<-ret_lag$df_lagged
  var_mod_lag_x_jr_act<-ret_lag$lag_col_names #variables to add to the model
  
  ################################################################################
  ######  variables qui gerent la préparation des variables à entrainer ##########
  ################################################################################
  
  #Modeles
  #list_var_mods<-c("RLM","Ridge","Lasso","RF")
  #list_var_mods<-c("RLM","Ridge","Lasso","GB")
  list_var_mods<-c("GB")
  
  #Exécution de l'ensemble des tests
  var_mod_list_Y <- c(var_mod_cible,var_noms_other_cibles)
  
  #Utiliser les variables laggées ?
  var_mod_lag <- TRUE
  
  #Normaliser les variables
  var_mod_normal <- FALSE
  
  #Selection de automtic des variables: critère BIC
  var_mod_BIC <- FALSE
  
  ############################################################
  
  if (var_mod_lag==TRUE)
  {
    #Ajouter les variables laggées aux quantitatives du modèle
    var_mod_quant<-c(var_mod_quant,var_mod_lag_x_jr,var_mod_lag_x_jr_act)
  }
  
  #on doit supprimer les NA générés suite aux "lagges et leads" dont on ne pourra pas s'en servir
  df<-na.omit(df)
  #need to reorder ?
  df<-df[with(df,order(Date_Heure_Locale)),]
  
  #split données d'entrainement et test
  deux_dfs<-split_X_Y(df,c(var_mod_cible,var_noms_other_cibles),var_mod_quant,var_mod_qual,var_mod_bool)
  
  if (var_mod_normal==TRUE)
  {
    #"standardisation" des données
    deux_dfs$train<-scale_data(deux_dfs$train,var_mod_quant,c(var_mod_cible,var_noms_other_cibles))
    deux_dfs$test<-scale_data(deux_dfs$test,var_mod_quant,c(var_mod_cible,var_noms_other_cibles))
  }
  
  #Préparer les index (var_mod_idx) avec les dates afin de les rajouter aux données prédites et leur donner du sens.
  idx_date<-df[!deux_dfs$split_cond,var_mod_idx]
  
  #######
  # GO! #
  #######
  
  #Créer liste des évaluations
  evaluations <- list(Var_Y=NA,heure_pred=NA,Methode=NA,MAPE=NA,RMSE=NA)[0]
  
  #sauver les cibles et les supprimmer pour juste travailler avec la Y qui corresponde à chaque itération
  cibles_train_Y<-deux_dfs$train[var_mod_list_Y]
  cibles_test_Y<-deux_dfs$test[var_mod_list_Y]
  
  deux_dfs$train<-select(deux_dfs$train,select=-all_of(var_mod_list_Y))
  deux_dfs$test<-select(deux_dfs$test,select=-all_of(var_mod_list_Y))
  
  for (var_mod in list_var_mods)
  {
    for (var_Y in var_mod_list_Y)
    {
      #supprime les variables cibles qu'on rajoutera pour chaque modèle
      train_df<-cbind(cibles_train_Y[var_Y],deux_dfs$train)
      test_df<-cbind(cibles_test_Y[var_Y],deux_dfs$test)
      
      #### extraire cible et heures en avance de prediction pour préparer les colonnes d'évaluation
      list_cible<-strsplit(var_Y,"_")
      
      var_cible<-unlist(list_cible)[1]
      heure_pred<-unlist(list_cible)[2]
      
      if (is.na(heure_pred))
      {
        heure_pred="0"
      }
      
      ##Selection des variables sous le critère BIC
      if (var_mod_BIC==TRUE)
      {
        #select variables
        sel_dfs<-select_vars(var_Y,train_df,test_df)
        train_df<-sel_dfs$train
        test_df<-sel_dfs$test
      }
      
      ##### entrainement et prédictions #####
      df_results<-train_and_predict(var_Y,train_df,test_df,idx_date,var_mod,var_mod_BIC,var_mod)
      
      #Rajoute les évaluations pour ce modèle
      evaluations<-rbind(evaluations,cbind(var_cible,heure_pred,(evaluate(var_mod,df_results,var_mod_BIC))))
    }
  }
  
  dump_evaluations(evaluations,var_cible)
  
  print("Fin du traitement des données")
  
  return (train_df)
}

#A partir d'une variable cible et un integer il donne le nom de la variable.
generer_var_nom<-function(par_raj,par_var_cible,separ="_")
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
  ret_df[par_var_mod_bool]<-lapply(ret_df[par_var_mod_bool],as.factor)
  #ret_df[par_var_mod_bool]<-lapply(ret_df[par_var_mod_bool],as.logical)
  
  return (ret_df)
}

#Rajouter variables à prédire dans notre dataframe
add_Y_vars<-function(par_df,par_var_Y,par_list_cibles)
{
  ret_df<-par_df
  
  #Nous nous assurons de l'ordre par date d'abord
  ret_df <-ret_df[order(ret_df$Date_Heure_Local),]
  
  #on lag !
  for (ind_lead in par_list_cibles)
  {
    var_nom_col=generer_var_nom(ind_lead,par_var_cible=par_var_Y)
    ret_df[,var_nom_col]<-lead(ret_df[,par_var_Y],ind_lead)
  }    
  
  return (ret_df)
}

#Rajouter des variables laggées dont le model s'appuiera pour s'en entrainer 
add_lagged_vars<-function(df,par_var_Y,list_deepness,order_index,ident="_hr_")
{
  ret_df<-df[order_index,]
  
  lag_names<-c()
  
  for (ind_lead in list_deepness)
  {
    var_nom_col=generer_var_nom(ind_lead,separ=paste0("_lag",ident),par_var_cible = par_var_Y)
    lag_names<-c(lag_names,var_nom_col)
    ret_df[,var_nom_col]<-lag(ret_df[,par_var_Y],ind_lead)
  }
  
  ret_list<-list(df_lagged=ret_df,lag_col_names=lag_names)
  
  return (ret_list)
}

#Séparer les jeux de données d'entrainement et test avec les variables à modèliser
#On va retourner une liste avec 3 éléments : les df de train, de test et aussi la condition de split.
split_X_Y<-function(par_df,par_var_mod_cible,par_var_mod_quant,par_var_mod_qual,par_var_mod_bool)
{
  #données avec les variables à modéliser
  df_vars<-par_df[c(par_var_mod_cible,par_var_mod_quant,par_var_mod_qual,par_var_mod_bool)]
  
  #condition de split pour le train
  cond_TRAIN=df_vars$Annee<2019
  
  #train data, pas besoin des dates ici
  ret_df_train<-df_vars[cond_TRAIN,]
  
  #test data
  ret_df_test<-df_vars[!cond_TRAIN,]
  
  return (list(train=ret_df_train,test=ret_df_test,split_cond=cond_TRAIN))
}

#standardise data
scale_data<-function(par_df,par_var_mod_quant,par_var_mod_cible)
{
  ret_df<-par_df
  
  #variable cible
  ret_df[par_var_mod_cible]<-scale(ret_df[par_var_mod_cible])
  
  #variables quantitatives
  ret_df[par_var_mod_quant]<-scale(ret_df[par_var_mod_quant])
  
  #This is how we'll got back unscale data
  #Unscaled_Pred <- predict(Mod, cars) * sd(cars$speed) + mean(cars$speed)
  
  return (ret_df)
  
}

#selection des variables selon le critère BIC pour notre modèle. Il retournera les df train et test.
select_vars<-function(par_var_Y,par_df_train,par_df_test)
{
  #on cherche les meilleurs colonnes
  formula <- as.formula(paste(par_var_Y, ' ~ .' ))
  recherche <- regsubsets(formula, int = T,nbest = 1, method = "exhaustive", data = par_df_train)
  resume <- summary(recherche)
  nomselec <- colnames(resume$which)[resume$which[which.min(resume$bic),]][-1]
  nomselec <- c(par_var_Y,nomselec)#plus Y
  
  #Les noms des variables qualitatives sont modifiés avec un SUFIX TRUE ou FALSE.
  #Nous les traitons pour enlever ce sufix, nous ne sommes intéréssés que au noms originaux.
  nomselec<-str_replace(nomselec,"TRUE|FALSE","")
  
  #nouveaux df
  ret_df_train<-par_df_train[nomselec]
  ret_df_test<-par_df_test[nomselec]
  
  #return
  return (list(train=ret_df_train,test=ret_df_test))
}

#Créer le modèles et prédire
train_and_predict<-function(par_Y,par_df_train,par_df_test,par_var_idx,par_mod,par_sel_mod=FALSE,model_tech)
{
  #fist Y
  #the_Y<-unlist(par_list_Y[1])
  the_Y<-par_Y
  
  #Sufix pour la selection du modèle
  if (par_sel_mod==TRUE)
  {
    par_sel_mod<-"msel"
  }
  else
  {
    par_sel_mod<-""
  }
  
  #########  train ##############
  
  print(paste0("Training for ",the_Y," with ",model_tech))
  
  #work
  formRL<-as.formula(paste0(the_Y,"~."))
  
  ### Selon le type de modèle choisi
  if (model_tech=="Ridge" | model_tech=="Lasso")
  {
    mat_x_train = model.matrix(formRL,data=par_df_train)[,-1]
    y_train = par_df_train[,the_Y]
    
    mat_x_test = model.matrix(formRL,data=par_df_test)[,-1]
    
    alphie<-ifelse(model_tech=="Ridge", 0, 1)
    
    # Using cross validation glmnet
    ridge_cv <- cv.glmnet(mat_x_train, y_train, alpha = alphie)
    
    # Best lambda value
    best_lambda <- ridge_cv$lambda.min
    
    mod_reg <- glmnet(mat_x_train, y_train, alpha = alphie, lambda = best_lambda)
  }
  else if (model_tech=="RF")
  {
    mod_reg<-randomForest(formRL,ntree=250,data=par_df_train)
  }
  else if (model_tech=="GB")
  {
    mod_reg<-gbm(formRL,data=par_df_train,distribution="gaussian",cv.fold=5,shrinkage=0.01,n.trees=3000)
    mopt.ada <- gbm.perf(mod_reg,method="cv")
  }
  else #by default RLM
  {
    mod_reg<-lm(formRL,par_df_train)
  }
  
  #show
  nom_fichier_sum<-paste0("sum_",par_mod,"_",the_Y,par_sel_mod,".sum")
  sink(paste(CT_PATH_DATA_OUT,nom_fichier_sum,sep="/"))
  print(summary(mod_reg))
  sink()
  
  #and save
  nom_fichier_rds<-paste0("mod_",par_mod,"_",the_Y,".rds")
  saveRDS(mod_reg, file = paste(CT_PATH_DATA_OUT,nom_fichier_rds,sep="/"))
  
  #########  train ##############
  
  ###########  predicting  ############
  
  ### Selon le type de modèle choisi
  if (model_tech=="Ridge" | model_tech=="Lasso")
  {
    predict_Y <- predict(mod_reg, s = best_lambda, mat_x_test)
  }
  else if (model_tech=="GB")
  {
    predict_Y <- predict(mod_reg,newdata=par_df_test,n.trees=mopt.ada)
  }
  else #by default RLM
  {
    predict_Y <- predict(mod_reg,par_df_test)
  }
  
  #We're adding the local date and time as reference for each prediction
  res<-cbind(Date_Heure_Locale=par_var_idx,real=par_df_test[the_Y],predicted=predict_Y,error=round(get_error(par_df_test[the_Y],predict_Y),2))
  names(res)<-c("Date_Heure_Locale","real","predicted","error")
  
  #Ecrire dans le fichier
  nom_fichier_pred<-paste0("pred_",par_mod,"_",the_Y,".csv")
  write.csv(res,paste(CT_PATH_DATA_OUT,nom_fichier_pred,sep="/"),row.names=FALSE)
  
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
evaluate<-function(par_meth,par_res,par_sel_mod)
{
  mape_res<-get_MAPE(par_res)
  rmse_res<-get_RMSE(par_res)
  
  ret_eval<-cbind(Methode=par_meth,Sel_Model=par_sel_mod,MAPE=round(mape_res,2),RMSE=round(rmse_res,2))
  
  return (ret_eval)
}

#MAPE
get_MAPE<-function(res)
{
  ret_MAPE<-100*(sum(res$error)/nrow(res))
  
  return (ret_MAPE)
}

#RMSE
get_RMSE<-function(res)
{
  ret_RMSE<-100*(sqrt((sum((res$error)^2))/nrow(res)))
  
  return (ret_RMSE)
}

#Ecrire dans les fichiers correspondants les évaluations
dump_evaluations<-function(evaluations,par_var_cible)
{
  #version analyse
  write.csv(evaluations,paste(CT_PATH_DATA_OUT,paste(paste("evaluations_analyse",par_var_cible,sep="_"),".csv"),sep="/"),row.names=FALSE)
  
  #version shiny
  write.csv(evaluations[,c("var_cible","heure_pred","Methode","MAPE","RMSE")],paste(CT_PATH_DATA_OUT,paste(paste("evaluations",par_var_cible,sep="_"),".csv"),sep="/"),row.names=FALSE)
}