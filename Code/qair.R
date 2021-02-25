##################################################
########## PROJET QAIR ##########################
#################################################

##########################################################
# Projet de data science autour la qualité de l’air      #
# circulant dans les enceintes ferroviaires souterraines.# 
##########################################################

###################################################
#
# Le but est de ce projet est de créer un modèle
# capable de prédire la qualité de l'aire aux 
# sousterraines des stations du metro en function
# de plusieurs variables méterorologique, 
# qualité de l'aire historiques et à l'extérieur.  
# 
# Nous travaillons avec plusieus phases d'exécution : 
#
# 1. Récuperation des données
# 2. Préparation des données
# 3. Exploration des données
# 4. Modélisation
#
###################################################

########### IMPORT ###############
load_import<-function()
{
  if (CT_INSTALL_PACKS==TRUE)
  {
    install.packages("tidyverse")
    install.packages("zoo")
    install.packages("plyr")
    install.packages("data.table")
    install.packages("FactoMineR")
    install.packages("https://cran.r-project.org/bin/windows/contrib/3.6/foreign_0.8-76.zip")
    install.packages("VIM")
    install.packages("missForest")
    install.packages("missMDA")
  }  
  
  library(tidyverse)
  library(zoo)
  library(plyr)
  library(data.table)
  library(FactoMineR)
  library(VIM)
  library(missForest)
  library(missMDA)
}

############################################
####      Constantes globales        #######
###########################################

load_symbols<-function()
{
  #Install packages, premier fois à exécuter?
  CT_INSTALL_PACKS<<-FALSE
  
  #Montrer exploration, normalment pas trop practique sur le pipeline d'exécution.
  #Plutôt sur un Notebook.
  CT_SHOW_EXPLORATIONS<<-FALSE
  
  #Modules fonctionnelles à exécuter
  CT_EXE_RECUP_DONNEES<<-FALSE
  CT_EXE_EX_PR_DONNEES<<-FALSE
  CT_EXE_PREP_DONNEES<<-FALSE
  CT_EXE_EXPLR_DONNEES<<-FALSE
  CT_EXE_MODEL_DONNEES<<-FALSE
  CT_EXE_EX_PR_AVANT_MODEL<<-FALSE
  CT_EXE_MODEL<<-TRUE
  
  #Paths
  
  #Chemins du code
  CT_PATH_CODE<<-"Code"
  CT_PATH_CODE_RECUP<<-paste(CT_PATH_CODE,"Recuperation des donnees",sep="/")
  CT_PATH_CODE_EXPL<<-paste(CT_PATH_CODE,"Exploration des donnees",sep="/")
  CT_PATH_CODE_PREP<<-paste(CT_PATH_CODE,"Preparation des donnees",sep="/")
  CT_PATH_CODE_MODEL<<-paste(CT_PATH_CODE,"Modelisation",sep="/")
  
  
  #chemins des données
  CT_PATH_DATA<<-"Data"
  CT_PATH_DATA_PREP<<-paste(CT_PATH_DATA,"Prepared",sep="/")
  CT_PATH_DATA_OUT<<-paste(CT_PATH_DATA,"out",sep="/")
  
  # Fichiers du projet
  CT_FICHIER_1<<-c("workingProjet.R",CT_PATH_CODE_RECUP)
  CT_FICHIER_2<<-c("Creation des dates.R",CT_PATH_CODE_RECUP)
  CT_FICHIER_3<<-c("explore_meteo.R",CT_PATH_CODE_RECUP)
  CT_FICHIER_4<<-c("Explore RATP qair Roosevelt.R",CT_PATH_CODE_EXPL)
  CT_FICHIER_5<<-c("Explorer Heures RATP.r",CT_PATH_CODE_EXPL)
  CT_FICHIER_6<<-c("PrepareDateHeuresRATP.r",CT_PATH_CODE_PREP)
  CT_FICHIER_7<<-c("InterpolationMeteo.r",CT_PATH_CODE_PREP)
  CT_FICHIER_8<<-c("QairExtDuJourAuxHeures.R",CT_PATH_CODE_PREP)
  CT_FICHIER_9<<-c("CorrelationQuatreSources.r",CT_PATH_CODE_PREP)
  CT_FICHIER_10<<-c("Explore base merge.R",CT_PATH_CODE_EXPL)
  CT_FICHIER_11<<-c("Exploration pre model.R",CT_PATH_CODE_EXPL)
  CT_FICHIER_12<<-c("Preparation des valeurs manquantes.R",CT_PATH_CODE_PREP)
  CT_FICHIER_12<<-c("modeling.R",CT_PATH_CODE_MODEL)
  
  CT_LIST_FICHIERS<<-list(CT_FICHIER_1,CT_FICHIER_2,CT_FICHIER_3,CT_FICHIER_4,CT_FICHIER_5,CT_FICHIER_6,CT_FICHIER_7,CT_FICHIER_8,CT_FICHIER_9,CT_FICHIER_10,CT_FICHIER_11,CT_FICHIER_12)
}

###################
####   MAIN  #####
##################

#Go
main()

main<-function()
{
  init_config()
  if (CT_EXE_RECUP_DONNEES)
    recuperation_des_donnees()
  if (CT_EXE_EX_PR_DONNEES)
    explor_prev_des_donnees()
  if (CT_EXE_PREP_DONNEES)
    preparation_des_donnees()
  if (CT_EXE_EXPLR_DONNEES)
    exploration_des_donnees()
  if (CT_EXE_EX_PR_AVANT_MODEL)
    preparation_avant_model()
  if (CT_EXE_MODEL)
    modelisation()
}

####################
# INIT CONFIG     #
###################
init_config <- function()
{
  #load constants
  load_symbols()
  
  #import
  load_import()
  
  #Inclure les fichiers du projet
  for (aux_fich in CT_LIST_FICHIERS)
  {
    print(paste(aux_fich[2],aux_fich[1],sep="/"))
    source(paste(aux_fich[2],aux_fich[1],sep="/"))
  }
}

#############################
# RECUPERATION DES DONNEES #
############################
recuperation_des_donnees <-function()
{
  recuperer_RATP()
  recuperer_calendrier()
  recuperer_meteo()
  #recuperer_air_q_ext()
}

###############################################
# EXPLORATION AVANT RIEN PREPARER DES DONNEES #
###############################################
explor_prev_des_donnees <-function()
{
  if (CT_SHOW_EXPLORATIONS==TRUE)
  {
    explorer_qair_RATP()
    explore_heures_RATP()
  }
}

#############################
# PREPARATION DES DONNEES #
############################
preparation_des_donnees <-function()
{
  prepar_qair_ratp()
  interpolation_meteo()
  etendreHeuresQairExt()
  join_all_sources()
}

##################################################
# EXPLORATION ET PREPARATION AVANT MODELISATION #
#################################################
exploration_des_donnees <-function()
{
  if (CT_SHOW_EXPLORATIONS==TRUE)
  {
    explorer_NAs()
    exploration_des_trous()
  }
  
  explore_prepare_before_model()
}

########################################
# DERNIERE PREPARATION AVANT LE MODEL #
#######################################
preparation_avant_model <- function()
{
  imputation_des_trous()
}

#############################
# MODELISATION #
############################
modelisation <-function()
{
  modeling()
}
