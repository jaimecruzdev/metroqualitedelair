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

############################################
####      Constantes globales        #######
###########################################

#Modules fonctionnelles à exécuter
CT_EXE_RECUP_DONNEES<-FALSE
CT_EXE_PREP_DONNEES<-FALSE
CT_EXE_EXPLR_DONNEES<-FALSE
CT_EXE_MODEL_DONNEES<-FALSE

#Path
CT_PATH_CODE<-"Code"
CT_PATH_CODE_RECUP<-paste(CT_PATH_CODE,"Recuperation des donnees",sep="/")
CT_PATH_CODE_EXPL<-paste(CT_PATH_CODE,"Exploration des donnes",sep="/")

# Fichiers du projet
CT_FICHIER_1<-c("workingProjet.R",CT_PATH_CODE_RECUP)
CT_FICHIER_2<-c("Creation des dates.R",CT_PATH_CODE_RECUP)
CT_FICHIER_3<-c("explore_meteo.R",CT_PATH_CODE_RECUP)
CT_FICHIER_4=c("Explore RATP qair Roosevelt.R",CT_PATH_CODE_EXPL)

CT_LIST_FICHIERS<-list(CT_FICHIER_1,CT_FICHIER_2,CT_FICHIER_3,CT_FICHIER_4)

###################
####   MAIN  #####
##################

main

main<-function()
{
  init_config()
  if (CT_EXE_RECUP_DONNEES)
    recuperation_des_donnes()
  if (CT_EXE_PREP_DONNEES)
    preparation_des_donnes()
  if (CT_EXE_EXPLR_DONNEES)
    exploration_des_donnes()
  if (CT_EXE_MODEL_DONNEES)
    modelisation()
}

####################
# INIT CONFIG     #
###################
init_config <- function()
{
  #Inclure les fichiers du projet
  for (aux_fich in CT_LIST_FICHIERS)
    source(paste(aux_fich[2],aux_fich[1],sep="/"))
}

#############################
# RECUPERATION DES DONNEES #
############################
recuperation_des_donnes <-function()
{
  recuperer_RATP()
  recuperer_calendrier()
  recuperer_meteo()
  #recuperer_air_q_ext()
}

#############################
# PREPARATION DES DONNEES #
############################
preparation_des_donnes <-function()
{}

#############################
# EXPLORATION DES DONNEES #
############################
exploration_des_donnes <-function()
{
  explorer_qair_RATP()
}

#############################
# MODELISATION #
############################
modelisation <-function()
{}
