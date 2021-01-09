#####################################################
# Ce fichier est utilisé pour récuperer les donnés  #
# du site de la RATP de qualité de l'aire avec JSON #
#####################################################

recuperer_RATP<-function()
{
  #Test de r?cuperation des donn?es ? travers de JSON
  
  #import
  library(jsonlite)
  
  #url exemple, testing
  # annee2Read<-2013
  # urlRATP<-"https://dataratp2.opendatasoft.com/api/records/1.0/search/?dataset=qualite-de-lair-mesuree-dans-la-station-franklin-d-roosevelt&q=&rows=10000&sort=-dateheure&facet=dateheure&refine.dateheure=2018"
  # dfAux<-fromJSON(urlRATP)
  
  #path where to write file
  path_2_write="C://Formation IA//CEPE//Projet//QAIR//Data//"
  file_2_write="RATP_qair_2013_2020.csv"
  file_qair_RATP=paste(path_2_write,file_2_write,sep="")
  
  #boucle pour r?cup?rer les donn?es
  urlBase="https://dataratp2.opendatasoft.com/api/records/1.0/search/?dataset=qualite-de-lair-mesuree-dans-la-station-franklin-d-roosevelt&q=&rows=10000&sort=-dateheure&facet=dateheure&refine.dateheure="
  donAnnee=list()
  
  #dfAux$records$fields
  
  #lire par an
  for (anInd in 2013:2020)
  {
    print(anInd)
    urlRATP=paste(urlBase,anInd,sep="")
    donAnnee[[anInd]]<-fromJSON(urlRATP)$records$fields
  }
  
  #on s'assure 
  for (anInd in 2013:2020)
  {
    #print(head(donAnnee[[anInd]]$records$fields),5)
    print(head(donAnnee[[anInd]]),5)
  }
  
  #export to a csv
  # 1. concat to a single df
  # 2. write to a file
  
  # 1. concat to a single df
  df_2_write=data.frame()
  for (anInd in 2013:2020)
  {
    df_2_write<-rbind(df_2_write,donAnnee[[anInd]])
  }
  
  # 2. write to a file
  write.csv(df_2_write,file_qair_RATP)
}
