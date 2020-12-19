#Test de récuperation des données à travers de JSON

#import
library(jsonlite)

#url exemple, testing
annee2Read<-2013
urlRATP<-"https://dataratp2.opendatasoft.com/api/records/1.0/search/?dataset=qualite-de-lair-mesuree-dans-la-station-franklin-d-roosevelt&q=&rows=10000&sort=-dateheure&facet=dateheure&refine.dateheure=2018"
dfAux<-fromJSON(urlRATP)

#boucle pour récupérer les données
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

#Quelles années ?
list_choix_an=list()







