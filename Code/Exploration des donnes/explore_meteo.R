library(jsonlite)
library(plyr)

urlBase <- "https://data.opendatasoft.com/api/records/1.0/search/?dataset=donnees-synop-essentielles-omm%40public&rows=10000&sort=date&facet=date&facet=nom&facet=temps_present&facet=libgeo&facet=nom_epci&facet=nom_dept&facet=nom_reg&refine.nom=ORLY&refine.date="

donAnnee=list()

#lire par année
for (anInd in 2013:2018)
{
  print(anInd)
  urlmeteo=paste(urlBase,anInd,sep="")
  donAnnee[[anInd]] <- fromJSON(urlmeteo)$records$fields
}

#on s'assure 
for (anInd in 2013:2018)
{
  print(head(donAnnee[[anInd]]),5)
}

for (anInd in 2013:2018)
{
  print(dim(donAnnee[[anInd]]))
}

######## pas le meme nombre de colonnes pour 2013 et 2014 mais on peut combiner avec NA (voir quelles colonnes manquent)
######## ok pour 2015 2016 2017 2018

for (anInd in 2013:2018)
{
  print(table(substr(donAnnee[[anInd]]$date,1,7)))
}

## Trou pour juin juillet août 2018


####################### pas de data 2019 et 2020
#######################

donmeteo <- do.call("rbind.fill", donAnnee)

# colonnes inutiles (references)
colinut <- c("code_epci", "nom_reg", "code_dep", "libgeo", "nom_dept", "numer_sta", "latitude", "nom", "nom_epci",
             "coordonnees", "longitude", "codegeo", "code_reg")
head(donmeteo[,colinut])

donmeteo <- donmeteo[,-which(colnames(donmeteo)  %in% colinut)]
summary(donmeteo)

# join_all_na_columns<-function(dfaux,var_name)
# {
#   nulls_per_year_month<-table(df[is.na(df[var_name]),"date"] %>% substr(1,7))
#   nulls_per_year_month<-data.frame(nulls_per_year_month)
#   colnames(nulls_per_year_month)<-c("var",var_name)    
#   df_joined<-merge(dfaux,nulls_per_year_month,by="var",all=TRUE)
#   return (df_joined)
# }
# 
# df_aux_empty<-data.frame(matrix(ncol = 1, nrow = 0))
# colnames(df_aux_empty)<-c("var")
# df_auxi<-df_aux_empty
# for (col_var in colnames(df)[-c(1,3)])
# {
#   df_auxi<-join_all_na_columns(df_auxi,col_var)
# }
# 
