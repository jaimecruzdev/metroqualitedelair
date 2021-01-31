library(jsonlite)
library(plyr)
library(data.table)
library(tidyverse)
library(corrplot)

#urlBase <- "https://data.opendatasoft.com/api/records/1.0/search/?dataset=donnees-synop-essentielles-omm%40public&rows=10000&sort=date&facet=date&facet=nom&facet=temps_present&facet=libgeo&facet=nom_epci&facet=nom_dept&facet=nom_reg&refine.nom=ORLY&refine.date="
urlBase <- "https://data.opendatasoft.com/api/records/1.0/search/?dataset=donnees-synop-essentielles-omm%40public&rows=10000&sort=date&facet=date&facet=nom&facet=temps_present&facet=libgeo&facet=nom_epci&facet=nom_dept&facet=nom_reg&refine.nom_reg=%C3%8Ele-de-France&refine.date="
donAnnee=list()

#lire par annee
for (anInd in 2013:2019)
{
  print(anInd)
  urlmeteo=paste(urlBase,anInd,sep="")
  donAnnee[[anInd]] <- fromJSON(urlmeteo)$records$fields
}

#on s'assure 
for (anInd in 2013:2019)
{
  print(head(donAnnee[[anInd]]),5)
}

for (anInd in 2013:2019)
{
  print(dim(donAnnee[[anInd]]))
}

######## pas le meme nombre de colonnes pour 2013 et 2014 mais on peut combiner avec NA (voir quelles colonnes manquent)
######## ok pour 2015 2016 2017 2018 2019

for (anInd in 2013:2019)
{
  print(table(substr(donAnnee[[anInd]]$date,1,4)))
  print(table(substr(donAnnee[[anInd]]$date,1,7)))
}

# Volumes OK par rapport au site
## Trou pour juillet 2018


####################### pas de data 2020
#######################

donmeteo <- do.call("rbind.fill", donAnnee)

# colonnes inutiles (references)
colinut <- c("code_epci", "nom_reg", "code_dep", "libgeo", "nom_dept", "numer_sta", "latitude", "nom", "nom_epci",
             "coordonnees", "longitude", "codegeo", "code_reg")
head(donmeteo[,colinut])

donmeteo <- donmeteo[,-which(colnames(donmeteo)  %in% colinut)]
summary(donmeteo)

#path where to write file
path_2_write = "C:/Users/A56814/Documents/Projet Data Science/metroqualitedelair-main/Data/"
file_2_write = "meteo_2013_2019.csv"
file_meteo   = paste(path_2_write,file_2_write,sep="")
#write.table(donmeteo, file_meteo, row.names = F, sep = ";")

df <- fread(file_meteo)
df <- data.frame(df)
summary(df)

# Calculer les NA
valNA <- sapply(df,function(x) sum(is.na(x)))
txNA  <- valNA / nrow(df)*100
valsup50 <- names(txNA)[txNA > 50]
df <- df[,-which(colnames(df)  %in% valsup50)]

df <- df[,!colnames(df) %in% c("ht_neige", "type_de_tendance_barometrique", "mois_de_l_annee", "ssfrai", "perssfrai", "vv",
             "altitude", "w2", "w1", "pmer", "per", "t", "etat_sol")]  # Suppression variables non pertinentes ou redondantes (Exemple t en Kelvin versus t en Celsius)
valnumeric <- sapply(df,function(x) is.numeric(x))
numvalue   <- names(valnumeric[valnumeric==TRUE])
mcor <- cor(df[,colnames(df) %in% numvalue], use = "complete.obs")
write.table(mcor, paste(path_2_write,"mcor.csv",sep=""), sep = ";")

corrplot(mcor, type="upper", order="hclust", tl.col="black", tl.srt=45)
View(mcor)


# on repasse les NA pour garder les mieux remplis sur les corrélées
valNA <- sapply(df,function(x) sum(is.na(x)))
txNA  <- valNA / nrow(df)*100

# td et tc corrélés on garde tc température plutot que point de rosée
# tc et u corrélés on garde tc température plutot que humidité
# cod_tend et tend + tend24 corrélés on garde cod_tend type de tendance barométrique, moins de modalités et idem en NA
# hnuage1 corrélé avec hbas cl ctype1 on garde cl 

# On renettoye pour y voir plus clair
df <- df[,!colnames(df) %in% c("td", "u", "tend", "tend24", "hnuage1", "hbas", "ctype1")]
numvalue   <- names(valnumeric[valnumeric==TRUE])
mcor <- cor(df[,colnames(df) %in% numvalue], use = "complete.obs")
corrplot(mcor, type="upper", order="hclust", tl.col="black", tl.srt=45)

# on repasse les NA pour garder les mieux remplis sur les corrélées
valNA <- sapply(df,function(x) sum(is.na(x)))
txNA  <- valNA / nrow(df)*100


# nnuage1 et n corrélés on garde n nébulosité totale avec moins de NA
# n et nbas corrélés on garde n nébulosité totale 
# ww corrélé avec cl cm et ch on garde ww temps présent avec moins de NA
# rr1 corrélé avec rr3 rr6 rr12 et rr24 on garde rr3 précipitations 3 dernieres heures avec moins de NA
# rafper corrélé avec ff et raf10 on garde ff vitesse vent moyen
# on peut aussi enlever temps_passe_1 car 5% de NA et en lien avec variable temps_present

# On renettoye pour y voir plus clair
df <- df[,!colnames(df) %in% c("nnuage1", "nbas", "cl", "cm", "ch", "rr1", "rr6", "rr12", "rr24", "rafper", "raf10", "temps_passe_1")]
numvalue   <- names(valnumeric[valnumeric==TRUE])
mcor <- cor(df[,colnames(df) %in% numvalue], use = "complete.obs")
corrplot(mcor, type="upper", order="hclust", tl.col="black", tl.srt=45)


show_var_missing_values<-function(var_name,printTOrPlotF=TRUE)
  
{    
  nulls_per_year_month<-table(df[is.na(df[var_name]),colnames(df)[9]] %>% substr(1,7))
  #Aide4 : Il faut choisir une option parce que les deux ne marche pas
  if (printTOrPlotF==TRUE)
  {
    print(var_name)
    print(nulls_per_year_month)
  }
  else
  {
    plot(nulls_per_year_month,main=var_name,type="l")
  }
}

for (col_var in colnames(df)[-9])
{
  show_var_missing_values(col_var)
}
#plot
for (col_var in colnames(df)[-9])
{
  show_var_missing_values(col_var,FALSE)
}

