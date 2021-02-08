library("tidyverse")

DATA_PATH="D://Jaime//Projet//QAIR//Data//Prepared//"
DATA_FILE_IN="DataCorrelated.csv"
DATA_FILE_OUT="data_model.csv"

fichier_donnees=paste(DATA_PATH,DATA_FILE_IN,sep="")
df<-read.csv(fichier_donnees)

df

head(df)

nrow(df)

summary(df)

levels(df$Conge_scolaire_raison) <- c("ete","hiver","hiver","hiver","hiver","hiver","hiver","hiver","Toussaint","Noel","printemps","printemps","printemps","printemps","printemps","printemps","printemps","")

levels(df$temps_present)<-c('Clairs','Clairs','Averse','Neige','Averse','Averse','Averse','Averse','Averse','Averse','Brouillard','Brouillard','Brouillard','Brouillard','Brouillard','Brouillard','Brouillard','Brouillard','Brouillard','Brouillard','Brouillard','Bruine','Bruine','Bruine','Bruine','Bruine','Bruine','Bruine','Bruine','Brume','Brume','Neige','Neige','Neige','Nuages','Brouillard','Neige','Neige','Nuages','Nuages','Orage','Orage','Orage','Pluie','Pluie','Pluie','Pluie','Pluie','Pluie','Pluie','Pluie','Pluie','Pluie','Pluie','Pluie','Pluie','Pluie')

names(df)[11]<-"Ferie"
names(df)[12]<-"Ferie_raison"

#On aura Paris et ailleurs comme congés escolaire
df[,"Conges_Escolaire_Zone_AB"]<-0
df[df["Conges_Escolaire_Zone_A"]==1 | df["Conges_Escolaire_Zone_B"]==1,"Conges_Escolaire_Zone_AB"]<-1

#Nous supprimons les colonnes dont nous n'avons plus besoin
df$Conge_zones<-NULL
df$Conges_Escolaire<-NULL
df$Conges_Escolaire_Zone_A<-NULL
df$Conges_Escolaire_Zone_B<-NULL
#df$Conges_Escolaire_Zone_C<-NULL

#Jour d'activité ?
#df["jour_activite"]<-(df["Ferie"]==0 & df["Jour_de_la_Semaine"]<=5)
df[,"jour_activite"]<-0
df[df["Ferie"]==0 & df["Jour_de_la_Semaine"]<=5,"jour_activite"]<-1
#On supprime ces variables 
df["Ferie"]<-NULL
df["Conge_hebdomadaire"]<-NULL
#df["Jour_de_la_Semaine"]<-NULL

#"Feriée raison", il est déjà pris en compte par feriée + moi. Pareil pour "Congés raison". 
df$Ferie_raison<-NULL
df$Conge_scolaire_raison<-NULL
df$Conge_scolaire<-NULL

df$ext_so2<-NULL
df$ext_co<-NULL

df$X<-NULL

summary(df)

df_mod_tr<-df[df["Annee"]>=2014 & df$Annee<2019,!colnames(df) %in% c("Date_Heure_Locale","Timestamp","temps_present","Heure")]
df_mod_test<-df[df$Annee==2019,!colnames(df) %in% c("Date_Heure_Locale","Timestamp","temps_present","Heure")]

mod<-lm(co2~.,df_mod_tr)
#?lm
pr<-predict(mod,df_mod_test)

length(df_mod_test$Annee)
length(pr)

pr_df<-data.frame(Date=df[df$Annee==2019,c("Date_Heure_Locale")],prev=pr,co2=df[df$Annee==2019,c("co2")])

pr_df

pr_df$Date<-as.POSIXlt(as.character(pr_df[, "Date"]), tz = "UTC")

amTimeSeries(pr_df, 'Date', c('prev', 'co2'))

plot(pr,type="l")
lines(df_mod_test$co2,col="red")

#install.packages("rAmCharts")
library("rAmCharts")

amPlot?
  
  fichier_donnees=paste(DATA_PATH,DATA_FILE_OUT,sep="")
write.csv(df,fichier_donnees)
