##############################################################################
#
# Ce fichier génére le calendrier dont on se serve dans un deuxième temps  
# avec un outil (Dataiku) pour l'enrichir avec les congés escolaires en France 
#
# Explications et commentaires dans la version Jupyter .ypnb, ce code a été 
# généré à travers de Jupyter. 
#
##############################################################################

recuperer_calendrier<-function()
{
  all_the_dates<-seq(as.Date("2013-1-1 00:00:00"),as.Date("2021-1-1"),by="days")
  dates
  
  as.POSIXct("2016-1-1 3:00")
  
  all_the_hours<-paste(seq(0:23)-1,":00:00",sep="")
  all_the_hours
  
  all_times<-as.vector(outer(all_the_dates, all_the_hours, paste, sep=" "))
  
  all_times
  
  as.POSIXlt("2019-4-12 02:00:00",tz="ddGMT")
  
  all_times_sorted<-sort(as.POSIXlt(all_times,tz="GMT"))
  all_times_sorted
  
  setwd("C://Formation IA//CEPE//Projet//QAIR//Data")
  
  df_all_times<-data.frame(all_times_sorted)
  names(df_all_times)<-"Date"
  df_all_times
  
  write.csv(df_all_times,"calendrier_date_heures.csv")
}
