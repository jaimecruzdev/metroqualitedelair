explore_heures_RATP<-function()
{
  library("tidyverse")
  
  DATA_PATH="C://Formation IA//CEPE//Projet//QAIR//Data//"
  DATA_FILE="RATP_qair_2013_2020.csv"
  
  fichier_donnees=paste(DATA_PATH,DATA_FILE,sep="")
  df<-read.csv(fichier_donnees,sep=";")
  
  df
  
  head(df)
  
  #file_name_capture_RATP="capture RATP Roosevelt.PNG"
  #file_capture_RATP=paste(DATA_PATH,file_name_capture_RATP,sep="")
  #display_png(file=file_capture_RATP)  
  
  fichier_csv_site="download du site qualite-de-lair-mesuree-dans-la-station-franklin-d-roosevelt.csv"
  fichier_donnees_site=paste(DATA_PATH,fichier_csv_site,sep="")
  df_site<-read.csv(fichier_donnees_site,sep=";",header=FALSE)
  head(df_site)
  
  nrow(df)
  
  #file_name_capture_RATP_nrow="Capture RATP nrow.PNG"
  #file_capture_RATP_nrow=paste(DATA_PATH,file_name_capture_RATP_nrow,sep="")
  #display_png(file=file_capture_RATP_nrow)  
  
  8758+8759+8760+8762+8758+8737+8751+8352
  
  table(df$dateheure %>% substr(1,4))
  
  summary(df)
  
  n_occur <- data.frame(table(df$dateheure))
  n_occur[n_occur$Freq > 1,]
  
  year_table<-list()
  for (year_ind in 2013:2020)
  {
      aux_ind<-df$dateheure %>% substr(1,4)==year_ind
      year_table[[year_ind]]<-as.POSIXct(df$dateheure[aux_ind],format="%Y-%m-%dT%H:%M:%OS",tz="GMT")
  }
  
  year_table[[2013]]
  
  for (year_ind in 2013:2020)
  {
      print(year_ind)
      print(min(year_table[[year_ind]]))
      print(max(year_table[[year_ind]]))
  }
  
  df$dateheure[df$dateheure %>% substr(1,4)==2014]
  
  df$dateheure[df$dateheure %>% substr(1,4)==2017]
  
  (year_table[[2013]])
  
  aux_ind<-df$dateheure %>% substr(1,4)==2013
  year_table[[2013]]<-df$dateheure[aux_ind]
  
  ind2014 <- df$dateheure %>% substr(1,4)==2014
  df$dateheure[ind2014]
  
  table(df$dateheure %>% substr(1,4))
  
  ind2013 <- df$dateheure %>% substr(1,4)==2013
  table(df$dateheure[ind2013] %>% substr(6,7))
  
  ind2014 <- df$dateheure %>% substr(1,4)==2014
  table(df$dateheure[ind2014] %>% substr(6,7))
  
  ind2015 <- df$dateheure %>% substr(1,4)==2015
  table(df$dateheure[ind2015] %>% substr(6,7))
  
  ind2016 <- df$dateheure %>% substr(1,4)==2016
  table(df$dateheure[ind2016] %>% substr(6,7))
  
  ind2016 <- df$dateheure %>% substr(1,4)==2016 
  dfDec<-df$dateheure[ind2016]
  ind2016Dec<-dfDec %>% substr(6,7)==12
  dfDec[ind2016Dec]
  
  table(dfDec[ind2016Dec] %>% substr(9,10))
  
  ind2017 <- df$dateheure %>% substr(1,4)==2017
  dfDec17<-df$dateheure[ind2017]
  table(dfDec17 %>% substr(6,7))
  
  df[59705,]
  df[59706,]
}