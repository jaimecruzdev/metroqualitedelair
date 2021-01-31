################################################
# Exploration des donnees de qualité de l'air
# Ce fichier est généré d'un ypnb Jupyter. 
# Nous conseillons d'exécuter le notebook au lieu 
# de ce code directement
################################################

explorer_qair_RATP<-function()
{
  #library("IRdisplay") #used to show png file 
  library("tidyverse")
  
  DATA_PATH="C://Formation IA//CEPE//Projet//QAIR//Data//"
  DATA_FILE="RATP_qair_2013_2020.csv"
  
  fichier_donnees=paste(DATA_PATH,DATA_FILE,sep="")
  df<-read.csv(fichier_donnees)
  
  df
  
  head(df)
  
  file_name_capture_RATP="capture RATP Roosevelt.PNG"
  file_capture_RATP=paste(DATA_PATH,file_name_capture_RATP,sep="")
  #display_png(file=file_capture_RATP)  
  
  fichier_csv_site="download du site qualite-de-lair-mesuree-dans-la-station-franklin-d-roosevelt.csv"
  fichier_donnees_site=paste(DATA_PATH,fichier_csv_site,sep="")
  df_site<-read.csv(fichier_donnees_site,sep=";",header=FALSE)
  head(df_site)
  
  nrow(df)
  
  file_name_capture_RATP_nrow="Capture RATP nrow.PNG"
  file_capture_RATP_nrow=paste(DATA_PATH,file_name_capture_RATP_nrow,sep="")
  #display_png(file=file_capture_RATP_nrow)  
  
  8758+8759+8760+8762+8758+8737+8751+8352
  
  table(df$dateheure %>% substr(1,4))
  
  summary(df)
  
  
  
  df_aux_empty<-data.frame(matrix(ncol = 1, nrow = 0))
  colnames(df_aux_empty)<-c("var")
  df_auxi<-df_aux_empty
  for (col_var in colnames(df)[-c(1,3)])
  {
      df_auxi<-join_all_na_columns(df_auxi,col_var)
  }
  
  df_auxi
  
  
  
  #Aide1 : Apply version for this function ?
  #print
  for (col_var in colnames(df)[-c(1,3)])
  {
      show_var_missing_values(col_var)
  }
  #plot
  for (col_var in colnames(df)[-c(1,3)])
  {
      show_var_missing_values(col_var,FALSE)
  }
  
  #Aide3 : plot toutes les dates et le nombre de NAs
  
  #test 
  
  #df_exp<-df
  #df_exp$year_month<-df_exp[,"dateheure"]%>%substr(1,7)
  #df_exp_na_per_month=df_exp%>%group_by(year_month)%>% summarise(cnt_na_x10=sum(is.na(X10fra1)))
  #df_exp_na_per_month
  
  #create_matrix_per_xvar<-function(x_var)
  #{
  #    df_exp_na_per_month<-df_exp%>%group_by(year_month)%>%summarise(cnt_na=sum(is.na(x_var)))
  #    return (df_exp_na_per_month)
  #}
  
  #create_na_matrix<-function(datfr)
  #{
  #    retdf=data.frame()
  #    
  #    for (col in datfr)
  #    {
  #        ext_df<-create_matrix_per_xvar(datfr,col)
  #        retdf[[col]]<-ext_df[2]
  #    }     
  #    
  #    return (retdf)
  #}
  
  #create_na_matrix(df) apply(df_exp,2,create_matrix_per_xvar) create_matrix_per_xvar(df_exp$X10fra1)
  
  ##Aide2
  
  ##df_exp%>%group_by(year_month)%>%summarise(cnt_na=sum(is.na(X10fra1)))
  
  ##df_exp%>%group_by(year_month)%>%summarise(cnt_na=sum(is.na("X10fra1"))) varcol="X10fra1" df_exp%>%group_by(year_month)%>%summarise(cnt_na=sum(is.na(varcol)))
  
  
  
  
  colnames(df)
  
  plot(df$tfra1,df$hyfra1,main="Température (°C) / Humidité (%)",col=3,xlab="Température (°C)",ylab="Humidité (%)")
  
  plot_colors=cut(df$hyfra1, breaks = c(-Inf, 30, 50,70,90,+Inf), 
                   labels = c("blue","red","yellow","green","black"), 
                   right = FALSE)
  
  plot(df$tfra1,df$c2fra1,main="Température (°C) / CO2 (ppm)",col=plot_colors,xlab="Température (°C)",ylab="CO2 (ppm) ")
  legend(title="Humidité","topleft",legend=c("<30%","30% - 50%","50% - 70%","70% - 90%",">90%"), pch=16, col=levels(plot_colors))
  
  plot_colors_2=cut(df$nofra1, breaks = c(-Inf, 10, 40,70,100,130,+Inf), 
                   labels = c("blue","red","yellow","green","orange","black"), 
                   right = FALSE)
  
  plot(df$nofra1,df$c2fra1,main="NO2 (μg/m3) / CO2 (ppm) ",col=plot_colors_2,xlab="NO2 (μg/m3)",ylab="CO2 (ppm)")
  legend(title="NO","topright", legend=c("<10 μg/m3","10 - 40 μg/m3","40 - 70 μg/m3","70 - 100 μg/m3","100 - 130 μg/m3",">130 μg/m3"), pch=16, col=levels(plot_colors_2))
  
  plot_colors_3=cut(df$tfra1, breaks = c(-Inf, 0,10,20,30,+Inf), 
                  labels = c("yellow","orange","blue","green","red"))
  
  plot_colors_3
  
  #plot_colors_3=cut(df$tfra1, breaks = c(-Inf, 0,10,20,30,+Inf), 
  #                 labels = c("yellow","orange","blue","green","red"))
  plot(df$X10fra1,df$c2fra1,col=plot_colors_3,main="Particules fines (μg/m3) / CO2 (ppm)",xlab="Particules fines (μg/m3)",ylab="CO2 (ppm) ")
  legend(title="Température","topright", legend=c("<0 (ºC)","0 - 10 (ºC)","10 - 20(ºC)","20 - 30(ºC)",">30 (ºC)"), pch=16, col=levels(plot_colors_3))
  
  plot(df$tfra1,df$X10fra1,col="blue",main="Température (²C) / Particules fines (μg/m3)",xlab="Température(ºC)",ylab="Particules fines (μg/m3)")
}

#This function will filter missing values, and show them numerically and in a plot
show_var_missing_values<-function(var_name,printTOrPlotF=TRUE)
{    
  nulls_per_year_month<-table(df[is.na(df[var_name]),colnames(df)[3]] %>% substr(1,7))
  #Aide4 : Il faut chosir une option parce que les deux ne marche pas
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

join_all_na_columns<-function(dfaux,var_name)
{
  nulls_per_year_month<-table(df[is.na(df[var_name]),colnames(df)[3]] %>% substr(1,7))
  nulls_per_year_month<-data.frame(nulls_per_year_month)
  colnames(nulls_per_year_month)<-c("var",var_name)    
  df_joined<-merge(dfaux,nulls_per_year_month,by="var",all=TRUE)
  return (df_joined)
}