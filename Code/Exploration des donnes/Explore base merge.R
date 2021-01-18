library(plyr)
library(data.table)
library(tidyverse)

path_2_read <- "C:/Users/A56814/Documents/Projet Data Science/metroqualitedelair-main/Data/Prepared/"
file_2_read <- "DataCorrelated.csv"
file_data   <- paste(path_2_read,file_2_read,sep="")

df <- fread(file_data)
# on renomme les colonnes meteo
setnames(df, c("rr3", "tc", "pres", "dd", "ww", "cod_tend", "ff", "n"),
              c("pluie_3_heures", "temperature_celsius", "pression", "direction_vent_10mn", "temps_present_num",
                "type_tendance_barometrique", "vitesse_vent_10mn", "nebulosite_totale"))
df <- data.frame(df)
df <- df[,-1]  # Un index en trop qui traine

# Calculer les NA
valNA <- sapply(df,function(x) sum(is.na(x)))
txNA  <- valNA / nrow(df)*100 # Tout est OK avec les dates et calendrier on ne va pas les représenter

show_var_missing_values<-function(var_name,printTOrPlotF=TRUE)
  
{    
  nulls_per_year_month<-table(df[is.na(df[var_name]),colnames(df)[4]] %>% substr(1,7))
 
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

for (col_var in colnames(df)[-c(1:18)])
{
  show_var_missing_values(col_var)
}
#plot
for (col_var in colnames(df)[-c(1:18)])
{
  show_var_missing_values(col_var,FALSE)
}

