
explorer_NAs<-function()
{

    file_2_read <- "DataCorrelated.csv"
    file_data   <- paste(CT_PATH_DATA_PREP,file_2_read,sep="/")
    
    df <- fread(file_data)
    
    df <- data.frame(df)
    df <- df[,-1]  # Un index en trop qui traine
    
    # Calculer les NA
    valNA <- sapply(df,function(x) sum(is.na(x)))
    txNA  <- valNA / nrow(df)*100 # Tout est OK avec les dates et calendrier on ne va pas les repr?senter

  show_var_missing_values2<-function(var_name,printTOrPlotF=TRUE)
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
    show_var_missing_values2(col_var)
  }
    
  #plot
  for (col_var in colnames(df)[-c(1:18)])
  {
    show_var_missing_values2(col_var,FALSE)
  }

}