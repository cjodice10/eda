#' @title get_frequency
#'
#' @description Get frequency distribution of non-numeric variables
#'
#' @param df A dataframe of variables
#' @param vars A list of non-numeric variables to get frequencies for
#' @param unique.threshold.pct A value between 0 and 1. If the number of unique values for a variable is > to this threshold, it will not output the frequency distribution.
#'
#' @return A list of two.  Each being a dataframe.  First dataframe will give the frequency distribution to the variables.  The second dataframe will show the percent missing for each variable
#'
#' @export
#Get frequency distribution of all non-numeric variables
get_frequency<-function(df,vars,unique.threshold.pct=0.8){

  #surpress warnings
  options(warn=-1)#use options(warn=0); to bring back warning
  options(scipen=999);
  `%ni%` = Negate(`%in%`);

  #generate empty dataframe
  df.2.return<- data.frame(  Variable =character()
                            ,Level    =character()
                            ,Frequency=integer()
                            ,Percent  =double()
                            ,Uniques  =integer()
                            ,stringsAsFactors = FALSE)
  nbr.records<- nrow(df);

  unique.values          <- as.data.frame(apply(df[colnames(df)   %in% vars], 2, function(x) length(unique(x))));
  unique.values$Variable <- row.names(unique.values)
  colnames(unique.values)<- c("UniqueValues","Variable");
  row.names(unique.values)<-NULL;
  unique.values          <- unique.values[,c("Variable","UniqueValues")]
  unique.values$display  <- ifelse(unique.values$UniqueValues/nbr.records > unique.threshold.pct,0,1)
  remove.variables       <- unique.values[which(unique.values$display==0),c("Variable")]
  unique.values$display  <- NULL

  #get frequency dist for all variables
  freq.dist<- apply(df[colnames(df) %in% vars], 2, function(x) table(x,exclude = NULL));

  #loop through the list
  for(i in 1:length(freq.dist)){
    #get name of variable
    name<-names(freq.dist[i])

    #change to df
    df<- as.data.frame(freq.dist[i])

    #create variable name
    df$Variable<-name

    #change column names
    colnames(df)<-c("Level","Frequency","Variable");

    #get percent
    df$Percent<-round(df$Frequency/sum(df$Frequency)*100,3)

    #re-order columns
    df<-df[,c("Variable","Level","Frequency","Percent")]

    #change to character
    df$Variable<-as.character(df$Variable)
    df$Level   <-as.character(df$Level);

    #bind it
    df.2.return<-dplyr::bind_rows(df.2.return,df)
  }

  #create a dataframe with variable and % missing;
  cat.pct.missing<- data.frame(Variable=unique(df.2.return$Variable));

  #merge
  cat.pct.missing<- merge(  x     = cat.pct.missing
                           ,y     = df.2.return[which(stringr::str_trim(df.2.return$Level)=="" |
                                                      stringr::str_trim(df.2.return$Level)=="<NA>" |
                                                      is.na(stringr::str_trim(df.2.return$Level))),c("Variable","Frequency","Percent")]
                           ,by.x  = c("Variable")
                           ,by.y  = c("Variable")
                           ,all.x = TRUE);

  #roll up if needed;
  cat.pct.missing<-cat.pct.missing %>%
    group_by(Variable) %>%
    dplyr::summarise(  NMissing   = sum(Frequency,na.rm=TRUE)
                      ,PctMissing = sum(Percent,na.rm=TRUE)) %>%
    data.frame();

  cat.pct.missing<- merge(  x     = cat.pct.missing
                           ,y     = unique.values
                           ,by.x  = c("Variable")
                           ,by.y  = c("Variable")
                           ,all.x = TRUE);

  cat.pct.missing$NonMissing = nbr.records - cat.pct.missing$NMissing
  cat.pct.missing =  cat.pct.missing[,c("Variable","NonMissing","NMissing","PctMissing","UniqueValues")]

  #remove variables if needed
  df.2.return<- df.2.return[which(df.2.return$Variable %ni% remove.variables),]

  row.names(cat.pct.missing)<-NULL;
  row.names(df.2.return)    <-NULL;
  return(list(df.2.return,cat.pct.missing))
}
