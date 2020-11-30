#' @title get_structure
#'
#' @description Get the structure of a dataframe
#' 
#' @param df A dataframe

#' @return A dataframe giving the structure of the input df
#'
#' @export

#get structure of dataframe
get_structure<-function(df){

  #surpress warnings
  options(warn=-1)#use options(warn=0); to bring back warning
  options(scipen=999);
  `%ni%` = Negate(`%in%`);

  #get structure
  structure<-as.data.frame(sapply(df, class))

  #change column name
  colnames(structure)<-c("Type");

  #get variable names
  structure$Variable<-row.names(structure);

  #re-order columns
  structure<-structure[,c("Variable","Type")]

  #change to character
  structure$Type      <-as.character(structure$Type);
  row.names(structure)<-NULL;

  return(structure)
}
