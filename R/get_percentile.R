#' @title get_percentile
#'
#' @description Get percentile distribution of all numeric variables
#' 
#' @param df A dataframe of variables
#' @param vars A list of numeric variables to get percentile distributions for

#' @return A dataframe with each numeric variable and the percentile distributions
#'
#' @export

#Get percentile distribution for all numeric variables;
get_percentile<-function(df, vars){

  #surpress warnings
  options(warn=-1)#use options(warn=0); to bring back warning
  options(scipen=999);
  `%ni%` = Negate(`%in%`);

  df$jExplore.tmp.variable<-1

  #get distribution of numeric vars
  percentile.dist<- t(apply(df[colnames(df) %in% vars], 2, function(x) round(as.matrix(quantile(x, c(0.00,0.01,0.05,0.10,0.25,0.50,0.75,0.90,0.95,0.99,1.0), na.rm=T)),5)));
  unique.values  <- apply(df[colnames(df)   %in% vars], 2, function(x) length(unique(x)));
  average        <- apply(df[colnames(df)   %in% vars], 2, function(x) mean(x,na.rm=T));
  stand.dev      <- apply(df[colnames(df)   %in% vars], 2, function(x) sd(x,na.rm=T));
  n.missing      <- apply(df[colnames(df)   %in% vars], 2, function(x) sum(is.na(x)));
  summation      <- apply(df[colnames(df)   %in% vars], 2, function(x) round(sum(x,na.rm=T),5));
  less.than.0    <- apply(df[colnames(df)   %in% vars], 2, function(x) sum(ifelse(x<0,1,0),na.rm=T));
  equal.to.0     <- apply(df[colnames(df)   %in% vars], 2, function(x) sum(ifelse(x==0,1,0),na.rm=T));
  gt.than.0      <- apply(df[colnames(df)   %in% vars], 2, function(x) sum(ifelse(x>0,1,0),na.rm=T));

  #bring together;
  combined           <- cbind(n.missing,unique.values,average,stand.dev,percentile.dist,summation,less.than.0,equal.to.0,gt.than.0);
  colnames(combined) <- c("NMissing","UniqueValues","Mean","StDev","Min","P1","P5","P10","Q1","Median","P75","P90","P95","P99","Max","Summation","LT0","ET0","GT0")
  combined           <- as.data.frame(combined);
  combined$Variable  <- row.names(combined);
  combined$PctMissing<- round(combined$NMissing/nrow(df)*100,4);
  combined$NonMissing<- nrow(df) - combined$NMissing;
  combined           <- combined[,c("Variable","NonMissing","NMissing","PctMissing","UniqueValues","Mean","StDev","Min","P1","P5","P10","Q1","Median","P75","P90","P95","P99","Max","Summation","LT0","ET0","GT0")]
  row.names(combined)<- NULL;

  combined[which(combined$Variable != "jExplore.tmp.variable"),]

  return(combined)
}
