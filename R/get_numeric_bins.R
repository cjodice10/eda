#' @title get_numeric_bins
#'
#' @description Numeric grouping
#'
#' @param run_id An identifier that will be used when outputting tables to the specified path (path_2_save parameter).  Example: 'MyRun1'
#' @param df A dataframe you are wanting to analyze
#' @param dv The name of the dependent variable (dv).  Example: 'target'
#' @param dv.type Can take on 1 of two inpunts - c('Binary','Frequency').  Both should be numeric.  If 'Frequency' is the input, it should be the numerator (if it is a rate).  The denominator will be specified as a separate parameter
#' @param dv.denominator The denominator of your dependent variable.  In many cases, this can be considered the exposure
#' @param var.list A list of non-numeric variables to analyze and create bins for
#' @param nbins Maximum number of bins to initially split the variable into
#' @param min.Pct The minimun percent of records a final bin should have.  Generally applies to only bins that are not NA
#' @param binning.Type The type of binning to use when splitting the variable.  One of two can be selected: c("Bucketing","Quantiles")
#' @param monotonic Logical TRUE/FALSE input.  If TRUE, it will force the bins to be monotonic based on the event rate
#' @param tracking Logical TRUE/FALSE input.  If set to TRUE, the user will be able to see what variable the function is analyzing.
#' @param path_2_save A path to a folder to save a log file
#'
#' @return A list of dataframes.  First in the list will be 'Numeric_eda' - this is an aggregated dataframe showing the groups created along with other key information.  The second is 'numeric_iv' - This is a dataframe with each variable processed and their information value.  The last is 'numeric_logics' - This is a dataframe with the information needed to apply to your dataframe and transform your variables.  This table will be the input to apply_numeric_logic(logic_df=numeric_logics)
#' @export

get_numeric_bins<-function(  run_id
                            ,df                           # dataframe
                            ,dv                           # Dependent Varaible
                            ,dv.type                      # Binary, Frequency
                            ,dv.denominator = NULL        # Only used for exposure of frequency
                            ,var.list                     # A list of numeric variables
                            ,nbins          = 20          # >1
                            ,min.Pct        = 0.02        # (0,1)
                            ,binning.Type   = "Bucketing" # Bucketing or Quantiles
                            ,monotonic      = TRUE        # TRUE or FALSE
                            ,tracking       = TRUE        # Do you want to track progress or not
                            ,path_2_save    = getwd()
                            ){

  #surpress warnings
  options(warn=-1)#use options(warn=0); to bring back warning
  options(scipen=999);
  `%ni%` = Negate(`%in%`);

  #if(!is.null(dv.denominator) & length(dv.denominator>0) & str_trim(dv.denominator)==" "){dv.denominator<-NULL}

  #some basic checks
  if(is.null(dv)){
    stop("Must have a dv")
  }

  if(!is.numeric(df[,dv])){
    stop("Dependent Variable must be numeric.  If dv.type == 'Binary' then a value of 1 signifies the 'event' you are trying to predict")
  }

  if((nlevels(factor(df[,dv]))==1 | nlevels(factor(df[,dv]))>2) & dv.type=="Binary"){
    stop("Dependent Variable should only have two values, 1 or 0")
  }

  if(length(var.list)==0){
    message("Numeric variable list is empty")
    return(NULL)
  }

  if(dv.type %ni% c("Binary","Frequency")){
    stop("dv.type can only take on values c('Binary','Frequency')")
  }

  if(!is.null(dv.denominator) && dv.denominator %ni% colnames(df)){
    stop("dv.denominator is not listed in your dataframe.  If your DV is purely a count variable (no exposure), then leave this NULL")
  }

  if(nbins<=1){
    stop("nbins must be >1")
  }

  if(min.Pct<=0 | min.Pct >=1){
    stop("min.Pct must be between 0 and 1:  (0,1)")
  }

  if(tracking==TRUE){
    write.table( data.frame(Logging = "Initial line in log file"),
                 file=paste(path_2_save,"/",run_id,"-numeric_log_file.txt",sep=""),
                 append = F,
                 sep='\t',
                 row.names=F,
                 col.names=T )
  }

  #remove dv and denom from varlist
  var.list = var.list[var.list %ni% c(dv, dv.denominator)]

  NbrRecords<-nrow(df)

  #create an empty table for summary edas;
  NumericEDA.fine<- data.frame(  Variable   = character()
                                ,bin_id     = double()
                                ,UpperBound = double()
                                ,Exposure   = double()
                                ,Records    = double()
                                ,Events     = double()
                                ,EventRate  = double()
                                ,WOE        = double()
                                ,stringsAsFactors = FALSE);

  #create an empty table for summary edas;
  Info.Values<- data.frame(  Variable = character()
                            ,IV       = double()
                            ,stringsAsFactors = FALSE);

  #begin looping through
  for(i in var.list){
    if(tracking==T){message("Variable: ",i)}

    monotonic.f<-1;
    minpct.f   <-1

    #if all missing, then go to next variable
    if(sum(is.na(df[,i])) == nrow(df)){
      if(tracking==T){message("Skipping variable ",i," because the number all inputs are missing based on is.na() ")}
      next
      }

    list.main.vars<-c(i,dv,dv.denominator)
    tmpDF         <-df[,list.main.vars];
    tmpDF$dv      <-tmpDF[,dv]
    tmpDF$curr_var<-as.numeric(tmpDF[,i])

    #if denominator is null, then make it 1
    if(is.null(dv.denominator)){
      tmpDF$dv.denominator<-1
    }else{
      tmpDF$dv.denominator<- tmpDF[,dv.denominator]
    }

    #bin using quantiles
    if(binning.Type=="Quantiles"){
      tmpDF$bin_id<- cut( x=tmpDF[,i]
                         ,breaks=c(-Inf, unique(quantile( tmpDF[,i]
                                                         ,probs=seq(0,1, by=1/nbins)
                                                         ,include.lowest=TRUE
                                                         ,na.rm=TRUE))));
    }

    #bin using deciles;
    if(binning.Type=="Bucketing"){
      tmpDF$bin_id<- as.numeric(cut( tmpDF[,i]
                                    ,breaks=nbins
                                    ,na.rm=T))
    }

    #if NA
    tmpDF$bin_i[is.na(tmpDF$bin_i)] <- "NA";

    #roll up
    roll.up.orig<- tmpDF %>%
                    dplyr::group_by(bin_id) %>%
                    dplyr::summarise(  Records    = n()
                                      ,Exposure   = sum(dv.denominator)
                                      ,Mean       = mean(curr_var)
                                      ,UpperBound = max(curr_var)
                                      ,Events     = sum(dv)) %>%
                    data.frame();

    #order by mean var
    roll.up.orig<- roll.up.orig[with(roll.up.orig,order(Mean)),]

    #create new bin_id
    roll.up.orig$bin_id<-1:nrow(roll.up.orig);

    #get event rate or freq (depending on dv.type)
    if(dv.type=="Binary"){
      roll.up.orig$EventRate<- roll.up.orig$Events/roll.up.orig$Records * 100;
      roll.up.orig$EventRate<- ifelse(is.na(roll.up.orig$EventRate),0,roll.up.orig$EventRate);
    }else if(dv.type=="Frequency"){
      roll.up.orig$EventRate<- roll.up.orig$Events/roll.up.orig$Exposure*100;
      roll.up.orig$EventRate<- ifelse(is.na(roll.up.orig$EventRate),0,roll.up.orig$EventRate);
    }

    #Variable
    roll.up.orig$Variable<- i;
    roll.up.orig         <- roll.up.orig[,c("Variable","bin_id","UpperBound","Records","Exposure","Events","EventRate")];

    #weight of evidence;
    total.bads <- sum(roll.up.orig$Events)
    total.goods<- sum(roll.up.orig$Records) - total.bads;

    #create WOE
    if(dv.type=="Binary"){
      roll.up.orig<- within(roll.up.orig,{
        WOE<- ifelse(Events==0, round(log((((Records - Events) / total.goods) / 0.01)),4),
                     ifelse(Events==Records,round(log((1 / total.goods) / (Events/total.bads)),4)   ,round(log(((Records - Events) / total.goods) / (Events/total.bads)),4)))
      })
    } else if(dv.type=="Frequency"){
      roll.up.orig<- within(roll.up.orig,{
        WOE<- ifelse(Events==0,round(log((Exposure/sum(Exposure)) / (1/sum(Events))),4)
                     ,round(log((Exposure/sum(Exposure)) / (Events/sum(Events))),4));
      })
    } else {print("WRONG dv.type INPUT")}

    #get correlations;
    if(dv.type=="Binary"){
      corr.with.var<- cor(tmpDF$curr_var, tmpDF[,dv], use="complete.obs")
    }
    if(dv.type=="Frequency"){
      tmpDF$new.dv <- tmpDF[,dv]/tmpDF$dv.denominator
      corr.with.var<- cor(tmpDF$curr_var, tmpDF$new.dv, use="complete.obs")
      tmpDF$new.dv <-NULL
    }

    #if NaN, then skip
    if(is.nan(corr.with.var)){next}
    sgn<- sign(corr.with.var);

    ### add monotonic logic here
    #remove rows where Missing;
    roll.up.adj.nomiss<- roll.up.orig[!is.na(roll.up.orig$UpperBound),];

    #keep rows where Missing;
    roll.up.adj.miss<- roll.up.orig[is.na(roll.up.orig$UpperBound),];

    #max rows
    max.orig.rows<- nrow(roll.up.adj.nomiss);

    if(tracking==TRUE){
      if(nrow(roll.up.adj.miss)>0){
        write_out_log_file(f="Missing bin"   ,fout=paste(path_2_save,"/",run_id,"-numeric_log_file.txt",sep=""),append=TRUE)
        write_out_log_file(f=roll.up.adj.miss,fout=paste(path_2_save,"/",run_id,"-numeric_log_file.txt",sep=""),append=TRUE)
      }

      #log origina bins
      write_out_log_file(f="Original Binning",fout=paste(path_2_save,"/",run_id,"-numeric_log_file.txt",sep=""),append=TRUE)
      write_out_log_file(f=roll.up.adj.nomiss,fout=paste(path_2_save,"/",run_id,"-numeric_log_file.txt",sep=""),append=TRUE)
    }


    #### start pct checking ####

    ################################
    ### Percent of Records Check ###
    ################################

    #Check the percent of records in each bin;
    numbRows<- nrow(roll.up.adj.nomiss);

    if(tracking==TRUE){
      write_out_log_file(f=paste("checking percent of records"),fout=paste(path_2_save,"/",run_id,"-numeric_log_file.txt",sep=""),append=TRUE)
    }

    a<-1;
    while(a<numbRows){

      #set j as the next bin;
      #b<- ifelse(a+1 != numbRows, a+1, numbRows);

      #set b as the next bin;
      b<- ifelse(a+1 != nrow(roll.up.adj.nomiss), a+1, nrow(roll.up.adj.nomiss));
      c<- ifelse(a ==1, 0,ifelse(a+1==nrow(roll.up.adj.nomiss),0,a-1))

      #get values for pct records;
      roll.up.adj.nomiss$PctRecords<- roll.up.adj.nomiss$Records/NbrRecords;

      #get values for bad rates on both bins;
      br_a_e<- roll.up.adj.nomiss[roll.up.adj.nomiss$bin_id==a,"EventRate"];
      br_b_e<- roll.up.adj.nomiss[roll.up.adj.nomiss$bin_id==b,"EventRate"];
      br_c_e<- roll.up.adj.nomiss[roll.up.adj.nomiss$bin_id==c,"EventRate"];

      br_a<- roll.up.adj.nomiss[roll.up.adj.nomiss$bin_id==a,"PctRecords"];
      br_b<- roll.up.adj.nomiss[roll.up.adj.nomiss$bin_id==b,"PctRecords"];
      br_c<- roll.up.adj.nomiss[roll.up.adj.nomiss$bin_id==c,"PctRecords"];

      #get intervals;
      binprev_e <- roll.up.adj.nomiss[roll.up.adj.nomiss$bin_id==c,"UpperBound"];
      binstart_e<- roll.up.adj.nomiss[roll.up.adj.nomiss$bin_id==a,"UpperBound"];
      binend_e<-   roll.up.adj.nomiss[roll.up.adj.nomiss$bin_id==b,"UpperBound"];

      binprev <- roll.up.adj.nomiss[roll.up.adj.nomiss$bin_id==c,"PctRecords"];
      binstart<- roll.up.adj.nomiss[roll.up.adj.nomiss$bin_id==a,"PctRecords"];
      binend<-   roll.up.adj.nomiss[roll.up.adj.nomiss$bin_id==b,"PctRecords"];

      #print('head of roll.up.adj.miss');print(head(roll.up.adj.miss))
      if(is.na(binstart) | is.nan(binstart) | is.null(binstart) | binstart=="<NA>"|is.na(binstart) | is.nan(binend) | is.null(binend) | binend=="<NA>")
      {
        a<- a+1;
        #message("binstart is NA or NaN");
      } else
        #if((br_a>=min.Pct) & (br_b>=min.Pct)){
        if(br_a>=min.Pct){
          a<- a+1;
        } else{
          if(tracking==T & minpct.f==1){print("Looping through because minimum percent threshold is not met...")}

          #create table with only the records needed and all columns;
          #roll.up.adj.nomiss_new<- roll.up.adj.nomiss[roll.up.adj.nomiss$bin_id==a | roll.up.adj.nomiss$bin_id==b,];
          roll.up.adj.nomiss_new<- roll.up.adj.nomiss[roll.up.adj.nomiss$bin_id==a |
                                                        roll.up.adj.nomiss$bin_id==b |
                                                        roll.up.adj.nomiss$bin_id==c,];

          rownames(roll.up.adj.nomiss_new)<-NULL;

          #get differences EventRate
          curr_event_rate = roll.up.adj.nomiss_new[which(roll.up.adj.nomiss_new$bin_id==a),"EventRate"]

          event_rate_checks      = roll.up.adj.nomiss_new[roll.up.adj.nomiss_new$bin_id %in% c(b,c),c("bin_id","EventRate")]
          event_rate_checks$diff = abs(event_rate_checks$EventRate - curr_event_rate)
          event_rate_checks      = event_rate_checks[order(event_rate_checks$diff),]
          bin_id_to_merge_with   = event_rate_checks[1,"bin_id"]

          #create new bin id and set it both the same;
          #roll.up.adj.nomiss_new$bin_id<- a;
          roll.up.adj.nomiss_new        = roll.up.adj.nomiss_new[which(roll.up.adj.nomiss_new$bin_id %in% c(a,bin_id_to_merge_with)),]
          roll.up.adj.nomiss_new$bin_id = bin_id_to_merge_with  #this is new

          if(tracking==TRUE){
            write_out_log_file(f=paste("bin_id ",a, "- merging with bin ",bin_id_to_merge_with, sep=""),fout=paste(path_2_save,"/",run_id,"-numeric_log_file.txt",sep=""),append=TRUE)
            write_out_log_file(f=roll.up.adj.nomiss_new                  ,fout=paste(path_2_save,"/",run_id,"-numeric_log_file.txt",sep=""),append=TRUE)
          }

          roll.up.adj.nomiss_new2<- roll.up.adj.nomiss_new %>%
            dplyr::group_by(Variable,bin_id) %>%
            dplyr::summarise(UpperBound=max(UpperBound)
                             ,Records  =sum(Records)
                             ,Exposure =sum(Exposure)
                             ,Events   =sum(Events))%>%
            data.frame();

          rownames(roll.up.adj.nomiss_new2)<-NULL;

          #create metrics;
          if(dv.type=="Binary")   {roll.up.adj.nomiss_new2$EventRate<- ifelse(is.na(roll.up.adj.nomiss_new2$Events/roll.up.adj.nomiss_new2$Records) ,0,round(roll.up.adj.nomiss_new2$Events/roll.up.adj.nomiss_new2$Records*100,4))};
          if(dv.type=="Frequency"){roll.up.adj.nomiss_new2$EventRate<- ifelse(is.na(roll.up.adj.nomiss_new2$Events/roll.up.adj.nomiss_new2$Exposure),0,round(roll.up.adj.nomiss_new2$Events/roll.up.adj.nomiss_new2$Exposure*100,4))};

          #remove pct records;
          roll.up.adj.nomiss$PctRecords<- NULL;

          #remove rows a and b;
          roll.up.adj.nomiss<- roll.up.adj.nomiss[roll.up.adj.nomiss$bin_id !=a,];
          roll.up.adj.nomiss<- roll.up.adj.nomiss[roll.up.adj.nomiss$bin_id !=bin_id_to_merge_with,];

          #add in new rows;
          roll.up.adj.nomiss<- bind_rows(roll.up.adj.nomiss_new2,roll.up.adj.nomiss);
          roll.up.adj.nomiss$bin_id<- NULL;

          #order by upper bound variable;
          roll.up.adj.nomiss<- roll.up.adj.nomiss[order(roll.up.adj.nomiss$UpperBound),];

          #reassign bin_id;
          roll.up.adj.nomiss$bin_id<-1:nrow(roll.up.adj.nomiss);

          #reorder
          roll.up.adj.nomiss<- roll.up.adj.nomiss[,c("Variable","bin_id","UpperBound","Records","Exposure","Events","EventRate")];

          a<- 1;
          numbRows<-max(roll.up.adj.nomiss$bin_id);
          minpct.f<-minpct.f+1
        }

      #remove pct records;
      roll.up.adj.nomiss$PctRecords<- NULL;

    }#END While Loop for checking percent of records;

    #check last row;
    if(nrow(roll.up.adj.nomiss)>1){
      roll.up.adj.nomiss$PctRecords = roll.up.adj.nomiss$Records/sum(roll.up.adj.nomiss$Records)
      #ID last two rows
      roll.up.adj.nomiss$last_2_rows = ifelse(roll.up.adj.nomiss$bin_id == max(roll.up.adj.nomiss$bin_id) |
                                              roll.up.adj.nomiss$bin_id == (max(roll.up.adj.nomiss$bin_id)-1),1,0)

      last_row_pct     = roll.up.adj.nomiss[which(roll.up.adj.nomiss$bin_id==max(roll.up.adj.nomiss$bin_id)),"PctRecords"]
      second_2last_pct = roll.up.adj.nomiss[which(roll.up.adj.nomiss$bin_id==(max(roll.up.adj.nomiss$bin_id)-1)),"PctRecords"]
      second_2last_id  = roll.up.adj.nomiss[which(roll.up.adj.nomiss$bin_id==(max(roll.up.adj.nomiss$bin_id)-1)),"bin_id"]

      if(last_row_pct<min.Pct){

        roll.up.adj.nomiss$bin_id = ifelse(roll.up.adj.nomiss$last_2_rows==1,second_2last_id,roll.up.adj.nomiss$bin_id)
        roll.up.adj.nomiss<- roll.up.adj.nomiss %>%
          dplyr::group_by(Variable,bin_id,last_2_rows) %>%
          dplyr::summarise( UpperBound=max(UpperBound)
                            ,Records  =sum(Records)
                            ,Exposure =sum(Exposure)
                            ,Events   =sum(Events))%>%
          data.frame();

        roll.up.adj.nomiss$last_2_rows=NULL

        #create metrics;
        if(dv.type=="Binary")   {roll.up.adj.nomiss$EventRate<- ifelse(is.na(roll.up.adj.nomiss$Events/roll.up.adj.nomiss$Records) ,0,round(roll.up.adj.nomiss$Events/roll.up.adj.nomiss$Records*100,4))};
        if(dv.type=="Frequency"){roll.up.adj.nomiss$EventRate<- ifelse(is.na(roll.up.adj.nomiss$Events/roll.up.adj.nomiss$Exposure),0,round(roll.up.adj.nomiss$Events/roll.up.adj.nomiss$Exposure*100,4))};

      }
    }

    #reorder
    roll.up.adj.nomiss<- roll.up.adj.nomiss[,c("Variable","bin_id","UpperBound","Records","Exposure","Events","EventRate")];




    #### end pct checking ####


    #max rows
    max.orig.rows<- nrow(roll.up.adj.nomiss);

    a<-1;

    if(tracking==TRUE & isTRUE(monotonic)){
      write_out_log_file(f=paste("checking monotonic binning...\n"),fout=paste(path_2_save,"/",run_id,"-numeric_log_file.txt",sep=""),append=TRUE)
    }

    while(a<max.orig.rows  & isTRUE(monotonic)){

      rownames(roll.up.adj.nomiss)<-NULL;

      #set b as the next bin;
      b<- ifelse(a+1 != nrow(roll.up.adj.nomiss), a+1, nrow(roll.up.adj.nomiss));
      c<- ifelse(a ==1, 0,ifelse(a+1==nrow(roll.up.adj.nomiss),0,a-1))
      #c<- ifelse(a ==1, 0,a-1)

      roll.up.adj.nomiss<- roll.up.adj.nomiss[,c("Variable","bin_id","UpperBound","Records","Exposure","Events","EventRate")];

      #get values for bad rates on both bins;
      br_a<- roll.up.adj.nomiss[roll.up.adj.nomiss$bin_id==a,"EventRate"];
      br_b<- roll.up.adj.nomiss[roll.up.adj.nomiss$bin_id==b,"EventRate"];
      br_c<- roll.up.adj.nomiss[roll.up.adj.nomiss$bin_id==c,"EventRate"];


      #get intervals;
      binprev <- roll.up.adj.nomiss[roll.up.adj.nomiss$bin_id==c,"UpperBound"];
      binstart<- roll.up.adj.nomiss[roll.up.adj.nomiss$bin_id==a,"UpperBound"];
      binend<-   roll.up.adj.nomiss[roll.up.adj.nomiss$bin_id==b,"UpperBound"];

      #if(tracking==TRUE){message("bin_id is : ",a," and rows are:")};
      #print(roll.up.adj.nomiss[which(roll.up.adj.nomiss$bin_id %in% c(a,b,c)),])

      if(is.na(binstart) | is.nan(binstart) | is.null(binstart) | binstart=="<NA>")
      {
        a<- a+1;
        #write_out_log_file(f=paste("completed bin_id ",a,sep=""),fout=paste(path_2_save,"/",run_id,"-numeric_log_file.txt",sep=""),append=TRUE)
        #write_out_log_file(f=roll.up.adj.nomiss       ,fout=paste(path_2_save,"/",run_id,"-numeric_log_file.txt",sep=""),append=TRUE)

      } else
        if((sgn==-1 & br_a > br_b) | (sgn==1 & br_a < br_b))
        {
          a<- a+1;
          #write_out_log_file(f=paste("bin_id ",a,sep=""),fout=paste(path_2_save,"/",run_id,"-numeric_log_file.txt",sep=""),append=TRUE)
          #write_out_log_file(f=roll.up.adj.nomiss       ,fout=paste(path_2_save,"/",run_id,"-numeric_log_file.txt",sep=""),append=TRUE)
        } else
        {
          if(tracking==TRUE && monotonic.f==1){print("Looping through because DV is not monotonic...")}

          #create table with only the records needed and all columns;
          #roll.up.adj.nomiss_new<- roll.up.adj.nomiss[roll.up.adj.nomiss$bin_id==a |
          #                                              roll.up.adj.nomiss$bin_id==b,];
          roll.up.adj.nomiss_new<- roll.up.adj.nomiss[roll.up.adj.nomiss$bin_id==a |
                                                      roll.up.adj.nomiss$bin_id==b |
                                                      roll.up.adj.nomiss$bin_id==c,];

          rownames(roll.up.adj.nomiss_new)<-NULL;

          #get differences EventRate
          curr_event_rate = roll.up.adj.nomiss_new[which(roll.up.adj.nomiss_new$bin_id==a),"EventRate"]

          event_rate_checks = roll.up.adj.nomiss_new[which(roll.up.adj.nomiss_new$bin_id %in% c(b,c)),c("bin_id","EventRate")]
          event_rate_checks$diff = abs(event_rate_checks$EventRate - curr_event_rate)
          event_rate_checks = event_rate_checks[order(event_rate_checks$diff),]
          bin_id_to_merge_with = event_rate_checks[1,"bin_id"]
          #message("before checking logic, bin to merge is: ", bin_id_to_merge_with)

          #override rules
          if((sgn==1 & a !=1  & roll.up.adj.nomiss_new[which(roll.up.adj.nomiss_new$bin_id %in% c(b)),"EventRate"]==0 & roll.up.adj.nomiss_new[which(roll.up.adj.nomiss_new$bin_id %in% c(b)),"Records"]/NbrRecords < min.Pct)){
            bin_id_to_merge_with = roll.up.adj.nomiss_new[which(roll.up.adj.nomiss_new$bin_id %in% c(b)),"bin_id"]
          }


          if(nrow(roll.up.adj.nomiss_new[which(roll.up.adj.nomiss_new$bin_id %in% c(c)),])>0){
            if((sgn==-1 & a !=1 & roll.up.adj.nomiss_new[which(roll.up.adj.nomiss_new$bin_id %in% c(c)),"EventRate"]==0 & roll.up.adj.nomiss_new[which(roll.up.adj.nomiss_new$bin_id %in% c(c)),"Records"]/NbrRecords < min.Pct)){
              bin_id_to_merge_with = roll.up.adj.nomiss_new[which(roll.up.adj.nomiss_new$bin_id %in% c(c)),"bin_id"]
            }
          }

          #create new bin id and set it both the same;
          #roll.up.adj.nomiss_new$bin_id<- a; #This was actual
          roll.up.adj.nomiss_new = roll.up.adj.nomiss_new[which(roll.up.adj.nomiss_new$bin_id %in% c(a,bin_id_to_merge_with)),]

          if(tracking==TRUE){
            write_out_log_file(f=paste("bin_id ",a, "- merging with bin ",bin_id_to_merge_with, sep=""),fout=paste(path_2_save,"/",run_id,"-numeric_log_file.txt",sep=""),append=TRUE)
            write_out_log_file(f=roll.up.adj.nomiss_new                  ,fout=paste(path_2_save,"/",run_id,"-numeric_log_file.txt",sep=""),append=TRUE)
          }

          roll.up.adj.nomiss_new$bin_id<- bin_id_to_merge_with  #this is new

          roll.up.adj.nomiss_new2<-roll.up.adj.nomiss_new %>%
            dplyr::group_by(Variable,bin_id) %>%
            dplyr::summarise(UpperBound=max(UpperBound)
                             ,Records  =sum(Records)
                             ,Exposure =sum(Exposure)
                             ,Events   =sum(Events))%>%
            data.frame();
          roll.up.adj.nomiss_new2<- roll.up.adj.nomiss_new2[order(roll.up.adj.nomiss_new2$UpperBound),]
          rownames(roll.up.adj.nomiss_new2)<-NULL;

          #create metrics;
          if(dv.type=="Binary")   {roll.up.adj.nomiss_new2$EventRate<- ifelse(is.na(roll.up.adj.nomiss_new2$Events/roll.up.adj.nomiss_new2$Records) ,0,roll.up.adj.nomiss_new2$Events/roll.up.adj.nomiss_new2$Records*100)};
          if(dv.type=="Frequency"){roll.up.adj.nomiss_new2$EventRate<- ifelse(is.na(roll.up.adj.nomiss_new2$Events/roll.up.adj.nomiss_new2$Exposure),0,roll.up.adj.nomiss_new2$Events/roll.up.adj.nomiss_new2$Exposure*100)};

          #remove rows a and b;
          roll.up.adj.nomiss<- roll.up.adj.nomiss[roll.up.adj.nomiss$bin_id !=a,];
          roll.up.adj.nomiss<- roll.up.adj.nomiss[roll.up.adj.nomiss$bin_id !=bin_id_to_merge_with,];

          #add in new rows;
          roll.up.adj.nomiss<- rbind(roll.up.adj.nomiss_new2,roll.up.adj.nomiss);
          roll.up.adj.nomiss$bin_id<- NULL;

          #order by upper bound variable;
          roll.up.adj.nomiss<- roll.up.adj.nomiss[order(roll.up.adj.nomiss$UpperBound),];

          #reassign bin_id;
          roll.up.adj.nomiss$bin_id<-1:nrow(roll.up.adj.nomiss);

          #reorder
          roll.up.adj.nomiss<- roll.up.adj.nomiss[,c("Variable","bin_id","UpperBound","Records","Exposure","Events","EventRate")];

          if(tracking==TRUE){
            write_out_log_file(f=paste("merge complete - circling back to bin_id 1", sep=""),fout=paste(path_2_save,"/",run_id,"-numeric_log_file.txt",sep=""),append=TRUE)
            #write_out_log_file(f=roll.up.adj.nomiss       ,fout=paste(path_2_save,"/",run_id,"-numeric_log_file.txt",sep=""),append=TRUE)
          }
          a<- 1;
          max.orig.rows<-max(roll.up.adj.nomiss$bin_id);
          monotonic.f<-monotonic.f+1;
          bin_id_to_merge_with = NULL
        };#End if else;


    }# End while loop

    roll.up.adj.nomiss<-roll.up.adj.nomiss %>%
      dplyr::group_by(Variable,bin_id) %>%
      dplyr::summarise(UpperBound=max(UpperBound)
                       ,Records  =sum(Records)
                       ,Exposure =sum(Exposure)
                       ,Events   =sum(Events))%>%
      data.frame();
    roll.up.adj.nomiss<- roll.up.adj.nomiss[order(roll.up.adj.nomiss$UpperBound),]
    rownames(roll.up.adj.nomiss)<-NULL;

    #create metrics;
    if(dv.type=="Binary")   {roll.up.adj.nomiss$EventRate<- ifelse(is.na(roll.up.adj.nomiss$Events/roll.up.adj.nomiss$Records) ,0,round(roll.up.adj.nomiss$Events/roll.up.adj.nomiss$Records*100,4))};
    if(dv.type=="Frequency"){roll.up.adj.nomiss$EventRate<- ifelse(is.na(roll.up.adj.nomiss$Events/roll.up.adj.nomiss$Exposure),0,round(roll.up.adj.nomiss$Events/roll.up.adj.nomiss$Exposure*100,4))};

    #order by binid;
    roll.up.adj.nomiss<- roll.up.adj.nomiss[order(roll.up.adj.nomiss$bin_id),];

    roll.up.adj.nomiss$bin_id<-1:nrow(roll.up.adj.nomiss);

    #reorder
    roll.up.adj.nomiss<- roll.up.adj.nomiss[,c("Variable","bin_id","UpperBound","Records","Exposure","Events","EventRate")];



    #########################


    ##### ADD BACK HERE #####


    #########################


    #add back missing (if any)
    if(length(roll.up.adj.miss)>0){
      roll.up.adj.nomiss<-bind_rows(roll.up.adj.nomiss,roll.up.adj.miss)
    }
    roll.up.adj.nomiss$bin_id<-1:nrow(roll.up.adj.nomiss);

    #weight of evidence;
    roll.up.orig<-roll.up.adj.nomiss
    total.bads <- sum(roll.up.orig$Events)
    total.goods<- sum(roll.up.orig$Records) - total.bads;

    if(dv.type=="Binary"){
      roll.up.orig<- within(roll.up.orig,{
        WOE<- ifelse(Events==0, round(log((((Records - Events) / total.goods) / 0.01)),4),
                     ifelse(Events==Records,round(log((1 / total.goods) / (Events/total.bads)),4)   ,round(log(((Records - Events) / total.goods) / (Events/total.bads)),4)))
      })
    } else if(dv.type=="Frequency"){
      roll.up.orig<- within(roll.up.orig,{
        WOE<- ifelse(Events==0,round(log((Exposure/sum(Exposure)) / (1/sum(Events))),4)
                     ,round(log((Exposure/sum(Exposure)) / (Events/sum(Events))),4));
      })
    } else {print("WRONG dv.type INPUT")}

    roll.up.orig$WOE<-round(roll.up.orig$WOE,4)

    #calculate information values
    iv.temp<- roll.up.orig
    if(dv.type=="Binary"){
      iv.temp<- within(iv.temp,{
        temp<- WOE *  (((Records - Events) / total.goods)  -  (Events/total.bads))
      })
    }else if(dv.type=="Frequency"){
      iv.temp<- within(iv.temp,{
        temp<- WOE *  ((Exposure/sum(Exposure)) - (Events/sum(Events)))
      })
    }

    iv.temp2   <- data.frame(Variable=i,IV=0);
    iv.temp2$IV<- sum(iv.temp$temp);
    iv.temp2$IV<- round(iv.temp2$IV,5);

    #roll.up.orig.final
    NumericEDA.fine<-dplyr::bind_rows(NumericEDA.fine,roll.up.orig);
    NumericEDA.fine<-NumericEDA.fine[,c("Variable","bin_id","UpperBound","Records","Exposure","Events","EventRate","WOE")]

    #info values
    Info.Values<-dplyr::bind_rows(Info.Values,iv.temp2);

  } #end for(i in var.list)

  #get percent oc records
  NumericEDA.fine$PctRecords = NumericEDA.fine$Records/NbrRecords
  NumericEDA.fine = NumericEDA.fine[,c("Variable","bin_id","UpperBound","PctRecords","Records","Exposure","Events","EventRate","WOE")]

  #create logic to use
  NumericEDA.fine$bin_id<- ifelse(is.na(NumericEDA.fine$UpperBound),-1,NumericEDA.fine$bin_id)
  NumericEDA.fine <- NumericEDA.fine[order(NumericEDA.fine$Variable, NumericEDA.fine$bin_id),]

  for(i in unique(NumericEDA.fine$Variable)){

    tmp_num_eda_fine = NumericEDA.fine[which(NumericEDA.fine$Variable==i),]
    max_bin_id = max(tmp_num_eda_fine$bin_id)

    #get previus upper bound
    tmp_num_eda_fine = tmp_num_eda_fine %>% mutate(prev_upper_bound = lag(UpperBound),prev_bin_id=lag(bin_id)) %>% data.frame()

    #create logic
    tmp_num_eda_fine<- within(tmp_num_eda_fine,{
      woe_logic_2_use <- ifelse(bin_id==-1,paste("if is.na(",i, ") then ",WOE,sep=""),
                         ifelse(bin_id==max(tmp_num_eda_fine$bin_id),paste("if ", i, " > ", prev_upper_bound, " then ",WOE,sep=""),paste("if ", i, " <= ", UpperBound, " then ",WOE,sep="")))

      grp_logic_2_use <- ifelse(bin_id==-1,paste("if is.na(",i, ") then ",bin_id,sep=""),
                                ifelse(bin_id==max(tmp_num_eda_fine$bin_id),paste("if ", i, " > ", prev_upper_bound, " then ",bin_id,sep=""),paste("if ", i, " <= ", UpperBound, " then ",bin_id,sep="")))

      })

    if(max_bin_id == 1){
      tmp_woe = tmp_num_eda_fine[which(tmp_num_eda_fine$bin_id==1),"WOE"]
      tmp_num_eda_fine[which(tmp_num_eda_fine$bin_id==1),]$woe_logic_2_use<-paste("if !is.na(",i,") then ",tmp_woe,sep="")
      tmp_num_eda_fine[which(tmp_num_eda_fine$bin_id==1),]$grp_logic_2_use<-paste("if !is.na(",i,") then ",1,sep="")
      tmp_woe = NA
    }

    tmp_num_eda_fine$prev_upper_bound = NULL
    tmp_num_eda_fine$prev_bin_id      = NULL
    NumericEDA.fine = NumericEDA.fine[which(NumericEDA.fine$Variable != i),]

    #merge it back
    NumericEDA.fine = bind_rows(NumericEDA.fine,tmp_num_eda_fine)

  }

  #reorder
  NumericEDA.fine = NumericEDA.fine[order(NumericEDA.fine$Variable,NumericEDA.fine$bin_id),]

  write_out_log_file(f=paste("final grouping"),fout=paste(path_2_save,"/",run_id,"-numeric_log_file.txt",sep=""),append=TRUE)
  write_out_log_file(f=NumericEDA.fine        ,fout=paste(path_2_save,"/",run_id,"-numeric_log_file.txt",sep=""),append=TRUE)

  Logics.2.Use = NumericEDA.fine[,c("Variable","grp_logic_2_use","woe_logic_2_use")]
  NumericEDA.fine$grp_logic_2_use = NULL
  NumericEDA.fine$woe_logic_2_use = NULL

  return(list(Numeric_eda=NumericEDA.fine,numeric_iv=Info.Values,numeric_logics=Logics.2.Use))
}
