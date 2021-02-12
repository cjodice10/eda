#' @title get_categorical_bins
#'
#' @description Categorical grouping
#'
#' @param run_id An identifier that will be used when naming output tables to the specified path (path_2_save parameter).  Example: 'MyRun1'
#' @param df A dataframe you are wanting to analyze
#' @param dv The name of the dependent variable (dv).  Example: 'target'
#' @param dv.type Can take on 1 of two inpunts - c('Binary','Frequency').  Both should be numeric.  If 'Frequency' is the input, it should be the numerator (if it is a rate).  The denominator will be specified as a separate parameter
#' @param dv.denominator The denominator of your dependent variable.  In many cases, this can be considered the exposure
#' @param var.list A list of non-numeric variables to analyze and create bins for
#' @param max.levels If a variable initially has more unique levels than max.levels, it will be skipped
#' @param min.Pct This is the minimun percent of records a final bin should have.  The input should be between (0,1).  Generally applies to only bins that are not NA.  Default is 0.02 (or 2 percent)
#' @param tracking Logical TRUE/FALSE inputs.  If set to TRUE, the user will be able to see what variable the function is analyzing.  Default is TRUE
#' @param path_2_save A path to a folder to save a log file
#'
#' @return A list of dataframes.  First in the list will be 'CategoricalEDA' - this is an aggregated dataframe showing the groups created along with other key information.  The second is 'categorical_iv' - This is a dataframe with each variable processed and their information value.  The last is 'categorical_logics' - This is a dataframe with the information needed to apply to your dataframe and transform your variables.  This table will be the input to apply_categorical_logic(logic_df=categorical_logics)
#' @export

get_categorical_bins<-function(  run_id
                                ,df
                                ,dv
                                ,dv.type                      # Binary, Frequency
                                ,dv.denominator = NULL        # Only used for exposure of frequency
                                ,var.list
                                ,max.levels     = 200         # if variable initially has more than these levels, skip it
                                ,min.Pct        = 0.02
                                ,tracking       = TRUE        # Do you want to track progress or not
                                ,path_2_save    = getwd()
                                ){
  #surpress warnings
  options(warn=-1)#use options(warn=0); to bring back warning
  options(scipen=999);
  `%ni%` = Negate(`%in%`);

  #some basic checks
  if(!is.numeric(df[,dv])){
    stop(message("Dependent Variable must be numeric with values of 1 and 0.  A value of 1 signifies the 'event' you are trying to predict"))
  }

  if(nlevels(factor(df[,dv]))==1 | nlevels(factor(df[,dv]))>2){
    stop(message("Dependent Variable should only have two values, 1 or 0"))
  }

  if(length(var.list)==0){
    message("Categorical variable list is empty.")
    return(NULL)
  }

  if(dv.type %ni% c("Binary","Frequency")){
    stop("dv.type can only take on values c('Binary','Frequency')")
  }

  if(!is.null(dv.denominator) && dv.denominator %ni% colnames(df)){
    stop("dv.denominator is not listed in your dataframe.  If your DV is purely a count variable (no exposure), then leave this NULL")
  }

  if(min.Pct<=0 | min.Pct >=1){
    stop("min.Pct must be between 0 and 1:  (0,1)")
  }

  if(tracking==TRUE){
    write.table( data.frame(Logging = "Initial line in log file"),
                 file=paste(path_2_save,"/",run_id,"-categorical_log_file.txt",sep=""),
                 append = F,
                 sep='\t',
                 row.names=F,
                 col.names=T )
  }

  #remove dv and denom from varlist
  var.list = var.list[var.list %ni% c(dv, dv.denominator)]

  NbrRecords<-nrow(df)

  #create an empty table for summary edas;
  CategoricalEDA<-      data.frame(  Variable  = character()
                                    ,bin_id    = character()
                                    ,Values    = character()
                                    ,Exposure   = double()
                                    ,Records    = double()
                                    ,Events     = double()
                                    ,EventRate  = double()
                                    ,WOE        = double()
                                    ,stringsAsFactors = FALSE);

  #create an empty table for summary edas;
  Info.Values<- data.frame(  Variable = character()
                             ,IV      = double()
                             ,stringsAsFactors = FALSE);

  #begin looping through
  for(i in var.list){
    if(tracking==T){
      message("Variable: ",i)
      write_out_log_file(f=paste("Variable:",i),fout=paste(path_2_save,"/",run_id,"-categorical_log_file.txt",sep=""),append=TRUE)
      }

    list.main.vars<-c(i,dv,dv.denominator)
    tmpDF         <-df[,list.main.vars];
    tmpDF$dv      <-tmpDF[,dv]
    tmpDF$curr_var<-tmpDF[,i]
    tmpDF$bin_i   <-factor(tmpDF[,i]);

    lvls<-nlevels(tmpDF[,"bin_i"]);

   if(tracking==TRUE){
     message("Number of initial levels: ", lvls)
     write_out_log_file(f=paste("Number of initial levels: ", lvls),fout=paste(path_2_save,"/",run_id,"-categorical_log_file.txt",sep=""),append=TRUE)
     };

   if(lvls==1){
      if(tracking==T){
        message("Skipping variable ",i," because the number of initial levels is 1")
        write_out_log_file(f=paste("Skipping variable ",i," because the number of initial levels is 1"),fout=paste(path_2_save,"/",run_id,"-categorical_log_file.txt",sep=""),append=TRUE)
        }
      next
      };
    if(lvls>max.levels){
      if(tracking==T){
        message("Skipping variable ",i," because the number of initial levels is > the max.levels parameter")
        write_out_log_file(f=paste("Skipping variable ",i," because the number of initial levels is > the max.levels parameter"),fout=paste(path_2_save,"/",run_id,"-categorical_log_file.txt",sep=""),append=TRUE)
        }
      next
      };

    #if all missing, then go to next variable
    if(sum(is.na(tmpDF[,i])) == nrow(tmpDF)){
      if(tracking==T){
        message("Skipping variable ",i," because the number all inputs are missing based on is.na()")
        write_out_log_file(f=paste("Skipping variable ",i," because the number all inputs are missing based on is.na()"),fout=paste(path_2_save,"/",run_id,"-categorical_log_file.txt",sep=""),append=TRUE)
        }
      next
      }

    #if denominator is null, then make it 1
    if(is.null(dv.denominator)){
      tmpDF$dv.denominator<-1
    }else{
      tmpDF$dv.denominator<- tmpDF[,dv.denominator]
    }


    tmpDF$bin_i = as.character(tmpDF$bin_i)

    #aggregate data;
    nbins_start<- tmpDF %>%
      dplyr::group_by(bin_i) %>%
      dplyr::summarise( Records  = n()
                       ,Exposure = sum(dv.denominator,na.rm=TRUE)
                       ,Events   = sum(dv)) %>%
      data.frame();

    #event rate
    #nbins_start$EventRate<- nbins_start$Events/nbins_start$Records
    nbins_start$EventRate<- nbins_start$Events/nbins_start$Exposure

    #get pct records;
    nbins_start$PctRecords<- nbins_start$Records/NbrRecords;

    #order;
    nbins_start<- nbins_start[order(nbins_start$EventRate),];

    #missing bin
    missing_bin = nbins_start[which(is.na(nbins_start$bin_i)),]

    #remove missing
    nbins_start = nbins_start[which(!is.na(nbins_start$bin_i)),]


    if(tracking==TRUE){
      if(nrow(missing_bin)>0){
        write_out_log_file(f="Missing bin",fout=paste(path_2_save,"/",run_id,"-categorical_log_file.txt",sep=""),append=TRUE)
        write_out_log_file(f=missing_bin  ,fout=paste(path_2_save,"/",run_id,"-categorical_log_file.txt",sep=""),append=TRUE)
      }

      #log origina bins
      write_out_log_file(f="Original Binning",fout=paste(path_2_save,"/",run_id,"-categorical_log_file.txt",sep=""),append=TRUE)
      write_out_log_file(f=nbins_start       ,fout=paste(path_2_save,"/",run_id,"-categorical_log_file.txt",sep=""),append=TRUE)
    }
    nstart<- nrow(nbins_start);

    #create bin ids;
    nbins_start$bin_id<- 1:nstart;
    rownames(nbins_start)<-NULL;

    ########################################################
    #############Must Check Percent of Records##############
    ########################################################
    #reset i and nstart values;
    a<-1;
    nstart<- max(nbins_start$bin_id);

    while(a<nstart)
    {
      rownames(nbins_start)<-NULL;

      #set j as the next bin;
      j<- ifelse(a+1 != nstart, a+1, nstart);
      c<- ifelse(a ==1, 0,ifelse(a+1==nrow(nbins_start),0,a-1))
      #c<- ifelse(a ==1, 0,ifelse(a+1==nrow(nbins_start),0,a-1))

      #get values for bad rates on both bins;
      br_i<- nbins_start[nbins_start$bin_id==a,c("PctRecords")];
      br_j<- nbins_start[nbins_start$bin_id==j,c("PctRecords")];
      br_c<- nbins_start[nbins_start$bin_id==c,c("PctRecords")];

      #get intervals;
      binbefore = nbins_start[nbins_start$bin_id==c,"bin_i"];
      binstart  = nbins_start[nbins_start$bin_id==a,"bin_i"];
      binend    = nbins_start[nbins_start$bin_id==j,"bin_i"];

      if(is.na(binstart) | is.nan(binstart) | is.null(binstart) | binstart=="<NA>" | binstart=="" | binstart==" " | is.na(binend) | is.nan(binend) | is.null(binend) | binend=="<NA>"  | binend=="" | binend==" " )
      {
        a<- a+1;
        #message("binstart is NA or NaN");
      }else
        if(a<nstart & br_i>=min.Pct)
        {
          a<- a+1;
        }else
          if(a<nstart & br_i<min.Pct)
          {
            if(tracking==T){
              #print("Minimum Percent of Records is not met - merging bins...")
              write_out_log_file(f=paste("Looping through because minimum percent threshold is not met..."),fout=paste(path_2_save,"/",run_id,"-categorical_log_file.txt",sep=""),append=TRUE)
            }

            #create table with only the records needed and all columns;
            #nbins_new<- nbins_start[nbins_start$bin_id==a | nbins_start$bin_id==j,];
            nbins_new<- nbins_start[nbins_start$bin_id==a |
                                    nbins_start$bin_id==j |
                                    nbins_start$bin_id==c,];

            #get differences EventRate
            curr_event_rate = nbins_new[which(nbins_new$bin_id==a),"EventRate"]

            event_rate_checks      = nbins_new[nbins_new$bin_id %in% c(j,c),c("bin_id","EventRate")]
            event_rate_checks$diff = abs(event_rate_checks$EventRate - curr_event_rate)
            event_rate_checks      = event_rate_checks[order(event_rate_checks$diff),]
            bin_id_to_merge_with   = event_rate_checks[1,"bin_id"]

            rownames(nbins_new)<-NULL;

            #nbins_new = nbins_new[which(nbins_new$bin_id %in% c(a,bin_id_to_merge_with)),]
            nbins_new = nbins_new[which(nbins_new$bin_id %in% c(j,a,c)),]
            nbins_new = nbins_new[order(nbins_new$bin_id),]

            if(tracking==TRUE){
              #message("merging bin_id: ",a, "   with bin_id: ",bin_id_to_merge_with)
              write_out_log_file(f=paste("merging bin_id: ",a, "   with bin_id: ",bin_id_to_merge_with),fout=paste(path_2_save,"/",run_id,"-categorical_log_file.txt",sep=""),append=TRUE)
              write_out_log_file(f=nbins_new,fout=paste(path_2_save,"/",run_id,"-categorical_log_file.txt",sep=""),append=TRUE)

            }
            nbins_new = nbins_new[which(nbins_new$bin_id %in% c(a,bin_id_to_merge_with)),]
            nbins_new$bin_id = bin_id_to_merge_with  #this is new

            binstart = nbins_new[1,"bin_i"]
            binend   = nbins_new[2,"bin_i"]

            #create new intervals;
            NewValues<- ifelse(nbins_new[1,"bin_i"]=="<NA>","<NA>", paste0(binstart,"*******",binend));

            nbins_new$bin_i<- NewValues;

            nbins_new2<- nbins_new %>%
              dplyr::group_by(bin_i,bin_id) %>%
              dplyr::summarise(  Records = sum(Records,na.rm=T)
                                ,Exposure= sum(Exposure,na.rm=T)
                                ,Events  = sum(Events,na.rm=T)) %>%
              data.frame();

            nbins_new2$EventRate<- nbins_new2$Events/nbins_new2$Exposure;
            nbins_new2<-nbins_new2[order(nbins_new2$EventRate),]

            rownames(nbins_new2)<-NULL;

            #get pct records;
            nbins_new2$PctRecords<- nbins_new2$Records/NbrRecords;

            #reorder columns;
            nbins_new2<- nbins_new2[,c("bin_i","Records","Exposure","PctRecords","Events","EventRate","bin_id")];

            #remove rows i and j;
            nbins_start<- nbins_start[nbins_start$bin_id !=a,];
            #nbins_start<- nbins_start[nbins_start$bin_id !=j,];
            nbins_start<- nbins_start[nbins_start$bin_id !=bin_id_to_merge_with,];

            #add in new rows;
            nbins_start<- bind_rows(nbins_new2,nbins_start);
            nbins_start$bin_id<- NULL;

            #order by eventrate variable;
            nbins_start<- nbins_start[order(nbins_start$EventRate),];

            #reassign bin_id;
            nbins_start$bin_id<-1:nrow(nbins_start);

            #i<- ifelse(i==1,1,i-1);
            a<- 1;
            nstart<- max(nbins_start$bin_id);

            br_i<-NULL;br_j<-NULL;j<-NULL;x<-NULL;y<-NULL;z<-NULL;NewValues<-NULL;bin_id_to_merge_with<-NULL
            j<-NULL;c<-NULL
          } #end loop for pct records
    } #end while loop;

    #if(tracking==TRUE){message('\nChecking last row')}

    #must check last row;
    rownames(nbins_start)<-NULL;

    #set nstart to max bin_id;
    nstart<- max(nbins_start$bin_id);

    #set j as the bin before;
    j<- ifelse(nstart==1,1,nstart-1);

    #get values for PctRecords on both bins;
    br_i<- nbins_start[nbins_start$bin_id==nstart,c("PctRecords")];
    br_j<- nbins_start[nbins_start$bin_id==j     ,c("PctRecords")];

    #get intervals;
    binstart<- nbins_start[nbins_start$bin_id==nstart,"bin_i"];
    binend<-   nbins_start[nbins_start$bin_id==j     ,"bin_i"];

    if(is.na(binstart) | is.nan(binstart) | is.null(binstart) | binstart=="<NA>" | binstart=="" | binstart==" " | is.na(binend) | is.nan(binend) | is.null(binend) | binend=="<NA>"  | binend=="" | binend==" " )
    {
      #message("good");
    } else
      if(br_i>=min.Pct)
      {
        #message("Last bin meets minimum percent of records.")
      }
    else
      {
        #message("INSIDE LOOP due to the last row not having enough records...");
        j<- a-1;

        #create table with only the records needed and all columns;
        nbins_new<- nbins_start[nbins_start$bin_id==nstart | nbins_start$bin_id==j,];

        rownames(nbins_new)<-NULL;

        #create new bin id and set it both the same;
        nbins_new$bin_id<- j;

        #create new intervals;
        NewValues<- ifelse(nbins_new[1,"bin_i"]=="<NA>","<NA>", paste0(binstart,"*******",binend));

        nbins_new$bin_i<- NewValues;

        nbins_new2<- nbins_new %>%
          dplyr::group_by(bin_i,bin_id) %>%
          dplyr::summarise(  Records = sum(Records,na.rm=T)
                            ,Exposure= sum(Exposure,na.rm=T)
                            ,Events  = sum(Events,na.rm=T)) %>%
          data.frame();
        nbins_new2$EventRate<- nbins_new2$Events/nbins_new2$Exposure;
        nbins_new2<-nbins_new2[order(nbins_new2$EventRate),]

        rownames(nbins_new2)<-NULL;

        #get pct records;
        nbins_new2$PctRecords<- nbins_new2$Records/NbrRecords;

        #reorder columns;
        nbins_new2<- nbins_new2[,c("bin_i", "Records", "Exposure", "PctRecords","Events","EventRate","bin_id")];

        #remove rows i and j;
        nbins_start<- nbins_start[nbins_start$bin_id !=nstart,];
        nbins_start<- nbins_start[nbins_start$bin_id !=j,];

        #add in new rows;
        nbins_start<- rbind(nbins_new2,nbins_start);
        nbins_start$bin_id<- NULL;

        #order;
        nbins_start<- nbins_start[order(nbins_start$EventRate),];

        #reassign bin_id;
        nbins_start$bin_id<-1:nrow(nbins_start);

      }

    #make m6 to make it easier so i dont have to recode;
    m6<- nbins_start;

    #reassign bin_id;
    m6$bin_id<-1:nrow(m6);

    #if missing
    if(nrow(missing_bin)>0){
      missing_bin$bin_id = -9999
      m6 = bind_rows(m6,missing_bin)
    }

    #weight of evidence;
    total.bads  = sum(m6$Events)
    total.goods = sum(m6$Records) - total.bads;
    total.gb    = total.bads+total.goods

    #changed woe logic for when events =0
    #create WOE
    if(dv.type=="Binary"){
      m6<- within(m6,{
        WOE<- ifelse(Events==0, round(log((((Records - Events) / total.goods) / (1/(total.gb+1)))),4),
                     ifelse(Events==Records,round(log((1 / total.goods) / (Events/total.bads)),4)   ,round(log(((Records - Events) / total.goods) / (Events/total.bads)),4)))
      })
    } else if(dv.type=="Frequency"){
      m6<- within(m6,{
        WOE<- ifelse(Events==0,round(log((Exposure/sum(Exposure)) / (1/sum(Events))),4)
                     ,round(log((Exposure/sum(Exposure)) / (Events/sum(Events))),4));
      })
    } else {print("WRONG dv.type INPUT")}

    m6$GRP<- m6$bin_id;

    #calculate information values
    iv.temp<- m6
    if(dv.type=="Binary"){
      iv.temp<- within(iv.temp,{
        temp<- WOE *  (((Records - Events) / total.goods)  -  (Events/total.bads))
      })
    }else if(dv.type=="Frequency"){
      iv.temp<- within(iv.temp,{
        temp<- WOE *  ((Exposure/sum(Exposure)) - (Events/sum(Events)))
      })
    }

    iv.temp2    = data.frame(Variable=i,IV=0);
    iv.temp2$IV = sum(iv.temp$temp);
    iv.temp2$IV = round(iv.temp2$IV,5);
    iv.temp2$Variable = as.character(iv.temp2$Variable)

    #remove bin_id;
    m6$bin_id = NULL;

    #now set bin_i to bin_id;
    m6$bin_id =  m6$bin_i;
    m6$bin_i  = NULL;

    #variable name;
    #m6$Variable<- paste0("bin.",i);
    m6$Variable<- i;

    #reorder;
    m6<- m6[,c("Variable","bin_id","Records","Events","EventRate","WOE","GRP")];
    m6$GRP = 1:nrow(m6)

    #Create a data set with this EDA;
    CategoricalEDA<- rbind(CategoricalEDA,m6);
    rownames(CategoricalEDA)<-NULL;

    Info.Values<- rbind(Info.Values,iv.temp2);
    rownames(Info.Values)<-NULL;

    #remove var;
    tmpDF$curr_var<- NULL;

    if(tracking==TRUE){
      message("Completed Binning Variable : ", i);
      message("\n");
      write_out_log_file(f=paste("Completed Binning Variable : ", i),fout=paste(path_2_save,"/",run_id,"-categorical_log_file.txt",sep=""),append=TRUE)
      }
    ######}

    m6<-NULL;

  }#END FOR LOOP FOR var.list

  #write_out_log_file(f=paste("final grouping"),fout=paste(path_2_save,"/",run_id,"-categorical_log_file.txt",sep=""),append=TRUE)
  #write_out_log_file(f=CategoricalEDA         ,fout=paste(path_2_save,"/",run_id,"-categorical_log_file.txt",sep=""),append=TRUE)


  CategoricalEDA.fine<-CategoricalEDA;
  Info.Values        <-Info.Values[,c("Variable","IV")]
  Info.Values        <-Info.Values[order(-Info.Values$IV),]


  #create logic
  #create logic to use
  CategoricalEDA.fine$bin_id = as.character(CategoricalEDA.fine$bin_id)
  CategoricalEDA.fine$GRP= ifelse(is.na(CategoricalEDA.fine$bin_id)|CategoricalEDA.fine$bin_id=="",-9999,CategoricalEDA.fine$GRP)
  CategoricalEDA.fine    = CategoricalEDA.fine[order(CategoricalEDA.fine$Variable, CategoricalEDA.fine$GRP),]

  CategoricalEDA.fine = CategoricalEDA.fine %>%
    dplyr::group_by(Variable) %>%
    dplyr::mutate(GRP = dplyr::row_number()) %>%
    data.frame();


  CategoricalEDA.fine$GRP= ifelse(is.na(CategoricalEDA.fine$bin_id)|CategoricalEDA.fine$bin_id=="",-9999,CategoricalEDA.fine$GRP)
  CategoricalEDA.fine    = CategoricalEDA.fine[order(CategoricalEDA.fine$Variable,CategoricalEDA.fine$GRP),]

  CategoricalEDA.fine$bin_id = as.character(CategoricalEDA.fine$bin_id)


  #get data in the right format
  CategoricalEDA.fine = CategoricalEDA.fine %>%
    dplyr::group_by(Variable) %>%
    #dplyr::mutate(bin_id = strsplit(bin_id,",")) %>%
    dplyr::mutate(bin_id = strsplit(bin_id,"*******",fixed=TRUE)) %>%
    tidyr::unnest(keep_empty=TRUE) %>%
    data.frame()
  CategoricalEDA.fine$bin_id = paste("'",CategoricalEDA.fine$bin_id,"'",sep="")

  CategoricalEDA.fine = CategoricalEDA.fine %>%
    dplyr::group_by(Variable,Records,Events,EventRate,WOE,GRP) %>%
    dplyr::summarise(bin_id = paste(bin_id, collapse = ",")) %>%
    data.frame();
  CategoricalEDA.fine = CategoricalEDA.fine[,c("Variable","bin_id","Records","Events","EventRate","WOE","GRP")]

  CategoricalEDA.fine$GRP= ifelse(CategoricalEDA.fine$bin_id %in% c("NA",""),-9999,CategoricalEDA.fine$GRP)
  CategoricalEDA.fine    = CategoricalEDA.fine[order(CategoricalEDA.fine$Variable,CategoricalEDA.fine$GRP),]

  if(min(CategoricalEDA.fine$GRP)==-9999){
    missing.row        = CategoricalEDA.fine[which(CategoricalEDA.fine$GRP==-9999),]

    CategoricalEDA.tmp = CategoricalEDA.fine[which(CategoricalEDA.fine$GRP!=-9999),]

    CategoricalEDA.tmp    = CategoricalEDA.tmp[order(CategoricalEDA.tmp$Variable,CategoricalEDA.tmp$GRP),]
    CategoricalEDA.tmp = CategoricalEDA.tmp %>%
      dplyr::group_by(Variable) %>%
      dplyr::mutate(GRP = dplyr::row_number()) %>%
      data.frame();

    CategoricalEDA.fine = bind_rows(CategoricalEDA.tmp,missing.row)
    CategoricalEDA.fine = CategoricalEDA.fine[order(CategoricalEDA.fine$Variable,CategoricalEDA.fine$GRP),]
  }



  #CategoricalEDA.fine$GRP= ifelse(is.na(CategoricalEDA.fine$bin_id),-1,CategoricalEDA.fine$GRP)
  CategoricalEDA.fine    = CategoricalEDA.fine[order(CategoricalEDA.fine$Variable,CategoricalEDA.fine$GRP),]
  #CategoricalEDA.fine$bin_id=gsub("'","\'",CategoricalEDA.fine$bin_id)
  #CategoricalEDA.fine$bin_id=gsub('"',"\"",CategoricalEDA.fine$bin_id)

  CategoricalEDA.fine$PctRecords = CategoricalEDA.fine$Records/NbrRecords
  CategoricalEDA.fine = CategoricalEDA.fine[,c("Variable","bin_id","PctRecords","Records","Events","EventRate","WOE","GRP")]

  #loop through each avariable
  for(i in unique(CategoricalEDA.fine$Variable)){

    tmp_cat_eda_fine = CategoricalEDA.fine[which(CategoricalEDA.fine$Variable==i),]
    max_bin_id = max(tmp_cat_eda_fine$GRP)

    #create logic
    tmp_cat_eda_fine<- within(tmp_cat_eda_fine,{
      woe_logic_2_use <- ifelse(GRP==-9999,paste("if is.na(",i, ") then ",WOE,sep=""),paste("if ", i, " %in%  c(", bin_id, ") then ",WOE,sep=""))

      grp_logic_2_use <- ifelse(GRP==-9999,paste("if is.na(",i, ") then ",GRP,sep=""),paste("if ", i, " %in% c(", bin_id, ") then ",GRP,sep=""))

    })

    if(max_bin_id == 1){
      tmp_woe = tmp_cat_eda_fine[which(tmp_cat_eda_fine$GRP==1),"WOE"]
      tmp_cat_eda_fine[which(tmp_cat_eda_fine$GRP==1),]$woe_logic_2_use<-paste("if !is.na(",i,") then ",tmp_woe,sep="")
      tmp_cat_eda_fine[which(tmp_cat_eda_fine$GRP==1),]$grp_logic_2_use<-paste("if !is.na(",i,") then ",1,sep="")
      tmp_woe = NA
    }

    CategoricalEDA.fine = CategoricalEDA.fine[which(CategoricalEDA.fine$Variable != i),]

    #merge it back
    CategoricalEDA.fine = bind_rows(CategoricalEDA.fine,tmp_cat_eda_fine)

  }

  #reorder
  CategoricalEDA.fine = CategoricalEDA.fine[order(CategoricalEDA.fine$Variable,CategoricalEDA.fine$GRP),]

  Logics.2.Use = CategoricalEDA.fine[,c("Variable","grp_logic_2_use","woe_logic_2_use")]
  CategoricalEDA.fine$grp_logic_2_use = NULL
  CategoricalEDA.fine$woe_logic_2_use = NULL


  return(list(CategoricalEDA=CategoricalEDA.fine,categorical_iv=Info.Values,categorical_logics=Logics.2.Use))
}
