#' @title process_pipeline
#'
#' @description Main EDA function call to use.  This is a wrapper around the functions that will bin the attributes in the dataframe and give back summarized tables.
#' 
#' @param run_id An identifier that will be used when outputting tables to the specified path (path_2_save parameter).  Example: 'MyRun1'
#' @param unique_id_var A variable in your dataframe that uniquely identifies a record. Can only be 1 variable.
#' @param df A dataframe you are wanting to analyze
#' @param dv_var The name of the dependent variable (dv).  Example: 'target'
#' @param dv_type Can take on 1 of two inpunts - c('Binary','Frequency').  Both should be numeric.  If 'Frequency' is the input, it should be the numerator (if it is a rate).  The denominator will be specified as a separate parameter
#' @param dv_denominator The denominator of your dependent variable.  In many cases, this can be considered the exposure
#' @param var_list A list of non-numeric variables to analyze and create bins for
#' @param num_nbins For numeric variables, this is the maximum number of bins to initially split the variable into
#' @param num_min_pct For numeric variables, this is the minimun percent of records a final bin should have.  Generally applies to only bins that are not NA
#' @param num_binning_type For numeric variables, this is the type of binning to use when splitting the variable.  One of two can be selected: c("Bucketing","Quantiles")
#' @param num_monotonic For numeric variables, this is a Logical TRUE/FALSE input.  If TRUE, it will force the bins to be monotonic based on the event rate
#' @param cat_max_levels For non-numeric variables, if a variable initially has more unique levels than cat_max_levels, it will be skipped
#' @param cat_min_pct For non-numeric variables, this is the minimun percent of records a final bin should have.  Generally applies to only bins that are not NA
#' @param eda_tracking Logical TRUE/FALSE inputs.  If set to TRUE, the user will be able to see what variable the function is analyzing.
#' @param path_2_save A path to a folder where the outputs will be stored.  Default is: getwd().  Or an example: /store/outputs/in/this/folder
#' 
#' @return A list of dataframes.  First in the list will be 'Numeric_eda' - this is an aggregated dataframe showing the groups created along with other key information.  The second is 'numeric_iv' - This is a dataframe with each variable processed and their information value.  The last is 'numeric_logics' - This is a dataframe with the information needed to apply to your dataframe and transform your variables.  This table will be the input to apply_numeric_logic(logic_df=numeric_logics)
#' @export

#process pipeline
process_pipeline = function( run_id
                            ,df
                            ,unique_id_var                  # unique identifier in your data (must be 1 variable)
                            ,dv_var            
                            ,dv_type          = "Binary"    # "Binary" or "Frequency"
                            ,dv_denominator   = NULL        # if used, ensure the 'dv' represents the numerator
                            ,var_list                       # Variables to conduct EDA on (do not include the dv)
                            ,num_nbins        = 200
                            ,num_min_pct      = 0.01
                            ,num_binning_type = "Bucketing" # "Bucketing" or "Quantiles"
                            ,num_monotonic    = TRUE
                            ,cat_max_levels   = 200
                            ,cat_min_pct      = 0.01
                            ,eda_tracking     = TRUE
                            ,path_2_save      = getwd()     # path to save outputs
                            ){
  
  #surpress warnings
  options(warn=-1)
  options(scipen=999);
  `%ni%` = Negate(`%in%`);
  
  
  if(length(unique_id_var) != 1){
    stop("unique_id_var must be 1 variable that is unique to each record.  If needed, create a column which concatenates multiple columns to give the unique value")
  }
  
  if(length(dv_type) !=1){
    stop("There is only 1 input allowed for dv_type")
  }
  
  if(length(num_binning_type) !=1){
    stop("There is only 1 input allowed for num_binning_type")
  }
  
  if(length(dv_denominator) >1){
    stop("dv_denominator can either be NULL or have 1 input")
  }
  
  
  #numeric variable list
  numeric_vars_2_use = which(sapply(df, is.numeric))
  numeric_vars_2_use = names(numeric_vars_2_use)
  numeric_vars_2_use = numeric_vars_2_use[numeric_vars_2_use %in% var_list]
  
  #get numeric binning
  numeric_eda_output<-get_numeric_bins(  df             = df        
                                        ,dv             = dv_var    
                                        ,dv.type        = dv_type  
                                        ,dv.denominator = dv_denominator
                                        ,var.list       = numeric_vars_2_use
                                        ,nbins          = num_nbins
                                        ,min.Pct        = num_min_pct   
                                        ,binning.Type   = num_binning_type
                                        ,monotonic      = num_monotonic      
                                        ,tracking       = eda_tracking      
  )
  
  #nonnumeric variable list
  
  cat_vars_2_use = df[, !(names(df) %in% names(df %>% dplyr::select_if(is.numeric)))]
  cat_vars_2_use = names(cat_vars_2_use)
  cat_vars_2_use = cat_vars_2_use[cat_vars_2_use %in% var_list]
  
  #get categorical binning
  categorical_eda_output<-get_categorical_bins(  df             = df               # dataframe
                                                ,dv             = dv_var          # Dependent Varaible
                                                ,dv.type        = dv_type         # Binary, Frequency
                                                ,dv.denominator = dv_denominator  # Only used for exposure of frequency
                                                ,var.list       = cat_vars_2_use  # A list of numeric variables
                                                ,max.levels     = cat_max_levels  # >1
                                                ,min.Pct        = cat_min_pct     # (0,1)
                                                ,tracking       = eda_tracking    # Do you want to track progress or not
  )
  
  #save outputs
  message("")
  message("Saving outputs to: ",path_2_save)
  message("")
  if(!is.null(numeric_eda_output)){
    for(i in names(numeric_eda_output)){
      write.csv( numeric_eda_output[i]
                 ,paste(path_2_save,"/",run_id,"-",i,".csv",sep=""))
      
      message("Saved ",i," to: ",paste(path_2_save,"/",run_id,"-",i,".csv",sep=""));
      
    }
    message("")
  }
  
  message("")
  if(!is.null(categorical_eda_output)){
    for(i in names(categorical_eda_output)){
      write.csv( categorical_eda_output[i]
                 ,paste(path_2_save,"/",run_id,"-",i,".csv",sep=""))
      
      message("Saved ",i," to: ",paste(path_2_save,"/",run_id,"-",i,".csv",sep=""));
      
    }
    message("")
  }
  
  return(c(numeric_eda_output,categorical_eda_output))
}

