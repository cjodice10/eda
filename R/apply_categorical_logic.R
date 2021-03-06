#' @title apply_categorical_logic
#'
#' @description Apply categorical grouping logic
#'
#' @param main_df A dataframe that you want to transform.  Ensure that it has a unique field which will be represented in the unique_key_id
#' @param logic_df A dataframe which provides the logic to apply to each variable
#' @param unique_key_id A single attribute representing the unique key in the dataframe
#' @param logic_type The logic to use to create either woe and/or grp variables. Options are c('woe','grp')
#'
#' @return A dataframe withe the unique_key_id field along with all transformed attributes (woe and/or grp).  The original attributes will not be in this data frame.  The only original variable that will be in this data frame is the unique_key_id field
#'
#' @export
apply_categorical_logic <- function( main_df
                                    ,logic_df
                                    ,unique_key_id
                                    ,logic_type    ="woe"  #either grp or woe
){
  `%ni%` = Negate(`%in%`);

  if(is.null(unique_key_id))                                  {stop("\nunique_key_id must be an input - this should be a column in your main_df dataframe that makes a record unique")}
  if(unique_key_id %ni% colnames(main_df))                    {stop("\nunique_key_id you specified is not in your main_df")}
  if(length(unique(main_df[,unique_key_id])) != nrow(main_df)){stop("\nThe unique_key_id is not unique - the number of unique values is not equal to the number of records")}

  if(is.null(logic_df))                                       {stop("\nMust include a logic_df")}
  if(tolower(logic_type) %ni% c("grp","woe"))                 {stop("\nlogic_type should be either 'grp' or 'woe'")}
  if("Variable" %ni% colnames(logic_df))                      {stop("\nThe column 'Variable' in your logic_df is missing.  Please ensure you name it 'Variable'")}
  if("grp_logic_2_use" %ni% colnames(logic_df) &
     "grp" %in% tolower(logic_type))                          {stop("\nThe column 'grp_logic_2_use' in your logic_df is missing.  Please ensure you name it 'grp_logic_2_use'")}
  if("woe_logic_2_use" %ni% colnames(logic_df) &
     "woe" %in% tolower(logic_type))                          {stop("\nThe column 'woe_logic_2_use' in your logic_df is missing.  Please ensure you name it 'woe_logic_2_use'")}


  list_of_vars = unique(logic_df$Variable)

  first_var_ind = 1
  #loop through each variable
  for(i in list_of_vars){
    message("transforming varaible ",i);

    #subset
    df_transformed = main_df[,c(unique_key_id,i)]
    #df_transformed[,i]=gsub("'","",df_transformed[,i],fixed = TRUE)
    #df_transformed[,i]=gsub('"',"",df_transformed[,i],fixed = TRUE)
    #df_transformed[,i]=gsub("/","",df_transformed[,i],fixed = TRUE)
    #df_transformed[,i]=gsub('\\\\',"",df_transformed[,i],fixed = TRUE)

    #logic for grp
    if("grp" %in% tolower(logic_type)){

      tmp_logic_df                 = logic_df[which(logic_df$Variable==i),c("Variable","grp_logic_2_use")]
      tmp_logic_df$parenth         = ")"
      tmp_logic_df$grp_logic_2_use = gsub(" then ",",",stringr::str_sub(tmp_logic_df$grp_logic_2_use, start = 4, end = -1L))
      tmp_logic_df$grp_logic_2_use = paste("ifelse(",tmp_logic_df$grp_logic_2_use,sep="")
      logic_2_use_df = tmp_logic_df %>%
        dplyr::group_by(Variable) %>%
        dplyr::summarise( logic_2_use = paste(grp_logic_2_use,collapse=", ")
                          ,parenth     = paste(parenth        ,collapse=""  )) %>%
        data.frame()

      #apply logic
      final_logic = paste(logic_2_use_df$logic_2_use,",NA",logic_2_use_df$parenth)

      df_transformed = within(df_transformed,{
        grp = eval(parse(text=final_logic))
      })

      #chnage var
      colnames(df_transformed)[which(colnames(df_transformed)=="grp")] = paste("grp_",i,sep="")
      logic_2_use=NULL
    }


    #logic for woe
    if("woe" %in% tolower(logic_type)){
      tmp_logic_df                 = logic_df[which(logic_df$Variable==i),c("Variable","woe_logic_2_use")]
      tmp_logic_df$parenth         = ")"
      tmp_logic_df$woe_logic_2_use = gsub(" then ",",",stringr::str_sub(tmp_logic_df$woe_logic_2_use, start = 4, end = -1L))
      tmp_logic_df$woe_logic_2_use = paste("ifelse(",tmp_logic_df$woe_logic_2_use,sep="")
      logic_2_use_df = tmp_logic_df %>%
        dplyr::group_by(Variable) %>%
        dplyr::summarise(logic_2_use = paste(woe_logic_2_use,collapse=", ")
                         ,parenth     = paste(parenth        ,collapse=""  )) %>%
        data.frame()

      final_logic = paste(logic_2_use_df$logic_2_use,",NA",logic_2_use_df$parenth)

      df_transformed = within(df_transformed,{
        woe = eval(parse(text=final_logic))
      })

      #chnage var
      colnames(df_transformed)[which(colnames(df_transformed)=="woe")] = paste("woe_",i,sep="")
      logic_2_use=NULL
    }

    #remove original variable
    df_transformed[,i]=NULL

    if(first_var_ind==1){
      df_2_return=df_transformed
    }else{
      df_2_return=#bind_cols(df_2_return,df_transformed[,-1])
        merge(  x    = df_2_return
               ,y    = df_transformed
               ,by.x = c(unique_key_id)
               ,by.y = c(unique_key_id))
    }

    first_var_ind=0
  }

  #loop through to check any NA values
  check_missing_df = df_2_return[,substr(colnames(df_2_return),1,4) %in% c("woe_","grp_")] %>% data.frame()
  if(nrow(check_missing_df)>0){
    has_missing = list()
    for(i in colnames(check_missing_df)){
      if(sum(is.na(check_missing_df[,i]))>0){
        i = substr(i,5,nchar(i))
        has_missing = c(has_missing,i)
        has_missing = unique(has_missing)
      }
    }
  }

  if(length(has_missing)>0){
    message("");
    message("Warning -- The following variables had values that are not in the original logic.");
    print(unlist(has_missing))
    message("");
  }

  #return
  return(df_2_return)
}
