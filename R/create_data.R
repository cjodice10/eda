#creating fake data to test

#create a dataframe
library(ggplot2)
df_example = data.frame(target = runif(100000))

#create dv
df_example$target = ifelse(df_example$target>0.9,1,0)
str(df_example)


#paramters
n = 50000
b0 = 10
b1 = 30
b2 = 5
b3 = -10
b4 = -20
sigma  = 50

x1 = rnorm(n)
x2 = rnorm(n)
x3 = rnorm(n)
x4 = rnorm(n)
y  = b0 + b1*x1 + b2*x2 + b3*x3 + b4*x4*x4*x4 + rnorm(n, sd=sigma)
qplot(x1, y)
qplot(x2, y)
qplot(x3, y)
qplot(x4, y)

#create dataframe
df_example = data.frame( y =y
                        ,x1=x1
                        ,x2=x2
                        ,x3=x3
                        ,x4=x4)

#create dv
df_example$target = ifelse(df_example$y>75,1,0)
str(df_example)
summary(df_example)

#create id
df_example$id = 1:nrow(df_example)

#get binning
my_eda = process_pipeline( run_id           = 'MyRun1'
                          ,df               = df_example
                          ,unique_id_var    = "id"
                          ,dv_var           = "target"
                          ,dv_type          = "Binary"
                          ,var_list         = c("x1","x2","x3","x4")
                          ,path_2_save      = "/Users/jodicefamily/Rprojects/Testing"
)
View(my_eda$Numeric_eda)


#get binning
my_eda = process_pipeline( run_id           = 'MyRun1'
                           ,df               = df_example
                           ,unique_id_var    = "id"
                           ,dv_var           = "target"
                           ,dv_type          = "Binary"
                           ,var_list         = c("x1","x2","x3","x4")
                           ,num_monotonic    = TRUE
                           ,num_min_pct      = 0.05
                           ,path_2_save      = "/Users/jodicefamily/Rprojects/Testing"
)
View(my_eda$Numeric_eda)

get_plot =function(i){
tmp_d_plot = my_eda$Numeric_eda
tmp_d_plot = tmp_d_plot[which(tmp_d_plot$Variable==i),]
ggplot(tmp_d_plot,aes(x=bin_id,y=EventRate))+
  geom_line()
}
get_plot(i="x4")

