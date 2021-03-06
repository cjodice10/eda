# Why `eda`?
I created this package to help analyst have an easier way of conducting their exploratory data analysis (`eda`) when using R. Historically I've used other products which have really good, some-what automated, EDA tools in their software, but unfortunately these products can be real expensive.  So I wanted to create a package that can help others and ease their `eda` process using R.  My goal is that this `eda` package will give the analyst a tool to better understand their data and can easily represent some of the outputs to their clients.

This package, as of now, mainly focuses on data sets where the analyst is either modeling a binary response, or a response that reflects count data.  This package will explore each attribute in the data by binning and grouping records based on user inputs and the relationship with the dependent variable (DV).  For example, it may be that the user wants to force monotonic binning on numeric variables, this package will allow this.  

Why bin variables though?  Well, depending on your analysis, binning does have it's benefits and at the same time has draw-backs.  The biggest draw-back is that when we bin variables to use in models, we naturally lose information, or lose the variability to be explained within the records of the bin.  But binning also has benefits.  It handles outliers very well, handles missing very well, and more.  

Please read through the vignette eda.Rmd for an example!

