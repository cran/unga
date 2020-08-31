## ---- echo = FALSE, message = FALSE-------------------------------------------
knitr::opts_chunk$set(collapse = T, comment = "#>")
library(unga)
set.seed(1014)

## ----setup--------------------------------------------------------------------
library(unga)


## ---- include = FALSE---------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ----data2--------------------------------------------------------------------
set.seed(123456)
gender<-round(runif(50,1,2))
variable_1<-round(runif(50,1,2))
variable_2<-round(runif(50,1,5))
variable_3<-round(runif(50,1,5))
df<-data.frame(gender,variable_1,variable_2,variable_3)

table_alt(df)


## ----data 5-------------------------------------------------------------------
table_unga(5,1,gender,df)

## ----data3--------------------------------------------------------------------

Output_2


## ----data4--------------------------------------------------------------------

set.seed(123456)
gender<-round(runif(50,1,2))
variable_1<-sample(c(1,2,3,4,5,6,NA),50,replace = TRUE)
variable_2<-sample(c(1,2,3,4,5,6,NA),50,replace = TRUE)
variable_3<-sample(c(1,2,3,4,5,6,NA),50,replace = TRUE)
df<-data.frame(gender,variable_1,variable_2,variable_3)

table_non_resp(df)


