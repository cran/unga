#' A function for cross tabulation.
#' @description Produces multiple prop.tables of
#' a data frame. Extract questions with a specified number of response
#' alternatives.
#' @param qalt Number of response alternatives in a survey question
#' @param  sumopt Options for summation of proportions questions with 5 or 6
#' response alternatives, argument can be 1 or 2
#' @param expvar Explanatory variable in your data frame, use numeric values
#' @param x is a data frame
#' @return the output from \code{\link{print}}
#' @export
#'
#' @examples
#' set.seed(123456)
#' gender<-round(runif(50,1,2))
#' variable_1<-round(runif(50,1,5))
#' variable_2<-round(runif(50,1,5))
#' variable_3<-round(runif(50,1,5))
#' df<-data.frame(gender,variable_1,variable_2,variable_3)
#' table_unga(5,1,gender,df)
#' @references Norman M. Bradburn et al. 2004. Asking questions. 2nd revised
#' edition. John Wiley & Sons

table_unga<-function(qalt,sumopt,expvar,x) {

  myfunc<-function(expvar=expvar,y=y) {prop.table(table(expvar,y),1)}

  Minlista_99<-list()
  namn_99<-list()
  i<-1

  for (i in 1:length(x)) {

    Minlista_99[[i]]<-myfunc(expvar=expvar,x[,i])
    namn_99[[i]]<-names(x[i])
  }

  names(Minlista_99) <- namn_99

  dimensioner<-sapply(Minlista_99, function(y) (dim(y)))
  t1<-dimensioner[2,]
  t3<-t1
  t4<-as.matrix(t3)


  min.lista<-Minlista_99[which(t4==qalt)]


  t5<-as.data.frame(min.lista)
  t5<-t5[
    with(t5, order(t5[1])),
  ]



  k10<-t5[seq(3,length(t5),3)]




  slaihop6_3<-function(Y,i) {if(qalt==6&sumopt==1) {



    L<-Y[,i][-seq(3,length(Y[,i]),3)]
    b1<-rowSums(matrix(L,ncol=2,byrow=TRUE))
    b2<-matrix(b1,ncol=2,byrow=TRUE)
    b3<-Y[,i][seq(3,length(Y[,i]),3)]
    b4<-matrix(b3,ncol=2,byrow=TRUE)
    obj1<-matrix(b2[,1],ncol=1)
    obj3<-matrix(b2[,2],ncol=1)
    obj2<-matrix(b4[,1],ncol=1)
    obj4<-matrix(b4[,2],ncol=1)

    L3<-matrix(t(cbind(obj1,obj2,obj3,obj4)))


  }


    else if (qalt==5&sumopt==1) {

      L<-Y[,i][-seq(5,length(Y[,i]),5)]
      b1<-rowSums(matrix(L,ncol=2,byrow=TRUE))
      b2<-matrix(b1,ncol=2,byrow=TRUE)
      b3<-Y[,i][seq(5,length(Y[,i]),5)]
      L3<-matrix(t(cbind(b2,b3)))

    }


    else if (qalt==5&sumopt==2) {

      L<-Y[,i][-seq(3,length(Y[,i]),5)]
      b1<-rowSums(matrix(L,ncol=2,byrow=TRUE))
      b2<-matrix(b1,ncol=2,byrow=TRUE)
      b3<-Y[,i][seq(3,length(Y[,i]),5)]
      b4<-cbind(b2,b3)
      b5<-cbind(b4[,1],b4[,3],b4[,2])
      b6<-as.vector(matrix(t(b5)))

    }
  }


  if (qalt==5|qalt==6&sumopt==1|sumopt==2) {

    p<-list()
    i<-1
    length(k10)

    for (i in 1:length(k10)) {

      p[[i]]<-slaihop6_3(k10,i)


    }


    p2<-as.data.frame(p)
    names(p2)<-names(k10)
    dimensions_exp<-dim(table(expvar))
    langd_1<-length(p2[,1])
    response_alt<-rep(1:(langd_1/dimensions_exp),dimensions_exp)
    Output_2<<-cbind(response_alt,p2)

  }


  Output_1<-Output_2<-NULL

  Output_1a<-t5[seq(3,length(t5),3)]
  Output_1<-cbind(t5[1],t5[2],Output_1a)
  print(Output_1,row.names = FALSE)




}



#' A function to explore response alternatives in a data frame
#'
#' @param x a data frame
#'
#' @return the output from \code{\link{return}}
#' @export
#'
#' @examples
#' set.seed(123456)
#' gender<-round(runif(50,1,2))
#' variable_1<-round(runif(50,1,5))
#' variable_2<-round(runif(50,1,5))
#' variable_3<-round(runif(50,1,5))
#' df<-data.frame(gender,variable_1,variable_2,variable_3)
#' table_alt(df)
#' @references Norman M. Bradburn et al. 2004. Asking questions. 2nd revised
#' edition. John Wiley & Sons

table_alt <- function(x) {

  p<-list()
  r<-list()
  i<-1
  length(x)

  for (i in 1:length(x)) {

    p[[i]]<-table(x[i])

  }

  dimensions<-sapply(p, function(y) (dim(y)))
  dimensions <- table(dimensions)
  dimensions<-t(as.matrix(dimensions))
  rownames(dimensions)<-"Freq"
  return(dimensions)
}


#' A function that return proportion of missing values in a data frame
#'
#' @param x data frame
#'
#' @return the output from \code{\link{return}}
#' @export
#'
#' @examples
#' set.seed(123456)
#' gender<-round(runif(50,1,2))
#' variable_1<-sample(c(1,2,3,4,5,6,NA),50,replace = TRUE)
#' variable_2<-sample(c(1,2,3,4,5,6,NA),50,replace = TRUE)
#' variable_3<-sample(c(1,2,3,4,5,6,NA),50,replace = TRUE)
#' df<-data.frame(gender,variable_1,variable_2,variable_3)
#' table_non_resp(df)
#'
#' @references Norman M. Bradburn et al. 2004. Asking questions. 2nd revised
#' edition. John Wiley & Sons

table_non_resp<-function(x) {
  missing_1<-apply(is.na(x),2,sum)
  not_missing<-apply(!is.na(x),2,sum)
  return(as.data.frame(missing_1/(missing_1+not_missing)))
}


#' A function that return absolute vaules of missing values in a data frame
#'
#' @param x a data frame
#'
#' @return the output from \code{\link{return}}
#' @export
#'
#' @examples
#' set.seed(123456)
#' gender<-round(runif(50,1,2))
#' variable_1<-sample(c(1,2,3,4,5,6,NA),50,replace = TRUE)
#' variable_2<-sample(c(1,2,3,4,5,6,NA),50,replace = TRUE)
#' variable_3<-sample(c(1,2,3,4,5,6,NA),50,replace = TRUE)
#' df<-data.frame(gender,variable_1,variable_2,variable_3)
#' table_non_resp_2(df)
#'
#' @references Norman M. Bradburn et al. 2004. Asking questions. 2nd revised
#'edition. John Wiley & Sons
#'
table_non_resp_2<-function(x) {
  missing_1<-apply(is.na(x),2,sum)
  not_missing<-apply(!is.na(x),2,sum)
  return(as.data.frame(missing_1))
}


#' Function to cross tabulate all variables in a data frame
#'
#' @param expvar explanatory variable
#' @param x data frame
#'
#' @return the output from \code{\link{return}}
#' @export
#'
#' @examples
#' #' set.seed(123456)
#' gender<-round(runif(50,1,2))
#' variable_1<-round(runif(50,1,5))
#' variable_2<-round(runif(50,1,5))
#' variable_3<-round(runif(50,1,5))
#' df<-data.frame(gender,variable_1,variable_2,variable_3)
#' table_unga_general(gender,df)
#' @references Norman M. Bradburn et al. 2004. Asking questions. 2nd revised
#' edition. John Wiley & Sons

table_unga_general<-function(expvar,x) {

  myfunc<-function(expvar=expvar,x) {prop.table(table(expvar,x),1)}

  multitable_99<-list()
  namn_99<-list()
  i<-1

  for (i in 1:length(x)) {

    multitable_99[[i]]<-myfunc(expvar=expvar,x[,i])
    namn_99[[i]]<-names(x[i])
  }
  names(multitable_99) <- namn_99
  return(multitable_99)
}
