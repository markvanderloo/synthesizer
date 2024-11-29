#' @name imports
#'
#' @importFrom randomForest randomForest 
#' @importFrom stats predict
NULL

# check if two datasets have the same structure
str_check <- function(synth, real){

  if (!all(suppressWarnings(names(synth) == names(real)))){
    msg1 <- "Mismatch between column names in real and synthetic data"
    msg2 <- "Names and order of columns must match"
    stop(paste0(msg1, msg2, sep="\n"), call. = FALSE) 
  }

  if (!all(sapply(synth, class) == sapply(real,class))){
    msg1 <- "Mismatch between column types in real and synthetic data"
    stypes <- sapply(synth, class)
    rtypes <- sapply(real, class)
    i <- paste0(which(stypes != rtypes), sep=", ")
    msg2 <- "Types of columns %s do not differ"
    stop(paste0(msg1, msg2,sep="\n"), call. = FALSE)
  }

}


#' Compute the pMSE metric between synthetic and real data
#' 
#' The propensity mean squared error is defined as
#' \eqn{\frac{1}{N}\sum_{i=1}^N(p_i-c)^2}, where \eqn{c} is the number of
#' synthetic records, divided by the sum of the number of synthetic and real
#' records. 
#'
#' @param synth \code{[data.frame]} Synthesized data.
#' @param real  \code{[real]} Data to compare with the synthesized data.
#' @param model \code{[character]} Model used to compute propensity scores. Options
#'              are \code{"lr"}: logistic regression, and \code{"rf"}: random forest.
#' @param nrep  \code{[integer]} Number of model repetitions to average the 
#'              pMSE value over. Ignored for \code{lr}.
#'
#' 
#' @return \code{[numeric]} scalar.
#'
#' @examples
#' scars <- synthesize(cars)
#' pmse(scars, cars)
#'
#' @family measures
#' @export
pmse <- function(synth, real, model=c("lr","rf"), nrep=NULL){
  str_check(synth, real)
  model <- match.arg(model)
  switch(model
    , "lr" = pmse_lr(synth, real)
    , "rf" = pmse_rf(synth, real, nrep = if(is.null(nrep)) 10 else nrep)
  )
}



# propensity mean squared error of prediction based on 
# Random Forest prediction
pmse_rf <- function(x, ref, nrep=10){
  stopifnot(requireNamespace("randomForest"))
  nr <- nrow(ref)
  ns <- nrow(x)
  c  <- nr + ns
  x$type <- factor(x = "synthetic", levels=c("real","synthetic"))
  ref$type <- factor(x = "real", levels=c("real","synthetic"))
  d <- rbind(x,ref)
  
  pmse <- numeric(nrep)
  for (i in seq_len(nrep) ){
    m <- randomForest::randomForest(type ~ ., data=d)
    p <- stats::predict(m,newdata=d,type="prob")[,1]
    pmse[i] <- mean((p - ns/c)^2)
  }
  
  mean(pmse)
}

# x  : synthetic data
# ref: real data
pmse_lr <- function(x, ref){
  nr <- nrow(ref)
  ns <- nrow(x)
  N  <- nr + ns
  # label the data and stack on top of each other
  x$type   <- factor(x = "synthetic", levels=c("real","synthetic"))
  ref$type <- factor(x = "real", levels=c("real","synthetic"))
  d <- rbind(x,ref)
  
  # try to predict from a model: synth or real?
  m <- stats::glm(type ~ ., data=d, family="binomial")
  p <- stats::predict(m, newdata=d, type="response")
  # compute the quality of the model (between 0 and 1, smaller mean 
  # better synthetic data)
  mean((p - ns/N)^2)
  
}



