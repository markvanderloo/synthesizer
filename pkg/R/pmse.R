#' importFrom randomForest randomForest predict.randomForest
#' importFrom stats predict
NULL

#' Compute the pMSE metric between synthetic and real data
#' 
#'
#'
#' @param synth \code{[data.frame]} Synthesized data
#' @param real  \code{[real]} Data to compare the synthesized data with
#' @param model \code{[character]} Model used to compute propensity scores. Options
#'              are \code{"lr"}: logistic regression, and \code{"rf"}: random forest.
#' @param nrep  \code{[integer]} Number of model repetitions to average the 
#'              pMSE value over. Ignored for \code{lr}.
#'
#'
#' @examples
#' scars <- synthesize(cars)
#' pmse(scars, cars)
#'
#'
#' @export
pmse <- function(synth, real, model=c("lr","rf"), nrep=NULL){
  model <- match.arg(model)
  switch(model
    , "lr" = pmse_lr(synth, real)
    , "rf" = pmse_rf(synth, real, nrep = if(is.null(nrep)) 10 else nrep)
  )
}



# propensity mean squared error of prediction
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


