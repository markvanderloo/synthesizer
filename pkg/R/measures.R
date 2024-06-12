#' @name imports
#' importFrom randomForest randomForest predict.randomForest
#' importFrom stats predict
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


diff_mean_x <- function(xs, xr, tol, ...){
  mr <- mean(xr,...)
  if (abs(mr) > tol){ 
    abs((mean(xs,...)-mr)/mr)
  } else {
    abs(mean(xs) - mr)
  }
}

#' Summarization of location and spread between synthetic and real data
#'
#' For each numerical variable in the two datasets, compute the relative
#' difference between the mean (standard deviation) of the real data and the
#' mean (standard deviation) of the synthetic data. The summary is the average
#' of these relative differences over all numerical variables. 
#' 
#'
#'
#' @param synth \code{[data.frame]} Synthetic data
#' @param real  \code{[data.frame]} Real data
#' @param tol   \code{[numeric]} Nonnegative tolerance. If the absolute 
#'              mean (standard deviation) of a variable is smaller than 
#'              \code{tol}, it is considered zero. In that case the 
#'              absolute difference instead of the absolute relative 
#'              difference is computed.
#' @param ...   Arguments passed to \code{mean}. e.g. use \code{trim=c(0.01,0.99)}
#'              for mean estimation that is less sensitive to outliers.
#'
#' @note
#' Real and synthetic data are expected to have the same column names,
#' orders, and data types.
#'
#'
#' @examples
#'
#' dmean(cars, cars) # 0
#' dmean(synthesize(cars), cars)
#'
#' dsd(cars, cars) # 0
#' dsd(synthesize(cars), cars)
#'
#'
#' @family measures
#' @export
dmean <- function(synth, real, tol=1e-8, ...){
  str_check(synth, real)
  i <- sapply(synth, is.numeric)
  mean(mapply(diff_mean_x, synth[i], real[i],tol=tol, ...))
}


diff_sd_x <- function(xs, xr, tol, ...){
  sr <- stats::sd(xr,...)
  if (abs(sr) > tol){ 
    abs((stats::sd(xs,...)-sr)/sr)
  } else {
    abs(stats::sd(xs) - sr)
  }
}

#' @rdname dmean
#' @family measures
#' @export
dsd <- function(synth, real, tol=1e-8, ...){
  str_check(synth, real)
  i <- sapply(synth, is.numeric)
  mean(mapply(diff_sd_x, synth[i], real[i],tol=tol, ...))
}

#' Difference between correlation
#'
#' Returns the Frobenius norm of the difference between the 
#' correlation matrices for numeric columns in synthetic
#' and real data.
#'
#' @param synth \code{[data.frame]} Synthetic data
#' @param real  \code{[data.frame]} Real data
#'
#'
#' @examples 
#' dcor(iris, iris) # 0
#' dcor(synthesize(cars), cars)
#'
#' @family measures
#' @export
dcor <- function(synth, real){
  str_check(real, synth)
  i <- sapply(synth, is.numeric)
  norm(stats::cor(synth[i]) - stats::cor(real[i]), type="F")
}


#' Quality assurance for synthesized data 
#'
#' Repeatedly synthesize a dataset, record a set of quality
#' measures for each repetition.
#'
#'
#' @param real \code{[data.frame]} A data set to be synthesized.
#' @param n    \code{[integer]} Number of repetitions
#' 
#' @return A \code{data.frame} with \code{n} rows and each column a quality measure.
#'
#'
#' @examples
#' 
#' qa(iris)
#'
#' @family measures
#' @export
qa <- function(real, n=10){
  stopifnot(n>=1)

  measures <- c("pmse", "dmean","dsd", "dcor")

  out <- array(NA_real_
          , dim=c(n, length(measures))
          , dimnames=list("run"=1:n, "measure" = measures))
  
  for ( i in seq_len(n) ){
    synth   <- synthesize(real)
    out[i,] <- sapply(measures, function(m) do.call(m, list(real, synth)))
  }
  as.data.frame(out)
}



