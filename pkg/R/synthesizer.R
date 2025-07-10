#' Create a function that generates synthetic data
#'
#' Create a function that accepts a non-negative integer \code{n}, and
#' that returns synthetic data sampled from the emperical (multivariate)
#' distribution of \code{x}. 
#' 
#'
#'
#' @param x \code{[vector|data.frame]} Template data to be synthesized.
#' @param ... arguments passed to other methods
#'
#' @return A \code{function} accepting a single integer argument: the number
#'         of synthesized values or records to return.  For objects of class
#'         \code{ts} \code{n} must be equal to the length of the original data
#'         (this is set as the default).
#'
#'
#' @examples
#' synth <- make_synthesizer(cars$speed)
#' synth(10)
#'
#'
#' synth <- make_synthesizer(iris)
#' synth(6)
#' synth(150)
#' synth(250)
#'
#' @family synthesis
#' @export
make_synthesizer <- function(x,...){
  UseMethod("make_synthesizer")
}

#' @rdname make_synthesizer
#' @param na.rm \code{[logical]} Remove missing values before creating a synthesizer
#' @export
make_synthesizer.numeric <- function(x,na.rm=FALSE,...){
  if (isTRUE(na.rm)) x <- x[!is.na(x)]
  if (sum(!is.na(x))<2){
    return(function(n,...) rep(NA_real_,n))
  }

  ys <- sort(x,na.last=FALSE)
  p  <- seq_along(x)/(length(x)+1)
  pmin <- min(p)
  pmax <- max(p)
  Qn <- stats::approxfun(x=p, y=ys)
  function(n,...){
    Qn(stats::runif(n, min = pmin, max = pmax))      
  }
}

#' @rdname make_synthesizer
#' @export
make_synthesizer.integer <- function(x,na.rm=FALSE,...){
  if (isTRUE(na.rm)) x <- x[!is.na(x)]
  R <- make_synthesizer(as.double(x))
  function(n,...) as.integer( round(R(n)) )
}


#' @rdname make_synthesizer
#' @export
make_synthesizer.logical <- function(x,na.rm=FALSE,...){
  if (isTRUE(na.rm)) x <- x[!is.na(x)]
  function(n,...) sample(x, n, replace=TRUE)
}


#' @rdname make_synthesizer
#' @export
make_synthesizer.factor <- function(x,na.rm=FALSE,...){
  if (isTRUE(na.rm)) x <- x[!is.na(x)]
  function(n,...) sample(x, n, replace=TRUE)
}

#' @rdname make_synthesizer
#' @export
make_synthesizer.character <- function(x,na.rm=FALSE,...){
  if (isTRUE(na.rm)) x <- x[!is.na(x)]
  function(n,...) sample(x, n, replace=TRUE)
}

#' @rdname make_synthesizer
#' @export
make_synthesizer.ts <- function(x,...){
  x_len   <- NROW(x)
  x_start <- start(x)
  x_end   <- end(x)
  x_freq  <- frequency(x)
  function(n=x_len,...){
    if (!identical(n,x_len)){
     err1 <- sprintf("Requested output lenght is %d, while input length is %d",x_len,n)
     err2 <- "Synthetic 'ts' objects must be of the same length as the input."
     stop(paste0(err1,err2,sep="\n"))
    }
    Y <-as.matrix(x,nrow=x_len,ncol=1)
    Y <- make_synthesizer(as.data.frame(Y))(x_len)
    ts(as.matrix(Y), start=x_start, end=x_end, frequency=x_freq)
  }
}

decor <- function(r, rho){
  stopifnot(rho>0)
  if (rho==1) return(r)
  
  n <- length(r)
  block_size <- max(n*(1-rho)/2,4)
  k          <- round(block_size/2)
 
  old_r <- r
  while(cor(old_r,r) > rho){
    i  <- sample(seq(k,n-k),size=1)
    ii <- seq(i-k, i+k)
    r[ii] <- r[sample(ii)]
  }
  r
}


make_decorrelating_synthesizer <- function(x, na.rm){
  f <- make_synthesizer(x, na.rm=na.rm)

  r <- rank(x, ties.method="first")
  m <- length(x)  

  function(n=m, rho=1, ii=NULL){
    stopifnot(is.numeric(rho),rho >=0, rho <= 1)
    
    if ( rho == 0 ) return(f(n))
   
    if ( n == m ) return(sort(f(n))[decor(r,rho)])

    stopifnot(!is.null(ii), length(ii)==n)

    if ( n < m ) return(sort(f(m))[decor(r,rho)][ii])

    if ( n > m ){
      do.call("c", lapply(seq_len(ceiling(n/m)), function(i) f(m)) )[ii]
    }
  }

}

# some logic to expand versions of the rankcor variable.
# Output: a named vector, where 'varnames' are the names of the variables in the data
get_rcors <- function(varnames, rankcor){
  p          <- length(varnames)
  out        <- rep(1,p)
  names(out) <- varnames

  if ( length(rankcor)==1 & is.null(names(rankcor)) ){ 
    out[1:p] <- rankcor
  } else {
    if (!all(names(rankcor) %in% varnames)){
      wrong_names <- names(rankcor)[!names(rankcor) %in% varnames]
      msg <- sprintf("Mismatch in specification of 'rankcor'. Variables not occurring in the data:\n%s"
                    , paste(wrong_names, collapse=", "))
      stop(msg)
    }
    out[ names(rankcor) ] <- rankcor
  }
  
  out
}


#' @rdname make_synthesizer
#'
#' @export
make_synthesizer.data.frame <- function(x,na.rm=FALSE,...){
  
  L <- lapply(x, make_decorrelating_synthesizer,na.rm=na.rm)
  m <- NROW(x)
  varnames <- names(x)
  function(n, rankcor=1,...){
    rcors <- get_rcors(varnames, rankcor)
    ii <- if ( n == m ){
      NULL
    } else if ( n < m ){
      sample(m, size=n, replace=FALSE)
    } else {
      sample(ceiling(n/m)*m, size=n, replace=FALSE)
    }
    lst <- mapply(function(f, rho) f(n, rho,ii), L, rcors, SIMPLIFY=FALSE)
    do.call("data.frame",lst)
  }

}




#' Create synthetic version of a dataset
#'
#' Create \code{n} values or records based on the emperical (multivariate)
#' distribution of \code{y}. For data frames it is possible to decorrelate synthetic
#' from the original variables by lowering the value for the \code{rankcor} parameter.
#'
#' @param x \code{[vector|data.frame]} data to synthesize.
#' @param n \code{[integer]} Number of values or records to synthesize.
#' @param na.rm \code{[logical]} Remove missing values before creating a synthesizer.
#'        Set to \code{TRUE} to avoid synthesizing missing values.      
#' @param rankcor \code{[numeric]} in \eqn{[0,1]}. Either a single rank correlation
#'        value that is applied to all variables, or a vector of the form
#'        \code{c(variable1=ut1lity1,...)}. Variables not explicitly mentioned
#'        will have \code{rankcor=1}. See also the note below. Ignored for 
#'        all types of \code{x}, except for objects of class \code{data.frame}.
#'
#'
#' @note
#' The utility of a synthetic variable is lowered by decorelating the rank
#' correlation between the real and synthetic data. If \code{rankcor=1}, the
#' synthetic data will ordered such that it has the same rank order as the
#' original data. If \code{rankcor=0}, no such reordering will take place. For
#' values between 0 and 1, blocks of data are randomly selected and randomly
#' permuted iteratively until the rank correlation between original and
#' synthetic data drops below the parameter.
#'
#' @return A data object of the same type and structure as \code{x}.
#'
#'
#' @examples
#' synthesize(cars$speed,10)
#' synthesize(cars)
#' synthesize(cars,25)
#'
#' s1 <- synthesize(iris, rankcor=1)
#' s2 <- synthesize(iris, rankcor=0.5)
#' s3 <- synthesize(iris, rankcor=c("Species"=0.5))
#' 
#' oldpar <- par(mfrow=c(2,2), pch=16, las=1)
#' plot(Sepal.Length ~ Sepal.Width, data=iris, col=iris$Species, main="Iris")
#' plot(Sepal.Length ~ Sepal.Width, data=s1, col=s1$Species, main="Synthetic Iris")
#' plot(Sepal.Length ~ Sepal.Width, data=s2, col=s2$Species, main="Low utility Iris")
#' plot(Sepal.Length ~ Sepal.Width, data=s3, col=s3$Species, main="Low utility Species")
#' par(oldpar)
#'
#'
#' @family synthesis
#' @export
synthesize <- function(x, na.rm=FALSE, n=NROW(x), rankcor=1) make_synthesizer(x,na.rm=na.rm)(n,rankcor)





