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
#'         of synthesized values or records to return.
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
#' @export
make_synthesizer.numeric <- function(x,...){
  if (sum(!is.na(x))<2){
    return(function(n) rep(NA_real_,n))
  }

  ys <- sort(x,na.last=FALSE)
  p  <- seq_along(x)/(length(x)+1)
  pmin <- min(p)
  pmax <- max(p)
  Qn <- stats::approxfun(x=p, y=ys)
  function(n){
    Qn(stats::runif(n, min = pmin, max = pmax))      
  }
}

#' @rdname make_synthesizer
#' @export
make_synthesizer.integer <- function(x,...){
  R <- make_synthesizer(as.double(x))
  function(n) as.integer( round(R(n)) )
}


#' @rdname make_synthesizer
#' @export
make_synthesizer.logical <- function(x,...){
  function(n) sample(x, n, replace=TRUE)
}


#' @rdname make_synthesizer
#' @export
make_synthesizer.factor <- function(x,...){
  function(n) sample(x, n, replace=TRUE)
}

#' @rdname make_synthesizer
#' @export
make_synthesizer.character <- function(x,...){
  function(n) sample(x, n, replace=TRUE)
}

#' @rdname make_synthesizer
#' @export
make_synthesizer.ts <- function(x,...){
  x_len   <- NROW(x)
  x_start <- start(x)
  x_end   <- end(x)
  x_freq  <- frequency(x)
  function(n=x_len){
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

# Randomly swap adjacent elements of x. 
# Each element has a probability of pswap to be part of a swap.
randomize <- function(x, rho){
  # if desired correlation equals zero (or close enough) we don't use
  # any rank-matching at al and leave the initially sampled variable
  # intact.
  if (rho < 0.05) return(sample(x))

  n          <- length(x)
  # theory suggests using a block size of n*(1-rho); We choose a smaller block size
  # to spread permutations accross the vector (at the cost of some extra iterations)
  block_size <- max(n*(1-rho)/2,4)
  k          <- round(block_size/2)
 
  old_x <- x
  while(cor(old_x,x) > rho){
    i  <- sample(seq(k,n-k),size=1)
    ii <- seq(i-k, i+k)
    x[ii] <- x[sample(ii)]
  }
  x
}




# randomize the order of a rank vector until the correlation with the original
# vecor has dropped below a maximum value.  Orders are randomized by selecting
# each time at random approximately 1% of the vector and cyclicly permuting the
# indices. 
decorrelate <- function(ranklist, cors){
  # case of perfect utility matching: no randomization necessary
  if ( (length(cors)==1 && cors==1) || 
       (length(cors)==length(ranklist) && all(cors==1)) ){
    return(ranklist)
  }

  if ( length(cors) == 1 && is.null(names(cors)) ){
    cors <- rep(cors,length(ranklist))
    names(cors) <- names(ranklist)
  }

  for ( variable in names(cors) ){
    ranklist[[variable]] <- randomize(ranklist[[variable]], cors[variable])
  }

  ranklist
}


#' @rdname make_synthesizer
#' @param rankcor \code{[numeric]} in \eqn{(0,1]} The correlations between the ranks of
#'        the real data and synthetic data. Either a single
#'        number or a vector of the form \code{c("variable1"=x1,...)}. Only used
#'        if \code{x} is a data frame. 
#'
#' @export
make_synthesizer.data.frame <- function(x, rankcor=1,...){
  stopifnot(all(rankcor >= 0), all(rankcor<=1))

  L  <- lapply(x, make_synthesizer)
  A  <- lapply(x, rank, na.last=FALSE)
  A  <- decorrelate(A, rankcor)
  nr <- nrow(x)
  f  <- function() as.data.frame(
          mapply(
            function(synth, rnk) sort(synth(nr), na.last = FALSE)[rnk]
          , L, A, SIMPLIFY = FALSE
         ) )
  function(n=nrow(x)){
    out <- f()
    if (n == nr) return(out)
    if (n < nr)  return( out[sample(seq_len(nr), size=n, replace=FALSE),,drop=FALSE] )
    i <- 0
    while ( i < n %/% nr ){
      out <- rbind(out, f())
      i <- i + 1
    }
    out[sample(seq_len(nrow(out)), size=n, replace=FALSE),,drop=FALSE]
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
#' @param rankcor \code{[numeric]} in \eqn{[0,1]}. Either a single rank correlation
#'        value that is applied to all variables, or a vector of the form
#'        \code{c(variable1=ut1lity1,...)}. Variables not explicitly mentioned
#'        will have \code{rankcor=1}. See also the note below.
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
#' @family synthesis
#' @export
synthesize <- function(x, n=NROW(x), rankcor=1) make_synthesizer(x,rankcor)(n) 




