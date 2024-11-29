
#' Create a function that generates synthetic data
#'
#' Create a function that accepts a non-negative integer \code{n}, and
#' that returns synthetic data sampled from the emperical (multivariate)
#' distribution of \code{y}. 
#' 
#'
#'
#' @param y \code{[vector|data.frame]} Template data to be synthesized.
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
make_synthesizer <- function(y,...){
  UseMethod("make_synthesizer")
}

#' @rdname make_synthesizer
#' @export
make_synthesizer.numeric <- function(y,...){
  if (sum(!is.na(y))<2){
    return(function(n) rep(NA_real_,n))
  }

  ys <- sort(y,na.last=FALSE)
  p  <- seq_along(y)/(length(y)+1)
  pmin <- min(p)
  pmax <- max(p)
  Qn <- stats::approxfun(x=p, y=ys)
  function(n){
    Qn(stats::runif(n, min = pmin, max = pmax))      
  }
}

#' @rdname make_synthesizer
#' @export
make_synthesizer.integer <- function(y,...){
  R <- make_synthesizer(as.double(y))
  function(n) as.integer( round(R(n)) )
}


#' @rdname make_synthesizer
#' @export
make_synthesizer.logical <- function(y,...){
  function(n) sample(y, n, replace=TRUE)
}


#' @rdname make_synthesizer
#' @export
make_synthesizer.factor <- function(y,...){
  function(n) sample(y, n, replace=TRUE)
}

#' @rdname make_synthesizer
#' @export
make_synthesizer.character <- function(y,...){
  function(n) sample(y, n, replace=TRUE)
}

randomize <- function(ranklist, cors){
  if ( all(cors==1) ) return(ranklist)
  
  if ( length(cors) == 1 && is.null(names(cors)) ){
    cors <- rep(cors,length(ranklist))
    names(cors) <- names(ranklist)
  }

  for ( variable in names(cors) ){
    old_rank <- ranklist[[variable]]
    new_rank <- old_rank
    while (cor(old_rank,new_rank)>cors[variable]){
      i <- sample(1:length(old_rank),size=4,replace=FALSE)
      new_rank[rev(i)] <- new_rank[i] 
    }
    ranklist[[variable]] <- new_rank
  }
  
  ranklist

}




#' @rdname make_synthesizer
#' @export
make_synthesizer.data.frame <- function(y, correlations=1,...){
  stopifnot(all(correlations > 0), all(correlations<=1))
  L  <- lapply(y, make_synthesizer)
  A  <- lapply(y, rank, na.last=FALSE)
  # TODO: insert randomization of ranks here.
  A <- randomize(A, correlations)
  nr <- nrow(y)
  f <- function() as.data.frame(
          mapply(
            function(synth, rnk) sort(synth(nr), na.last = FALSE)[rnk]
          , L, A, SIMPLIFY = FALSE
          ) )
  function(n=nrow(y)){
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
#' distribution of \code{y}. For data frames, it is possible to decorrelate
#' the variables by lowering the value for the \code{correlations} parameter.
#'
#' @param y \code{[vector|data.frame]} data to synthesize.
#' @param n \code{[integer]} Number of values or records to synthesize.
#' @param correlations \code{[numeric]} The correlations between the rank of
#'        the real dataset and the ranks of the synthetic dataset. Either a single
#'        number or a vector of the form \code{c(variable1=x1,...)}. Only used
#'        if \code{y} is a data frame. 
#'
#' @return A data object of the same type and structure as \code{y}.
#'
#'
#' @examples
#' synthesize(cars$speed,10)
#' synthesize(cars)
#' synthesize(cars,25)
#'
#' @family synthesis
#' @export
synthesize <- function(y, n=NROW(y),correlations=1) make_synthesizer(y,correlations)(n) 




