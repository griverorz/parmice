#' Check parallel setup
#'
#' Checks that there is a registered backend.
setup_parallel <- function() {
    if (foreach::getDoParWorkers() == 1) {
        stop("No parallel backend registered")
  }
}

#' Parallel mice
#'
#' Thin wrapper around mice to perform imputations in parallel
#' @param ... Arguments passed to \code{mice}
#' @param paropts A named list of arguments passed to \code{foreach}
#' @return A \code{mids} object
#' @examples
#' ncores <- 2
#' cl <- makeCluster(ncores)
#' clusterSetRNGStream(cl, 31313)
#' registerDoParallel(cl)
#' parmice(nhanes, m=5, paropts=list(".combine"=ibind))
#' @export
parmice <- function(..., paropts=list(".combine"=ibind)) {
    
    setup_parallel()
    ncores <- foreach::getDoParWorkers()
    
    ## Create a call to mice
    ## Passing dot-dot-dot would print the evaluated data in the Call
    Call <- match.call(expand.dots=TRUE)
    Call[[1L]] <- as.name("mice")
    
    if (!"m" %in% names(Call)) {
        stop("Number of imputations is missing")
    }
    m <- Call$m

    if (!is.na(Call$seed)) {
        warning("Deleting seed. It should be set via clusterSetRNGStream")
        Call$seed <- NA ## Do not overwrite the seed from the cluster
    }
    
    ## Load mice package
    paropts$.packages <- "mice"
    
    parargs <- names(paropts)
    forargs <- names(formals(foreach::foreach))
    for (i in parargs) {
        if (!i %in% forargs) {
            stop("Argument ", dQuote(i), " not recognized by foreach")
        }
    }

    i <- seq_len(ceiling(m/ncores))
    fe_call <- as.call(c(list(quote(foreach::foreach), i), paropts))
    fe <- eval(fe_call)
    
    result <- foreach::`%dopar%`(fe, eval.parent(Call))

    return(result)
}

