
#' Title
#'
#' @param sigma_2
#' @param nsim
#' @param dt
#' @param init_val
#'
#' @return
#' @export simulate
#'
#' @examples

simulate <- function(nsim, sigma_2, init_val){

  bm <- c()
  bm[1] <- init_val # BM starts from init_val
  for (i in 2:nsim) {
    innov <- rnorm(1,0, sigma_2) # generate one innovation
    bm[i] <- bm[i-1] + innov # generate next value of BM
  }


  return(bm)

}




#approximate bm with linear interpolation, then use naive Montecarlo generating
# dens*length(bm) time values and evaluate the function on those values. Then, take
#the minimum time for which the function is bigger than the treshold a

#' Title
#'
#' @param bm
#' @param a
#' @param dens
#' @param ...
#'
#' @return
#' @export firstpassage
#'
#' @examples

firstpassage <- function(bm,a, dens){

  if(a<0){
    return("a must be positive")
  }

  fun <- approxfun(x=seq(1,length(bm),1), y = bm, method = "linear")
  u<- runif(dens*length(bm),0,length(bm))
  fpt <- min(na.omit(u[fun(u)>a]))

  if(fpt==Inf){
    return("level a never passed")
  }


  return(fpt)

  }





