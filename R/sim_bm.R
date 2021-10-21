
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

simulate <- function(sigma_2, nsim, dt, init_val){

  B <- c()
  B[1] <- init_val # BM starts from init_val
  for (i in 2:nsim) {
    innov <- rnorm(1,0, sigma_2) # generate one innovation
    B[i] <- B[i-1] + innov # generate next value of BM
  }
  return(B)

}
