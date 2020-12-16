#' @title Generating samples from Maxwell distribution
#' @descriptionuse Generating samples from Maxwell distribution by using the relationship with normal distribution
#' @importFrom stats rnorm
#' @param n the number of samples
#' @param sigma the parameter of Maxwell distribution
#' @return a random sample of size \code{n}
#' @examples
#' \dontrun{
#' rmaxwell(100,2)
#' }
#' @export
rmaxwell<-function(n,sigma){
  x<-rnorm(n,0,sigma)
  y<-rnorm(n,0,sigma)
  z<-rnorm(n,0,sigma)
  maxwell<-sqrt(x^2+y^2+z^2)
  return(maxwell)
}




