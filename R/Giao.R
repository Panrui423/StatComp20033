#' @title Use leave-two-out cross validation to compare the models.
#' @descriptionuse The proposed models for (Y) from (X) are:Linear: Y = \beta_0+\beta_1X+\epsilon;Quadratic: Y = \beta_0+\beta_1X+\beta_2X^2+\epsilon;Exponential: log(Y) = log(\beta_0)+\beta_1X+\epsilon;Log-Log: log(Y) = \beta_0+\beta_1log(X)+\epsilon.
#' @importFrom stats lm
#' @param y the dependent variable data.
#' @param x the independent variable data.
#' @return Mean square error of the four models.
#' @examples
#' \dontrun{
#' two_fold(mtcars$mpg,mtcars$cyl)
#' }
#' @export
two_fold<-function(y,x){
  n <- length(y)
  e1 <- e2 <- e3 <- e4 <- matrix(1,n,n)

  # fit models on leave-two-out samples
  for (i in 1:(n-1)) {
    for (k in (i+1):n) {
      y1 <- y[-c(i,k)]
      x1 <- x[-c(i,k)]

      J1 <- lm(y1 ~ x1)
      yhat1 <- J1$coef[1] + J1$coef[2] * x[k]
      yhat1_ <- J1$coef[1] + J1$coef[2] * x[i]
      e1[i,k] <- y[k] - yhat1
      e1[k,i] <- y[i] - yhat1_


      J2 <- lm(y1 ~ x1 + I(x1^2))
      yhat2 <- J2$coef[1] + J2$coef[2] * x[k]   +J2$coef[3] * x[k]^2
      yhat2_ <- J2$coef[1] + J2$coef[2] * x[i] +J2$coef[3] * x[i]^2
      e2[i,k] <- y[k] - yhat2
      e2[k,i] <- y[i] - yhat2_

      J3 <- lm(log(y1) ~ x1)
      logyhat3 <- J3$coef[1] + J3$coef[2] * x[k]
      yhat3 <- exp(logyhat3)
      logyhat3_ <- J3$coef[1] + J3$coef[2] * x[i]
      yhat3_ <- exp(logyhat3_)
      e3[i,k] <- y[k] - yhat3
      e3[k,i] <- y[i] - yhat3_

      J4 <- lm(log(y1) ~ log(x1))
      logyhat4 <- J4$coef[1] + J4$coef[2] * log(x[k])
      yhat4 <- exp(logyhat4)
      e4[i,k] <- y[k] - yhat4
      logyhat4_ <- J4$coef[1] + J4$coef[2] * log(x[i])
      yhat4_ <- exp(logyhat4_)
      e4[k,i] <- y[i] - yhat4_
    }
  }

  e11<-e22<-e33<-e44<-numeric()
  e11<-c(e1[which(upper.tri(e1))],e1[which(lower.tri(e1))])
  e22<-c(e2[which(upper.tri(e2))],e2[which(lower.tri(e2))])
  e33<-c(e3[which(upper.tri(e3))],e3[which(lower.tri(e3))])
  e44<-c(e4[which(upper.tri(e4))],e4[which(lower.tri(e4))])

  MSE<-c(mean(e11^2), mean(e22^2), mean(e33^2), mean(e44^2))

  Models<-c('Linear','Quadratic','Exponential','Log-Log')
  cbind(Models,MSE)
}
