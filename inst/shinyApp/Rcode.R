
#' Calcualtes R Squared for Piece Wise Linear Regression of Two Knots
#'
#' Given two knots and the original data, this function calculates the
#' R squared value by use of the lm() function.
#'
#' @param xk1 Numeric Vector
#'
#' @param xk2 Numeric Vector
#'
#' @param data Numeric dataframe
#'
#' @return R Squared Numeric Vector
#'
#' @examples
#' knot1 = 4
#' knot2 = 5
#' data = rnorm(10, mean=5, sd=2)
#' rsq = rsqMult(knot1, knot2, data)
#'
rsqMult = function(xk1, xk2, data) { # data=spruce.df
  if(xk1 >= xk2) {
    return(0)
  }
  df=within(data, {
    X<-(BHDiameter-xk1)*(BHDiameter>xk1)
    X2<-(BHDiameter-xk2)*(BHDiameter>xk2)
  }
  )
  lmp=lm(Height ~ BHDiameter + X + X2, data=df)
  return(summary(lmp)$r.squared)
}



#' Calcualtes the Coefficients for Piece Wise Linear Regression of Two Knots
#'
#' Given two knots and the original data, this function calculates the
#' Beta coefficients by use of the lm() function.
#'
#' @param xk1 Numeric Vector
#'
#' @param xk2 Numeric Vector
#'
#' @param data Numeric dataframe
#'
#' @return Coefficients Numeric Vector
#'
#' @examples
#' knot1 = 4
#' knot2 = 5
#' data = rnorm(10, mean=5, sd=2)
#' cf = coeff(knot1, knot2, data)
#'
coeff = function(xk,xk2,data){ # data=spruce.df
  df=within(data, {
    X<-(BHDiameter-xk)*(BHDiameter>xk)
    X2<-(BHDiameter-xk2)*(BHDiameter>xk2)
  }
  )
  lmp=lm(Height ~ BHDiameter + X + X2, data=df)
  coef(lmp)
}

#' Calcualtes the Y value for Piece Wise Linear Regression of Two Knots
#'
#' Given an x value, two knots, beta coefficients, and the original data, this function calculates the
#' Y value.
#'
#' @param x Numeric Vector
#'
#' @param xk1 Numeric Vector
#'
#' @param xk2 Numeric Vector
#'
#' @param coef Numeric Vector
#'
#' @return Predicted Y value Vector
#'
#' @examples
#' x = 3
#' knot1 = 4
#' knot2 = 5
#' cf = coeff(knot1, knot2, data)
#' y = myf2(x, knot1, knot2, cf)
#'
myf2 = function(x,xk1,xk2,coef){
  coef[1]+coef[2]*(x) + coef[3]*(x-xk1)*(x-xk1>0)+ coef[4]*(x-xk2)*(x-xk2>0)
}
