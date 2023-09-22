PBS <- function(correlation){
  percent <- function(x, digits = 2, format = "f", ...) {
    paste0(formatC(100 * x, format = format, digits = digits, ...), "%")
  }
  # If I take two random people from a bivariate gaussian with a correlation of X, the person that scores higher on X has a PBS percent chance to score higher on Y. 
  output <- (asin(correlation)/pi)+.5
  description <- paste0("If I take two random people from a bivariate gaussian with a correlation of ",round(correlation,3),", the person that scores higher on X has a ",percent(output)," percent chance to score higher on Y. ")
  return( list(output, description ) )
}
