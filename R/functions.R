

#' @export
percent <- function(x, digits = 2, format = "f", ...) {
  paste0(formatC(100 * x, format = format, digits = digits, ...), "%")
}

#' @export
price <- function(x){
  format(x,big.mark=" ", scientific=FALSE, nsmall = 0)
}

#' @export
aoperat_options <- function(){
  options(stringsAsFactors=FALSE)
  options(max.print=100)
  options(scipen=10)
  options(Encoding="UTF-8")
  options(show.error.locations=TRUE)
  options(show.error.messages=TRUE)
  options(verbose=TRUE)
  options(warnPartialMatchArgs=TRUE)
  options(warnPartialMatchAttr=TRUE)
  options(warnPartialMatchDollar=TRUE)
  knitr::opts_chunk$set(echo = F,message=F,warning=F,cache = F)
}
