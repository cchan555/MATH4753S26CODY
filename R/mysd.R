

mysd <- function(x, na.rm = TRUE) {
    if(na.rm) {
      x<- x[!is.na(x)]
    }
  sqrt(var(x))
}
