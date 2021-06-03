#===== Source file: ../notANumber.r on 2021-06-02
#-----

notANumber <- function(x, result=c("logical", "values")) 
{
  if (!is.character(x))  stop("'x' is not a character vector")
  x[is.na(x) | x == ""] <- "0"
  xx <- suppressWarnings(as.numeric(x))   # Invalid strings will become NA
  result <- match.arg(result)
  if (result == "logical")  is.na(xx)  else  unique(x[is.na(xx)])
}

