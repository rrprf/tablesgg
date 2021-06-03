#===== Source file: ../format_xtable.r on 2021-06-02
#-----

format_xtable <- function(x, row.names=TRUE, na="NA", mathExponents=TRUE, ...)
{
  pos <- { if (row.names)  1  else  0 }
  format.args <- list(...)
  if (is.null(format.args$decimal.mark)) {
      format.args$decimal.mark <- options()$OutDec
  }
  if (any(c("digits", "display") %in% names(format.args)))  stop(
    "Formatting args 'digits' and 'display' are not allowed")

  cols <- matrix("", nrow = nrow(x), ncol = ncol(x) + pos)
  if (row.names)  cols[, 1] <- row.names(x)
  varying.digits <- is.matrix(attr(x, "digits", exact = TRUE))
  for (i in 1:ncol(x)) {
    xcol <- x[, i]
    if (is.factor(xcol)) 
        xcol <- as.character(xcol)
    if (is.list(xcol)) 
        xcol <- sapply(xcol, unlist)
    ina <- is.na(xcol)
    is.numeric.column <- is.numeric(xcol)
    if (is.character(xcol)) {
        cols[, i + pos] <- xcol
    } else {
      if (!varying.digits) {
          curFormatArgs <- c(list(x = xcol, format = ifelse(attr(x, 
            "digits", exact = TRUE)[i + 1] < 0, "E", attr(x, 
            "display", exact = TRUE)[i + 1]), digits = abs(attr(x, 
            "digits", exact = TRUE)[i + 1])), format.args)
          cols[, i + pos] <- do.call("formatC", curFormatArgs)
      } else {
          for (j in 1:nrow(cols)) {
            curFormatArgs <- c(list(x = xcol[j], format = ifelse(attr(x, 
              "digits", exact = TRUE)[j, i + 1] < 0, "E", 
              attr(x, "display", exact = TRUE)[i + 1]), 
              digits = abs(attr(x, "digits", exact = TRUE)[j, 
                i + 1])), format.args)
            cols[j, i + pos] <- do.call("formatC", curFormatArgs)
          }
      }
    }
    if (any(ina))  cols[ina, i + pos] <- na
    if (is.numeric.column) {
        if (mathExponents)  cols[, i + pos] <- sci_fmt_pm(cols[, i + pos])
    }
  }
  cols
}

