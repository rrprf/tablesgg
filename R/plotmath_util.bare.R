#===== Source file: ../plotmath_util.r on 2020-11-29
#-----

paste_pm <- function(x, ..., sep=c("", "*"))
{
  dots <- list(...)
  if (length(dots) == 0)  return(x[, c("text", "math"), drop=FALSE])
  sep <- rep(sep, length.out=2)
  x <- c(list(x), dots)
  nr <- max(sapply(x, nrow))
  if (nr == 0)  return(x[, c("text", "math"), drop=FALSE])
  x <- lapply(x, function(y) { 
              if (!("fontface" %in% names(y)))  y$fontface <- rep(1, nrow(y))
              y[rep(seq_len(nrow(y)), length.out=nr), 
                c("text", "math", "fontface"), drop=FALSE] })
  narg <- length(x)
  tlst <- lapply(x, "[[", "text")
  plst <- lapply(x, "[[", "math")
  flst <- lapply(x, "[[", "fontface")
  # If any in a set of strings to be concatenated contains plotmath, then 
  # so does the concatenated string:
  math <- rep(FALSE, nr)
  for (j in seq_len(narg))  math <- (math | plst[[j]])
  # If the set of strings mixes different font faces, also use plotmath.
  for (j in seq_len(narg-1))  math <- (math | (flst[[narg]] != flst[[j]]))
  
  # For rows which ultimately will be plotmath, wrap the non-plotmath 
  # substrings according to their font face.
  for (j in seq_len(narg)) {
    wch <- (math & !(plst[[j]]))  # need wrapping for plotmath
    if (any(grepl('"', tlst[[j]][wch])))  stop(
      'Unable to handle double quote within strings to be displayed with plotmath')
    face <- c("plain", "bold", "italic", "bolditalic")[flst[[j]][wch]]
    tlst[[j]][wch] <- paste0(face, '("', tlst[[j]][wch], '")')
  }
  
  text <- rep(NA_character_, nr)
  text[!math] <- do.call(paste, c(lapply(tlst, "[", !math), list(sep=sep[1])))
  text[math] <- do.call(paste, c(lapply(tlst, "[", math), list(sep=sep[2])))
                                   
  data.frame(text, math, stringsAsFactors=FALSE)
}

#-----

sci_fmt_pm <- function(x)
{
  stopifnot(is.character(x))
  # Regular expression matching scientific notation as produced by 'formatC'.
  re1 <- "([-+]?[[:digit:]]*\\.?[[:digit:]]*)[eE]([-+]?[[:digit:]]+)"
  has_sci <- grepl(re1, x)
  # Remove superfluous '+' and leading 0's in exponent.
  x[has_sci] <- sub("[eE]\\+0*", "e", x[has_sci])
  x[has_sci] <- sub("[eE]-0*", "e-", x[has_sci])
  paste0(ifelse(has_sci, "MATH_", ""), sub(re1, "plain('\\1') %*% 10^{\\2}", x))
}

