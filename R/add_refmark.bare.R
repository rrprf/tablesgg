#===== Source file: ../add_refmark.r on 2020-11-29
#-----

add_refmark <- function(text, is_math=NULL, mark, side, raise)
{
  if (is.null(is_math)) {
    MATH_prefix <- TRUE
    is_math <- grepl("^MATH_", text)
    text[] <- sub("^MATH_", "", text)
  } else  MATH_prefix <- FALSE
  wrk1 <- is_math  # text already includes plotmath
  wrk2 <- !is_math
  if (side == "before") {
    if (raise) {
      text[wrk1] <- paste0('phantom()^paste("', mark, '")*', text[wrk1])
      text[wrk2] <- paste0('phantom()^paste("', mark, '")*paste("', 
                           text[wrk2], '")')
    } else {
      text[wrk1] <- paste0('paste("', mark, '")*', text[wrk1])
      text[wrk2] <- paste0(mark, text[wrk2])
    }
  } else {
    if (raise) {
      text[wrk1] <- paste0(text[wrk1], '*phantom()^paste("', mark, '")')
      text[wrk2] <- paste0('paste("', text[wrk2], 
                           '")*phantom()^paste("', mark, '")')
    } else {
      text[wrk1] <- paste0(text[wrk1], '*paste("', mark, '")')
      text[wrk2] <- paste0(text[wrk2], mark)
    }
  }
  if (raise)  is_math[] <- TRUE
  if (MATH_prefix)  text[is_math] <- paste0("MATH_", text[is_math])
  text
}

