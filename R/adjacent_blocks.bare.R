#===== Source file: ../adjacent_blocks.r on 2020-11-29
#-----

adjacent_blocks <- function(x)
{
  id <- x[, "id"]
  nblk <- nrow(x)
  rslt <- array(list(character(0)), dim=c(nblk, 4), 
                dimnames=list(id, c("top", "right", "bottom", "left")))

  # In each call to 'outer', the first argument represents the index block 
  # A (row of 'rslt') and the second argument represents blocks potentially 
  # adjacent to it, B (column of 'rslt').
  dcolBA <- outer(x[, "acol1"], x[, "acol2"], FUN="-")  # > 0 => B is left of A
  # outer(x[, "acol2"], x[, "acol1"], FUN="-") == -t(dcolBA)
  drowBA <- outer(x[, "arow1"], x[, "arow2"], FUN="-")  # > 0 => B is above A
  # outer(x[, "arow2"], x[, "arow1"], FUN="-") == -t(drowBA)
  use <- !is.na(dcolBA + t(dcolBA) + drowBA + t(drowBA))
  
  col_overlap <- (use & dcolBA <= 0 & t(dcolBA) <= 0)
  row_overlap <- (use & drowBA <= 0 & t(drowBA) <= 0)
  # B adjacent to A along the top edge of A:
  adjacent <- (use & drowBA == 1 & col_overlap)
  rslt[, "top"] <- lapply(seq_len(nblk), function(i) { id[adjacent[i, ]] })
  
  # B adjacent to A along the right edge of A:
  adjacent <- (use & t(dcolBA) == 1 & row_overlap)
  rslt[, "right"] <- lapply(seq_len(nblk), function(i) { id[adjacent[i, ]] })
  
  # B adjacent to A along the bottom edge of A:
  adjacent <- (use & t(drowBA) == 1 & col_overlap)
  rslt[, "bottom"] <- lapply(seq_len(nblk), function(i) { id[adjacent[i, ]] })
  
  # B adjacent to A along the left edge of A:
  adjacent <- (use & dcolBA == 1 & row_overlap)
  rslt[, "left"] <- lapply(seq_len(nblk), function(i) { id[adjacent[i, ]] })

  rslt
}

