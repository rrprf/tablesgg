#===== Source file: ../tblHvrules.r on 2020-11-29
#-----

tblHvrules <- function(x)
{
  stopifnot(inherits(x, "tblBlocks"))
  nblk <- NROW(x)
  
  # Template for a 'tblHvrules' data frame:
  hvrules <- data.frame("id"=character(0), "direction"=character(0), 
                        "block"=character(0), "side"=character(0), 
                        "adjacent_blocks"=character(0), 
                        "arow1"=numeric(0), "arow2"=numeric(0), 
                        "acol1"=numeric(0), "acol2"=numeric(0), 
                        "enabled"=logical(0), stringsAsFactors=FALSE)

  if (nblk > 0) {
    adj <- adjacent_blocks(x)  # nblk x 4 list array
    x$block <- x$id
    # Generate four hvrules per block, one for each side.
    # An hrule will be assigned a half-integer for its 'arow', a vrule will 
    # be assigned a half-integer for its 'acol'.
    h1 <- within(x, { direction <- "hrule";  side <- "top"
                      a <- arow1 - 0.5;  arow1 <- a;  arow2 <- a })
    h1$adjacent_blocks <- vapply(adj[, "top"], paste, character(1), 
                                 collapse=";", USE.NAMES=FALSE)    
    h2 <- within(x, { direction <- "hrule";  side <- "bottom"
                      a <- arow2 + 0.5;  arow1 <- a;  arow2 <- a })
    h2$adjacent_blocks <- vapply(adj[, "bottom"], paste, character(1), 
                                 collapse=";", USE.NAMES=FALSE)    
    v1 <- within(x, { direction <- "vrule";  side <- "left"
                      a <- acol1 - 0.5;  acol1 <- a;  acol2 <- a })
    v1$adjacent_blocks <- vapply(adj[, "left"], paste, character(1), 
                                 collapse=";", USE.NAMES=FALSE)    
    v2 <- within(x, { direction <- "vrule";  side <- "right"
                      a <- acol2 + 0.5;  acol1 <- a;  acol2 <- a })
    v2$adjacent_blocks <- vapply(adj[, "right"], paste, character(1), 
                                 collapse=";", USE.NAMES=FALSE)    
    hv <- rbind(h1, h2, v1, v2)
    hv$id <- with(hv, paste(block, side, sep="_"))
    hv$adjacent_blocks <- trimws(hv$adjacent_blocks)
    hv$enabled <- rep(FALSE, nrow(hv))
    hvrules <- hv[order(hv$direction, hv$block, hv$side), 
                  names(hvrules), drop=FALSE]
  }
  
  row.names(hvrules) <- hvrules$id
  class(hvrules) <- c("tblHvrules", "data.frame")
  hvrules
}

