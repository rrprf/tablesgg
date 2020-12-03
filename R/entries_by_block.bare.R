#===== Source file: ../entries_by_block.r on 2020-11-29
#-----

entries_by_block <- function(x, blocks, strict)
{
  #x <- as.tblEntries(x)  # validity checks
  rslt <- rep(list(integer(0)), nrow(blocks))
  names(rslt) <- blocks$id
  naF <- function(y) { ifelse(is.na(y), FALSE, y) }
  for (i in which(blocks[, "nr"] > 0 & blocks[, "nc"] > 0)) {
    blkarow1 <- blocks[i, "arow1"]
    blkarow2 <- blocks[i, "arow2"]
    blkacol1 <- blocks[i, "acol1"]
    blkacol2 <- blocks[i, "acol2"]
    inside <- naF(x[, "arow1"] >= blkarow1 & x[, "arow2"] <= blkarow2 & 
                  x[, "acol1"] >= blkacol1 & x[, "acol2"] <= blkacol2)
    outside <- (x[, "arow1"] > blkarow2 | x[, "arow2"] < blkarow1 | 
                x[, "acol1"] > blkacol2 | x[, "acol2"] < blkacol1)
    use <- { if (strict)  inside  else  naF(!outside) }
    rslt[[i]] <- which(use, useNames=FALSE)
  }
  rslt
}

