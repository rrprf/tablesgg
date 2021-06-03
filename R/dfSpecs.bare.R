#===== Source file: ../dfSpecs.r on 2021-06-02
#-----

dfSpecs <- function(objClass)
{
  objClass <- match.arg(objClass, c('tblEntries', 'prEntries', 'tblBlocks', 
                                    'prBlocks', 'prHvrules'))

  tblEntries <- data.frame(
    name=c(  "id",        "part",           "partrow",   "partcol",       
             "headlayer", "level_in_layer", "text"  ,    "type",          
             "textspec",  "arow1",          "arow2",     "acol1",         
             "acol2"), 
    mode=c(  "character", "character",      "numeric",   "numeric", 
             "numeric",   "numeric",        "character", "character", 
             "character", "numeric",        "numeric",   "numeric", 
             "numeric"), 
    NA_ok=c( FALSE,       FALSE,            FALSE,       TRUE, 
             FALSE,       FALSE,            FALSE,       TRUE, 
             FALSE,       FALSE,            FALSE,       FALSE, 
             FALSE), 
    stringsAsFactors=FALSE)

  prEntries <- rbind(tblEntries, grSpecs("entry")[, c("name", "mode", "NA_ok")])
  
  tblBlocks <- data.frame(
    name=c(  "id",        "nr",      "nc",      "arow1", 
             "arow2",     "acol1",   "acol2"), 
    mode=c(  "character", "numeric", "numeric", "numeric", 
             "numeric",   "numeric", "numeric"), 
    NA_ok=c( FALSE,       FALSE,     FALSE,     TRUE, 
             TRUE,        TRUE,      TRUE), 
    stringsAsFactors=FALSE)

  prBlocks <- rbind(tblBlocks, grSpecs("block")[, c("name", "mode", "NA_ok")])
  
  prHvrules <- data.frame(
    name=c(  "id",        "direction",      "arow1",    "arow2",    
             "acol1",     "acol2",          "enabled"), 
    mode=c(  "character", "character",      "numeric",  "numeric", 
             "numeric",   "numeric",        "logical"), 
    NA_ok=c( FALSE,       FALSE,            FALSE,      FALSE, 
             FALSE,       FALSE,            FALSE), 
    stringsAsFactors=FALSE)
  prHvrules <- rbind(prHvrules, grSpecs("hvrule")[, c("name", "mode", "NA_ok")])

  rslt <- get(objClass, inherits=FALSE)
  row.names(rslt) <- rslt$name
  rslt
}

#-----

df_from_spec <- function(x, n)
{
  rslt <- as.list(rep(NA, nrow(x)))
  names(rslt) <- x$name
  for (i in seq_along(rslt))  { mode(rslt[[i]]) <- x$mode[i] }
  rslt <- data.frame(rslt, row.names=NULL, stringsAsFactors=FALSE)
  rslt[rep(1, n), , drop=FALSE]
}

