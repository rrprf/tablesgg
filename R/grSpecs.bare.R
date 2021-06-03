#===== Source file: ../grSpecs.r on 2021-06-02
#-----

grSpecs <- function(type)
{
  type <- match.arg(type, c("entry", "block", "hvrule"))

  entry <- data.frame(
    name=c(    "hjust",        "vjust",     "color",      "alpha",   "size", 
               "family",       "fontface",  "lineheight", "angle",   "hpad", 
               "vpad",         "fill",      "fill_alpha", "border_size", 
               "border_color", "minwidth",  "maxwidth"), 
    mode=c(    "numeric",      "numeric",   "character",  "numeric", "numeric", 
               "character",    "numeric",   "numeric",    "numeric", "numeric", 
               "numeric",      "character", "numeric",    "numeric",     
               "character",    "numeric",   "numeric"), 
    NA_ok=c(   FALSE,          FALSE,       TRUE,         TRUE,      FALSE, 
               TRUE,           TRUE,        TRUE,         TRUE,      FALSE, 
               FALSE,          TRUE,        TRUE,         FALSE, 
               TRUE,           TRUE,        TRUE), 
    scalable=c(FALSE,          FALSE,       FALSE,        FALSE,     TRUE, 
               FALSE,          FALSE,       FALSE,        FALSE,     TRUE, 
               TRUE,           FALSE,       FALSE,        TRUE, 
               FALSE,          TRUE,        TRUE), 
    stringsAsFactors=FALSE)

  block <- data.frame(
    name=c(     "fill",         "fill_alpha", "border_size", "border_color"), 
    mode=c(     "character",    "numeric",    "numeric",     "character"), 
    NA_ok=c(    TRUE,           FALSE,        FALSE,         TRUE), 
    scalable=c( FALSE,          FALSE,        TRUE,          FALSE), 
    stringsAsFactors=FALSE)
    
  hvrule <- data.frame(
    name=c(     "linetype",   "size",      "color",      "alpha", 
                "space",      "fill",      "fill_alpha"), 
    mode=c(     "numeric",    "numeric",   "character",  "numeric", 
                "numeric",    "character", "numeric"),
    NA_ok=c(    FALSE,        FALSE,       TRUE,         TRUE, 
                FALSE,        TRUE,        TRUE), 
    scalable=c( FALSE,        FALSE,       FALSE,        FALSE, 
                TRUE,         FALSE,       FALSE), 
    stringsAsFactors=FALSE)

  rslt <- get(type, inherits=FALSE)
  row.names(rslt) <- rslt$name
  rslt
}

