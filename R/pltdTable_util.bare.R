#===== Source file: ../pltdTable_util.r on 2021-06-02
#-----

pltdSize <- function(x, units=c("mm", "inches", "cm"))
{
  units <- match.arg(units)
  if (!inherits(x, "pltdTable"))  stop(
    "This function only works with 'pltdTable' objects")
  size_mm <- attr(x, "size_mm")
  if (is.null(size_mm))  size_mm <- structure(c(NA_real_, NA_real_), 
                                              device=NA_character_)
  structure( size_mm / (c("inches"=25.4, "mm"=1, "cm"=10)[units]), 
             units=units)
}

#-----

print.pltdTable <- function(x, scale=NULL, newpage=TRUE, position=c(0.5, 0.5), 
                            vpx=grid::unit(0.5, "npc"), 
                            vpy=grid::unit(0.5, "npc"), just="center", ...)
{
  wrkx <- { if (is.null(scale))  x  else  update(x, scale=scale) }
  sz <- pltdSize(wrkx, "mm")
  stopifnot(inherits(wrkx, "ggplot"))
  if (!missing(position)) {
    position <- rep(position, length.out=2)
    if (is.character(position)) {
      position <- substr(position, 1, 1)
      position <- unname(c("l"=0, "b"=0, "c"=0.5, "r"=1, "t"=1)[position])
    }
    if (!missing(vpx) || !missing(vpy) || !missing(just))  warning(
      "Ignoring args 'vpx', 'vpy', 'just' in favor of 'position'")
    vpx <- grid::unit(position[1], "npc")
    vpy <- grid::unit(position[2], "npc")
    just <- position
  }
  vp <- grid::viewport(x=vpx, y=vpy, just=just, 
                       width=grid::unit(sz[1], "mm"), 
                       height=grid::unit(sz[2], "mm"), 
                       ...)
  class(wrkx) <- class(wrkx)[class(wrkx) != "pltdTable"]
  print(wrkx, newpage=newpage, vp=vp)
  invisible(x)  # 'print' methods are supposed to return 'x' unmodified.
}

