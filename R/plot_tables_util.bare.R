#===== Source file: ../plot_tables_util.r on 2020-11-29
#-----

angle_adj <- function(width, height, theta)
{
  if (theta == 0)  return(c(width, height))
  theta <- theta * (2*pi/360)  # radians
  costh <- cos(theta)
  sinth <- sin(theta)
  # Matrix with coordinates of corners of the rectangle (assume one corner is 
  # the origin).
  arr <- matrix(c(0,0, width,0, 0,height, width,height), nrow=2)
  rotmat <- matrix(c(costh, sinth, -sinth, costh), nrow=2)
  rotarr <- rotmat %*% arr
  w <- diff(range(rotarr[1, ]))
  h <- diff(range(rotarr[2, ]))
  c(w, h)
}

#-----

entrySize_mm <- function(entryInfo)
{
  stopifnot(inherits(entryInfo, "data.frame"))
  n <- nrow(entryInfo)
  width <- rep(NA_real_, n)
  height <- rep(NA_real_, n)
  # If no graphics device is already open, the 'grid::convert*' functions 
  # will open one.  Rather than letting a default window be opened on screen, 
  # silently open a temporary PDF file and delete it at the end.
#  if (dev.cur() == 1) {  # null device
  if (TRUE) {
    devcur <- dev.cur()
    grfile <- paste0(tempfile(), ".pdf")
    pdf(grfile)
    on.exit({dev.off();  unlink(grfile);  if (devcur > 1)  dev.set(devcur) })
  }
  for (i in seq_len(n)) {
    vp <- grid::viewport(gp=grid::gpar(fontsize=entryInfo[i, "size"], 
                                       fontfamily=entryInfo[i, "family"], 
                                       fontface=entryInfo[i, "fontface"], 
                                       lineheight=entryInfo[i, "lineheight"], 
                                       cex=1))
    grid::pushViewport(vp, recording=FALSE)
    value <- entryInfo[i, "text"]
    if (entryInfo[i, "math"])  value <- parse(text=value)
    w <- grid::convertWidth(grid::unit(1, units="strwidth", data=value), 
                            "mm", valueOnly=TRUE)
    h <- grid::convertHeight(grid::unit(1, units="strheight", data=value), 
                             "mm", valueOnly=TRUE)
    if ((angle <- entryInfo[i, "angle"]) == 0) {
      width[i] <- w
      height[i] <- h
    } else {
      # 'angle' is not a valid 'gpar' argument, and adding 'angle' arg to 
      # 'viewport' does not affect reported string width, height.  So need to 
      #  do the trigonometry here.
      wh <- angle_adj(w, h, angle)
      width[i] <- wh[1]
      height[i] <- wh[2]
    }
    grid::popViewport(recording=FALSE)
  }
  structure(data.frame(width=width, height=height, 
                       row.names=row.names(entryInfo), stringsAsFactors=FALSE), 
            device=names(dev.cur()))
}

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

