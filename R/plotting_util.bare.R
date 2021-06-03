#===== Source file: ../plotting_util.r on 2021-06-02
#-----

angle_adj <- function(hsize, vsize, theta)
{
  if (theta == 0)  return(c(hsize, vsize))
  theta <- theta * (2*pi/360)  # radians
  costh <- cos(theta)
  sinth <- sin(theta)
  # Matrix with coordinates of corners of the rectangle (assume one corner is 
  # the origin).
  arr <- matrix(c(0,0, hsize,0, 0,vsize, hsize,vsize), nrow=2)
  rotmat <- matrix(c(costh, sinth, -sinth, costh), nrow=2)
  rotarr <- rotmat %*% arr
  w <- diff(range(rotarr[1, ]))
  h <- diff(range(rotarr[2, ]))
  c(w, h)
}

#-----

coord_justif <- function(df, x0, x1, y0, y1, size, align=df$align)
{
  # Angle must be a multiple of 90 degrees.
  angle <- df[, "angle"]
  std_angle <- ifelse(is.na(angle) | angle == 360, 0, 
                 ifelse(angle < 0, angle + 360, angle))
  if (any(chk <- !(std_angle %in% c(0, 90, 180, 270))))  stop(
    "Text rotation angle is not a multiple of 90 deg for entries: ", 
    toString(row.names(df)[chk], width=40))
  
  n <- nrow(df)
  hsize <- size[, "hsize"]
  vsize <- size[, "vsize"]
  hj <- df[, "hjust"]
  vj <- df[, "vjust"]
  if (is.null(align))  align <- rep(NA_real_, n)
  x <- rep(NA_real_, n)
  y <- rep(NA_real_, n)
  gg_hjust <- rep(NA_real_, n)
  gg_vjust <- rep(NA_real_, n)
  # Move cell boundaries inward based on padding.
  x0 <- x0 + df[, "hpad"]
  x1 <- x1 - df[, "hpad"]
  y0 <- y0 + df[, "vpad"]
  y1 <- y1 - df[, "vpad"]
  # Space included for text descenders is like extra padding on one side. 
  # (Which side depends on angle.)  Generally we want to cancel out this 
  # extra padding by moving the boundary on that side back out.
  if (!internalOpt[["rcsize_include_descenders"]]) {
    descender <- size[, "descender"]
    chg <- (std_angle == 0)
    y1[chg] <- y1[chg] + descender[chg]
    chg <- (std_angle == 90)
    x1[chg] <- x1[chg] + descender[chg]
    chg <- (std_angle == 180)
    y0[chg] <- y0[chg] - descender[chg]
    chg <- (std_angle == 270)
    x0[chg] <- x0[chg] - descender[chg]
  }
  # Starting values for x, y.  (These will be modified based on 'align' and 
  # 'angle'.)
  xwrk <- x0 + (x1 - x0)*hj
  ywrk <- y0 + (y1 - y0)*vj
  # Consider each angle separately.
  if (any(use <- (std_angle == 0))) {
    align[use] <- ifelse(is.na(align[use]), hj[use], align[use])
    gg_hjust[use] <- align[use]
    gg_vjust[use] <- 1 - vj[use]
    x[use] <- xwrk[use] + (align[use] - hj[use])*hsize[use]
    y[use] <- ywrk[use]
  }
  if (any(use <- (std_angle == 90))) {
    align[use] <- ifelse(is.na(align[use]), 1 - vj[use], align[use])
    gg_hjust[use] <- align[use]
    gg_vjust[use] <- hj[use]
    x[use] <- xwrk[use] + (1 - (2*hj[use]))*hsize[use]
    y[use] <- ywrk[use] + (1 - align[use] - vj[use])*vsize[use]
  }
  if (any(use <- (std_angle == 180))) {
    align[use] <- ifelse(is.na(align[use]), 1 - hj[use], align[use])
    gg_hjust[use] <- align[use]
    gg_vjust[use] <- vj[use]
    x[use] <- xwrk[use] + (1 - align[use] - hj[use])*hsize[use]
    y[use] <- ywrk[use]
  }
  if (any(use <- (std_angle == 270))) {
    align[use] <- ifelse(is.na(align[use]), vj[use], align[use])
    gg_hjust[use] <- align[use]
    gg_vjust[use] <- 1 - hj[use]
    x[use] <- xwrk[use] + (1 - (2*hj[use]))*hsize[use]
    y[use] <- ywrk[use] + (align[use] - vj[use])*vsize[use]
  }
  rslt <- data.frame(x, y, gg_hjust, gg_vjust, 
                     row.names=row.names(df), stringsAsFactors=FALSE)
  stopifnot(!any(sapply(rslt, anyNA)))
  rslt
}

