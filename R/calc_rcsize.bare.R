#===== Source file: ../calc_rcsize.r on 2021-06-02
#-----

calc_rcsize <- function(entryInfo, vrule_hsize, hrule_vsize, sizeAdjust, 
                        nominal_rcmin, method)
{
  # Nomenclature:
  #  'size' refers to a dimension in the table's frame of reference: 'h' 
  #    prefix meaning horizontal, 'v' prefix meaning vertical.
  #  'width' (or 'wd'), 'height' (or 'ht') refer to a dimension in the 
  #    frame of reference of a piece of text: 'width' being in the direction 
  #    of reading English text, 'height' being perpendicular to that direction.
  #  'x' prefix means a dimension includes the effect of padding and 
  #    'sizeAdjust' in the appropriate direction.
  
  WRK_MAXITER <- internalOpt[["rcsize_maxiter"]]
  WRK_VERBOSE <- internalOpt[["rcsize_verbose"]]
  include_descenders <- internalOpt[["rcsize_include_descenders"]]
  enabled <- entryInfo[, "enabled"]
  stopifnot(all(enabled))  # want sizes for disabled entries to be NA, but 
                           # that hasn't been implemented
  nentries <- nrow(entryInfo)
  arow1 <- entryInfo[, "arow1"]
  arow2 <- entryInfo[, "arow2"]
  acol1 <- entryInfo[, "acol1"]
  acol2 <- entryInfo[, "acol2"]
  # Omit hvrule sizes at top and left edges of the table, since they are never 
  # spanned.
  hrule_vsize <- hrule_vsize[-1]
  nr_tbl <- length(hrule_vsize)
  vrule_hsize <- vrule_hsize[-1]  
  nc_tbl <- length(vrule_hsize)
  
  # Text dimensions in the table's reference frame, after rotation but without 
  # wrapping or padding or sizeAdjust; i.e., natural size.
  sz <- entrySize_mm(entryInfo, allowWrap=FALSE)
  hsize0 <- sz$hsize
  vsize0 <- sz$vsize
  horiz <- sz$horiz
  wrap <- sz$wrap
  if (!include_descenders) {
    hsize0 <- hsize0 - ifelse(horiz, 0, sz$descender)
    vsize0 <- vsize0 - ifelse(horiz, sz$descender, 0)
  }
  hpad <- entryInfo[, "hpad"]
  vpad <- entryInfo[, "vpad"]
  
  # Entry sizes after 'sizeAdjust' and adding padding ("extended size").  Set 
  # to 0 for disabled entries.
  xhsize <- ifelse(enabled, hsize0*sizeAdjust[1] + 2*hpad, 0)
  xvsize <- ifelse(enabled, vsize0*sizeAdjust[2] + 2*vpad, 0)
    
  # Dimensions in the text's reference frame.  These will be flipped relative 
  # to the table's if rotation is 90 or 270 degrees.
  xwidth <- ifelse(horiz, xhsize, xvsize)
  width0 <- ifelse(horiz, hsize0, vsize0)
  wdpad <- ifelse(horiz, hpad, vpad)
  wdAdj <- ifelse(horiz, sizeAdjust[1], sizeAdjust[2])
  
  minwidth <- entryInfo[, "minwidth"]
  maxwidth <- entryInfo[, "maxwidth"]  # may be NA (not equivalent to Inf)
  # Negative min/max width = multiple of natural width.  Convert to absolute 
  # width.
  minwidth <- ifelse(minwidth < 0, abs(minwidth)*width0*wdAdj + 2*wdpad, minwidth)
  maxwidth <- ifelse(maxwidth < 0, abs(maxwidth)*width0*wdAdj + 2*wdpad, maxwidth)
  # minwidth should always allow for padding.
  minwidth <- pmax(minwidth, 2*wdpad, na.rm=TRUE)  # never NA
  # minwidth and maxwidth should be unconstrained for disabled entries.
  minwidth[!enabled] <- 0
  maxwidth[!enabled] <- Inf
  if (any(chk <- (!is.na(maxwidth) & maxwidth < 2*wdpad)))  stop(
    "'maxwidth' is less than padding width for entries: ", 
    toString(row.names(entryInfo)[chk], width=60))
  if (any(chk <- (!is.na(maxwidth) & maxwidth < minwidth)))  stop(
    "'minwidth' is greater than 'maxwidth' for entries: ", 
    toString(row.names(entryInfo)[chk], width=60))

  # Alternate versions of 'min', 'max' functions that return NA if their 
  # argument is empty or all NA.
  min2 <- function(z) { if (all(is.na(z)))  NA  else  min(z, na.rm=TRUE) }
  max2 <- function(z) { if (all(is.na(z)))  NA  else  max(z, na.rm=TRUE) }

  #-----
  # Identify sets of entries that span the same columns, and sets that span 
  # the same rows.
  acol12 <- paste(acol1, acol2, sep=" ")
  esetsCol <- split(seq_len(nentries), factor(acol12), drop=TRUE)
  arow12 <- paste(arow1, arow2, sep=" ")
  esetsRow <- split(seq_len(nentries), factor(arow12), drop=TRUE)
  nsetsCol <- length(esetsCol)
  nsetsRow <- length(esetsRow)
  
  # List with the columns and vrule sizes spanned by each set.
  spannedCol <- vector("list", nsetsCol)
  spannedVrule_size <- numeric(nsetsCol)
  for (i in seq_len(nsetsCol)) {
    idx <- esetsCol[[i]][1]  # index into one representative row of 'entryInfo'
    spanned <- (acol1[idx]):(acol2[idx])
    spannedCol[[i]] <- spanned
    spannedVrule_size[i] <- sum(vrule_hsize[head(spanned, -1)])
  }
  # List with the rows and hrule sizes spanned by each set.
  spannedRow <- vector("list", nsetsRow)
  spannedHrule_size <- numeric(nsetsRow)
  for (i in seq_len(nsetsRow)) {
    idx <- esetsRow[[i]][1]  # index into one representative row of 'entryInfo'
    spanned <- (arow1[idx]):(arow2[idx])
    spannedRow[[i]] <- spanned
    spannedHrule_size[i] <- sum(hrule_vsize[head(spanned, -1)])
  }

  #-----
  rcsize_simple <- function(nrc, arc1, arc2, xsize, esets, spanned_rc, rulesize)
  # Function to calculate sizes of individual rows or columns based on the 
  # sizes of the table entries that are contained within them, or span them.
  # A simple heuristic is used to expand individual column or row sizes if 
  # needed to accomodate entries that span multiple colums or rows.  (Do 
  # not expand hvrule sizes.) 
    # nrc : Number of rows or columns in the table.
    # arc* : 'arow*' or 'acol*', vectors for entries.
    # xsize : Vector of sizes (in the appropriate direction) of each 
    #     table entry, including padding and 'sizeAdjust'.  Entries with a 
    #     value of NA are ignored in the calculations.
    # esets : List with one component per set of entries that span a given 
    #     set of rows or columns.  Each component is a vector of indices into 
    #     entries in 'xsize'.
    # spanned_rc : List like 'esets', but with each component a vector with 
    #     the row or column numbers spanned by the set of entries.
    # rulesize : Vector like 'esets', with each element the sum of the 
    #     sizes of hrules (if we're dealing with rows) or vrules (if we're 
    #     dealing with columns) spanned by a set of entries.
  # The returned value is a vector of length 'nrc', containing row or column 
  # sizes large enough to accomodate all contained and spanning entries.  
  {
    rcsize <- rep_len(0, nrc)
    nspanned <- lengths(spanned_rc)
    ord <- order(nspanned)
    for (i in ord) {  # start with single row/col entries, move on to multi
      eidx <- esets[[i]]
      rcidx <- spanned_rc[[i]]
      esize <- max2(xsize[eidx])  # size needed for this entry set; may be NA
      spanned_rcsize <- sum(rcsize[rcidx]) # size currently allocated 
      spanned_rulesize <- rulesize[i]
      if (is.na(esize) || esize <= spanned_rcsize + spanned_rulesize)  next
      if (spanned_rcsize > 0) {
        # Expand spanned rows/columns proportionately.
        expfac <- max(1, (esize - spanned_rulesize) / spanned_rcsize)
        rcsize[rcidx] <- expfac * rcsize[rcidx]
      } else {
        # Expand (empty) spanned rows/columns by the same absolute amount.
        rcsize[rcidx] <- max(0, (esize - spanned_rulesize) / length(rcidx))
      }
    }
    rcsize
  }
  
  #-----
  calc_xsize <- function(hsizeCol, vsizeRow)
  # Function to calculate the effective size of each entry implied by the 
  # sizes of the columns or rows it spans.  This includes padding and 
  # sizeAdjust ('extended size'), as well as any spanned hvrules.
  {
    # Defined by lexical scoping:  nentries, esetsCol, spannedCol, esetsRow, 
    #   spannedRow, spannedVrule_size, spannedHrule_size
    h_implied <- rep(NA_real_, nentries)
    for (i in seq_along(esetsCol)) {
      spanned <- spannedCol[[i]]
      h_implied[esetsCol[[i]]] <- sum(hsizeCol[spanned]) + spannedVrule_size[i]
    }
    v_implied <- rep(NA_real_, nentries)
    for (i in seq_along(esetsRow)) {
      spanned <- spannedRow[[i]]
      v_implied[esetsRow[[i]]] <- sum(vsizeRow[spanned]) + spannedHrule_size[i]
    }
    list(xhsize=h_implied, xvsize=v_implied)
  }
  
  #-----
  if (method == 0) {
    # Row and column sizes, without constraints, based on natural sizes of 
    # entries and no wrapping.
    hsizeCol <- rcsize_simple(nc_tbl, arc1=acol1, arc2=acol2, xsize=xhsize, 
                               rulesize=spannedVrule_size, esets=esetsCol, 
                               spanned_rc=spannedCol)
    vsizeRow <- rcsize_simple(nr_tbl, arc1=arow1, arc2=arow2, xsize=xvsize, 
                               rulesize=spannedHrule_size, esets=esetsRow, 
                               spanned_rc=spannedRow)
  } else if (method == 1) { 
    # Use 'minwidth' constraints but not 'maxwidth' (no wrapping).
    minhsize <- ifelse(horiz, pmax(xhsize, minwidth), xhsize)
    minvsize <- ifelse(horiz, xvsize, pmax(xvsize, minwidth))
    hsizeCol <- rcsize_simple(nc_tbl, arc1=acol1, arc2=acol2, xsize=minhsize, 
                               rulesize=spannedVrule_size, esets=esetsCol, 
                               spanned_rc=spannedCol)
    vsizeRow <- rcsize_simple(nr_tbl, arc1=arow1, arc2=arow2, xsize=minvsize, 
                               rulesize=spannedHrule_size, esets=esetsRow, 
                               spanned_rc=spannedRow)
  } else {
    # Methods 2-5: (a) 'maxwidth' constraints are respected; (b) final 
    # column or row sizes are calculated from a formal constrained 
    # optimization problem.
    
    stopifnot(requireNamespace("quadprog", quietly=TRUE))
    # Calculate row and column sizes by minimizing a quadratic measure of 
    # the difference from their initialized sizes, subject to the minwidth and 
    # maxwidth constraints on entry widths.  Methods 2-5 differ only in 
    # the specific difference measure that is used.
    nparm <- nc_tbl + nr_tbl
    idx_vert <- which(!horiz)
    idx_horiz <- which(horiz)
    maxwidth_orig <- entryInfo[, "maxwidth"]  # to be restored at end
    hsize <- hsize0  # initialize
    vsize <- vsize0
    wrap_width <- xwidth  # initialize: natural width
    new_wrap_width <- pmin(maxwidth, xwidth, na.rm=TRUE)  # never NA
    
    # Constraint matrix for the QP.  'nparm' rows.  Each column defines a 
    # sum of certain column widths or row heights, namely, those columns or 
    # rows spanned by a particular set of entries.  (This maps the 'minwidth'/
    # 'maxwidth' constraints on entries to constraints on the columns or rows 
    # containing them.)
    nsets <- nsetsCol + nsetsRow
    Amat <- matrix(0, nrow=nparm, ncol=nsets)
    for (i in seq_len(nsetsCol)) {
      rcidx <- spannedCol[[i]]
      Amat[rcidx, i] <- 1  # defines a sum of column widths 
    }
    for (i in seq_len(nsetsRow)) {
      rcidx <- spannedRow[[i]]
      Amat[nc_tbl + rcidx, nsetsCol + i] <- 1  # defines a sum of row heights
    }
    spanned_rulesize <- c(spannedVrule_size, spannedHrule_size)
    
    for (niter in seq_len(WRK_MAXITER+1)) {
      # It is possible that wrapping entries in one direction will trigger 
      # or avoid constraints in the other, so may need to cycle a few times.
      
      # Recalculate entry sizes for those that need to be wrapped, or whose 
      # wrap width has changed.
      recalc <- (enabled & abs(new_wrap_width - wrap_width) > 0.01)
      if (WRK_VERBOSE > 0) {
        cat("Method =", method, ", niter =", niter, ", # recalc entries =", 
            sum(recalc), "\n")
      }
      if (sum(recalc) > 0) {
        if (niter > WRK_MAXITER)  stop(
          "Method = ", method, ", niter = ", niter, ": limit reached but ", 
          sum(recalc), " entry sizes are still changing")
        wrap_width <- new_wrap_width
        entryInfo[recalc, "maxwidth"] <- wrap_width[recalc]
        sz[recalc, ] <- entrySize_mm(entryInfo[recalc, , drop=FALSE], 
                                     allowWrap=TRUE, sizeAdjust=sizeAdjust)
        hsize[recalc] <- sz$hsize[recalc]
        vsize[recalc] <- sz$vsize[recalc]
        wrap[recalc] <- sz$wrap[recalc]
        if (!include_descenders) {
          hsize[recalc] <- hsize[recalc] - ifelse(horiz[recalc], 0, 
                                                  sz$descender[recalc])
          vsize[recalc] <- vsize[recalc] - ifelse(horiz[recalc], 
                                                  sz$descender[recalc], 0)
        }
        # Entry sizes after 'sizeAdjust' and adding padding ("extended size"). 
        # Set to 0 for disabled entries.
        xhsize <- ifelse(enabled, hsize*sizeAdjust[1] + 2*hpad, 0)
        xvsize <- ifelse(enabled, vsize*sizeAdjust[2] + 2*vpad, 0)
        xwidth <- ifelse(horiz, xhsize, xvsize)
      } else {
        if (niter > 1)  break  # have to go through the for-loop at least once
      }
    
      # Define the objective function to be minimized: squared deviations 
      # from a natural/preferred hsize for each column, vsize for each row.
      #-- Preferred size is from method 0 applied to the (possibly updated) 
      #   entry sizes.
      # 'xsize' should be NA for entries with NA maxwidth, so that they 
      # are not counted in determining column widths or row heights (depending 
      # on their orientation).
      wrk_xhsize <- ifelse(horiz & is.na(maxwidth), NA, xhsize)
      hsizeCol2 <- rcsize_simple(nc_tbl, arc1=acol1, arc2=acol2, 
                                 xsize=wrk_xhsize, 
                                 esets=esetsCol, spanned_rc=spannedCol, 
                                 rulesize=spannedVrule_size)
      wrk_xvsize <- ifelse(!horiz & is.na(maxwidth), NA, xvsize)
      vsizeRow2 <- rcsize_simple(nr_tbl, arc1=arow1, arc2=arow2, 
                                 xsize=wrk_xvsize, 
                                 esets=esetsRow, spanned_rc=spannedRow, 
                                 rulesize=spannedHrule_size)
      size0 <- c(hsizeCol2, vsizeRow2)
      if (any(chk <- is.na(size0)))  stop(
        "Too many entries have NA for 'maxwidth'; unable to determine ", 
        "widths for columns: ", toString(which(chk[1:nc_tbl])), 
        "; heights for rows: ", toString(which(chk[nc_tbl + (1:nr_tbl)])))
                              
      #-- The quadratic objective function to be minimized.
      denom <- { if (method %in% c(2, 3))  pmax(size0, nominal_rcmin)
                 else if (method %in% c(4, 5))  size0 + nominal_rcmin }
      Dmat_factorized <- diag(denom, nrow=nparm)
        # so Dmat = diag((1 / denom)^2, nrow=nparm)
      dvec <- { if (method %in% c(2, 4))  size0/(denom^2)
                else if (method %in% c(3, 5))  1/denom }
      
      # Constraints on hsize for each column, vsize for each row.
      #-- Min and max horizontal and vertical sizes for each entry.  For 
      #   horizontal entries: horizontal size limits are taken from minwidth, 
      #   maxwidth; vertical size has a lower bound of 'xvsize', no upper 
      #   bound.  Vice versa for vertical entries.
      minhsize <- ifelse(horiz, minwidth, xhsize)
      maxhsize <- ifelse(horiz, maxwidth, Inf)  # may be NA
      minvsize <- ifelse(!horiz, minwidth, xvsize)
      maxvsize <- ifelse(!horiz, maxwidth, Inf)  # may be NA
      
      #-- Convert min, max constraints on horizontal size of entries to 
      #   constraints on sums of horizontal sizes of each set of columns.
      bvecCol_min <- rep(0, nsetsCol)
      bvecCol_max <- rep(NA_real_, nsetsCol)
      for (i in seq_len(nsetsCol)) {
        ttl_rulesize <- spanned_rulesize[i]
        eidx <- esetsCol[[i]]
        # Lower and upper constraints on the sum:
        bvecCol_min[i] <- max(0, max2(minhsize[eidx]) - ttl_rulesize)  # never NA
        bvecCol_max[i] <- max(0, min2(maxhsize[eidx]) - ttl_rulesize, na.rm=FALSE)  # may be NA
      }
      #-- Same for constraints on vertical size of each set of rows.
      bvecRow_min <- rep(0, nsetsRow)
      bvecRow_max <- rep(NA_real_, nsetsRow)
      for (i in seq_len(nsetsRow)) {
        ttl_rulesize <- spanned_rulesize[nsetsCol + i]
        eidx <- esetsRow[[i]]
        # Lower and upper constraints on the sum:
        bvecRow_min[i] <- max(0, max2(minvsize[eidx]) - ttl_rulesize)  # never NA
        bvecRow_max[i] <- max(0, min2(maxvsize[eidx]) - ttl_rulesize, na.rm=FALSE)  # may be NA
      }
      # Combine constraints for column and row sizes. 
      btemp_min <- c(bvecCol_min, bvecRow_min)
      btemp_max <- c(bvecCol_max, bvecRow_max)
      #-- Duplicate the constraints, with first 'nsets' columns representing 
      #   'minsize' constraints; second 'nsets' columns representing 'maxsize' 
      #   constraints.  
      Amat_wrk <- cbind(Amat, -1*Amat)
      bvec <- c(btemp_min, -1*btemp_max)  # constraint = lower bounds on sums
      #-- Identify equality constraints.
      eq <- (!is.na(btemp_max) & abs(btemp_max - btemp_min) < 0.01)
      bvec[nsets + which(eq)] <- NA  # max constraint is redundant
      #   Move equality constraints to the front.
      eq <- c(eq, rep(FALSE, nsets))
      Amat_wrk <- cbind(Amat_wrk[, eq, drop=FALSE], Amat_wrk[, !eq, drop=FALSE])
      bvec <- c(bvec[eq], bvec[!eq])
      #-- Remove redundant or useless (Inf or NA) constraints.
      Amat_wrk <- Amat_wrk[, is.finite(bvec), drop=FALSE]
      bvec <- bvec[is.finite(bvec)]
      if (WRK_VERBOSE > 1) {
        cat("   nparm =", nparm, ", nsets =", nsets, ", # constraints =", 
            length(bvec), ", # equality constr. =", sum(eq), "\n")
      }
      
      # Solve for column/row sizes.
      if (length(bvec) == 0) {
        # Effectively no constraints, so initial solution is okay.
        size <- size0
      } else {
        neq <- sum(eq)
        soln <- try(quadprog::solve.QP(Dmat=Dmat_factorized, dvec=dvec, 
                                       Amat=Amat_wrk, bvec=bvec, meq=sum(eq), 
                                       factorized=TRUE), 
                    silent=TRUE)
        if (inherits(soln, "try-error"))  stop(
          "Unable to find row, column sizes that satisfy 'minwidth', ", 
          "'maxwidth' constraints for all entries")
        size <- soln$solution
      }
      hsizeCol <- head(size, nc_tbl)
      vsizeRow <- tail(size, nr_tbl)
      # Get the size for each entry implied by the sizes of the rows, columns,  
      # and hvrules it spans.  Use them to update 'wrap_width'.
      effective_size <- calc_xsize(hsizeCol, vsizeRow)
      effective_width <- ifelse(horiz, effective_size$xhsize, 
                                effective_size$xvsize)
      new_wrap_width <- pmin(xwidth, effective_width, maxwidth, na.rm=TRUE)
      if (WRK_VERBOSE > 2) {
        message("Creation of data frames for debugging is disabled ", 
                "(not allowed by CRAN checks)")
        #assign(paste0("rcsize_WIDTHS", niter), 
        #       data.frame(xwidth, effective_width, minwidth, maxwidth, 
        #                  wrap_width, new_wrap_width, 
        #                  row.names=row.names(entryInfo)), 
        #       envir=.GlobalEnv)
      }
    }
    entryInfo[, "maxwidth"] <- maxwidth_orig
  }

  # Get the size for each entry implied by the sizes of the rows and columns  
  # it spans:
  effective_size <- calc_xsize(hsizeCol, vsizeRow)
  # Check whether any constraints are violated for enabled entries.
  effective_width <- ifelse(horiz, effective_size$xhsize, 
                            effective_size$xvsize)
  if (any(chk <- (enabled & (effective_width < minwidth - 0.01))))  warning(
    "Width is less than 'minwidth' for entries: ", 
    toString(row.names(entryInfo)[chk], width=60))
  if (any(chk <- (enabled & !is.na(maxwidth) & 
                  (effective_width > maxwidth + 0.01)))) {
    details <- toString(row.names(entryInfo)[chk], width=60)
    if (tablesggOpt("allowWrap"))  stop(
      "Wrapping failed; width is greater than 'maxwidth' for entries: ", 
      details)
    warning("Text wrapping is disabled and width is greater than 'maxwidth' ", 
            "for entries: ", details)
  }
  # 'sizeInfo' should look like return from 'entrySize_mm()'.  Do *not* 
  # subtract descender size from 'hsize', 'vsize', since they may be passed 
  # to 'geom_textbox', which expects descenders to be included.
  sizeInfo <- sz
  attr(sizeInfo, "device") <- attr(sz, "device")
  list(hsizeCol=hsizeCol, vsizeRow=vsizeRow, sizeInfo=sizeInfo)
}

