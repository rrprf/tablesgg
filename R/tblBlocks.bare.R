#===== Source file: ../tblBlocks.r on 2020-11-29
#-----

tblBlocks <- function(x, rowgroupSize=0, ...)
{
  x <- { if (inherits(x, "textTable"))  tblEntries(x, ...)
         else  as.tblEntries(x) }
  rowheadInside <- attr(x, "rowheadInside")
  required <- c("id", "part", "headlayer", "level_in_layer", "arow1", "arow2", 
                "acol1", "acol2")
  if (length(chk <- setdiff(required, names(x))) > 0)  stop(
    "Following columns are required to define standard ", 
    "blocks, but are missing from 'x': ", toString(chk))
  if (is.na(rowheadInside))  stop(
    "Can't create standard blocks because 'rowheadInside' is NA")
  if (!rowheadInside && any(with(x, part == "rowhead" & headlayer == 0))) stop(
    "Found 'rowhead' entries for layer 0, but 'rowheadInside' is FALSE")

  # Dimensions of various table parts (both logical sizes, and actual sizes 
  # in the table's expanded row-column grid).
  partinfo <- tblParts(x)  # logical sizes (forces rowheadInside FALSE)
  n_rowhead <- partinfo["rowhead", "nc"]
  nc_rowhead <- { if (rowheadInside && n_rowhead > 0)  n_rowhead <- n_rowhead - 1
                  else  n_rowhead }  # actual size
  nc_body <- partinfo["body", "nc"]
  n_colhead <- partinfo["colhead", "nr"]
  nr_stub <- max(partinfo[c("colhead", "rowheadLabels"), "nr"])
  nr_prebody <- sum(partinfo[c("title", "subtitle"), "nr"]) + nr_stub
  ad <- adim(x)  # actual whole table size (respects rowheadInside)
  nr_tbl <- ad[1]
  nc_tbl <- ad[2]
  # Number of rows for rowhead_and_body (handles rowheadInside, incl edge cases):
  nr_rhbody <- nr_tbl - nr_prebody - partinfo["foot", "nr"]
  # (nr is never NA, nc is NA only for annotation parts.)
  stopifnot((nc_tbl == nc_rowhead + nc_body) || 
            # Edge case:
            (rowheadInside && nc_body == 0 && nc_rowhead == 0 && nc_tbl == 1))
  # Set 'nc' for annotation parts to table width:
  partinfo[c("title", "subtitle", "foot"), "nc"] <- nc_tbl
  
  # Alternative versions of 'min' and 'max' that return NA when all values 
  # are NA.
  min2 <- function(x) { if (length(x[!is.na(x)]) == 0)  NA_real_
                        else  min(x, na.rm=TRUE) }
  max2 <- function(x) { if (length(x[!is.na(x)]) == 0)  NA_real_
                        else  max(x, na.rm=TRUE) }

  #----- Initialization.  Whole table is block "table".
  blocks <- data.frame(id="table", type="table", subtype=NA_character_, 
                       headlayer=NA_real_, level_in_layer=NA_real_, 
                       group_in_level=NA_real_, 
                       nr=nr_tbl, nc=nc_tbl, 
                       arow1={ if (nr_tbl == 0)  NA  else  1 }, 
                       arow2={ if (nr_tbl == 0)  NA  else  nr_tbl }, 
                       acol1={ if (nc_tbl == 0)  NA  else  1 }, 
                       acol2={ if (nc_tbl == 0)  NA  else  nc_tbl }, 
                       row.names="table", stringsAsFactors=FALSE)

  #----- Each table part is a block (except that when there are interior 
  # row header entries, only the union 'rowhead_and_body' is a valid block, 
  # since 'rowhead' and 'body' are interleaved).
  if (rowheadInside) {
    id <- c("title", "subtitle", "colhead", "rowheadLabels", "rowhead_and_body", 
            "foot")
    # Need to allow for incorrect 'nr', 'nc' in 'partinfo' for rowhead, 
    # rowheadLabels, and body when rowheadInside.
    cumrows <- cumsum(c(partinfo[c("title", "subtitle", "colhead"), "nr"], 
                        nr_rhbody, partinfo["foot", "nr"]))
    cumcols <- c(nc_rowhead, nc_tbl)
    arow2 <- cumrows[c(1, 2, 3, 3, 4, 5)]
    acol2 <- cumcols[c(2, 2, 2, 1, 2, 2)]
    nr <- c(partinfo[c("title", "subtitle", "colhead", "rowheadLabels"), "nr"], 
            nr_rhbody, partinfo["foot", "nr"])
    nc <- c(nc_tbl, nc_tbl, partinfo["colhead", "nc"], nc_rowhead, nc_tbl, 
            nc_tbl)
  } else {
    id <- c("title", "subtitle", "colhead", "rowheadLabels", "rowhead", "body", 
            "foot")
    arow2 <- partinfo[id, "arow2"]
    acol2 <- partinfo[id, "acol2"]
    nr <- partinfo[id, "nr"]
    nc <- partinfo[id, "nc"]  # NA for annotation was replaced above
  }
  arow2[nr == 0] <- NA  # NA propagates to arow1, acol1
  acol2[nc == 0] <- NA
  dfr <- data.frame(id, type=id, subtype=NA_character_, 
                    headlayer=NA_real_, level_in_layer=NA_real_, 
                    group_in_level=NA_real_, nr, nc, 
                    arow1=arow2 - nr + 1, arow2, 
                    acol1=acol2 - nc + 1, acol2, 
                    row.names=id, stringsAsFactors=FALSE)
  blocks <- rbind(blocks, dfr)
  
  #----- The stub of a table is the block of cells immediately above the row 
  # headers and to the left of the column headers.  (If present, row head 
  # labels fill the bottom row of the stub.)
  dfr <- data.frame(id="stub", type="stub", subtype=NA_character_, 
                    headlayer=NA_real_, level_in_layer=NA_real_, 
                    group_in_level=NA_real_, 
                    nr=nr_stub, nc=nc_rowhead, 
                    arow1=blocks["colhead", "arow1"],
                    arow2=blocks["colhead", "arow2"],
                    acol1={ if (nc_rowhead == 0)  NA  else  1 },
                    acol2={ if (nc_rowhead == 0)  NA  else  nc_rowhead }, 
                    row.names="stub", stringsAsFactors=FALSE)
  blocks <- rbind(blocks, dfr)
  
  # Function to form the union of two or more blocks represented by rows of 
  # data frame 'blocks'.  The union is the smallest rectangular region that 
  # includes all the blocks.
  blockUnion <- function(blks, which_rows, new_id, type=new_id, 
                         subtype=NA_character_) {
    # Defined by lexical scoping:  min2, max2
    mat <- data.matrix(blks[which_rows, c("nr", "nc", "arow1", "arow2", 
                                          "acol1", "acol2")])
    rownames(mat) <- NULL
    arow1 <- min2(mat[, "arow1"])
    arow2 <- max2(mat[, "arow2"])
    acol1 <- min2(mat[, "acol1"])
    acol2 <- max2(mat[, "acol2"])
    nr <- { if (is.na(arow2 - arow1))  0  else  arow2 - arow1 + 1 }
    nc <- { if (is.na(acol2 - acol1))  0  else  acol2 - acol1 + 1 }
    dfr <- data.frame(id=new_id, type=type, subtype=subtype, 
                      headlayer=NA_real_, level_in_layer=NA_real_, 
                      group_in_level=NA_real_, nr=nr, nc=nc, 
                      arow1=arow1, arow2=arow2, acol1=acol1, acol2=acol2, 
                      row.names=new_id, stringsAsFactors=FALSE)
    dfr
  }
  
  #----- Define a block that combines the 'title' and 'subtitle' parts.
  dfr <- blockUnion(blocks, new_id="titles", which_rows=c("title", "subtitle"))
  blocks <- rbind(blocks, dfr)
  
  #----- Define blocks that combine rowhead and colhead blocks with the stub. 
  if (!rowheadInside) {  # otherwise 'rowhead' is not a valid block
    dfr1 <- blockUnion(blocks, new_id="rowhead_and_stub", 
                       which_rows=c("stub", "rowhead"))
    blocks <- rbind(blocks, dfr1)
  }
  dfr2 <- blockUnion(blocks, new_id="colhead_and_stub", 
                     which_rows=c("stub", "colhead"))
  blocks <- rbind(blocks, dfr2)

  #----- Define blocks that combine the row headers and body, and 
  # the column headers and body.
  if (!rowheadInside) {  # otherwise 'rowhead_and_body' was already defined above
    dfr1 <- blockUnion(blocks, new_id="rowhead_and_body", 
                       which_rows=c("body", "rowhead"))
    blocks <- rbind(blocks, dfr1)
  }
  dfr2 <- blockUnion(blocks, new_id="colhead_and_body", 
                     which_rows=c("body", "colhead"))
  blocks <- rbind(blocks, dfr2)
  
  #----- Additional blocks based on the hierarchical structure of row  
  # and column headers.
  # Each row in each data frame in 'rowhier', 'colhier' is one node of the 
  # hierarchy, and should have A,B,C blocks defined for it.  These blocks 
  # are unaffected by 'mergeRuns'; they reflect the logical hierarchy, not 
  # whether that hierarchy is made visually explicit.
  # NOTE:  'nr', 'nc' for table parts in 'partinfo' cannot be taken at face 
  #        value when 'rowheadInside' is TRUE.  Rely on previously defined 
  #        blocks instead.
  xhiers <- list(rowhead=attr(x, "rowhier"), colhead=attr(x, "colhier"))
  for (part in c("rowhead", "colhead")) {
    which_head <- substr(part, 1, 3)
    xhier <- xhiers[[part]]
    n_head <- length(xhier)
    if (n_head == 0)  next
    # Stack hierarchy data frames into a single data frame, one row per node.
    xhier <- do.call(rbind, xhier)
    nnodes <- nrow(xhier)
    if (nnodes > 0) {
      layer <- xhier$headlayer
      varnum <- n_head - layer + 1
      if (which_head == "row" && rowheadInside) {
        layer <- ifelse(layer == n_head, 0, layer)
      }
      # Subtype "A" blocks consist of just the entries for the node itself.
      # To determine arow, acol values for the blocks, start with their 
      # values for the upper left entry in 'x' associated with the node, then 
      # extend in the appropriate direction by 'runlen'.
      # ID of the first entry in each node:
      eid <- { if (which_head == "row")  paste("rowhead", xhier$start, varnum, 
                                               sep=",")
               else  paste("colhead", varnum, xhier$start, sep=",") }
      if (!all(okay <- (eid %in% x$id)))  stop(
        "Following entries not found in 'x': ", toString(eid[!okay], width=60))
      arow1 <- x[eid, "arow1"]
      acol1 <- x[eid, "acol1"]
      if (which_head == "row") {
        arow2 <- ifelse(layer == 0, arow1, arow1 + xhier$runlen - 1)
        acol2 <- x[eid, "acol2"]  # OK even for interior row header
      } else {
        arow2 <- x[eid, "arow2"]  # Currently, should equal arow1
        acol2 <- acol1 + xhier$runlen - 1
      }
      dfrA <- data.frame(type=paste0(which_head, "block"), subtype="A", 
                         headlayer=layer, 
                         level_in_layer=xhier[, "level_in_layer"], 
                         group_in_level=NA_real_, 
                         arow1, arow2, acol1, acol2, 
                         stringsAsFactors=FALSE)
      # Subtype "B" blocks extend from the node through all nested header 
      # layers.  Subtype "C" blocks extend type "B" blocks through the table 
      # body.
      dfrB <- dfrA
      dfrB$subtype <- "B"
      dfrC <- dfrB
      dfrC$subtype <- "C"
      if (which_head == "row") {
        # Same rows as node, but acol2 is for the innermost header layer (B) 
        # or right edge of table (C).
        dfrB$acol2 <- blocks["stub", "acol2"]
        dfrC$acol2 <- nc_tbl
        if (rowheadInside) {
          # Need to modify "B" & "C" blocks in layer 0 to contain just the 
          # rows below 'rowblock/A/0/level', not 'rowblock/A/0/level' itself.
          lyr0 <- (layer == 0)
          if (nc_rowhead + nc_body > 0) {
            dfrB[lyr0, "arow1"] <- dfrA[lyr0, "arow1"] + 1
            dfrB[lyr0, "arow2"] <- dfrA[lyr0, "arow1"] + xhier[lyr0, "runlen"]
            dfrC[lyr0, c("arow1", "arow2")] <- dfrB[lyr0, c("arow1", "arow2")]
            if (nc_rowhead == 0) {
              # 'rowblock/B/0/*' has no columns.
              dfrB[lyr0, c("acol1", "acol2")] <- NA_real_
            }
          } else {
            # 'rowblock/B/0/*' and 'rowblock/C/0/*' are both empty.
            dfrB[lyr0, c("arow1", "arow2", "acol1", "acol2")] <- NA_real_
            dfrC[lyr0, c("arow1", "arow2", "acol1", "acol2")] <- NA_real_
          }
        }
      } else {
        # Same columns as node ("A" block), but arow2 is for the innermost 
        # header layer (B) or bottom of table body (C).
        dfrB$arow2 <- blocks["stub", "arow2"]
        dfrC$arow2 <- blocks["stub", "arow2"] + nr_rhbody
      }
      dfr2 <- rbind(dfrA, dfrB, dfrC)
      id <- with(dfr2, paste(type, subtype, headlayer, level_in_layer, sep="/"))
      nr <- with(dfr2, ifelse(is.na(arow2 - arow1), 0, arow2 - arow1 + 1))
      nc <- with(dfr2, ifelse(is.na(acol2 - acol1), 0, acol2 - acol1 + 1))
      dfr2 <- data.frame(id, dfr2[, c("type", "subtype", "headlayer", 
                                      "level_in_layer", "group_in_level")], 
                         nr, nc, 
                         dfr2[, c("arow1", "arow2", "acol1", "acol2")], 
                         row.names=id, stringsAsFactors=FALSE)
      blocks <- rbind(blocks, dfr2)
    }
  }

  #----- Blocks for row groups.
  if (rowgroupSize > 0) {
    grpblks <- make_rowgroups(blocks, rowheadruns=attr(x, "rowhier"), 
                              rowgroupSize=rowgroupSize)
    blocks <- rbind(blocks, grpblks)
  }
  
  stopifnot(!any(duplicated(blocks$id)), !anyNA(blocks$nr), 
            !anyNA(blocks$nc))
  stopifnot(with(blocks, all((is.na(arow2-arow1) & nr==0) | 
                             (!is.na(arow2-arow1) & nr==arow2-arow1+1 & 
                              nr > 0))))
  stopifnot(with(blocks, all((is.na(acol2-acol1) & nc==0) | 
                             (!is.na(acol2-acol1) & nc==acol2-acol1+1 & 
                              nc > 0))))

  # Set 'had_enabled_entries' field.
  had_enabled_entries <- rep(NA, nrow(blocks))
  wch_entries <- entries_by_block(x, blocks=blocks, strict=FALSE)  # list of index vectors
  for (i in seq_along(wch_entries)) {
    had_enabled_entries[i] <- any(x[wch_entries[[i]], "enabled"])
  }
  blocks$had_enabled_entries <- had_enabled_entries
  
  row.names(blocks) <- blocks$id
  blocks <- structure(blocks, rowheadInside=rowheadInside, 
                      rowgroupSize=rowgroupSize)
  class(blocks) <- c("tblBlocks", "data.frame")
  blocks
}

