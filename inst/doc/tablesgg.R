## ---- include=FALSE-----------------------------------------------------------
knitr::opts_chunk$set(echo=TRUE, fig.align="center")

## ---- echo=FALSE--------------------------------------------------------------
ggplot2::theme_update(plot.background=ggplot2::element_rect(fill=NA))

## ---- eval=(Sys.getenv("RUNNING_PKG_SETUP", unset="") == "")------------------
library(tablesgg)

## -----------------------------------------------------------------------------
str(iris2)

## ---- echo=FALSE--------------------------------------------------------------
plt <- plot(textTable(head(iris2)))
sz <- pltdSize(plt, units="in")

## ---- fig.width=sz[1], fig.height=sz[2]---------------------------------------
plot(textTable(head(iris2)))

## ---- eval=FALSE--------------------------------------------------------------
#  library(ggplot2)
#  theme_update(plot.background=element_rect(fill=NA))

## -----------------------------------------------------------------------------
library(tables)
iris2_tab <- tabular(Species*Heading()*value*Format(digits=2)*(mean + sd) ~ 
                     Heading("Flower part")*flower_part*Heading()*direction, 
                     data=iris2)

## ---- echo=FALSE--------------------------------------------------------------
plt <- plot(textTable(iris2_tab))
sz <- pltdSize(plt, units="in")

## ---- fig.width=sz[1], fig.height=sz[2]---------------------------------------
plot(textTable(iris2_tab))

## -----------------------------------------------------------------------------
methods(textTable)

## ---- echo=TRUE, eval=FALSE---------------------------------------------------
#  plot(textTable(head(iris2)))

## ---- echo=FALSE--------------------------------------------------------------
plt <- plot(textTable(head(iris2), row.names="Obs. #"))
sz <- pltdSize(plt, units="in")

## ---- fig.width=sz[1], fig.height=sz[2]---------------------------------------
plot(textTable(head(iris2), row.names="Obs. #"))

## ---- echo=TRUE, eval=FALSE---------------------------------------------------
#  iris2_tab <- tabular(Species*Heading()*value*Format(digits=2)*(mean + sd) ~
#                       Heading("Flower part")*flower_part*Heading()*direction,
#                       data=iris2)

## ---- echo=TRUE, eval=FALSE---------------------------------------------------
#  plot(textTable(iris2_tab))

## ---- echo=TRUE---------------------------------------------------------------
ttbl <- textTable(iris2_tab, title="The iris data", 
                  subtitle=c("Summary statistics by species", 
                             "A second subtitle line"), 
                  foot="sd = standard deviation")

## ---- echo=FALSE--------------------------------------------------------------
sz <- pltdSize(plot(ttbl), units="in")

## ---- fig.width=sz[1], fig.height=sz[2]---------------------------------------
plot(ttbl)

## ---- echo=FALSE--------------------------------------------------------------
tmp <- plot(ttbl, title="A new title", subtitle=character(0))
sz <- pltdSize(tmp, units="in")

## ---- fig.width=sz[1], fig.height=sz[2]---------------------------------------
# Change the main title, remove the subtitles.
plot(ttbl, title="A new title", subtitle=character(0))

## ---- echo=TRUE---------------------------------------------------------------
plt <- plot(iris2_tab, title="The iris data")
pltdSize(plt)

## ---- echo=TRUE, eval=FALSE---------------------------------------------------
#  sz <- pltdSize(plt, units="in")   # R expects device dimensions in inches
#  dev.new(width=sz[1], height=sz[2])
#  plt

## ---- echo=FALSE--------------------------------------------------------------
tmp <- plot(iris2_tab, scale=0.8, title="The iris data (scale=0.8)")
sz <- pltdSize(tmp, units="in")

## ---- fig.width=sz[1], fig.height=sz[2]---------------------------------------
plt2 <- plot(iris2_tab, scale=0.8, title="The iris data (scale=0.8)")
plt2

## -----------------------------------------------------------------------------
plt1 <- plot(iris2_tab, title="The iris data", subtitle="With rowheadInside = TRUE", 
             rowheadInside=TRUE)
plt2 <- plot(textTable(iris2[1:9, ]), title="The first 9 rows of 'iris2'", 
             subtitle="In groups of 4 (rowgroupSize=4)", rowgroupSize=4)

## ---- echo=FALSE--------------------------------------------------------------
sz1 <- pltdSize(plt1, units="in")
sz2 <- pltdSize(plt2, units="in")

## ---- fig.width=sz1[1]+sz2[1]+0.5, fig.height=max(sz1[2], sz2[2])-------------
print(plt1, position=c("left", "center"))
print(plt2, position=c("right", "center"), newpage=FALSE)

## -----------------------------------------------------------------------------
ttbl <- textTable(iris2_tab, title=paste0("MATH_plain('The length of vector')~", 
                                    "group('(', list(a, b), ')')~plain('is ')~", 
                                    "sqrt(a^2 + b^2)"))

## ---- echo=FALSE--------------------------------------------------------------
sz <- pltdSize(plot(ttbl), units="in")

## ---- fig.width=sz[1], fig.height=sz[2]---------------------------------------
plot(ttbl)

## -----------------------------------------------------------------------------
ttbl <- textTable(iris2_tab, foot="sd = standard deviation")
ttbl <- addRefmark(ttbl, mark="a", before="sd =", after="sd$", raise=TRUE)

## ---- echo=FALSE--------------------------------------------------------------
sz <- pltdSize(plot(ttbl), units="in")

## ---- fig.width=sz[1], fig.height=sz[2]---------------------------------------
plot(ttbl)

## ---- echo=FALSE, fig.align="center"------------------------------------------
partid <- c("title", "subtitle", "rowhead", "rowheadLabels", "colhead", 
            "body", "foot")
partcolor <- c("#1B9E77", "#D95F02", "#7570B3", "#E7298A", "#66A61E", 
               "#E6AB02", "#A6761D")

ttbl <- textTable(iris2_tab, title="The iris data", 
                  subtitle=c("Summary statistics by species", 
                             "A second subtitle line"), 
                  foot="sd = standard deviation")
plt <- plot(ttbl)
for (i in seq_along(partid)) {
  props(plt, id=partid[i]) <- element_block(fill=partcolor[i], fill_alpha=0.5)
}
# Create a color key as a simple 2-column table:
key <- data.frame("Part"=c("Title", "Subtitle", "Row header", 
                           "Row header labels", "Column header", "Body", 
                           "Foot lines"), 
                  "Part ID"=partid, stringsAsFactors=FALSE, check.names=FALSE)
keyplt <- plot(textTable(key, row.names=FALSE))
for (i in seq_along(partid)) {
  propsd(keyplt, subset=(part == "body" & partcol == 1 & partrow == i)) <- 
    element_entry(fill=partcolor[i], fill_alpha=0.5)
  # Add extra space between each row.
  if (i < 7)  keyplt <- addHvrule(keyplt, direction="hrule", 
                                  arow=arow(keyplt, "body")[i] + 0.5, 
                                  props=element_hvrule(linetype=0, space=2))
}
propsd(keyplt, subset=TRUE) <- element_entry(hjust=0, family="serif")
propsd(keyplt, subset=(part == "body" & partcol == 2)) <- 
  element_entry(family="mono")
propsd(keyplt, subset=enabled) <- element_hvrule(linetype=0)

# Show the tables side by side.
sz1 <- pltdSize(plt, units="in")
sz2 <- pltdSize(keyplt, units="in")
sz <- c(sz1[1] + sz2[1] + 0.5, max(sz1[2], sz2[2]))

## ---- echo=FALSE, fig.width=sz[1], fig.height=sz[2]---------------------------
print(plt, position=c(0, 0.5))
print(keyplt, position=c(1, 0.5), newpage=FALSE)

## -----------------------------------------------------------------------------
summary(ttbl)

## -----------------------------------------------------------------------------
adim(ttbl)

## ---- echo=FALSE--------------------------------------------------------------
plt1 <- plot(ttbl, title="Highlight borders of table entries")
plt2 <- plot(ttbl, title="Highlight hvrules")
propsd(plt1, subset=enabled) <- element_entry(border_color="blue", border_size=0.5)
propsd(plt2, subset=enabled) <- element_hvrule(fill="red", fill_alpha=0.3)
# Show the tables side by side.
sz1 <- pltdSize(plt1, units="in")
sz2 <- pltdSize(plt2, units="in")
sz <- c(sz1[1] + sz2[1] + 0.5, max(sz1[2], sz2[2]) + 0.5)

## ---- echo=FALSE, fig.width=sz[1], fig.height=sz[2]---------------------------
print(plt1, position=c(0, 0.5))
print(plt2, position=c(1, 0.5), newpage=FALSE)

## ---- echo=TRUE---------------------------------------------------------------
plt1 <- plot(ttbl, title="Default style for entries")
plt2 <- plot(ttbl, entryStyle=styles_pkg$entryStyle_pkg_base, 
             title="The 'base' style for entries")

## ---- echo=FALSE--------------------------------------------------------------
# Show the tables side by side.
sz1 <- pltdSize(plt1, units="in")
sz2 <- pltdSize(plt2, units="in")
sz <- c(sz1[1] + sz2[1] + 0.5, max(sz1[2], sz2[2]))

## ---- echo=TRUE, fig.width=sz[1], fig.height=sz[2]----------------------------
print(plt1, position=c("left", "center"))
print(plt2, position=c("right", "center"), newpage=FALSE)

## -----------------------------------------------------------------------------
subttbl <- ttbl[-4, c(1,2,5,6,3,4)]
# Also change annotation:
subttbl <- update(subttbl, title="Example of subscripting a 'textTable'")

## ---- echo=FALSE--------------------------------------------------------------
sz <- pltdSize(plot(subttbl), units="in")

## ---- fig.width=sz[1], fig.height=sz[2]---------------------------------------
plot(subttbl)

## -----------------------------------------------------------------------------
i <- arow(ttbl, "colhead")[1]  # row number of first column header row
j1 <- acol(ttbl, "rowhead")  # column numbers for row header
j2 <- acol(ttbl, "colhead")  # column numbers for column header
subttbl2 <- ttbl[-i, c(j1, j2[c(3,4,1,2)])]
subttbl2 <- update(subttbl2, title="Example of subscripting a 'textTable'")
identical(subttbl, subttbl2)

## -----------------------------------------------------------------------------
plt1 <- plot(ttbl)
plt2 <- update(plt1, scale=0.8)
plt3 <- update(plt2, scale=1.0)
rbind(pltdSize(plt1), pltdSize(plt2), pltdSize(plt3))

## ---- echo=TRUE---------------------------------------------------------------
head(elements(plt1, type="entry"))

## -----------------------------------------------------------------------------
plt <- plot(ttbl)
props(plt, id="body") <- element_entry(fontface=3, fill="gray85")
props(plt, id="subtitle,2") <- element_entry(text="Properties changed", 
                                            fill="gray85")
props(plt, id="rowhead_right") <- element_hvrule(linetype=1, color="black")

## ---- echo=FALSE--------------------------------------------------------------
sz <- pltdSize(plt, units="in")

## ---- fig.width=sz[1], fig.height=sz[2]---------------------------------------
plt

## -----------------------------------------------------------------------------
plt <- plot(textTable(iris2_tab, foot="sd = standard deviation"))
props(plt, regex="^sd$") <- element_refmark(mark="*", side="after")
props(plt, regex="^sd =") <- element_refmark(mark="*", side="before")

## ---- echo=FALSE--------------------------------------------------------------
sz <- pltdSize(plt, units="in")

## ---- fig.width=sz[1], fig.height=sz[2]---------------------------------------
plt

## -----------------------------------------------------------------------------
propsa(plt, arows=c(5, 7, 9), acols=5) <- element_entry(color="red")

## ---- echo=FALSE--------------------------------------------------------------
sz <- pltdSize(plt, units="in")

## ---- fig.width=sz[1], fig.height=sz[2]---------------------------------------
plt

## -----------------------------------------------------------------------------
propsa(plt, arows=arow(plt, hpath=c(NA, "mean")), 
       acols=acol(plt, id="body")) <- element_entry(fontface=2)

## ---- echo=FALSE--------------------------------------------------------------
sz <- pltdSize(plt, units="in")

## ---- fig.width=sz[1], fig.height=sz[2]---------------------------------------
plt

## -----------------------------------------------------------------------------
plt <- plot(textTable(iris2_tab))
propsd(plt, subset=(enabled)) <- element_hvrule(color="red")
propsd(plt, subset=(part == "colhead" & headlayer == 1)) <- 
       element_entry(angle=90, hjust=0.5, vjust=0.5)

## ---- echo=FALSE--------------------------------------------------------------
sz <- pltdSize(plt, units="in")

## ---- fig.width=sz[1], fig.height=sz[2]---------------------------------------
plt

## ---- echo=TRUE, fig.align="center"-------------------------------------------
plt <- addBlock(plt, arows=c(6, 7), acols=c(3, 4), 
                props=element_block(border_color="red", border_size=1.0), 
                enabled=TRUE)

## ---- echo=FALSE--------------------------------------------------------------
sz <- pltdSize(plt, units="in")

## ---- fig.width=sz[1], fig.height=sz[2]---------------------------------------
plt

## ---- echo=TRUE, fig.align="center"-------------------------------------------
plt <- addHvrule(plt, direction="vrule", acols=4.5, arows=arow(plt, "body"), 
                 props=element_hvrule(linetype=2, color="blue"), enabled=TRUE)

## ---- echo=FALSE--------------------------------------------------------------
sz <- pltdSize(plt, units="in")

## ---- fig.width=sz[1], fig.height=sz[2]---------------------------------------
plt

## ---- echo=TRUE, eval=FALSE---------------------------------------------------
#  plt + theme(plot.background=element_rect(fill=NA, color="black", size=1))

## ---- echo=TRUE---------------------------------------------------------------
styles_pkg$entryStyle_pkg_1

## ---- echo=TRUE---------------------------------------------------------------
styles_pkg$blockStyle_pkg_1[, 1:5]

## ---- eval=FALSE--------------------------------------------------------------
#  {
#  #  ... code to create character vectors/matrices for table parts, then ...
#  z <- list(title=title, subtitle=subtitle, rowhead=rowhead,
#            rowheadLabels=rowheadLabels, colhead=colhead, body=body, foot=foot)
#  # Invoke 'textTable' on the list to finish up processing and for validity
#  # checks (uses the default method).
#  textTable(z)
#  }

## ---- echo=FALSE--------------------------------------------------------------
ttbl <- textTable(iris2_tab, foot="sd = standard deviation")
ttblA <- update(ttbl, title="Highlight a 'colblock' of subtype 'A'", 
                subtitle="ID of the highlighted block is 'colblock/A/2/1'")
pltA <- plot(ttblA, scale=0.9)
props(pltA, id="colblock/A/2/1") <- element_block(fill="gray85", 
  border_color="red", border_size=0.9)
ttblB <- update(ttbl, title="Highlight a 'colblock' of subtype 'B'", 
                subtitle="ID of the highlighted block is 'colblock/B/2/1'")
pltB <- plot(ttblB, scale=0.9)
props(pltB, id="colblock/B/2/1") <- element_block(fill="gray85", 
  border_color="red", border_size=0.9)
ttblC <- update(ttbl, title="Highlight a 'colblock' of subtype 'C'", 
                subtitle="ID of the highlighted block is 'colblock/C/2/1'")
pltC <- plot(ttblC, scale=0.9)
props(pltC, id="colblock/C/2/1") <- element_block(fill="gray85", 
  border_color="red", border_size=0.9)
szA <- pltdSize(pltA, units="in")
szB <- pltdSize(pltB, units="in")
szC <- pltdSize(pltC, units="in")
sz <- c(szA[1] + szB[1] + 1.0, max(szA[2], szB[2]) + szC[2] + 1.0)

## ---- echo=FALSE, fig.width=sz[1], fig.height=sz[2]---------------------------
print(pltA, vpx=0.25, vpy=0.75)
print(pltB, vpx=0.75, vpy=0.75, newpage=FALSE)
print(pltC, vpx=0.5, vpy=0.25, newpage=FALSE)

## ---- echo=FALSE--------------------------------------------------------------
# Row header blocks when 'rowheadInside' is TRUE
ttblABC <- update(ttbl, title=c("Highlight a set of layer-0 row header blocks", 
                                "('rowheadInside' set to TRUE)"))
plt <- plot(ttblABC, rowheadInside=TRUE)
props(plt, id="rowblock/A/0/2") <- element_block(border_color="red", border_size=1.0, fill=NA)
props(plt, id="rowblock/B/0/2") <- element_block(fill="gray85")
props(plt, id="rowblock/C/0/2") <- element_block(border_color="blue", border_size=1.0, fill=NA)

# Create a key as a simple 2-column table:
key <- data.frame("Block subtype"=c("A", "B", "C"), 
                  "Block ID"=paste0("rowblock/", c("A", "B", "C"), "/0/2"), 
                  stringsAsFactors=FALSE, check.names=FALSE)
keyplt <- plot(textTable(key, row.names=FALSE))
props(keyplt, id="body,1,2") <- element_entry(border_color="red", border_size=1.0)
props(keyplt, id="body,2,2") <- element_entry(fill="gray85")
props(keyplt, id="body,3,2") <- element_entry(border_color="blue", border_size=1.0)
propsd(keyplt, subset=(partcol == 1)) <- element_entry(hjust=0.5)
propsd(keyplt, subset=(partcol == 2)) <- element_entry(hjust=0)
propsd(keyplt, subset=(part == "body" & partcol == 2)) <- 
  element_entry(family="mono")
propsd(keyplt, subset=enabled) <- element_hvrule(linetype=0)
# Add extra space between each row.
for (i in c(1:2)) {
  keyplt <- addHvrule(keyplt, direction="hrule", 
                      arows=arow(keyplt, "body")[i] + 0.5, 
                      props=element_hvrule(linetype=0, space=2))
}

# Show the tables side by side.
sz1 <- pltdSize(plt, units="in")
sz2 <- pltdSize(keyplt, units="in")
sz <- c(sz1[1] + sz2[1] + 0.5, max(sz1[2], sz2[2]))

## ---- echo=FALSE, fig.width=sz[1], fig.height=sz[2]---------------------------
print(plt, position=c(0, 0.5))
print(keyplt, position=c(1, 0.5), newpage=FALSE)

