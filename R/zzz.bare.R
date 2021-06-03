#===== Source file: ../zzz.bare.R on 2020-11-27
# 25 Apr 2021 : Mark 'styles_pkg' as a global variable.

load("./R/sysdata.rda")  # contains '.tablesggOpt', 'internalOpt'
currentOpt <- .tablesggOpt  # Needs to exist in pkg namespace.  Not exported.
utils::globalVariables(c("styles_pkg"))

#-----

.onLoad <- function(libname, pkgname)
{
  # Initialize package options.
  tablesggOpt(reset=TRUE)
}
