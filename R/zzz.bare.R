#===== Source file: ../zzz.bare.R on 2020-11-27

load("./R/sysdata.rda")  # contains .tablesggOpt
currentOpt <- .tablesggOpt  # Needs to exist in pkg namespace.  Not exported.

#-----

.onLoad <- function(libname, pkgname)
{
  # Initialize package options.
  tablesggOpt(reset=TRUE)
}
