.onAttach <- function(lib, pkg)  {
  packageStartupMessage("This is darleq3 version ", utils::packageDescription("darleq3", fields="Version"), "\nType ?darleq3 for help.", appendLF = TRUE)
}
