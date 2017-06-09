.onAttach <- function(lib, pkg)  {
  packageStartupMessage("This is darleq3 demo version ", utils::packageDescription("darleq3", fields="Version"), appendLF = TRUE)
}
