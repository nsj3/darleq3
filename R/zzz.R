.onAttach <- function(lib, pkg)  {
  if (.Platform$OS.type == "windows") {
    pth <- system.file("zip/zip.exe", package="darleq3")
    Sys.setenv("R_ZIPCMD" = pth)
  }
  packageStartupMessage("This is darleq3 version ", utils::packageDescription("darleq3", fields="Version"), appendLF = TRUE)
}
