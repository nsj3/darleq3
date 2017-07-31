library(knitr)
library(rmarkdown)

render("vignettes/darleq3UserGuide.Rmd", pdf_document(), output_file="darleq3UserGuidePDF.pdf")

pack <- "darleq3"
path <- find.package(pack)
if (file.exists("darleq3.pdf"))
  file.remove("darleq3.pdf")
system(paste(shQuote(file.path(R.home("bin"), "R")),
             "CMD", "Rd2pdf", shQuote(path)))
file.copy("darleq3.pdf", "vignettes/darleq3.pdf")
file.remove("darleq3.pdf")
file.remove("darleq3.log")
file.remove("darleq3.md")

