library(knitr)
library(rmarkdown)

render("vignettes/darleq3UserGuide.Rmd", pdf_document(), output_file="darleq3UserGuidePDF.pdf")
render("vignettes/darleq3UserGuide.Rmd", html_document(), output_file="darleq3UserGuide.html")

pack <- "darleq3"
path <- find.package(pack)

pth <- paste(shQuote(file.path(R.home("bin"), "R")),
      "CMD", "Rd2pdf", "--no-preview", "--force", "--output=vignettes/darleq3.pdf", shQuote(path))

#system(paste(shQuote(file.path(R.home("bin"), "R")), "CMD", "Rd2pdf", "--no-preview", "--force", "--output=vignettes/darleq3.pdf", shQuote(path)))

system(paste(shQuote(file.path(R.home("bin"), "R")),
             "CMD", "Rd2pdf", "--no-preview", "--force", "--output=vignettes/darleq3.pdf", "."))


file.copy("darleq3.pdf", "vignettes/darleq3.pdf", overwrite=TRUE)
file.copy("vignettes/darleq3UserGuidePDF.pdf", "inst/shiny_app/www/darleq3UserGuidePDF.pdf", overwrite=TRUE)
file.copy("vignettes/darleq3UserGuidePDF.html", "inst/shiny_app/www/darleq3UserGuidePDF.html", overwrite=TRUE)
file.copy("vignettes/Interpreting_NGS_data.pdf", "inst/shiny_app/www/Interpreting_NGS_data.pdf", overwrite=TRUE)
file.copy("inst/extdata/DarleqTaxonList2017_Master.xlsx", "inst/shiny_app/www/DarleqTaxonList2017_Master.xlsx", overwrite=TRUE)

file.copy("vignettes/darleq3.pdf", "inst/doc/darleq3.pdf", overwrite=TRUE)
file.copy("vignettes/darleq3UserGuidePDF.pdf", "inst/doc/darleq3UserGuidePDF.pdf", overwrite=TRUE)
file.copy("vignettes/darleq3UserGuide.html", "inst/doc/darleq3UserGuide.html", overwrite=TRUE)
file.copy("vignettes/Interpreting_NGS_data.pdf", "inst/doc/Interpreting_NGS_data.pdf", overwrite=TRUE)
if (file.exists("darleq3.pdf"))
  file.remove("darleq3.pdf")
if (file.exists("darleq3.log"))
  file.remove("darleq3.log")
if (file.exists("darleq3.md"))
  file.remove("darleq3.md")

