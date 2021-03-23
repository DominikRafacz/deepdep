library(knitr)
all_vignette_files <- list.files("vignettes", full.names = TRUE)
vignettes_original_files <- all_vignette_files[grep("\\.orig", all_vignette_files)]
for (filename in vignettes_original_files) {
  newname <- sub("\\.orig", "", filename)
  #knit(filename, newname)
  content <- readLines(newname)
  writeLines(gsub('(?<=<img src=\\")figure/(.*)(?=\\.png)', "\\1", content, perl=TRUE), newname)
}
  knit(filename, sub("\\.orig", "", filename))

file.remove(all_vignette_files[grep("\\.png", all_vignette_files)])
all_figure_files <- list.files("figure", full.names = TRUE)
file.rename(all_figure_files, sub("figure", "vignettes", all_figure_files))
file.remove("figure")

for (filename in vignettes_original_files)
