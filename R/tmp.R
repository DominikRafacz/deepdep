# tmp <- httr::GET("http://bioconductor.org/packages/release/bioc/VIEWS")
# tmp <- httr::content(tmp, as = "text", encoding = "UTF-8")
# 
# library(stringi)
# mat <- stri_match_all_regex(tmp, "(.*):(?> |\\n)((?>.|\\n        )*)\\n")[[1]][, -1]
# n <- nrow(mat)
# mat[,2] <- stri_replace_all_regex(mat[, 2], "(\\n)?        |\\n", " ")
# pkg_begs <- (1:n)[mat[, 1] == "Package"]
# pkg_ends <- c((pkg_inds - 1)[-1], n)
# 
# 
# lapply(1:length(pkg_inds), function(i) {
#   ret <- as.list(mat[pkg_inds[i]:pkg_ends[i], 2])
#   names(ret) <- mat[pkg_inds[i]:pkg_ends[i], 1]
#   ret
# }) -> pkgsa
# 
# names(pkgs) <- lapply(pkgs, function(pkg) pkg$Package)
# 
# lapply(pkgs, function(pkg) {
#   nms <- names(pkg)
#   for (dep_type in c("Depends", "Imports", "Suggests", "LinkingTo", "Enhances")) {
#     if (dep_type %in% nms)
#       pkg[[dep_type]] <- 
#         stri_match_all_regex(pkg[[dep_type]], "(?>\\s*)([^,]+)(?>,|$)")[[1]][,2]
#   }
#   pkg
# }) 
