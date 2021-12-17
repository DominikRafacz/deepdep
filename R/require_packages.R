require_packages <- function(packages, use_case = "access some functions") {
  installed <- vapply(packages, require_pkg, FUN.VALUE = logical(1),
                      use_case = use_case)
  if (!all(installed)) {
    packages <- deparse(packages[!installed])
    message("To ", use_case, ", install dependencies with install.packages(", packages, ").")
  }
  all(installed)
}

require_pkg <- function(pkg, use_case = "access some functions") {
  if (!is_pkg_installed(pkg) && interactive()) {
    install_prompt(pkg, use_case = use_case)
  }
  # Return TRUE if installation successful
  is_pkg_installed(pkg)
}

is_pkg_installed <- function(pkg) {
  check_pkg <- try(find.package(pkg), silent = TRUE)
  !inherits(check_pkg, "try-error")
}

#' @importFrom utils menu install.packages
#' @importFrom stats setNames
install_prompt <- function(pkg, use_case = "access some functions") {
  if (!getOption(paste0(pkg, "_suppress_prompt"), default = FALSE)) {
    response <- menu(
      c("yes", "no", "no and don't ask me anymore"),
      title = paste0("To ", use_case, ", you should install '", pkg, "' available on CRAN. Install?")
    )
    switch(response,
           tryCatch(install.packages(pkg),
                    finally = {
                      if (!is_pkg_installed(pkg))
                        warning("There was an error during an attempt to install '", pkg, "'.", call. = FALSE)
                    }),
           message("You cannot access full functionality of this package without having installed '", pkg, "' first. "),
           {
             options(setNames(list(TRUE), paste0(pkg, "_suppress_prompt")))
             cat("Ok, but you cannot access full functionality of this package without having installed '", pkg, "' first.",
                 sep = "")
           },
           message("You cannot access full functionality of this package without having installed '", pkg, "' first. "))
  }
}
