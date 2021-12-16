is_pkg_installed <- function(pkg) {
  check_pkg <- try(find.package(pkg), silent = TRUE)
  !inherits(check_pkg, "try-error")
}

require_pkg <- function(pkg, use_case = "access some functions") {
  if (interactive()) {
    if (!is_pkg_installed(pkg) &&
        !getOption(paste0(pkg, "_suppress_prompt"), default = FALSE)) {
      response <- menu(
        c("yes", "no", "no and don't ask me anymore"),
        title = paste0("To access some functionalities, you should install '", pkg, "' available on CRAN. Install?")
      )
      switch(response,
             tryCatch(install.packages(pkg),
                      finally = {
                        if (!is_pkg_installed(pkg))
                          warning("There was an error during an attempt to install '", pkg, "'.", call. = FALSE)
                      }),
             message("You cannot access full functionality of this package without having installed '", pkg, "' first. ",
                     "You can do it manually by calling install.packages('", pkg, "')."),
             {
               options(setNames(list(TRUE), paste0(pkg, "_suppress_prompt")))
               cat("Ok, but you cannot access full functionality of this package without having installed '", pkg, "' first.",
                   sep = "")
             },
             message("You cannot access full functionality of this package without having installed '", pkg, "' first. ",
                     "You can do it manually by calling install.packages('", pkg, "')."))
    }
  } else {
    message("To ", use_case, ", install '", pkg, "' with install.packages('", pkg, "').")
  }
}

require_packages <- function(packages, use_case = "access some functions") {
  if (interactive()) {
    for (pkg in packages) {
      require_pkg(pkg)
    }
  } else {
    packages <- deparse(packages)
    message("To ", use_case, ", install dependencies with install.packages(", packages, ").")
  }
}
