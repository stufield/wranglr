
#' @keywords internal package
"_PACKAGE"

#' @import dplyr
NULL

.dummy <- function() { }   # nolint: brace_linter.

.onLoad <- function(...) {
  pkgenv <- environment(.dummy)
  # Re-export data for internal/testing
  utils::data("sim_adat", package = "splyr", envir = pkgenv)

  # this is to register the internal S3 methods
  # this avoids having to export the methods in the NAMESPACE file
  invisible()
}


# this wrapper registers the methods during pkg load
# but ensures the package passes R CMD check that it can
# be installed even though pillar isn't imported
register_s3_method <- function(pkg, generic, class, fun = NULL) {
  stopifnot(
    is.character(pkg), length(pkg) == 1L,
    is.character(generic), length(generic) == 1L,
    is.character(class), length(class) == 1L
  )

  if ( is.null(fun) ) {
    fun <- get(paste0(generic, ".", class), envir = parent.frame())
  } else {
    stopifnot(is.function(fun))
  }

  if ( pkg %in% loadedNamespaces() ) {
    registerS3method(generic, class, fun, envir = asNamespace(pkg))
  }

  # Always register hook in case package is later unloaded & reloaded
  setHook(
    packageEvent(pkg, "onLoad"),
    function(...) {
      registerS3method(generic, class, fun, envir = asNamespace(pkg))
    }
  )
}
