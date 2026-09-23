# okay-r environment provisioning (foreign-managed-env). A fixed script:
# the library, the packages and the repository arrive as DATA in
# environment variables, never as source.
lib <- Sys.getenv("OKAY_R_LIB")
pkgs <- strsplit(Sys.getenv("OKAY_R_PACKAGES"), ",", fixed = TRUE)[[1]]
repos <- Sys.getenv("OKAY_R_REPOS")
dir.create(lib, recursive = TRUE, showWarnings = FALSE)
present <- function(p) requireNamespace(p, lib.loc = c(lib, .libPaths()), quietly = TRUE)
missing <- pkgs[!vapply(pkgs, present, logical(1))]
if (length(missing) > 0) install.packages(missing, lib = lib, repos = repos, quiet = TRUE)
# install.packages only WARNS on failure: the check after it is the verdict
still <- pkgs[!vapply(pkgs, present, logical(1))]
if (length(still) > 0) {
  cat("not installed:", paste(still, collapse = ", "), "\n")
  quit(status = 1)
}
