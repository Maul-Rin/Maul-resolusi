# Simple R package installation script
# Fix encoding and install core packages only

# Set environment
Sys.setenv(LANG = "en_US.UTF-8")
Sys.setenv(LC_ALL = "en_US.UTF-8")
options(repos = c(CRAN = "https://cran.rstudio.com/"))

# Install core packages one by one
core_packages <- c("shiny", "DT", "ggplot2", "dplyr", "readr")

for (pkg in core_packages) {
  cat("Installing", pkg, "...\n")
  tryCatch({
    install.packages(pkg, dependencies = FALSE, quiet = TRUE)
    cat("✓ Installed", pkg, "\n")
  }, error = function(e) {
    cat("✗ Failed to install", pkg, ":", e$message, "\n")
  })
}

cat("Installation complete!\n")