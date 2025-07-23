# Script untuk install semua paket R yang diperlukan untuk dashboard
# Jalankan ini sebelum deploy

cat("Installing required R packages for Shiny dashboard...\n")

# Set CRAN mirror
options(repos = c(CRAN = "https://cran.rstudio.com/"))

# Daftar paket yang diperlukan
required_packages <- c(
  "shiny", "shinydashboard", "DT", "ggplot2", "dplyr", "readr", 
  "leaflet", "car", "tidyr", "shinyjs", "stats", "psych",
  "plotly", "corrplot", "VIM", "mice", "Hmisc", 
  "htmltools", "RColorBrewer", "rsconnect"
)

# Install missing packages
missing_packages <- required_packages[!sapply(required_packages, requireNamespace, quietly = TRUE)]

if (length(missing_packages) > 0) {
  cat("Installing missing packages:", paste(missing_packages, collapse = ", "), "\n")
  
  # Install with error handling
  for (pkg in missing_packages) {
    tryCatch({
      install.packages(pkg, dependencies = TRUE, quiet = FALSE)
      cat("✓ Successfully installed:", pkg, "\n")
    }, error = function(e) {
      cat("✗ Failed to install", pkg, ":", e$message, "\n")
    })
  }
} else {
  cat("All required packages are already installed!\n")
}

# Test loading packages
cat("\nTesting package loading...\n")
success_count <- 0
for (pkg in required_packages) {
  tryCatch({
    library(pkg, character.only = TRUE, quietly = TRUE)
    cat("✓", pkg, "\n")
    success_count <- success_count + 1
  }, error = function(e) {
    cat("✗", pkg, "- Error:", e$message, "\n")
  })
}

cat("\nPackage installation summary:\n")
cat("Total packages:", length(required_packages), "\n")
cat("Successfully loaded:", success_count, "\n")

if (success_count == length(required_packages)) {
  cat("🎉 All packages installed and loaded successfully!\n")
  cat("You can now run your Shiny dashboard.\n")
} else {
  cat("⚠️  Some packages failed to install. Check the errors above.\n")
}