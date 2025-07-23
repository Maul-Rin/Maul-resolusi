# Test script untuk memastikan app.R bisa diload tanpa error
# Jalankan script ini sebelum deployment

cat("Testing Dashboard Kerentanan Sosial...\n")

# Test 1: Check if app.R exists
if (!file.exists("app.R")) {
  stop("ERROR: app.R file not found!")
}
cat("✓ app.R file found\n")

# Test 2: Try to parse the R file
tryCatch({
  parse("app.R")
  cat("✓ app.R syntax is valid\n")
}, error = function(e) {
  cat("✗ Syntax error in app.R:", e$message, "\n")
  stop("Fix syntax errors before deployment")
})

# Test 3: Check if required packages can be loaded
required_packages <- c(
  "shiny", "shinydashboard", "DT", "ggplot2", "dplyr", 
  "readr", "leaflet", "car", "tidyr", "shinyjs", "stats", 
  "psych", "plotly", "corrplot", "VIM", "mice", "Hmisc", 
  "htmltools", "RColorBrewer"
)

missing_packages <- c()
for (pkg in required_packages) {
  if (!requireNamespace(pkg, quietly = TRUE)) {
    missing_packages <- c(missing_packages, pkg)
  }
}

if (length(missing_packages) > 0) {
  cat("✗ Missing packages:", paste(missing_packages, collapse = ", "), "\n")
  cat("Installing missing packages...\n")
  install.packages(missing_packages, dependencies = TRUE)
} else {
  cat("✓ All required packages are available\n")
}

# Test 4: Try to source the app (without running)
tryCatch({
  # Source the app in a new environment to avoid conflicts
  app_env <- new.env()
  source("app.R", local = app_env)
  cat("✓ app.R can be sourced successfully\n")
}, error = function(e) {
  cat("✗ Error sourcing app.R:", e$message, "\n")
  stop("Fix runtime errors before deployment")
})

# Test 5: Check if UI and server are defined
if (exists("ui", envir = app_env) && exists("server", envir = app_env)) {
  cat("✓ UI and server objects are defined\n")
} else {
  cat("✗ UI or server objects not found\n")
  stop("Ensure UI and server are properly defined")
}

cat("\n=== ALL TESTS PASSED ===\n")
cat("Your app is ready for deployment!\n")
cat("\nTo deploy:\n")
cat("1. For ShinyApps.io: Run source('deploy.R') and choose option 2\n")
cat("2. For local testing: Run shiny::runApp('app.R')\n")
cat("3. For RStudio: Use the 'Publish' button\n")