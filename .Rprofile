# .Rprofile untuk Dashboard Kerentanan Sosial
# File ini akan dijalankan setiap kali R session dimulai

# Set CRAN mirror untuk instalasi paket yang lebih cepat
options(repos = c(CRAN = "https://cran.rstudio.com/"))

# Set locale untuk Indonesia
Sys.setlocale("LC_ALL", "C")

# Increase memory limit jika memungkinkan
if (Sys.info()["sysname"] == "Windows") {
  memory.limit(size = 4000)
}

# Set timeout untuk download yang lebih panjang
options(timeout = 300)

# Disable scientific notation untuk angka
options(scipen = 999)

# Set default encoding
options(encoding = "UTF-8")

# Print startup message
cat("Dashboard Kerentanan Sosial - Environment Ready\n")
cat("Locale:", Sys.getlocale("LC_CTYPE"), "\n")
cat("Working Directory:", getwd(), "\n")

# Function to check and install required packages
check_packages <- function() {
  required_packages <- c(
    "shiny", "shinydashboard", "DT", "ggplot2", "dplyr", 
    "readr", "leaflet", "car", "tidyr", "shinyjs", "stats", 
    "psych", "plotly", "corrplot", "VIM", "mice", "Hmisc", 
    "htmltools", "RColorBrewer"
  )
  
  missing_packages <- required_packages[!sapply(required_packages, requireNamespace, quietly = TRUE)]
  
  if (length(missing_packages) > 0) {
    cat("Missing packages detected:", paste(missing_packages, collapse = ", "), "\n")
    cat("Run install.packages(c(", paste(paste0('"', missing_packages, '"'), collapse = ", "), ")) to install them.\n")
  } else {
    cat("All required packages are available.\n")
  }
}

# Auto-check packages on startup
check_packages()