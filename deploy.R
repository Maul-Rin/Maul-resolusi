# Script Deployment untuk Dashboard Kerentanan Sosial
# Jalankan script ini untuk deploy ke berbagai platform

# 1. Install dan load paket yang diperlukan
required_packages <- c("rsconnect", "shiny", "shinydashboard", "DT", 
                      "ggplot2", "dplyr", "readr", "leaflet", "car", 
                      "tidyr", "shinyjs", "stats", "psych", "plotly", 
                      "corrplot", "VIM", "mice", "Hmisc", "htmltools", 
                      "RColorBrewer")

# Function to install missing packages
install_missing_packages <- function(packages) {
  missing_packages <- packages[!sapply(packages, requireNamespace, quietly = TRUE)]
  
  if (length(missing_packages) > 0) {
    cat("Installing missing packages:", paste(missing_packages, collapse = ", "), "\n")
    install.packages(missing_packages, dependencies = TRUE)
  } else {
    cat("All packages already installed!\n")
  }
}

# Install missing packages
install_missing_packages(required_packages)

# Load rsconnect
library(rsconnect)

# 2. Fungsi untuk deploy ke ShinyApps.io
deploy_to_shinyapps <- function(app_name = "dashboard-kerentanan-sosial") {
  cat("Deploying to ShinyApps.io...\n")
  
  # Check if account is configured
  accounts <- rsconnect::accounts()
  
  if (nrow(accounts) == 0) {
    cat("ERROR: No ShinyApps.io account configured!\n")
    cat("Please run the following commands with your account details:\n")
    cat("rsconnect::setAccountInfo(name='your-account', token='your-token', secret='your-secret')\n")
    return(FALSE)
  }
  
  # Deploy
  tryCatch({
    rsconnect::deployApp(
      appDir = ".",
      appName = app_name,
      appTitle = "Dashboard Analisis Kerentanan Sosial Indonesia",
      launch.browser = TRUE,
      forceUpdate = TRUE
    )
    cat("SUCCESS: App deployed to ShinyApps.io!\n")
    return(TRUE)
  }, error = function(e) {
    cat("ERROR deploying to ShinyApps.io:", e$message, "\n")
    return(FALSE)
  })
}

# 3. Fungsi untuk deploy ke RStudio Connect
deploy_to_connect <- function(server_url, app_name = "dashboard-kerentanan-sosial") {
  cat("Deploying to RStudio Connect...\n")
  
  tryCatch({
    rsconnect::deployApp(
      appDir = ".",
      appName = app_name,
      appTitle = "Dashboard Analisis Kerentanan Sosial Indonesia",
      server = server_url,
      launch.browser = TRUE,
      forceUpdate = TRUE
    )
    cat("SUCCESS: App deployed to RStudio Connect!\n")
    return(TRUE)
  }, error = function(e) {
    cat("ERROR deploying to RStudio Connect:", e$message, "\n")
    return(FALSE)
  })
}

# 4. Fungsi untuk test app locally
test_app_locally <- function() {
  cat("Testing app locally...\n")
  
  tryCatch({
    # Check if app.R exists
    if (!file.exists("app.R")) {
      stop("app.R file not found!")
    }
    
    # Run the app
    shiny::runApp("app.R", launch.browser = TRUE)
    
  }, error = function(e) {
    cat("ERROR running app locally:", e$message, "\n")
    return(FALSE)
  })
}

# 5. Main deployment function
main_deploy <- function() {
  cat("=== Dashboard Kerentanan Sosial - Deployment Script ===\n\n")
  
  # Show menu
  cat("Choose deployment option:\n")
  cat("1. Test locally\n")
  cat("2. Deploy to ShinyApps.io\n")
  cat("3. Deploy to RStudio Connect\n")
  cat("4. Install packages only\n")
  cat("5. Exit\n\n")
  
  choice <- readline(prompt = "Enter your choice (1-5): ")
  
  switch(choice,
    "1" = {
      test_app_locally()
    },
    "2" = {
      app_name <- readline(prompt = "Enter app name (default: dashboard-kerentanan-sosial): ")
      if (app_name == "") app_name <- "dashboard-kerentanan-sosial"
      deploy_to_shinyapps(app_name)
    },
    "3" = {
      server_url <- readline(prompt = "Enter RStudio Connect server URL: ")
      app_name <- readline(prompt = "Enter app name (default: dashboard-kerentanan-sosial): ")
      if (app_name == "") app_name <- "dashboard-kerentanan-sosial"
      deploy_to_connect(server_url, app_name)
    },
    "4" = {
      install_missing_packages(required_packages)
      cat("Package installation completed!\n")
    },
    "5" = {
      cat("Exiting...\n")
      return()
    },
    {
      cat("Invalid choice. Please run the script again.\n")
    }
  )
}

# 6. Auto-deployment function (non-interactive)
auto_deploy_shinyapps <- function(account_name, token, secret, app_name = "dashboard-kerentanan-sosial") {
  cat("Setting up ShinyApps.io account...\n")
  
  # Set account info
  rsconnect::setAccountInfo(name = account_name, token = token, secret = secret)
  
  # Deploy
  deploy_to_shinyapps(app_name)
}

# Run main function if script is executed directly
if (interactive()) {
  main_deploy()
} else {
  cat("Deployment script loaded. Use main_deploy() to start interactive deployment.\n")
  cat("Or use auto_deploy_shinyapps(account_name, token, secret) for automated deployment.\n")
}