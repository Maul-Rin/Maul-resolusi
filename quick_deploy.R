# Quick Deployment Script untuk Dashboard Kerentanan Sosial
# Jalankan script ini untuk deploy ke ShinyApps.io

cat("=== QUICK DEPLOY SCRIPT ===\n")

# 1. Install rsconnect jika belum ada
if (!requireNamespace("rsconnect", quietly = TRUE)) {
  cat("Installing rsconnect...\n")
  install.packages("rsconnect")
}

library(rsconnect)

# 2. Setup instructions
cat("\n=== SETUP INSTRUCTIONS ===\n")
cat("1. Buat akun di https://www.shinyapps.io/\n")
cat("2. Dapatkan token dan secret dari Account > Tokens\n")
cat("3. Jalankan command berikut dengan data Anda:\n\n")

cat("rsconnect::setAccountInfo(\n")
cat("  name = 'your-account-name',\n")
cat("  token = 'your-token',\n")
cat("  secret = 'your-secret'\n")
cat(")\n\n")

# 3. Check if configured
if (length(rsconnect::accounts()) == 0) {
  cat("❌ Akun belum dikonfigurasi!\n")
  cat("Jalankan setAccountInfo() terlebih dahulu.\n")
  stop("Configuration required")
} else {
  cat("✅ Akun sudah dikonfigurasi!\n")
  cat("Account:", rsconnect::accounts()$name[1], "\n")
}

# 4. Deploy simple app
cat("\n=== DEPLOYING SIMPLE APP ===\n")

tryCatch({
  # Deploy simple_app.R
  rsconnect::deployApp(
    appFiles = c("simple_app.R"),
    appName = "kerentanan-sosial-dashboard",
    appTitle = "Dashboard Analisis Kerentanan Sosial Indonesia",
    launch.browser = FALSE,
    forceUpdate = TRUE
  )
  
  cat("🎉 DEPLOYMENT BERHASIL!\n")
  cat("Dashboard dapat diakses di:\n")
  cat("https://", rsconnect::accounts()$name[1], ".shinyapps.io/kerentanan-sosial-dashboard/\n")
  
}, error = function(e) {
  cat("❌ Deployment gagal:\n")
  cat(e$message, "\n")
  cat("\nTroubleshooting:\n")
  cat("1. Pastikan internet connection stabil\n")
  cat("2. Cek quota ShinyApps.io account\n")
  cat("3. Coba deploy manual dengan deployApp()\n")
})

cat("\n=== DEPLOYMENT COMPLETE ===\n")