# Dashboard Analisis Kerentanan Sosial - Versi Sederhana
# Minimal dependencies untuk deployment

# Load hanya library yang tersedia
library(stats)

# Buat data contoh untuk demo
set.seed(123)
n <- 100

# Data SOVI sederhana
sovi_data <- data.frame(
  Kode_Distrik = paste0("D", sprintf("%03d", 1:n)),
  Wilayah = paste("Wilayah", 1:n),
  Anakanak = round(runif(n, 5, 25), 2),
  Perempuan = round(runif(n, 45, 55), 2),
  Lansia = round(runif(n, 3, 15), 2),
  Kepala_RT_Perempuan = round(runif(n, 10, 40), 2),
  Ukuran_Keluarga = round(runif(n, 2.5, 6.5), 2),
  Tanpa_Listrik = round(runif(n, 0, 30), 2),
  Pendidikan_Rendah = round(runif(n, 10, 70), 2),
  Pertumbuhan = round(runif(n, -2, 8), 2),
  Kemiskinan = round(runif(n, 5, 45), 2),
  Buta_Huruf = round(runif(n, 2, 25), 2),
  Tidak_Pelatihan = round(runif(n, 20, 80), 2),
  LATITUDE = round(runif(n, -10, 6), 4),
  LONGITUDE = round(runif(n, 95, 141), 4),
  Populasi = round(runif(n, 10000, 500000)),
  Luas = round(runif(n, 50, 5000), 2),
  stringsAsFactors = FALSE
)

# Hitung SOVI Score sederhana
numeric_cols <- c("Anakanak", "Perempuan", "Lansia", "Kepala_RT_Perempuan", 
                  "Ukuran_Keluarga", "Tanpa_Listrik", "Pendidikan_Rendah", 
                  "Kemiskinan", "Buta_Huruf", "Tidak_Pelatihan")

# Standardisasi data
for (col in numeric_cols) {
  sovi_data[[paste0(col, "_std")]] <- scale(sovi_data[[col]])[,1]
}

# Hitung SOVI Score
sovi_data$SOVI_Score <- rowMeans(sovi_data[, paste0(numeric_cols, "_std")])
sovi_data$SOVI_Category <- cut(sovi_data$SOVI_Score, 
                               breaks = c(-Inf, -0.5, 0.5, Inf),
                               labels = c("Rendah", "Sedang", "Tinggi"))

# Metadata
metadata_sovi <- data.frame(
  Variabel = c("Kode_Distrik", "Anakanak", "Perempuan", "Lansia", "Kepala_RT_Perempuan", 
               "Ukuran_Keluarga", "Tanpa_Listrik", "Pendidikan_Rendah", "Pertumbuhan", 
               "Kemiskinan", "Buta_Huruf", "Tidak_Pelatihan", "LATITUDE", "LONGITUDE", 
               "Wilayah", "Populasi", "Luas"),
  Deskripsi = c(
    "Kode unik untuk setiap distrik/kabupaten.",
    "Persentase populasi di bawah lima tahun.",
    "Persentase populasi perempuan.",
    "Persentase populasi 65 tahun ke atas.",
    "Persentase rumah tangga dengan kepala rumah tangga perempuan.",
    "Rata-rata jumlah anggota rumah tangga di satu distrik.",
    "Persentase rumah tangga yang tidak menggunakan listrik sebagai sumber penerangan.",
    "Persentase populasi 15 tahun ke atas dengan pendidikan rendah.",
    "Persentase perubahan populasi (pertumbuhan populasi).",
    "Persentase penduduk miskin.",
    "Persentase populasi yang tidak bisa membaca dan menulis.",
    "Persentase rumah tangga yang tidak mendapatkan pelatihan bencana.",
    "Koordinat lintang geografis.",
    "Koordinat bujur geografis.",
    "Nama wilayah/daerah.",
    "Jumlah total populasi.",
    "Luas wilayah dalam km²."
  ),
  Tipe = c("Kategorik/ID", rep("Numerik", 11), "Numerik", "Numerik", "Kategorik", "Numerik", "Numerik"),
  stringsAsFactors = FALSE
)

# Cek apakah Shiny tersedia
if (requireNamespace("shiny", quietly = TRUE)) {
  library(shiny)
  
  # UI
  ui <- fluidPage(
    titlePanel("Dashboard Analisis Kerentanan Sosial Indonesia"),
    
    sidebarLayout(
      sidebarPanel(
        h3("Pengaturan"),
        selectInput("variable", "Pilih Variabel:",
                    choices = numeric_cols,
                    selected = "Kemiskinan"),
        br(),
        h4("Statistik Deskriptif"),
        verbatimTextOutput("stats")
      ),
      
      mainPanel(
        tabsetPanel(
          tabPanel("Data Explorer", 
                   h3("Data SOVI Indonesia"),
                   p("Dashboard ini menampilkan data kerentanan sosial Indonesia berdasarkan indeks SOVI."),
                   tableOutput("data_table")
          ),
          
          tabPanel("Visualisasi",
                   h3("Histogram Variabel Terpilih"),
                   plotOutput("histogram"),
                   br(),
                   h3("Scatter Plot vs SOVI Score"),
                   plotOutput("scatter")
          ),
          
          tabPanel("Analisis SOVI",
                   h3("Distribusi SOVI Score"),
                   plotOutput("sovi_dist"),
                   br(),
                   h3("Kategori Kerentanan"),
                   tableOutput("sovi_summary")
          ),
          
          tabPanel("Metadata",
                   h3("Informasi Variabel"),
                   tableOutput("metadata_table")
          )
        )
      )
    )
  )
  
  # Server
  server <- function(input, output) {
    
    output$data_table <- renderTable({
      head(sovi_data[, c("Kode_Distrik", "Wilayah", input$variable, "SOVI_Score", "SOVI_Category")], 20)
    })
    
    output$stats <- renderText({
      var_data <- sovi_data[[input$variable]]
      paste(
        paste("Mean:", round(mean(var_data, na.rm = TRUE), 2)),
        paste("Median:", round(median(var_data, na.rm = TRUE), 2)),
        paste("SD:", round(sd(var_data, na.rm = TRUE), 2)),
        paste("Min:", round(min(var_data, na.rm = TRUE), 2)),
        paste("Max:", round(max(var_data, na.rm = TRUE), 2)),
        sep = "\n"
      )
    })
    
    output$histogram <- renderPlot({
      hist(sovi_data[[input$variable]], 
           main = paste("Distribusi", input$variable),
           xlab = input$variable,
           ylab = "Frekuensi",
           col = "lightblue",
           border = "black")
    })
    
    output$scatter <- renderPlot({
      plot(sovi_data[[input$variable]], sovi_data$SOVI_Score,
           main = paste(input$variable, "vs SOVI Score"),
           xlab = input$variable,
           ylab = "SOVI Score",
           pch = 16,
           col = "darkblue")
      abline(lm(SOVI_Score ~ sovi_data[[input$variable]], data = sovi_data), col = "red", lwd = 2)
    })
    
    output$sovi_dist <- renderPlot({
      hist(sovi_data$SOVI_Score,
           main = "Distribusi SOVI Score",
           xlab = "SOVI Score",
           ylab = "Frekuensi",
           col = c("green", "yellow", "red")[cut(sovi_data$SOVI_Score, breaks = 3)],
           border = "black")
    })
    
    output$sovi_summary <- renderTable({
      table(sovi_data$SOVI_Category)
    })
    
    output$metadata_table <- renderTable({
      metadata_sovi
    })
  }
  
  # Run the app
  cat("Starting Shiny app...\n")
  shinyApp(ui = ui, server = server)
  
} else {
  cat("Shiny package not available. Please install shiny first.\n")
  cat("Run: install.packages('shiny')\n")
  
  # Tampilkan data sederhana tanpa Shiny
  cat("\n=== DASHBOARD KERENTANAN SOSIAL ===\n")
  cat("Data Sample (10 baris pertama):\n")
  print(head(sovi_data[, c("Wilayah", "Kemiskinan", "SOVI_Score", "SOVI_Category")], 10))
  
  cat("\nStatistik SOVI Score:\n")
  print(summary(sovi_data$SOVI_Score))
  
  cat("\nDistribusi Kategori Kerentanan:\n")
  print(table(sovi_data$SOVI_Category))
}