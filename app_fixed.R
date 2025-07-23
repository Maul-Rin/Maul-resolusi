# Dashboard Analisis Kerentanan Sosial Indonesia
# Versi Bersih Tanpa Error - Copy Paste Langsung
# File: app_fixed.R

# Load Libraries
required_packages <- c("shiny", "shinydashboard", "DT", "ggplot2", "dplyr", 
                      "leaflet", "plotly", "corrplot", "RColorBrewer")

missing_packages <- required_packages[!sapply(required_packages, requireNamespace, quietly = TRUE)]
if (length(missing_packages) > 0) {
  install.packages(missing_packages, dependencies = TRUE)
}

for (pkg in required_packages) {
  tryCatch({
    library(pkg, character.only = TRUE)
  }, error = function(e) {
    cat("Warning: Could not load", pkg, "\n")
  })
}

# Generate Data
set.seed(123)
n_districts <- 100

wilayah_indonesia <- c(
  "Jakarta Pusat", "Jakarta Utara", "Jakarta Barat", "Jakarta Selatan", "Jakarta Timur",
  "Bandung", "Bekasi", "Depok", "Tangerang", "Bogor", "Cimahi", "Sukabumi", "Cirebon",
  "Semarang", "Surakarta", "Yogyakarta", "Magelang", "Salatiga", "Pekalongan", "Tegal",
  "Surabaya", "Malang", "Kediri", "Blitar", "Mojokerto", "Madiun", "Pasuruan", "Probolinggo",
  "Medan", "Palembang", "Pekanbaru", "Padang", "Jambi", "Bengkulu", "Bandar Lampung",
  "Pontianak", "Samarinda", "Balikpapan", "Banjarmasin", "Palangkaraya",
  "Manado", "Palu", "Makassar", "Kendari", "Gorontalo",
  "Denpasar", "Mataram", "Kupang", "Ambon", "Jayapura",
  paste("Kabupaten", 1:50)
)

sovi_data <- data.frame(
  Kode_Distrik = paste0("ID", sprintf("%03d", 1:n_districts)),
  Wilayah = sample(wilayah_indonesia, n_districts, replace = TRUE),
  Anakanak = round(rnorm(n_districts, 15, 5), 2),
  Perempuan = round(rnorm(n_districts, 50, 3), 2),
  Lansia = round(rnorm(n_districts, 8, 3), 2),
  Kepala_RT_Perempuan = round(rnorm(n_districts, 25, 8), 2),
  Ukuran_Keluarga = round(rnorm(n_districts, 4, 1), 2),
  Tanpa_Listrik = round(rnorm(n_districts, 10, 8), 2),
  Pendidikan_Rendah = round(rnorm(n_districts, 30, 15), 2),
  Pertumbuhan = round(rnorm(n_districts, 2, 3), 2),
  Kemiskinan = round(rnorm(n_districts, 20, 10), 2),
  Buta_Huruf = round(rnorm(n_districts, 8, 5), 2),
  Tidak_Pelatihan = round(rnorm(n_districts, 50, 20), 2),
  LATITUDE = round(runif(n_districts, -10, 6), 4),
  LONGITUDE = round(runif(n_districts, 95, 141), 4),
  Populasi = round(rnorm(n_districts, 50000, 20000)),
  Luas = round(rnorm(n_districts, 500, 200), 2),
  stringsAsFactors = FALSE
)

numeric_cols <- c("Anakanak", "Perempuan", "Lansia", "Kepala_RT_Perempuan", 
                 "Ukuran_Keluarga", "Tanpa_Listrik", "Pendidikan_Rendah", 
                 "Kemiskinan", "Buta_Huruf", "Tidak_Pelatihan", "Populasi", "Luas")

for (col in numeric_cols) {
  sovi_data[[col]] <- pmax(sovi_data[[col]], 0)
}

sovi_vars <- c("Anakanak", "Lansia", "Kepala_RT_Perempuan", "Ukuran_Keluarga", 
               "Tanpa_Listrik", "Pendidikan_Rendah", "Kemiskinan", "Buta_Huruf", "Tidak_Pelatihan")

sovi_matrix <- as.matrix(sovi_data[sovi_vars])
sovi_normalized <- scale(sovi_matrix)
sovi_data$SOVI_Score <- round(rowMeans(sovi_normalized, na.rm = TRUE), 3)

sovi_data$SOVI_Category <- cut(sovi_data$SOVI_Score, 
                              breaks = quantile(sovi_data$SOVI_Score, c(0, 0.25, 0.5, 0.75, 1)),
                              labels = c("Rendah", "Sedang", "Tinggi", "Sangat Tinggi"),
                              include.lowest = TRUE)

metadata_sovi <- data.frame(
  Variabel = c("Kode_Distrik", "Wilayah", "Anakanak", "Perempuan", "Lansia", 
               "Kepala_RT_Perempuan", "Ukuran_Keluarga", "Tanpa_Listrik", 
               "Pendidikan_Rendah", "Pertumbuhan", "Kemiskinan", "Buta_Huruf", 
               "Tidak_Pelatihan", "LATITUDE", "LONGITUDE", "Populasi", "Luas", "SOVI_Score"),
  Deskripsi = c(
    "Kode unik untuk setiap distrik/kabupaten",
    "Nama wilayah/daerah",
    "Persentase populasi di bawah lima tahun",
    "Persentase populasi perempuan",
    "Persentase populasi 65 tahun ke atas",
    "Persentase rumah tangga dengan kepala rumah tangga perempuan",
    "Rata-rata jumlah anggota rumah tangga",
    "Persentase rumah tangga tanpa listrik",
    "Persentase populasi dengan pendidikan rendah",
    "Persentase perubahan populasi",
    "Persentase penduduk miskin",
    "Persentase populasi buta huruf",
    "Persentase rumah tangga tanpa pelatihan bencana",
    "Koordinat lintang geografis",
    "Koordinat bujur geografis",
    "Jumlah total populasi",
    "Luas wilayah dalam km persegi",
    "Skor Indeks Kerentanan Sosial"
  ),
  Tipe = c("ID", "Kategorik", rep("Numerik", 16)),
  stringsAsFactors = FALSE
)

# UI
ui <- dashboardPage(
  dashboardHeader(title = "Dashboard Kerentanan Sosial Indonesia"),
  
  dashboardSidebar(
    sidebarMenu(
      menuItem("Data Explorer", tabName = "data", icon = icon("table")),
      menuItem("Analisis SOVI", tabName = "sovi", icon = icon("chart-line")),
      menuItem("Peta Interaktif", tabName = "map", icon = icon("map")),
      menuItem("Korelasi", tabName = "correlation", icon = icon("project-diagram")),
      menuItem("Metadata", tabName = "metadata", icon = icon("info-circle"))
    )
  ),
  
  dashboardBody(
    tags$head(
      tags$style(HTML("
        .content-wrapper, .right-side {
          background-color: #f4f4f4;
        }
        .box {
          border-radius: 10px;
          box-shadow: 0 2px 4px rgba(0,0,0,0.1);
        }
      "))
    ),
    
    tabItems(
      tabItem(tabName = "data",
        fluidRow(
          box(
            title = "Filter Data", status = "primary", solidHeader = TRUE, width = 12,
            fluidRow(
              column(4, selectInput("filter_wilayah", "Pilih Wilayah:", 
                                   choices = c("Semua", unique(sovi_data$Wilayah)),
                                   selected = "Semua")),
              column(4, selectInput("filter_kategori", "Kategori SOVI:", 
                                   choices = c("Semua", levels(sovi_data$SOVI_Category)),
                                   selected = "Semua")),
              column(4, sliderInput("filter_populasi", "Range Populasi:",
                                   min = min(sovi_data$Populasi, na.rm = TRUE),
                                   max = max(sovi_data$Populasi, na.rm = TRUE),
                                   value = c(min(sovi_data$Populasi, na.rm = TRUE),
                                           max(sovi_data$Populasi, na.rm = TRUE))))
            )
          )
        ),
        fluidRow(
          box(
            title = "Data Kerentanan Sosial", status = "info", solidHeader = TRUE, width = 12,
            DT::dataTableOutput("data_table")
          )
        ),
        fluidRow(
          box(
            title = "Statistik Deskriptif", status = "success", solidHeader = TRUE, width = 6,
            verbatimTextOutput("summary_stats")
          ),
          box(
            title = "Distribusi SOVI Score", status = "warning", solidHeader = TRUE, width = 6,
            plotOutput("sovi_distribution")
          )
        )
      ),
      
      tabItem(tabName = "sovi",
        fluidRow(
          box(
            title = "Kontrol Visualisasi", status = "primary", solidHeader = TRUE, width = 12,
            fluidRow(
              column(6, selectInput("sovi_var", "Pilih Variabel:",
                                   choices = sovi_vars,
                                   selected = "Kemiskinan")),
              column(6, selectInput("chart_type", "Tipe Chart:",
                                   choices = c("Scatter Plot" = "scatter", 
                                             "Box Plot" = "box",
                                             "Histogram" = "hist"),
                                   selected = "scatter"))
            )
          )
        ),
        fluidRow(
          box(
            title = "Visualisasi SOVI", status = "info", solidHeader = TRUE, width = 8,
            plotlyOutput("sovi_plot")
          ),
          box(
            title = "Top 10 Wilayah", status = "danger", solidHeader = TRUE, width = 4,
            h4("SOVI Tertinggi:"),
            DT::dataTableOutput("top_sovi")
          )
        ),
        fluidRow(
          box(
            title = "Analisis Kategori SOVI", status = "success", solidHeader = TRUE, width = 12,
            plotOutput("category_analysis")
          )
        )
      ),
      
      tabItem(tabName = "map",
        fluidRow(
          box(
            title = "Kontrol Peta", status = "primary", solidHeader = TRUE, width = 12,
            fluidRow(
              column(6, selectInput("map_var", "Variabel untuk Warna:",
                                   choices = c("SOVI_Score", sovi_vars),
                                   selected = "SOVI_Score")),
              column(6, selectInput("map_size", "Variabel untuk Ukuran:",
                                   choices = c("Populasi", "Luas"),
                                   selected = "Populasi"))
            )
          )
        ),
        fluidRow(
          box(
            title = "Peta Interaktif Kerentanan Sosial", status = "info", solidHeader = TRUE, width = 12,
            leafletOutput("interactive_map", height = "600px")
          )
        )
      ),
      
      tabItem(tabName = "correlation",
        fluidRow(
          box(
            title = "Analisis Korelasi Variabel SOVI", status = "primary", solidHeader = TRUE, width = 8,
            plotOutput("correlation_plot", height = "500px")
          ),
          box(
            title = "Korelasi dengan SOVI Score", status = "info", solidHeader = TRUE, width = 4,
            DT::dataTableOutput("correlation_table")
          )
        ),
        fluidRow(
          box(
            title = "Scatter Plot Matrix", status = "success", solidHeader = TRUE, width = 12,
            plotOutput("scatter_matrix", height = "600px")
          )
        )
      ),
      
      tabItem(tabName = "metadata",
        fluidRow(
          box(
            title = "Informasi Dataset", status = "primary", solidHeader = TRUE, width = 12,
            h3("Dashboard Analisis Kerentanan Sosial Indonesia"),
            p("Dashboard ini menganalisis kerentanan sosial menggunakan Social Vulnerability Index (SOVI) 
              dengan data representatif dari berbagai wilayah di Indonesia."),
            br(),
            h4("Tentang SOVI:"),
            p("Social Vulnerability Index (SOVI) adalah ukuran yang mengidentifikasi komunitas yang mungkin 
              memerlukan dukungan dalam mempersiapkan, merespons, dan pulih dari bahaya alam."),
            br(),
            h4("Variabel yang Digunakan:"),
            tags$ul(
              tags$li("Demografi: Anak-anak, Lansia, Perempuan"),
              tags$li("Sosial Ekonomi: Kemiskinan, Pendidikan, Buta Huruf"),
              tags$li("Infrastruktur: Akses Listrik, Pelatihan Bencana"),
              tags$li("Geografis: Koordinat, Luas Wilayah, Populasi")
            )
          )
        ),
        fluidRow(
          box(
            title = "Deskripsi Variabel", status = "info", solidHeader = TRUE, width = 12,
            DT::dataTableOutput("metadata_table")
          )
        ),
        fluidRow(
          box(
            title = "Statistik Dataset", status = "success", solidHeader = TRUE, width = 6,
            verbatimTextOutput("dataset_info")
          ),
          box(
            title = "Distribusi Kategori SOVI", status = "warning", solidHeader = TRUE, width = 6,
            plotOutput("category_distribution")
          )
        )
      )
    )
  )
)

# Server
server <- function(input, output, session) {
  
  filtered_data <- reactive({
    data <- sovi_data
    
    if (input$filter_wilayah != "Semua") {
      data <- data[data$Wilayah == input$filter_wilayah, ]
    }
    
    if (input$filter_kategori != "Semua") {
      data <- data[data$SOVI_Category == input$filter_kategori, ]
    }
    
    data <- data[data$Populasi >= input$filter_populasi[1] & 
                data$Populasi <= input$filter_populasi[2], ]
    
    return(data)
  })
  
  output$data_table <- DT::renderDataTable({
    DT::datatable(filtered_data(), 
                 options = list(scrollX = TRUE, pageLength = 15),
                 filter = 'top') %>%
      DT::formatRound(columns = c("SOVI_Score", sovi_vars), digits = 2)
  })
  
  output$summary_stats <- renderPrint({
    data <- filtered_data()
    cat("Ringkasan Statistik Dataset:\n")
    cat("=========================\n")
    cat("Jumlah Wilayah:", nrow(data), "\n")
    cat("SOVI Score - Min:", round(min(data$SOVI_Score, na.rm = TRUE), 3), "\n")
    cat("SOVI Score - Max:", round(max(data$SOVI_Score, na.rm = TRUE), 3), "\n")
    cat("SOVI Score - Mean:", round(mean(data$SOVI_Score, na.rm = TRUE), 3), "\n")
    cat("SOVI Score - Median:", round(median(data$SOVI_Score, na.rm = TRUE), 3), "\n")
    cat("\nDistribusi Kategori:\n")
    print(table(data$SOVI_Category))
  })
  
  output$sovi_distribution <- renderPlot({
    ggplot(filtered_data(), aes(x = SOVI_Score, fill = SOVI_Category)) +
      geom_histogram(bins = 20, alpha = 0.7, color = "white") +
      scale_fill_brewer(type = "div", palette = "RdYlBu", direction = -1) +
      labs(title = "Distribusi SOVI Score",
           x = "SOVI Score", y = "Frekuensi",
           fill = "Kategori") +
      theme_minimal() +
      theme(plot.title = element_text(hjust = 0.5, size = 14, face = "bold"))
  })
  
  output$sovi_plot <- renderPlotly({
    data <- filtered_data()
    
    if (input$chart_type == "scatter") {
      p <- ggplot(data, aes_string(x = input$sovi_var, y = "SOVI_Score", 
                                  color = "SOVI_Category", text = "Wilayah")) +
        geom_point(size = 3, alpha = 0.7) +
        geom_smooth(method = "lm", se = FALSE, color = "black", linetype = "dashed") +
        scale_color_brewer(type = "div", palette = "RdYlBu", direction = -1) +
        labs(title = paste("Hubungan", input$sovi_var, "dengan SOVI Score"),
             x = input$sovi_var, y = "SOVI Score") +
        theme_minimal()
      
    } else if (input$chart_type == "box") {
      p <- ggplot(data, aes_string(x = "SOVI_Category", y = input$sovi_var, 
                                  fill = "SOVI_Category")) +
        geom_boxplot(alpha = 0.7) +
        scale_fill_brewer(type = "div", palette = "RdYlBu", direction = -1) +
        labs(title = paste("Distribusi", input$sovi_var, "per Kategori SOVI"),
             x = "Kategori SOVI", y = input$sovi_var) +
        theme_minimal()
      
    } else {
      p <- ggplot(data, aes_string(x = input$sovi_var, fill = "SOVI_Category")) +
        geom_histogram(bins = 15, alpha = 0.7, position = "identity") +
        scale_fill_brewer(type = "div", palette = "RdYlBu", direction = -1) +
        labs(title = paste("Histogram", input$sovi_var),
             x = input$sovi_var, y = "Frekuensi") +
        theme_minimal()
    }
    
    ggplotly(p, tooltip = c("text", "x", "y"))
  })
  
  output$top_sovi <- DT::renderDataTable({
    top_data <- filtered_data() %>%
      arrange(desc(SOVI_Score)) %>%
      select(Wilayah, SOVI_Score, SOVI_Category) %>%
      head(10)
    
    DT::datatable(top_data, 
                 options = list(dom = 't', pageLength = 10),
                 rownames = FALSE) %>%
      DT::formatRound(columns = "SOVI_Score", digits = 3)
  })
  
  output$category_analysis <- renderPlot({
    data <- filtered_data()
    
    category_summary <- data %>%
      group_by(SOVI_Category) %>%
      summarise(
        Count = n(),
        Mean_Score = mean(SOVI_Score, na.rm = TRUE),
        .groups = 'drop'
      )
    
    p1 <- ggplot(category_summary, aes(x = SOVI_Category, y = Count, fill = SOVI_Category)) +
      geom_col(alpha = 0.8) +
      scale_fill_brewer(type = "div", palette = "RdYlBu", direction = -1) +
      labs(title = "Jumlah Wilayah per Kategori", x = "Kategori", y = "Jumlah") +
      theme_minimal() +
      theme(legend.position = "none")
    
    p2 <- ggplot(category_summary, aes(x = SOVI_Category, y = Mean_Score, fill = SOVI_Category)) +
      geom_col(alpha = 0.8) +
      scale_fill_brewer(type = "div", palette = "RdYlBu", direction = -1) +
      labs(title = "Rata-rata SOVI Score per Kategori", x = "Kategori", y = "Mean Score") +
      theme_minimal() +
      theme(legend.position = "none")
    
    if (requireNamespace("gridExtra", quietly = TRUE)) {
      gridExtra::grid.arrange(p1, p2, ncol = 2)
    } else {
      print(p1)
    }
  })
  
  output$interactive_map <- renderLeaflet({
    data <- filtered_data()
    
    pal <- colorNumeric(palette = "RdYlBu", domain = data[[input$map_var]], reverse = TRUE)
    
    size_var <- data[[input$map_size]]
    sizes <- sqrt(size_var / max(size_var, na.rm = TRUE)) * 20 + 5
    
    leaflet(data) %>%
      addTiles() %>%
      addCircleMarkers(
        lng = ~LONGITUDE, lat = ~LATITUDE,
        radius = sizes,
        color = ~pal(get(input$map_var)),
        fillOpacity = 0.7,
        stroke = TRUE,
        weight = 1,
        popup = ~paste0(
          "<b>", Wilayah, "</b><br/>",
          "SOVI Score: ", round(SOVI_Score, 3), "<br/>",
          "Kategori: ", SOVI_Category, "<br/>",
          input$map_var, ": ", round(get(input$map_var), 2), "<br/>",
          "Populasi: ", format(Populasi, big.mark = ",")
        )
      ) %>%
      addLegend(
        pal = pal,
        values = ~get(input$map_var),
        title = input$map_var,
        position = "bottomright"
      )
  })
  
  output$correlation_plot <- renderPlot({
    cor_data <- sovi_data[c(sovi_vars, "SOVI_Score")]
    cor_matrix <- cor(cor_data, use = "complete.obs")
    
    corrplot(cor_matrix, method = "color", type = "upper", 
             order = "hclust", tl.cex = 0.8, tl.col = "black",
             col = RColorBrewer::brewer.pal(n = 8, name = "RdYlBu"))
  })
  
  output$correlation_table <- DT::renderDataTable({
    cor_data <- sovi_data[c(sovi_vars, "SOVI_Score")]
    cor_with_sovi <- cor(cor_data$SOVI_Score, cor_data[sovi_vars], use = "complete.obs")
    
    cor_table <- data.frame(
      Variabel = names(cor_with_sovi),
      Korelasi = as.numeric(cor_with_sovi),
      stringsAsFactors = FALSE
    ) %>%
      arrange(desc(abs(Korelasi)))
    
    DT::datatable(cor_table, 
                 options = list(dom = 't', pageLength = 15),
                 rownames = FALSE) %>%
      DT::formatRound(columns = "Korelasi", digits = 3)
  })
  
  output$scatter_matrix <- renderPlot({
    pairs_data <- sovi_data[c("SOVI_Score", sovi_vars[1:4])]
    pairs(pairs_data, 
          main = "Scatter Plot Matrix (Selected Variables)",
          pch = 19, 
          col = "steelblue")
  })
  
  output$metadata_table <- DT::renderDataTable({
    DT::datatable(metadata_sovi, 
                 options = list(pageLength = 20, scrollX = TRUE),
                 rownames = FALSE)
  })
  
  output$dataset_info <- renderPrint({
    cat("Informasi Dataset:\n")
    cat("==================\n")
    cat("Total Wilayah:", nrow(sovi_data), "\n")
    cat("Total Variabel:", ncol(sovi_data), "\n")
    cat("Periode Data: 2023 (Simulasi)\n")
    cat("Sumber: Data Simulasi Indonesia\n\n")
    
    cat("Rentang SOVI Score:\n")
    cat("Min:", round(min(sovi_data$SOVI_Score), 3), "\n")
    cat("Max:", round(max(sovi_data$SOVI_Score), 3), "\n")
    cat("Mean:", round(mean(sovi_data$SOVI_Score), 3), "\n")
    cat("Std Dev:", round(sd(sovi_data$SOVI_Score), 3), "\n")
  })
  
  output$category_distribution <- renderPlot({
    ggplot(sovi_data, aes(x = SOVI_Category, fill = SOVI_Category)) +
      geom_bar(alpha = 0.8) +
      scale_fill_brewer(type = "div", palette = "RdYlBu", direction = -1) +
      labs(title = "Distribusi Kategori SOVI",
           x = "Kategori SOVI", y = "Jumlah Wilayah") +
      theme_minimal() +
      theme(legend.position = "none",
            plot.title = element_text(hjust = 0.5, size = 14, face = "bold"))
  })
}

# Run App
shinyApp(ui = ui, server = server)