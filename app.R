# Dashboard Analisis Kerentanan Sosial - VERSI DEPLOYMENT READY
# Nama File: app.R

# 1. Load Libraries dengan error handling yang lebih robust
required_packages <- c(
  "shiny", "shinydashboard", "DT", "ggplot2", "dplyr", "readr", 
  "leaflet", "car", "tidyr", "shinyjs", "stats", "psych",
  "plotly", "corrplot", "VIM", "mice", "Hmisc", 
  "htmltools", "RColorBrewer"
)

# Function to safely install and load packages
load_packages <- function(packages) {
  for (pkg in packages) {
    if (!requireNamespace(pkg, quietly = TRUE)) {
      tryCatch({
        install.packages(pkg, dependencies = TRUE)
      }, error = function(e) {
        cat("Failed to install", pkg, ":", e$message, "\n")
      })
    }
    
    tryCatch({
      library(pkg, character.only = TRUE)
    }, error = function(e) {
      cat("Failed to load", pkg, ":", e$message, "\n")
    })
  }
}

# Load all packages
load_packages(required_packages)

# 2. METADATA LENGKAP - 17 VARIABEL
metadata_sovi <- data.frame(
  Variabel = c("Kode_Distrik", "Anakanak", "Perempuan", "Lansia", "Kepala_RT_Perempuan", "Ukuran_Keluarga",
               "Tanpa_Listrik", "Pendidikan_Rendah", "Pertumbuhan", "Kemiskinan", "Buta_Huruf", "Tidak_Pelatihan",
               "LATITUDE", "LONGITUDE", "Wilayah", "Populasi", "Luas"),
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

# 3. Fungsi untuk membuat data Indonesia yang realistis
create_indonesia_region_names <- function(n = 50) {
  set.seed(123)
  
  provinces_cities <- c(
    "Jakarta Pusat", "Jakarta Selatan", "Jakarta Timur", "Jakarta Barat", "Jakarta Utara",
    "Bandung", "Bekasi", "Depok", "Tangerang", "Bogor",
    "Semarang", "Solo", "Yogyakarta", "Magelang", "Purwokerto",
    "Surabaya", "Malang", "Kediri", "Blitar", "Mojokerto",
    "Medan", "Palembang", "Padang", "Pekanbaru", "Jambi",
    "Bandar Lampung", "Bengkulu", "Pontianak", "Samarinda", "Banjarmasin",
    "Makassar", "Manado", "Palu", "Kendari", "Gorontalo",
    "Denpasar", "Mataram", "Kupang", "Jayapura", "Sorong",
    "Ambon", "Ternate", "Mamuju", "Palangkaraya", "Tarakan",
    "Bitung", "Tomohon", "Baubau", "Tual", "Sabang"
  )
  
  return(sample(provinces_cities, min(n, length(provinces_cities)), replace = FALSE))
}

# 4. Fungsi untuk generate data sintetis yang realistis
generate_realistic_sovi_data <- function(n_regions = 50) {
  set.seed(123)
  
  # Generate region names
  region_names <- create_indonesia_region_names(n_regions)
  
  # Generate realistic coordinates for Indonesia
  latitudes <- runif(n_regions, -11, 6)  # Indonesia latitude range
  longitudes <- runif(n_regions, 95, 141)  # Indonesia longitude range
  
  # Generate correlated realistic data
  data <- data.frame(
    Kode_Distrik = sprintf("ID_%03d", 1:n_regions),
    Wilayah = region_names,
    LATITUDE = round(latitudes, 4),
    LONGITUDE = round(longitudes, 4),
    
    # Demographics (realistic Indonesian values)
    Anakanak = round(rnorm(n_regions, 8.5, 2.5), 1),
    Perempuan = round(rnorm(n_regions, 49.8, 1.2), 1),
    Lansia = round(rnorm(n_regions, 9.2, 2.1), 1),
    Kepala_RT_Perempuan = round(rnorm(n_regions, 14.5, 3.2), 1),
    Ukuran_Keluarga = round(rnorm(n_regions, 3.8, 0.6), 1),
    
    # Infrastructure and Education
    Tanpa_Listrik = round(pmax(0, rnorm(n_regions, 5.2, 4.1)), 1),
    Pendidikan_Rendah = round(rnorm(n_regions, 25.3, 8.7), 1),
    Buta_Huruf = round(pmax(0, rnorm(n_regions, 6.8, 4.2)), 1),
    Tidak_Pelatihan = round(rnorm(n_regions, 45.7, 12.3), 1),
    
    # Economic indicators
    Kemiskinan = round(pmax(0, rnorm(n_regions, 12.4, 6.8)), 1),
    Pertumbuhan = round(rnorm(n_regions, 1.8, 1.2), 2),
    
    # Area and Population
    Populasi = round(rnorm(n_regions, 85000, 45000)),
    Luas = round(rnorm(n_regions, 350, 200), 1),
    
    stringsAsFactors = FALSE
  )
  
  # Ensure realistic bounds
  data$Anakanak <- pmax(2, pmin(15, data$Anakanak))
  data$Perempuan <- pmax(45, pmin(55, data$Perempuan))
  data$Lansia <- pmax(3, pmin(20, data$Lansia))
  data$Kepala_RT_Perempuan <- pmax(5, pmin(30, data$Kepala_RT_Perempuan))
  data$Ukuran_Keluarga <- pmax(2.5, pmin(6, data$Ukuran_Keluarga))
  data$Tanpa_Listrik <- pmax(0, pmin(25, data$Tanpa_Listrik))
  data$Pendidikan_Rendah <- pmax(10, pmin(60, data$Pendidikan_Rendah))
  data$Buta_Huruf <- pmax(0, pmin(20, data$Buta_Huruf))
  data$Tidak_Pelatihan <- pmax(15, pmin(80, data$Tidak_Pelatihan))
  data$Kemiskinan <- pmax(0, pmin(40, data$Kemiskinan))
  data$Pertumbuhan <- pmax(-2, pmin(8, data$Pertumbuhan))
  data$Populasi <- pmax(10000, data$Populasi)
  data$Luas <- pmax(50, data$Luas)
  
  return(data)
}

# 5. Generate data
sovi_data <- generate_realistic_sovi_data(50)

# 6. Fungsi analisis yang robust
calculate_sovi_index <- function(data) {
  tryCatch({
    # Select numeric variables for SOVI calculation
    sovi_vars <- c("Anakanak", "Perempuan", "Lansia", "Kepala_RT_Perempuan", 
                   "Ukuran_Keluarga", "Tanpa_Listrik", "Pendidikan_Rendah", 
                   "Kemiskinan", "Buta_Huruf", "Tidak_Pelatihan", "Pertumbuhan")
    
    # Ensure all variables exist
    missing_vars <- sovi_vars[!sovi_vars %in% names(data)]
    if (length(missing_vars) > 0) {
      stop(paste("Missing variables:", paste(missing_vars, collapse = ", ")))
    }
    
    # Get numeric data
    numeric_data <- data[sovi_vars]
    
    # Handle missing values
    if (any(is.na(numeric_data))) {
      numeric_data[is.na(numeric_data)] <- 0
    }
    
    # Standardize variables
    standardized_data <- scale(numeric_data)
    
    # Calculate SOVI as sum of standardized scores
    sovi_scores <- rowSums(standardized_data, na.rm = TRUE)
    
    # Normalize to 0-100 scale
    sovi_normalized <- ((sovi_scores - min(sovi_scores, na.rm = TRUE)) / 
                       (max(sovi_scores, na.rm = TRUE) - min(sovi_scores, na.rm = TRUE))) * 100
    
    # Create categories
    sovi_category <- cut(sovi_normalized, 
                        breaks = c(0, 25, 50, 75, 100),
                        labels = c("Rendah", "Sedang", "Tinggi", "Sangat Tinggi"),
                        include.lowest = TRUE)
    
    return(list(
      scores = sovi_normalized,
      categories = sovi_category,
      standardized_data = standardized_data
    ))
    
  }, error = function(e) {
    # Return default values if calculation fails
    n <- nrow(data)
    return(list(
      scores = rep(50, n),
      categories = factor(rep("Sedang", n), levels = c("Rendah", "Sedang", "Tinggi", "Sangat Tinggi")),
      standardized_data = matrix(0, nrow = n, ncol = 11)
    ))
  })
}

# Calculate SOVI
sovi_results <- calculate_sovi_index(sovi_data)
sovi_data$SOVI_Score <- round(sovi_results$scores, 2)
sovi_data$SOVI_Category <- sovi_results$categories

# 7. UI Definition
ui <- dashboardPage(
  dashboardHeader(title = "Dashboard Analisis Kerentanan Sosial Indonesia"),
  
  dashboardSidebar(
    sidebarMenu(
      menuItem("Beranda", tabName = "home", icon = icon("home")),
      menuItem("Data Explorer", tabName = "data", icon = icon("table")),
      menuItem("Analisis SOVI", tabName = "analysis", icon = icon("chart-line")),
      menuItem("Peta Interaktif", tabName = "map", icon = icon("map")),
      menuItem("Korelasi", tabName = "correlation", icon = icon("project-diagram")),
      menuItem("Metadata", tabName = "metadata", icon = icon("info-circle"))
    )
  ),
  
  dashboardBody(
    useShinyjs(),
    
    tags$head(
      tags$style(HTML("
        .content-wrapper, .right-side {
          background-color: #f4f4f4;
        }
        .box {
          border-radius: 5px;
          box-shadow: 0 1px 3px rgba(0,0,0,0.12), 0 1px 2px rgba(0,0,0,0.24);
        }
        .value-box {
          border-radius: 5px;
        }
      "))
    ),
    
    tabItems(
      # Home Tab
      tabItem(tabName = "home",
        fluidRow(
          box(
            title = "Selamat Datang di Dashboard Analisis Kerentanan Sosial",
            status = "primary", solidHeader = TRUE, width = 12,
            h3("Tentang Dashboard"),
            p("Dashboard ini menyediakan analisis komprehensif tentang kerentanan sosial di berbagai wilayah Indonesia menggunakan indeks SOVI (Social Vulnerability Index)."),
            
            h4("Fitur Utama:"),
            tags$ul(
              tags$li("Eksplorasi data interaktif dengan 17 variabel"),
              tags$li("Analisis SOVI dengan visualisasi yang mendalam"),
              tags$li("Peta interaktif untuk analisis spasial"),
              tags$li("Analisis korelasi antar variabel"),
              tags$li("Metadata lengkap untuk setiap variabel")
            ),
            
            h4("Cara Menggunakan:"),
            tags$ol(
              tags$li("Mulai dengan tab 'Data Explorer' untuk melihat data mentah"),
              tags$li("Gunakan tab 'Analisis SOVI' untuk melihat hasil perhitungan indeks"),
              tags$li("Eksplorasi 'Peta Interaktif' untuk analisis geografis"),
              tags$li("Periksa 'Korelasi' untuk memahami hubungan antar variabel"),
              tags$li("Rujuk 'Metadata' untuk penjelasan setiap variabel")
            )
          )
        ),
        
        fluidRow(
          valueBoxOutput("total_regions"),
          valueBoxOutput("avg_sovi"),
          valueBoxOutput("high_vulnerability")
        )
      ),
      
      # Data Explorer Tab
      tabItem(tabName = "data",
        fluidRow(
          box(
            title = "Filter Data", status = "primary", solidHeader = TRUE, width = 3,
            selectInput("region_filter", "Pilih Wilayah:",
                       choices = c("Semua" = "all", unique(sovi_data$Wilayah)),
                       selected = "all"),
            
            sliderInput("sovi_range", "Range SOVI Score:",
                       min = 0, max = 100, value = c(0, 100)),
            
            selectInput("category_filter", "Kategori Kerentanan:",
                       choices = c("Semua" = "all", "Rendah", "Sedang", "Tinggi", "Sangat Tinggi"),
                       selected = "all"),
            
            actionButton("reset_filters", "Reset Filter", class = "btn-warning")
          ),
          
          box(
            title = "Data Kerentanan Sosial", status = "primary", solidHeader = TRUE, width = 9,
            DT::dataTableOutput("data_table")
          )
        )
      ),
      
      # Analysis Tab
      tabItem(tabName = "analysis",
        fluidRow(
          box(
            title = "Distribusi SOVI Score", status = "primary", solidHeader = TRUE, width = 6,
            plotlyOutput("sovi_distribution")
          ),
          
          box(
            title = "Kategori Kerentanan", status = "primary", solidHeader = TRUE, width = 6,
            plotlyOutput("sovi_categories")
          )
        ),
        
        fluidRow(
          box(
            title = "Top 10 Wilayah Paling Rentan", status = "danger", solidHeader = TRUE, width = 6,
            DT::dataTableOutput("top_vulnerable")
          ),
          
          box(
            title = "Top 10 Wilayah Paling Tahan", status = "success", solidHeader = TRUE, width = 6,
            DT::dataTableOutput("least_vulnerable")
          )
        ),
        
        fluidRow(
          box(
            title = "Analisis Komponen SOVI", status = "primary", solidHeader = TRUE, width = 12,
            selectInput("analysis_variable", "Pilih Variabel untuk Analisis:",
                       choices = c("Anakanak", "Perempuan", "Lansia", "Kepala_RT_Perempuan", 
                                 "Ukuran_Keluarga", "Tanpa_Listrik", "Pendidikan_Rendah", 
                                 "Kemiskinan", "Buta_Huruf", "Tidak_Pelatihan", "Pertumbuhan")),
            plotlyOutput("variable_analysis")
          )
        )
      ),
      
      # Map Tab
      tabItem(tabName = "map",
        fluidRow(
          box(
            title = "Peta Kerentanan Sosial Indonesia", status = "primary", solidHeader = TRUE, width = 12,
            leafletOutput("vulnerability_map", height = "600px")
          )
        ),
        
        fluidRow(
          box(
            title = "Statistik Regional", status = "info", solidHeader = TRUE, width = 12,
            verbatimTextOutput("map_stats")
          )
        )
      ),
      
      # Correlation Tab
      tabItem(tabName = "correlation",
        fluidRow(
          box(
            title = "Matriks Korelasi", status = "primary", solidHeader = TRUE, width = 8,
            plotOutput("correlation_matrix", height = "500px")
          ),
          
          box(
            title = "Pengaturan Korelasi", status = "primary", solidHeader = TRUE, width = 4,
            checkboxGroupInput("correlation_vars", "Pilih Variabel:",
                              choices = c("Anakanak", "Perempuan", "Lansia", "Kepala_RT_Perempuan", 
                                        "Ukuran_Keluarga", "Tanpa_Listrik", "Pendidikan_Rendah", 
                                        "Kemiskinan", "Buta_Huruf", "Tidak_Pelatihan", "Pertumbuhan", "SOVI_Score"),
                              selected = c("Kemiskinan", "Pendidikan_Rendah", "Buta_Huruf", "SOVI_Score")),
            
            radioButtons("correlation_method", "Metode Korelasi:",
                        choices = list("Pearson" = "pearson", "Spearman" = "spearman"),
                        selected = "pearson"),
            
            actionButton("update_correlation", "Update Korelasi", class = "btn-primary")
          )
        ),
        
        fluidRow(
          box(
            title = "Scatter Plot Korelasi", status = "primary", solidHeader = TRUE, width = 12,
            fluidRow(
              column(6, selectInput("corr_var1", "Variabel X:", choices = names(sovi_data)[sapply(sovi_data, is.numeric)])),
              column(6, selectInput("corr_var2", "Variabel Y:", choices = names(sovi_data)[sapply(sovi_data, is.numeric)]))
            ),
            plotlyOutput("correlation_scatter")
          )
        )
      ),
      
      # Metadata Tab
      tabItem(tabName = "metadata",
        fluidRow(
          box(
            title = "Metadata Variabel", status = "primary", solidHeader = TRUE, width = 12,
            DT::dataTableOutput("metadata_table")
          )
        ),
        
        fluidRow(
          box(
            title = "Informasi Dataset", status = "info", solidHeader = TRUE, width = 6,
            h4("Sumber Data:"),
            p("Data sintetis berdasarkan karakteristik demografis dan sosial-ekonomi Indonesia"),
            
            h4("Periode Data:"),
            p("Data representatif untuk analisis kerentanan sosial terkini"),
            
            h4("Cakupan Geografis:"),
            p("50 wilayah representatif di seluruh Indonesia"),
            
            h4("Metodologi SOVI:"),
            p("Indeks dihitung berdasarkan standardisasi dan agregasi 11 variabel kunci kerentanan sosial")
          ),
          
          box(
            title = "Statistik Deskriptif", status = "success", solidHeader = TRUE, width = 6,
            verbatimTextOutput("descriptive_stats")
          )
        )
      )
    )
  )
)

# 8. Server Logic
server <- function(input, output, session) {
  
  # Reactive data based on filters
  filtered_data <- reactive({
    data <- sovi_data
    
    if (input$region_filter != "all") {
      data <- data[data$Wilayah == input$region_filter, ]
    }
    
    data <- data[data$SOVI_Score >= input$sovi_range[1] & data$SOVI_Score <= input$sovi_range[2], ]
    
    if (input$category_filter != "all") {
      data <- data[data$SOVI_Category == input$category_filter, ]
    }
    
    return(data)
  })
  
  # Value boxes for home page
  output$total_regions <- renderValueBox({
    valueBox(
      value = nrow(sovi_data),
      subtitle = "Total Wilayah",
      icon = icon("map-marker-alt"),
      color = "blue"
    )
  })
  
  output$avg_sovi <- renderValueBox({
    valueBox(
      value = round(mean(sovi_data$SOVI_Score, na.rm = TRUE), 1),
      subtitle = "Rata-rata SOVI Score",
      icon = icon("chart-line"),
      color = "green"
    )
  })
  
  output$high_vulnerability <- renderValueBox({
    high_vuln <- sum(sovi_data$SOVI_Category %in% c("Tinggi", "Sangat Tinggi"), na.rm = TRUE)
    valueBox(
      value = high_vuln,
      subtitle = "Wilayah Kerentanan Tinggi",
      icon = icon("exclamation-triangle"),
      color = "red"
    )
  })
  
  # Data table
  output$data_table <- DT::renderDataTable({
    DT::datatable(
      filtered_data(),
      options = list(
        scrollX = TRUE,
        pageLength = 15,
        dom = 'Bfrtip',
        buttons = c('copy', 'csv', 'excel')
      ),
      rownames = FALSE
    ) %>%
      DT::formatRound(columns = c("SOVI_Score", "LATITUDE", "LONGITUDE"), digits = 2) %>%
      DT::formatRound(columns = c("Anakanak", "Perempuan", "Lansia", "Kepala_RT_Perempuan", 
                                "Ukuran_Keluarga", "Tanpa_Listrik", "Pendidikan_Rendah", 
                                "Kemiskinan", "Buta_Huruf", "Tidak_Pelatihan", "Pertumbuhan"), digits = 1)
  })
  
  # Reset filters
  observeEvent(input$reset_filters, {
    updateSelectInput(session, "region_filter", selected = "all")
    updateSliderInput(session, "sovi_range", value = c(0, 100))
    updateSelectInput(session, "category_filter", selected = "all")
  })
  
  # SOVI distribution plot
  output$sovi_distribution <- renderPlotly({
    p <- ggplot(sovi_data, aes(x = SOVI_Score)) +
      geom_histogram(bins = 20, fill = "steelblue", alpha = 0.7, color = "white") +
      geom_vline(aes(xintercept = mean(SOVI_Score)), color = "red", linetype = "dashed", size = 1) +
      labs(title = "Distribusi SOVI Score",
           x = "SOVI Score",
           y = "Frekuensi") +
      theme_minimal() +
      theme(plot.title = element_text(hjust = 0.5))
    
    ggplotly(p)
  })
  
  # SOVI categories plot
  output$sovi_categories <- renderPlotly({
    category_counts <- table(sovi_data$SOVI_Category)
    category_df <- data.frame(
      Category = names(category_counts),
      Count = as.numeric(category_counts)
    )
    
    p <- ggplot(category_df, aes(x = Category, y = Count, fill = Category)) +
      geom_bar(stat = "identity", alpha = 0.8) +
      scale_fill_manual(values = c("Rendah" = "green", "Sedang" = "yellow", 
                                  "Tinggi" = "orange", "Sangat Tinggi" = "red")) +
      labs(title = "Distribusi Kategori Kerentanan",
           x = "Kategori",
           y = "Jumlah Wilayah") +
      theme_minimal() +
      theme(plot.title = element_text(hjust = 0.5), legend.position = "none")
    
    ggplotly(p)
  })
  
  # Top vulnerable regions
  output$top_vulnerable <- DT::renderDataTable({
    top_vuln <- sovi_data[order(sovi_data$SOVI_Score, decreasing = TRUE), ][1:10, c("Wilayah", "SOVI_Score", "SOVI_Category")]
    DT::datatable(
      top_vuln,
      options = list(dom = 't', pageLength = 10),
      rownames = FALSE
    ) %>%
      DT::formatRound(columns = "SOVI_Score", digits = 2)
  })
  
  # Least vulnerable regions
  output$least_vulnerable <- DT::renderDataTable({
    least_vuln <- sovi_data[order(sovi_data$SOVI_Score), ][1:10, c("Wilayah", "SOVI_Score", "SOVI_Category")]
    DT::datatable(
      least_vuln,
      options = list(dom = 't', pageLength = 10),
      rownames = FALSE
    ) %>%
      DT::formatRound(columns = "SOVI_Score", digits = 2)
  })
  
  # Variable analysis plot
  output$variable_analysis <- renderPlotly({
    req(input$analysis_variable)
    
    p <- ggplot(sovi_data, aes_string(x = input$analysis_variable, y = "SOVI_Score")) +
      geom_point(aes(color = SOVI_Category), alpha = 0.7, size = 2) +
      geom_smooth(method = "lm", se = TRUE, color = "blue") +
      scale_color_manual(values = c("Rendah" = "green", "Sedang" = "yellow", 
                                   "Tinggi" = "orange", "Sangat Tinggi" = "red")) +
      labs(title = paste("Hubungan", input$analysis_variable, "dengan SOVI Score"),
           x = input$analysis_variable,
           y = "SOVI Score",
           color = "Kategori") +
      theme_minimal() +
      theme(plot.title = element_text(hjust = 0.5))
    
    ggplotly(p)
  })
  
  # Vulnerability map
  output$vulnerability_map <- renderLeaflet({
    # Color palette for SOVI categories
    pal <- colorFactor(
      palette = c("green", "yellow", "orange", "red"),
      domain = c("Rendah", "Sedang", "Tinggi", "Sangat Tinggi")
    )
    
    leaflet(sovi_data) %>%
      addTiles() %>%
      addCircleMarkers(
        lng = ~LONGITUDE,
        lat = ~LATITUDE,
        radius = ~sqrt(SOVI_Score) / 2,
        color = ~pal(SOVI_Category),
        fillOpacity = 0.7,
        popup = ~paste(
          "<strong>", Wilayah, "</strong><br/>",
          "SOVI Score: ", round(SOVI_Score, 2), "<br/>",
          "Kategori: ", SOVI_Category, "<br/>",
          "Populasi: ", format(Populasi, big.mark = ","), "<br/>",
          "Kemiskinan: ", Kemiskinan, "%"
        )
      ) %>%
      addLegend(
        "bottomright",
        pal = pal,
        values = ~SOVI_Category,
        title = "Kategori Kerentanan",
        opacity = 1
      ) %>%
      setView(lng = 118, lat = -2, zoom = 5)
  })
  
  # Map statistics
  output$map_stats <- renderText({
    paste(
      "Statistik Peta:\n",
      "- Total wilayah yang ditampilkan:", nrow(sovi_data), "\n",
      "- Rata-rata SOVI Score:", round(mean(sovi_data$SOVI_Score), 2), "\n",
      "- Wilayah dengan kerentanan tinggi:", sum(sovi_data$SOVI_Category %in% c("Tinggi", "Sangat Tinggi")), "\n",
      "- Wilayah dengan kerentanan rendah:", sum(sovi_data$SOVI_Category == "Rendah"), "\n",
      "- Range koordinat: Lat (", round(min(sovi_data$LATITUDE), 2), " - ", round(max(sovi_data$LATITUDE), 2), 
      "), Lng (", round(min(sovi_data$LONGITUDE), 2), " - ", round(max(sovi_data$LONGITUDE), 2), ")"
    )
  })
  
  # Correlation matrix
  output$correlation_matrix <- renderPlot({
    req(input$correlation_vars)
    
    if (length(input$correlation_vars) < 2) {
      plot.new()
      text(0.5, 0.5, "Pilih minimal 2 variabel untuk analisis korelasi", cex = 1.5)
      return()
    }
    
    cor_data <- sovi_data[input$correlation_vars]
    cor_matrix <- cor(cor_data, use = "complete.obs", method = input$correlation_method)
    
    corrplot(cor_matrix, 
             method = "color", 
             type = "upper", 
             order = "hclust",
             tl.cex = 0.8, 
             tl.col = "black",
             addCoef.col = "black",
             number.cex = 0.7)
  })
  
  # Update correlation when button is clicked
  observeEvent(input$update_correlation, {
    output$correlation_matrix <- renderPlot({
      req(input$correlation_vars)
      
      if (length(input$correlation_vars) < 2) {
        plot.new()
        text(0.5, 0.5, "Pilih minimal 2 variabel untuk analisis korelasi", cex = 1.5)
        return()
      }
      
      cor_data <- sovi_data[input$correlation_vars]
      cor_matrix <- cor(cor_data, use = "complete.obs", method = input$correlation_method)
      
      corrplot(cor_matrix, 
               method = "color", 
               type = "upper", 
               order = "hclust",
               tl.cex = 0.8, 
               tl.col = "black",
               addCoef.col = "black",
               number.cex = 0.7)
    })
  })
  
  # Correlation scatter plot
  output$correlation_scatter <- renderPlotly({
    req(input$corr_var1, input$corr_var2)
    
    if (input$corr_var1 == input$corr_var2) {
      p <- ggplot() + 
        annotate("text", x = 0.5, y = 0.5, label = "Pilih variabel yang berbeda", size = 6) +
        theme_void()
    } else {
      correlation_coef <- cor(sovi_data[[input$corr_var1]], sovi_data[[input$corr_var2]], use = "complete.obs")
      
      p <- ggplot(sovi_data, aes_string(x = input$corr_var1, y = input$corr_var2)) +
        geom_point(aes(color = SOVI_Category), alpha = 0.7, size = 2) +
        geom_smooth(method = "lm", se = TRUE, color = "blue") +
        scale_color_manual(values = c("Rendah" = "green", "Sedang" = "yellow", 
                                     "Tinggi" = "orange", "Sangat Tinggi" = "red")) +
        labs(title = paste("Korelasi:", input$corr_var1, "vs", input$corr_var2, 
                          "\n(r =", round(correlation_coef, 3), ")"),
             x = input$corr_var1,
             y = input$corr_var2,
             color = "Kategori") +
        theme_minimal() +
        theme(plot.title = element_text(hjust = 0.5))
    }
    
    ggplotly(p)
  })
  
  # Metadata table
  output$metadata_table <- DT::renderDataTable({
    DT::datatable(
      metadata_sovi,
      options = list(
        pageLength = 20,
        dom = 'Bfrtip',
        buttons = c('copy', 'csv', 'excel')
      ),
      rownames = FALSE
    )
  })
  
  # Descriptive statistics
  output$descriptive_stats <- renderText({
    numeric_vars <- sovi_data[sapply(sovi_data, is.numeric)]
    stats_summary <- summary(numeric_vars)
    
    paste(capture.output(print(stats_summary)), collapse = "\n")
  })
}

# 9. Run the application
shinyApp(ui = ui, server = server)