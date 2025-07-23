# Dashboard Analisis Kerentanan Sosial Indonesia - VERSI FINAL TANPA ERROR
# File: dashboard_sovi_indonesia_final.R

# 1. Load Libraries
required_packages <- c(
  "shiny", "shinydashboard", "DT", "ggplot2", "dplyr", "readr",
  "leaflet", "car", "tidyr", "shinyjs", "stats", "psych",
  "sf", "spdep", "geojsonio", "RColorBrewer", "htmltools",
  "plotly", "corrplot", "VIM", "mice", "Hmisc", "knitr", "rmarkdown",
  "webshot", "htmlwidgets", "zip", "openxlsx", "gridExtra", "rmapshaper", "jsonlite")

missing_packages <- required_packages[!sapply(required_packages, requireNamespace, quietly = TRUE)]
if (length(missing_packages) > 0) {
  cat("Installing missing packages:", paste(missing_packages, collapse = ", "), "\n")
  install.packages(missing_packages)
}

lapply(required_packages, library, character.only = TRUE)

# 2. METADATA
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
  stringsAsFactors = FALSE)

# 3. URLs dan Paths
sovi_url <- "sovi_data.csv"
metadata_article_url <- "https://www.sciencedirect.com/science/article/pii/S2352340921010180"
indonesia_geojson_url <- "indonesia511.geojson"

# 4. Fungsi untuk membuat data Indonesia
create_indonesia_region_names <- function(n = 50) {
  set.seed(123)
  
  provinces_cities <- data.frame(
    PROVINCE = c("DKI Jakarta", "Jawa Barat", "Jawa Tengah", "Jawa Timur", "Sumatera Utara",
                 "Sumatera Barat", "Riau", "Sumatera Selatan", "Lampung", "Kalimantan Barat",
                 "Kalimantan Tengah", "Kalimantan Selatan", "Kalimantan Timur", "Sulawesi Utara",
                 "Sulawesi Tengah", "Sulawesi Selatan", "Sulawesi Tenggara", "Gorontalo",
                 "Banten", "Bali", "Nusa Tenggara Barat", "Nusa Tenggara Timur", "Maluku",
                 "Maluku Utara", "Papua", "Papua Barat", "Bengkulu", "Jambi", "Aceh",
                 "Yogyakarta", "Kalimantan Utara", "Sulawesi Barat", "Papua Tengah", 
                 "Papua Pegunungan", "Papua Selatan", "Papua Barat Daya"),
    CITY = c("Jakarta Pusat", "Bandung", "Semarang", "Surabaya", "Medan",
             "Padang", "Pekanbaru", "Palembang", "Bandar Lampung", "Pontianak",
             "Palangka Raya", "Banjarmasin", "Samarinda", "Manado", "Palu",
             "Makassar", "Kendari", "Gorontalo", "Serang", "Denpasar",
             "Mataram", "Kupang", "Ambon", "Ternate", "Jayapura", "Manokwari",
             "Bengkulu", "Jambi", "Banda Aceh", "Yogyakarta", "Tanjung Selor",
             "Mamuju", "Nabire", "Wamena", "Merauke", "Sorong"),
    stringsAsFactors = FALSE
  )
  
  if (n <= nrow(provinces_cities)) {
    return(provinces_cities[1:n, ])
  } else {
    additional_cities <- paste("Kota", sample(1:1000, n - nrow(provinces_cities)))
    additional_provinces <- sample(provinces_cities$PROVINCE, n - nrow(provinces_cities), replace = TRUE)
    
    additional_data <- data.frame(
      PROVINCE = additional_provinces,
      CITY = additional_cities,
      stringsAsFactors = FALSE
    )
    
    return(rbind(provinces_cities, additional_data))
  }
}

# 5. Fungsi untuk generate data SOVI
generate_sovi_data <- function(n_districts = 50) {
  set.seed(123)
  
  regions <- create_indonesia_region_names(n_districts)
  
  lat_range <- c(-11, 6)
  lon_range <- c(95, 141)
  
  data <- data.frame(
    Kode_Distrik = sprintf("ID%03d", 1:n_districts),
    Anakanak = round(runif(n_districts, 5, 15), 2),
    Perempuan = round(runif(n_districts, 48, 52), 2),
    Lansia = round(runif(n_districts, 3, 12), 2),
    Kepala_RT_Perempuan = round(runif(n_districts, 15, 35), 2),
    Ukuran_Keluarga = round(runif(n_districts, 2.5, 5.5), 2),
    Tanpa_Listrik = round(runif(n_districts, 0, 25), 2),
    Pendidikan_Rendah = round(runif(n_districts, 10, 40), 2),
    Pertumbuhan = round(runif(n_districts, -2, 5), 2),
    Kemiskinan = round(runif(n_districts, 5, 30), 2),
    Buta_Huruf = round(runif(n_districts, 2, 20), 2),
    Tidak_Pelatihan = round(runif(n_districts, 20, 70), 2),
    LATITUDE = round(runif(n_districts, lat_range[1], lat_range[2]), 6),
    LONGITUDE = round(runif(n_districts, lon_range[1], lon_range[2]), 6),
    Wilayah = paste(regions$CITY, regions$PROVINCE, sep = ", "),
    Populasi = round(runif(n_districts, 50000, 2000000)),
    Luas = round(runif(n_districts, 100, 5000), 2),
    stringsAsFactors = FALSE
  )
  
  return(data)
}

# 6. Fungsi untuk load spatial data
load_spatial_data <- function() {
  tryCatch({
    if (file.exists(indonesia_geojson_url)) {
      indonesia_sf <- st_read(indonesia_geojson_url, quiet = TRUE)
      cat("GeoJSON berhasil dimuat\n")
      return(indonesia_sf)
    } else {
      cat("File GeoJSON tidak ditemukan, menggunakan fallback\n")
      return(NULL)
    }
  }, error = function(e) {
    cat("Error loading spatial data:", e$message, "\n")
    return(NULL)
  })
}

# 7. Generate data
sovi_data <- generate_sovi_data(50)
indonesia_sf_global <- load_spatial_data()

# 8. Fungsi kalkulasi SOVI
calculate_sovi <- function(data) {
  positive_vars <- c("Anakanak", "Lansia", "Kepala_RT_Perempuan", "Ukuran_Keluarga", 
                     "Tanpa_Listrik", "Pendidikan_Rendah", "Kemiskinan", "Buta_Huruf", "Tidak_Pelatihan")
  
  negative_vars <- c("Perempuan", "Pertumbuhan")
  
  data_normalized <- data
  
  for (var in positive_vars) {
    if (var %in% names(data)) {
      data_normalized[[paste0(var, "_z")]] <- scale(data[[var]])[,1]
    }
  }
  
  for (var in negative_vars) {
    if (var %in% names(data)) {
      data_normalized[[paste0(var, "_z")]] <- -scale(data[[var]])[,1]
    }
  }
  
  z_vars <- names(data_normalized)[grepl("_z$", names(data_normalized))]
  data_normalized$SOVI_Score <- rowSums(data_normalized[z_vars], na.rm = TRUE)
  
  data_normalized$SOVI_Category <- cut(data_normalized$SOVI_Score, 
                                       breaks = quantile(data_normalized$SOVI_Score, c(0, 0.2, 0.4, 0.6, 0.8, 1.0)),
                                       labels = c("Sangat Rendah", "Rendah", "Sedang", "Tinggi", "Sangat Tinggi"),
                                       include.lowest = TRUE)
  
  return(data_normalized)
}

# 9. UI Dashboard
ui <- dashboardPage(
  dashboardHeader(title = "Dashboard Analisis Kerentanan Sosial Indonesia"),
  
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
    useShinyjs(),
    
    tabItems(
      tabItem(tabName = "data",
              fluidRow(
                box(
                  title = "Filter Data", status = "primary", solidHeader = TRUE, width = 12,
                  fluidRow(
                    column(4, selectInput("filter_province", "Pilih Provinsi:", 
                                          choices = c("Semua" = "all"), multiple = TRUE)),
                    column(4, sliderInput("population_range", "Range Populasi:",
                                          min = 0, max = 2000000, value = c(0, 2000000), step = 10000)),
                    column(4, sliderInput("poverty_range", "Range Kemiskinan (%):",
                                          min = 0, max = 50, value = c(0, 50), step = 1))
                  )
                )
              ),
              fluidRow(
                box(
                  title = "Data SOVI", status = "info", solidHeader = TRUE, width = 12,
                  DT::dataTableOutput("sovi_table")
                )
              ),
              fluidRow(
                box(
                  title = "Statistik Deskriptif", status = "success", solidHeader = TRUE, width = 6,
                  verbatimTextOutput("descriptive_stats")
                ),
                box(
                  title = "Download Data", status = "warning", solidHeader = TRUE, width = 6,
                  br(),
                  downloadButton("download_data", "Download CSV", class = "btn-primary"),
                  br(), br(),
                  downloadButton("download_excel", "Download Excel", class = "btn-success")
                )
              )
      ),
      
      tabItem(tabName = "sovi",
              fluidRow(
                box(
                  title = "Pengaturan Visualisasi", status = "primary", solidHeader = TRUE, width = 4,
                  selectInput("sovi_var", "Pilih Variabel untuk Analisis:",
                              choices = c("Anakanak", "Perempuan", "Lansia", "Kepala_RT_Perempuan", 
                                          "Ukuran_Keluarga", "Tanpa_Listrik", "Pendidikan_Rendah", 
                                          "Pertumbuhan", "Kemiskinan", "Buta_Huruf", "Tidak_Pelatihan")),
                  selectInput("chart_type", "Tipe Chart:",
                              choices = c("Histogram" = "hist", "Boxplot" = "box", "Scatter" = "scatter")),
                  checkboxInput("show_trend", "Tampilkan Tren", value = TRUE)
                ),
                box(
                  title = "Distribusi SOVI Score", status = "info", solidHeader = TRUE, width = 8,
                  plotlyOutput("sovi_distribution")
                )
              ),
              fluidRow(
                box(
                  title = "Analisis Variabel Terpilih", status = "success", solidHeader = TRUE, width = 6,
                  plotlyOutput("variable_analysis")
                ),
                box(
                  title = "SOVI vs Variabel", status = "warning", solidHeader = TRUE, width = 6,
                  plotlyOutput("sovi_vs_variable")
                )
              ),
              fluidRow(
                box(
                  title = "Top 10 Daerah Paling Rentan", status = "danger", solidHeader = TRUE, width = 6,
                  DT::dataTableOutput("top_vulnerable")
                ),
                box(
                  title = "Top 10 Daerah Paling Aman", status = "success", solidHeader = TRUE, width = 6,
                  DT::dataTableOutput("top_safe")
                )
              )
      ),
      
      tabItem(tabName = "map",
              fluidRow(
                box(
                  title = "Pengaturan Peta", status = "primary", solidHeader = TRUE, width = 4,
                  selectInput("map_indicator", "Pilih Indikator:",
                              choices = c("SOVI_Score", "Kemiskinan", "Pendidikan_Rendah", 
                                          "Tanpa_Listrik", "Buta_Huruf", "Anakanak", "Lansia")),
                  selectInput("color_palette", "Pilih Palet Warna:",
                              choices = c("Reds", "Blues", "Greens", "Oranges", "Purples", "YlOrRd", "Spectral")),
                  selectInput("map_type", "Tipe Peta:",
                              choices = c("Choropleth" = "choropleth", "Points" = "points")),
                  sliderInput("map_opacity", "Opacity:", min = 0.1, max = 1, value = 0.7, step = 0.1)
                ),
                box(
                  title = "Peta Interaktif Kerentanan Sosial", status = "info", solidHeader = TRUE, width = 8,
                  leafletOutput("choropleth_map", height = "600px")
                )
              ),
              fluidRow(
                box(
                  title = "Legenda dan Informasi", status = "success", solidHeader = TRUE, width = 12,
                  htmlOutput("map_legend_info")
                )
              )
      ),
      
      tabItem(tabName = "correlation",
              fluidRow(
                box(
                  title = "Pengaturan Analisis Korelasi", status = "primary", solidHeader = TRUE, width = 4,
                  selectInput("corr_vars", "Pilih Variabel untuk Korelasi:",
                              choices = c("Anakanak", "Perempuan", "Lansia", "Kepala_RT_Perempuan", 
                                          "Ukuran_Keluarga", "Tanpa_Listrik", "Pendidikan_Rendah", 
                                          "Pertumbuhan", "Kemiskinan", "Buta_Huruf", "Tidak_Pelatihan", "SOVI_Score"),
                              multiple = TRUE,
                              selected = c("Kemiskinan", "Pendidikan_Rendah", "Tanpa_Listrik", "SOVI_Score")),
                  selectInput("corr_method", "Metode Korelasi:",
                              choices = c("Pearson" = "pearson", "Spearman" = "spearman", "Kendall" = "kendall")),
                  numericInput("corr_threshold", "Threshold Korelasi:", value = 0.5, min = 0, max = 1, step = 0.1)
                ),
                box(
                  title = "Matrix Korelasi", status = "info", solidHeader = TRUE, width = 8,
                  plotOutput("correlation_matrix")
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
                  h4("Social Vulnerability Index (SOVI) - Indonesia"),
                  p("Dataset ini berisi indikator kerentanan sosial untuk wilayah Indonesia berdasarkan 17 variabel demografis dan sosio-ekonomi."),
                  h5("Sumber Data:"),
                  p("Data simulasi berdasarkan metodologi SOVI dengan karakteristik demografis Indonesia."),
                  a("Referensi Metodologi", href = metadata_article_url, target = "_blank", class = "btn btn-info")
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
                  title = "Metodologi SOVI", status = "success", solidHeader = TRUE, width = 6,
                  h5("Cara Perhitungan SOVI:"),
                  tags$ol(
                    tags$li("Normalisasi data menggunakan Z-score"),
                    tags$li("Variabel positif: meningkatkan kerentanan"),
                    tags$li("Variabel negatif: mengurangi kerentanan"),
                    tags$li("Penjumlahan semua Z-score untuk mendapatkan SOVI Score"),
                    tags$li("Kategorisasi berdasarkan kuantil")
                  )
                ),
                box(
                  title = "Interpretasi Kategori", status = "warning", solidHeader = TRUE, width = 6,
                  h5("Kategori Kerentanan:"),
                  tags$ul(
                    tags$li(tags$strong("Sangat Rendah:"), " Kerentanan minimal"),
                    tags$li(tags$strong("Rendah:"), " Kerentanan di bawah rata-rata"),
                    tags$li(tags$strong("Sedang:"), " Kerentanan rata-rata"),
                    tags$li(tags$strong("Tinggi:"), " Kerentanan di atas rata-rata"),
                    tags$li(tags$strong("Sangat Tinggi:"), " Kerentanan maksimal")
                  )
                )
              )
      )
    )
  )
)

# 10. Server Logic
server <- function(input, output, session) {
  
  sovi_calculated <- reactive({
    calculate_sovi(sovi_data)
  })
  
  filtered_data <- reactive({
    data <- sovi_calculated()
    
    data <- data[data$Populasi >= input$population_range[1] & data$Populasi <= input$population_range[2], ]
    data <- data[data$Kemiskinan >= input$poverty_range[1] & data$Kemiskinan <= input$poverty_range[2], ]
    
    return(data)
  })
  
  observe({
    provinces <- unique(sapply(strsplit(sovi_data$Wilayah, ", "), function(x) x[2]))
    updateSelectInput(session, "filter_province",
                      choices = c("Semua" = "all", setNames(provinces, provinces)))
  })
  
  output$sovi_table <- DT::renderDataTable({
    DT::datatable(filtered_data(), 
                  options = list(scrollX = TRUE, pageLength = 15),
                  filter = 'top')
  })
  
  output$descriptive_stats <- renderPrint({
    data <- filtered_data()
    numeric_vars <- sapply(data, is.numeric)
    summary(data[numeric_vars])
  })
  
  output$download_data <- downloadHandler(
    filename = function() {
      paste("sovi_data_", Sys.Date(), ".csv", sep = "")
    },
    content = function(file) {
      write.csv(filtered_data(), file, row.names = FALSE)
    }
  )
  
  output$download_excel <- downloadHandler(
    filename = function() {
      paste("sovi_data_", Sys.Date(), ".xlsx", sep = "")
    },
    content = function(file) {
      openxlsx::write.xlsx(filtered_data(), file)
    }
  )
  
  output$sovi_distribution <- renderPlotly({
    data <- filtered_data()
    
    p <- ggplot(data, aes(x = SOVI_Score, fill = SOVI_Category)) +
      geom_histogram(bins = 30, alpha = 0.7) +
      scale_fill_brewer(type = "div", palette = "RdYlBu", direction = -1) +
      labs(title = "Distribusi SOVI Score",
           x = "SOVI Score", y = "Frekuensi") +
      theme_minimal()
    
    ggplotly(p)
  })
  
  output$variable_analysis <- renderPlotly({
    data <- filtered_data()
    var <- input$sovi_var
    
    if (input$chart_type == "hist") {
      p <- ggplot(data, aes_string(x = var)) +
        geom_histogram(bins = 20, fill = "steelblue", alpha = 0.7) +
        labs(title = paste("Distribusi", var), x = var, y = "Frekuensi") +
        theme_minimal()
    } else if (input$chart_type == "box") {
      p <- ggplot(data, aes_string(y = var)) +
        geom_boxplot(fill = "lightblue", alpha = 0.7) +
        labs(title = paste("Boxplot", var), y = var) +
        theme_minimal()
    } else {
      p <- ggplot(data, aes_string(x = "SOVI_Score", y = var)) +
        geom_point(alpha = 0.6) +
        labs(title = paste("Scatter:", var, "vs SOVI Score"), 
             x = "SOVI Score", y = var) +
        theme_minimal()
      
      if (input$show_trend) {
        p <- p + geom_smooth(method = "lm", se = TRUE, color = "red")
      }
    }
    
    ggplotly(p)
  })
  
  output$sovi_vs_variable <- renderPlotly({
    data <- filtered_data()
    var <- input$sovi_var
    
    p <- ggplot(data, aes_string(x = var, y = "SOVI_Score", color = "SOVI_Category")) +
      geom_point(alpha = 0.7, size = 2) +
      scale_color_brewer(type = "div", palette = "RdYlBu", direction = -1) +
      labs(title = paste("SOVI Score vs", var),
           x = var, y = "SOVI Score") +
      theme_minimal()
    
    if (input$show_trend) {
      p <- p + geom_smooth(method = "lm", se = TRUE, aes(group = 1), color = "black")
    }
    
    ggplotly(p)
  })
  
  output$top_vulnerable <- DT::renderDataTable({
    data <- filtered_data()
    top_vulnerable <- data[order(data$SOVI_Score, decreasing = TRUE), ][1:10, 
                           c("Wilayah", "SOVI_Score", "SOVI_Category", "Kemiskinan", "Pendidikan_Rendah")]
    DT::datatable(top_vulnerable, options = list(pageLength = 10))
  })
  
  output$top_safe <- DT::renderDataTable({
    data <- filtered_data()
    top_safe <- data[order(data$SOVI_Score, decreasing = FALSE), ][1:10, 
                     c("Wilayah", "SOVI_Score", "SOVI_Category", "Kemiskinan", "Pendidikan_Rendah")]
    DT::datatable(top_safe, options = list(pageLength = 10))
  })
  
  output$choropleth_map <- renderLeaflet({
    req(input$map_indicator, input$color_palette)
    
    data <- filtered_data()
    
    if (nrow(data) == 0) {
      return(leaflet() %>% 
             addTiles() %>% 
             setView(lng = 118, lat = -2, zoom = 5) %>%
             addPopups(lng = 118, lat = -2, popup = "Tidak ada data yang sesuai dengan filter"))
    }
    
    if (!input$map_indicator %in% names(data)) {
      return(leaflet() %>% 
             addTiles() %>% 
             setView(lng = 118, lat = -2, zoom = 5) %>%
             addPopups(lng = 118, lat = -2, popup = "Indikator tidak ditemukan dalam data"))
    }
    
    tryCatch({
      sf_data <- indonesia_sf_global
      
      if (is.null(sf_data) || input$map_type == "points") {
        pal <- colorNumeric(palette = input$color_palette, domain = data[[input$map_indicator]])
        
        leaflet(data) %>%
          addTiles() %>%
          setView(lng = 118, lat = -2, zoom = 5) %>%
          addCircleMarkers(
            lng = ~LONGITUDE, lat = ~LATITUDE,
            radius = 8,
            color = ~pal(get(input$map_indicator)),
            stroke = TRUE, fillOpacity = input$map_opacity,
            popup = ~paste(
              "<strong>", Wilayah, "</strong><br/>",
              input$map_indicator, ": ", round(get(input$map_indicator), 2), "<br/>",
              "Populasi: ", format(Populasi, big.mark = ","), "<br/>",
              "SOVI Score: ", round(SOVI_Score, 2), "<br/>",
              "Kategori: ", SOVI_Category
            )
          ) %>%
          addLegend(
            "bottomright", pal = pal, values = ~get(input$map_indicator),
            title = input$map_indicator,
            opacity = 1
          )
        
      } else {
        sf_data$Wilayah_Clean <- gsub("[^A-Za-z0-9 ]", "", sf_data$NAME_2)
        data$Wilayah_Clean <- gsub("[^A-Za-z0-9 ]", "", sapply(strsplit(data$Wilayah, ","), function(x) trimws(x[1])))
        
        merged_data <- merge(sf_data, data, by.x = "Wilayah_Clean", by.y = "Wilayah_Clean", all.x = TRUE)
        
        if (nrow(merged_data) == 0) {
          pal <- colorNumeric(palette = input$color_palette, domain = data[[input$map_indicator]])
          
          return(leaflet(data) %>%
                 addTiles() %>%
                 setView(lng = 118, lat = -2, zoom = 5) %>%
                 addCircleMarkers(
                   lng = ~LONGITUDE, lat = ~LATITUDE,
                   radius = 8,
                   color = ~pal(get(input$map_indicator)),
                   stroke = TRUE, fillOpacity = input$map_opacity,
                   popup = ~paste(
                     "<strong>", Wilayah, "</strong><br/>",
                     input$map_indicator, ": ", round(get(input$map_indicator), 2), "<br/>",
                     "Populasi: ", format(Populasi, big.mark = ","), "<br/>",
                     "SOVI Score: ", round(SOVI_Score, 2), "<br/>",
                     "Kategori: ", SOVI_Category
                   )
                 ) %>%
                 addLegend(
                   "bottomright", pal = pal, values = ~get(input$map_indicator),
                   title = input$map_indicator,
                   opacity = 1
                 ))
        }
        
        indicator_values <- merged_data[[input$map_indicator]]
        indicator_values <- indicator_values[!is.na(indicator_values)]
        
        if (length(indicator_values) == 0) {
          return(leaflet() %>% 
                 addTiles() %>% 
                 setView(lng = 118, lat = -2, zoom = 5) %>%
                 addPopups(lng = 118, lat = -2, popup = "Tidak ada data valid untuk indikator ini"))
        }
        
        pal <- colorNumeric(palette = input$color_palette, domain = indicator_values)
        
        leaflet(merged_data) %>%
          addTiles() %>%
          setView(lng = 118, lat = -2, zoom = 5) %>%
          addPolygons(
            fillColor = ~pal(get(input$map_indicator)),
            weight = 1,
            opacity = 1,
            color = "white",
            dashArray = "3",
            fillOpacity = input$map_opacity,
            highlight = highlightOptions(
              weight = 2,
              color = "#666",
              dashArray = "",
              fillOpacity = 0.8,
              bringToFront = TRUE),
            popup = ~ifelse(
              !is.na(get(input$map_indicator)),
              paste(
                "<div style='font-family: Arial, sans-serif; font-size: 12px;'>",
                "<strong style='color: #2E86C1; font-size: 14px;'>", Wilayah, "</strong><br/>",
                "<hr style='margin: 5px 0; border: 1px solid #BDC3C7;'/>",
                "<strong>", input$map_indicator, ":</strong> ", 
                "<span style='color: #E74C3C; font-weight: bold;'>", round(get(input$map_indicator), 2), "</span><br/>",
                "<strong>Populasi:</strong> ", format(Populasi, big.mark = ","), "<br/>",
                "<strong>SOVI Score:</strong> ", 
                "<span style='color: #8E44AD; font-weight: bold;'>", round(SOVI_Score, 2), "</span><br/>",
                "<strong>Kategori:</strong> ", 
                "<span style='background-color: #F8C471; padding: 2px 5px; border-radius: 3px;'>", SOVI_Category, "</span><br/>",
                "<strong>Kemiskinan:</strong> ", round(Kemiskinan, 2), "%<br/>",
                "<strong>Pendidikan Rendah:</strong> ", round(Pendidikan_Rendah, 2), "%<br/>",
                "<strong>Tanpa Listrik:</strong> ", round(Tanpa_Listrik, 2), "%",
                "</div>"
              ),
              paste("<strong>", NAME_2, "</strong><br/>Data tidak tersedia")
            )
          ) %>%
          addLegend(
            "bottomright", pal = pal, values = ~get(input$map_indicator),
            title = HTML(paste("<strong>", input$map_indicator, "</strong>")),
            opacity = 1,
            labFormat = labelFormat(suffix = ifelse(grepl("Score", input$map_indicator), "", "%"))
          )
      }
      
    }, error = function(e) {
      cat("Error in map rendering:", e$message, "\n")
      
      pal <- colorNumeric(palette = input$color_palette, domain = data[[input$map_indicator]])
      
      leaflet(data) %>%
        addTiles() %>%
        setView(lng = 118, lat = -2, zoom = 5) %>%
        addCircleMarkers(
          lng = ~LONGITUDE, lat = ~LATITUDE,
          radius = 8,
          color = ~pal(get(input$map_indicator)),
          stroke = TRUE, fillOpacity = input$map_opacity,
          popup = ~paste(
            "<strong>", Wilayah, "</strong><br/>",
            input$map_indicator, ": ", round(get(input$map_indicator), 2), "<br/>",
            "Populasi: ", format(Populasi, big.mark = ","), "<br/>",
            "SOVI Score: ", round(SOVI_Score, 2), "<br/>",
            "Kategori: ", SOVI_Category
          )
        ) %>%
        addLegend(
          "bottomright", pal = pal, values = ~get(input$map_indicator),
          title = input$map_indicator,
          opacity = 1
        )
    })
  })
  
  output$map_legend_info <- renderUI({
    data <- filtered_data()
    indicator <- input$map_indicator
    
    if (indicator %in% names(data)) {
      min_val <- min(data[[indicator]], na.rm = TRUE)
      max_val <- max(data[[indicator]], na.rm = TRUE)
      mean_val <- mean(data[[indicator]], na.rm = TRUE)
      
      HTML(paste(
        "<div style='padding: 10px; background-color: #f8f9fa; border-radius: 5px;'>",
        "<h5><strong>Informasi Indikator: ", indicator, "</strong></h5>",
        "<p><strong>Minimum:</strong> ", round(min_val, 2), " | ",
        "<strong>Maksimum:</strong> ", round(max_val, 2), " | ",
        "<strong>Rata-rata:</strong> ", round(mean_val, 2), "</p>",
        "<p><em>Klik pada area peta untuk melihat detail informasi wilayah.</em></p>",
        "</div>"
      ))
    }
  })
  
  output$correlation_matrix <- renderPlot({
    data <- filtered_data()
    selected_vars <- input$corr_vars
    
    if (length(selected_vars) < 2) {
      plot.new()
      text(0.5, 0.5, "Pilih minimal 2 variabel untuk analisis korelasi", cex = 1.5)
      return()
    }
    
    cor_data <- data[selected_vars]
    cor_matrix <- cor(cor_data, use = "complete.obs", method = input$corr_method)
    
    corrplot(cor_matrix, method = "color", type = "upper", 
             order = "hclust", tl.cex = 0.8, tl.col = "black",
             addCoef.col = "black", number.cex = 0.7)
  })
  
  output$scatter_matrix <- renderPlot({
    data <- filtered_data()
    selected_vars <- input$corr_vars
    
    if (length(selected_vars) < 2) {
      plot.new()
      text(0.5, 0.5, "Pilih minimal 2 variabel untuk scatter matrix", cex = 1.5)
      return()
    }
    
    if (length(selected_vars) > 6) {
      selected_vars <- selected_vars[1:6]
    }
    
    pairs(data[selected_vars], 
          col = rgb(0.2, 0.5, 0.8, 0.6),
          pch = 16,
          main = "Scatter Plot Matrix")
  })
  
  output$metadata_table <- DT::renderDataTable({
    DT::datatable(metadata_sovi, 
                  options = list(pageLength = 17, scrollX = TRUE),
                  rownames = FALSE)
  })
}

# 11. Run App
shinyApp(ui = ui, server = server)