# Nama File: vulnera_dashboard_complete_enhanced_final_FIXED_v5.R
# Dashboard Analisis Kerentanan Sosial - VERSI DENGAN PERBAIKAN STRUKTUR DAN PETA

# ===============================================================================
# 1. LOAD LIBRARIES
# ===============================================================================
library(shiny)
library(shinydashboard)
library(DT)
library(ggplot2)
library(dplyr)
library(readr)
library(leaflet)
library(car)
library(tidyr)
library(shinyjs)
library(stats)
library(psych)
library(sf)
library(spdep)
library(RColorBrewer)
library(htmltools)
library(plotly)
library(corrplot)
library(VIM)
library(mice)
library(Hmisc)
library(knitr)
library(rmarkdown)
library(webshot)
library(htmlwidgets)
library(zip)
library(openxlsx)
library(gridExtra)
library(rmapshaper)
library(jsonlite)

# ===============================================================================
# 2. METADATA LENGKAP - 17 VARIABEL
# ===============================================================================
metadata_sovi <- data.frame(
  Variabel = c("DISTRICTCODE", "CHILDREN", "FEMALE", "ELDERLY", "FHEAD", "FAMILYSIZE",
               "NOELECTRIC", "LOWEDU", "GROWTH", "POVERTY", "ILLITERATE", "NOTRAINING",
               "LATITUDE", "LONGITUDE", "REGION", "POPULATION", "AREA"),
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
    "Nama REGION/daerah.",
    "Jumlah total populasi.",
    "Luas REGION dalam km²."
  ),
  Tipe = c("Kategorik/ID", rep("Numerik", 11), "Numerik", "Numerik", "Kategorik", "Numerik", "Numerik"),
  stringsAsFactors = FALSE
)

# ===============================================================================
# 3. DATA GENERATION FUNCTIONS
# ===============================================================================

# Fungsi untuk membuat nama region Indonesia yang realistis
create_indonesia_region_names <- function(n = 50) {
  set.seed(123)
  
  provinces_cities <- c(
    "DKI Jakarta", "Jakarta Utara", "Jakarta Selatan", "Jakarta Timur", "Jakarta Barat",
    "Bandung", "Bekasi", "Depok", "Tangerang", "Bogor",
    "Semarang", "Surakarta", "Yogyakarta", "Magelang", "Tegal",
    "Surabaya", "Malang", "Kediri", "Blitar", "Probolinggo",
    "Medan", "Palembang", "Pekanbaru", "Padang", "Jambi",
    "Bandar Lampung", "Bengkulu", "Batam", "Tanjungpinang", "Pontianak",
    "Banjarmasin", "Samarinda", "Balikpapan", "Makassar", "Palopo",
    "Kendari", "Palu", "Gorontalo", "Manado", "Ambon",
    "Ternate", "Jayapura", "Sorong", "Merauke", "Timika",
    "Denpasar", "Singaraja", "Mataram", "Bima", "Kupang"
  )
  
  return(sample(provinces_cities, min(n, length(provinces_cities)), replace = FALSE))
}

# Fungsi untuk generate data SOVI
generate_sovi_data <- function(n_regions = 50) {
  set.seed(123)
  
  regions <- create_indonesia_region_names(n_regions)
  
  # Generate realistic coordinates for Indonesia
  lat_range <- c(-11, 6)  # Indonesia latitude range
  lon_range <- c(95, 141) # Indonesia longitude range
  
  data.frame(
    DISTRICTCODE = sprintf("ID%03d", 1:n_regions),
    CHILDREN = round(runif(n_regions, 5, 25), 2),
    FEMALE = round(runif(n_regions, 48, 52), 2),
    ELDERLY = round(runif(n_regions, 3, 15), 2),
    FHEAD = round(runif(n_regions, 10, 35), 2),
    FAMILYSIZE = round(runif(n_regions, 2.5, 6.5), 2),
    NOELECTRIC = round(runif(n_regions, 0, 30), 2),
    LOWEDU = round(runif(n_regions, 5, 45), 2),
    GROWTH = round(runif(n_regions, -2, 8), 2),
    POVERTY = round(runif(n_regions, 2, 40), 2),
    ILLITERATE = round(runif(n_regions, 1, 25), 2),
    NOTRAINING = round(runif(n_regions, 20, 80), 2),
    LATITUDE = round(runif(n_regions, lat_range[1], lat_range[2]), 6),
    LONGITUDE = round(runif(n_regions, lon_range[1], lon_range[2]), 6),
    REGION = regions,
    POPULATION = round(runif(n_regions, 50000, 2000000)),
    AREA = round(runif(n_regions, 100, 5000), 2),
    stringsAsFactors = FALSE
  )
}

# ===============================================================================
# 4. CALCULATE SOVI INDEX
# ===============================================================================
calculate_sovi_index <- function(data) {
  # Variabel untuk perhitungan SOVI (exclude ID, coordinates, region name, population, area)
  sovi_vars <- c("CHILDREN", "FEMALE", "ELDERLY", "FHEAD", "FAMILYSIZE",
                 "NOELECTRIC", "LOWEDU", "GROWTH", "POVERTY", "ILLITERATE", "NOTRAINING")
  
  # Standardisasi variabel
  data_std <- data
  for(var in sovi_vars) {
    data_std[[var]] <- scale(data[[var]])[,1]
  }
  
  # Hitung SOVI sebagai rata-rata tertimbang
  weights <- c(0.15, 0.10, 0.15, 0.10, 0.05, 0.10, 0.15, 0.05, 0.20, 0.10, 0.05)
  
  data$SOVI_INDEX <- rowSums(data_std[sovi_vars] * weights)
  data$SOVI_CATEGORY <- cut(data$SOVI_INDEX, 
                           breaks = quantile(data$SOVI_INDEX, c(0, 0.25, 0.5, 0.75, 1)),
                           labels = c("Rendah", "Sedang", "Tinggi", "Sangat Tinggi"),
                           include.lowest = TRUE)
  
  return(data)
}

# ===============================================================================
# 5. GENERATE DATA
# ===============================================================================
sovi_data <- generate_sovi_data(50)
sovi_data <- calculate_sovi_index(sovi_data)

# ===============================================================================
# 6. MAP FUNCTIONS - SECTION YANG DIPERBAIKI
# ===============================================================================

# Fungsi untuk membuat peta dasar
create_base_map <- function() {
  leaflet() %>%
    addTiles(group = "OpenStreetMap") %>%
    addProviderTiles(providers$CartoDB.Positron, group = "CartoDB") %>%
    addProviderTiles(providers$Esri.WorldImagery, group = "Satellite") %>%
    setView(lng = 118, lat = -2, zoom = 5) %>%
    addLayersControl(
      baseGroups = c("OpenStreetMap", "CartoDB", "Satellite"),
      options = layersControlOptions(collapsed = FALSE)
    )
}

# Fungsi untuk membuat color palette
create_color_palette <- function(data, variable) {
  if(is.numeric(data[[variable]])) {
    colorNumeric(
      palette = "RdYlBu",
      domain = data[[variable]],
      reverse = TRUE
    )
  } else {
    colorFactor(
      palette = RColorBrewer::brewer.pal(length(unique(data[[variable]])), "Set3"),
      domain = data[[variable]]
    )
  }
}

# Fungsi untuk membuat popup content
create_popup_content <- function(data) {
  paste0(
    "<strong>", data$REGION, "</strong><br/>",
    "Kode Distrik: ", data$DISTRICTCODE, "<br/>",
    "SOVI Index: ", round(data$SOVI_INDEX, 3), "<br/>",
    "Kategori: ", data$SOVI_CATEGORY, "<br/>",
    "Populasi: ", format(data$POPULATION, big.mark = ","), "<br/>",
    "Luas: ", data$AREA, " km²<br/>",
    "Kemiskinan: ", data$POVERTY, "%<br/>",
    "Buta Huruf: ", data$ILLITERATE, "%"
  )
}

# Fungsi utama untuk membuat peta interaktif
create_interactive_map <- function(data, color_variable = "SOVI_INDEX") {
  # Buat base map
  map <- create_base_map()
  
  # Buat color palette
  pal <- create_color_palette(data, color_variable)
  
  # Buat popup content
  popup_content <- create_popup_content(data)
  
  # Tambahkan markers
  map <- map %>%
    addCircleMarkers(
      data = data,
      lng = ~LONGITUDE,
      lat = ~LATITUDE,
      radius = ~sqrt(POPULATION/10000),
      color = "white",
      weight = 1,
      fillColor = ~pal(get(color_variable)),
      fillOpacity = 0.7,
      popup = popup_content,
      label = ~REGION,
      labelOptions = labelOptions(
        style = list("font-weight" = "normal", padding = "3px 8px"),
        textsize = "13px",
        direction = "auto"
      )
    )
  
  # Tambahkan legend
  if(is.numeric(data[[color_variable]])) {
    map <- map %>%
      addLegend(
        pal = pal,
        values = data[[color_variable]],
        opacity = 0.7,
        title = color_variable,
        position = "bottomright"
      )
  }
  
  return(map)
}

# ===============================================================================
# 7. UI DEFINITION
# ===============================================================================
ui <- dashboardPage(
  dashboardHeader(title = "Dashboard Analisis Kerentanan Sosial Indonesia"),
  
  dashboardSidebar(
    sidebarMenu(
      menuItem("Overview", tabName = "overview", icon = icon("home")),
      menuItem("Peta Interaktif", tabName = "map", icon = icon("map")),
      menuItem("Analisis Statistik", tabName = "stats", icon = icon("chart-bar")),
      menuItem("Data Explorer", tabName = "data", icon = icon("table")),
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
        }
        .leaflet-container {
          border-radius: 5px;
        }
      "))
    ),
    
    tabItems(
      # Tab Overview
      tabItem(tabName = "overview",
        fluidRow(
          box(
            title = "Ringkasan Dataset", status = "primary", solidHeader = TRUE,
            width = 12, height = "200px",
            h4("Dataset Kerentanan Sosial Indonesia"),
            p("Dataset ini berisi 17 variabel kerentanan sosial untuk", nrow(sovi_data), "region di Indonesia."),
            p("Variabel utama meliputi demografi, sosial-ekonomi, dan infrastruktur."),
            p("SOVI Index dihitung berdasarkan standardisasi dan pembobotan 11 variabel kunci.")
          )
        ),
        
        fluidRow(
          valueBoxOutput("total_regions"),
          valueBoxOutput("avg_sovi"),
          valueBoxOutput("high_vulnerability")
        ),
        
        fluidRow(
          box(
            title = "Distribusi SOVI Index", status = "info", solidHeader = TRUE,
            width = 6,
            plotlyOutput("sovi_distribution")
          ),
          box(
            title = "Kategori Kerentanan", status = "success", solidHeader = TRUE,
            width = 6,
            plotlyOutput("vulnerability_categories")
          )
        )
      ),
      
      # Tab Peta - SECTION YANG DIPERBAIKI
      tabItem(tabName = "map",
        fluidRow(
          box(
            title = "Kontrol Peta", status = "warning", solidHeader = TRUE,
            width = 3,
            selectInput("map_variable", "Pilih Variabel untuk Peta:",
                       choices = c("SOVI_INDEX", "POVERTY", "ILLITERATE", "NOELECTRIC", 
                                 "CHILDREN", "ELDERLY", "POPULATION"),
                       selected = "SOVI_INDEX"),
            br(),
            checkboxInput("show_labels", "Tampilkan Label Region", value = TRUE),
            br(),
            downloadButton("download_map", "Download Peta", class = "btn-primary")
          ),
          
          box(
            title = "Peta Kerentanan Sosial Indonesia", status = "primary", solidHeader = TRUE,
            width = 9,
            leafletOutput("interactive_map", height = "600px")
          )
        ),
        
        fluidRow(
          box(
            title = "Informasi Peta", status = "info", solidHeader = TRUE,
            width = 12,
            h5("Panduan Penggunaan Peta:"),
            tags$ul(
              tags$li("Klik pada marker untuk melihat detail informasi region"),
              tags$li("Gunakan kontrol layer di kanan atas untuk mengganti base map"),
              tags$li("Ukuran marker menunjukkan jumlah populasi"),
              tags$li("Warna marker menunjukkan nilai variabel yang dipilih"),
              tags$li("Hover pada marker untuk melihat nama region")
            )
          )
        )
      ),
      
      # Tab Analisis Statistik
      tabItem(tabName = "stats",
        fluidRow(
          box(
            title = "Kontrol Analisis", status = "warning", solidHeader = TRUE,
            width = 3,
            selectInput("x_variable", "Variabel X:",
                       choices = names(select_if(sovi_data, is.numeric)),
                       selected = "POVERTY"),
            selectInput("y_variable", "Variabel Y:",
                       choices = names(select_if(sovi_data, is.numeric)),
                       selected = "SOVI_INDEX"),
            br(),
            actionButton("run_correlation", "Analisis Korelasi", class = "btn-success")
          ),
          
          box(
            title = "Scatter Plot", status = "primary", solidHeader = TRUE,
            width = 9,
            plotlyOutput("scatter_plot", height = "400px")
          )
        ),
        
        fluidRow(
          box(
            title = "Matriks Korelasi", status = "info", solidHeader = TRUE,
            width = 6,
            plotOutput("correlation_matrix")
          ),
          
          box(
            title = "Statistik Deskriptif", status = "success", solidHeader = TRUE,
            width = 6,
            DT::dataTableOutput("descriptive_stats")
          )
        )
      ),
      
      # Tab Data Explorer
      tabItem(tabName = "data",
        fluidRow(
          box(
            title = "Filter Data", status = "warning", solidHeader = TRUE,
            width = 3,
            selectInput("filter_category", "Filter berdasarkan Kategori SOVI:",
                       choices = c("Semua", levels(sovi_data$SOVI_CATEGORY)),
                       selected = "Semua"),
            br(),
            numericInput("min_population", "Populasi Minimum:",
                        value = min(sovi_data$POPULATION),
                        min = min(sovi_data$POPULATION),
                        max = max(sovi_data$POPULATION)),
            br(),
            downloadButton("download_data", "Download Data", class = "btn-primary")
          ),
          
          box(
            title = "Data SOVI", status = "primary", solidHeader = TRUE,
            width = 9,
            DT::dataTableOutput("sovi_table")
          )
        )
      ),
      
      # Tab Metadata
      tabItem(tabName = "metadata",
        fluidRow(
          box(
            title = "Metadata Variabel", status = "primary", solidHeader = TRUE,
            width = 12,
            DT::dataTableOutput("metadata_table")
          )
        ),
        
        fluidRow(
          box(
            title = "Informasi Dataset", status = "info", solidHeader = TRUE,
            width = 6,
            h5("Sumber Data:"),
            p("Dataset simulasi berdasarkan struktur SOVI (Social Vulnerability Index)"),
            h5("Metodologi SOVI:"),
            p("SOVI dihitung menggunakan standardisasi Z-score dan pembobotan berdasarkan literatur ilmiah."),
            h5("Periode Data:"),
            p("Data simulasi untuk tahun 2023")
          ),
          
          box(
            title = "Statistik Dataset", status = "success", solidHeader = TRUE,
            width = 6,
            verbatimTextOutput("dataset_summary")
          )
        )
      )
    )
  )
)

# ===============================================================================
# 8. SERVER LOGIC
# ===============================================================================
server <- function(input, output, session) {
  
  # Reactive data berdasarkan filter
  filtered_data <- reactive({
    data <- sovi_data
    
    if(input$filter_category != "Semua") {
      data <- data[data$SOVI_CATEGORY == input$filter_category, ]
    }
    
    if(!is.null(input$min_population)) {
      data <- data[data$POPULATION >= input$min_population, ]
    }
    
    return(data)
  })
  
  # Value boxes untuk overview
  output$total_regions <- renderValueBox({
    valueBox(
      value = nrow(sovi_data),
      subtitle = "Total Region",
      icon = icon("map-marker-alt"),
      color = "blue"
    )
  })
  
  output$avg_sovi <- renderValueBox({
    valueBox(
      value = round(mean(sovi_data$SOVI_INDEX), 3),
      subtitle = "Rata-rata SOVI Index",
      icon = icon("chart-line"),
      color = "green"
    )
  })
  
  output$high_vulnerability <- renderValueBox({
    high_vuln <- sum(sovi_data$SOVI_CATEGORY %in% c("Tinggi", "Sangat Tinggi"))
    valueBox(
      value = high_vuln,
      subtitle = "Region Kerentanan Tinggi",
      icon = icon("exclamation-triangle"),
      color = "red"
    )
  })
  
  # Plot distribusi SOVI
  output$sovi_distribution <- renderPlotly({
    p <- ggplot(sovi_data, aes(x = SOVI_INDEX)) +
      geom_histogram(bins = 20, fill = "steelblue", alpha = 0.7) +
      labs(title = "Distribusi SOVI Index", x = "SOVI Index", y = "Frekuensi") +
      theme_minimal()
    
    ggplotly(p)
  })
  
  # Plot kategori kerentanan
  output$vulnerability_categories <- renderPlotly({
    category_counts <- table(sovi_data$SOVI_CATEGORY)
    
    p <- ggplot(data.frame(category_counts), aes(x = Var1, y = Freq, fill = Var1)) +
      geom_bar(stat = "identity") +
      labs(title = "Distribusi Kategori Kerentanan", x = "Kategori", y = "Jumlah Region") +
      theme_minimal() +
      theme(legend.position = "none")
    
    ggplotly(p)
  })
  
  # PETA INTERAKTIF - IMPLEMENTASI YANG DIPERBAIKI
  output$interactive_map <- renderLeaflet({
    create_interactive_map(sovi_data, input$map_variable)
  })
  
  # Update peta ketika variabel berubah
  observe({
    leafletProxy("interactive_map", data = sovi_data) %>%
      clearMarkers() %>%
      clearControls()
    
    # Buat color palette baru
    pal <- create_color_palette(sovi_data, input$map_variable)
    popup_content <- create_popup_content(sovi_data)
    
    # Tambahkan markers baru
    leafletProxy("interactive_map", data = sovi_data) %>%
      addCircleMarkers(
        lng = ~LONGITUDE,
        lat = ~LATITUDE,
        radius = ~sqrt(POPULATION/10000),
        color = "white",
        weight = 1,
        fillColor = ~pal(get(input$map_variable)),
        fillOpacity = 0.7,
        popup = popup_content,
        label = if(input$show_labels) ~REGION else NULL,
        labelOptions = labelOptions(
          style = list("font-weight" = "normal", padding = "3px 8px"),
          textsize = "13px",
          direction = "auto"
        )
      )
    
    # Tambahkan legend baru
    if(is.numeric(sovi_data[[input$map_variable]])) {
      leafletProxy("interactive_map") %>%
        addLegend(
          pal = pal,
          values = sovi_data[[input$map_variable]],
          opacity = 0.7,
          title = input$map_variable,
          position = "bottomright",
          layerId = "legend"
        )
    }
  })
  
  # Scatter plot
  output$scatter_plot <- renderPlotly({
    p <- ggplot(sovi_data, aes_string(x = input$x_variable, y = input$y_variable)) +
      geom_point(aes(color = SOVI_CATEGORY, size = POPULATION), alpha = 0.7) +
      geom_smooth(method = "lm", se = TRUE) +
      labs(title = paste("Hubungan", input$x_variable, "vs", input$y_variable)) +
      theme_minimal()
    
    ggplotly(p)
  })
  
  # Matriks korelasi
  output$correlation_matrix <- renderPlot({
    numeric_vars <- select_if(sovi_data, is.numeric)
    cor_matrix <- cor(numeric_vars, use = "complete.obs")
    
    corrplot(cor_matrix, method = "color", type = "upper", 
             order = "hclust", tl.cex = 0.8, tl.col = "black")
  })
  
  # Statistik deskriptif
  output$descriptive_stats <- DT::renderDataTable({
    numeric_data <- select_if(sovi_data, is.numeric)
    desc_stats <- data.frame(
      Variabel = names(numeric_data),
      Mean = round(sapply(numeric_data, mean, na.rm = TRUE), 3),
      Median = round(sapply(numeric_data, median, na.rm = TRUE), 3),
      SD = round(sapply(numeric_data, sd, na.rm = TRUE), 3),
      Min = round(sapply(numeric_data, min, na.rm = TRUE), 3),
      Max = round(sapply(numeric_data, max, na.rm = TRUE), 3)
    )
    
    DT::datatable(desc_stats, options = list(pageLength = 15, scrollX = TRUE))
  })
  
  # Tabel data SOVI
  output$sovi_table <- DT::renderDataTable({
    DT::datatable(filtered_data(), 
                  options = list(pageLength = 15, scrollX = TRUE),
                  filter = "top")
  })
  
  # Tabel metadata
  output$metadata_table <- DT::renderDataTable({
    DT::datatable(metadata_sovi, 
                  options = list(pageLength = 20, scrollX = TRUE))
  })
  
  # Summary dataset
  output$dataset_summary <- renderPrint({
    summary(select_if(sovi_data, is.numeric))
  })
  
  # Download handlers
  output$download_data <- downloadHandler(
    filename = function() {
      paste("sovi_data_", Sys.Date(), ".csv", sep = "")
    },
    content = function(file) {
      write.csv(filtered_data(), file, row.names = FALSE)
    }
  )
  
  output$download_map <- downloadHandler(
    filename = function() {
      paste("sovi_map_", Sys.Date(), ".html", sep = "")
    },
    content = function(file) {
      map <- create_interactive_map(sovi_data, input$map_variable)
      htmlwidgets::saveWidget(map, file)
    }
  )
}

# ===============================================================================
# 9. RUN APPLICATION
# ===============================================================================
shinyApp(ui = ui, server = server)