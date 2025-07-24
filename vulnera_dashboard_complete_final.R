# ===============================================================================
# DASHBOARD ANALISIS KERENTANAN SOSIAL INDONESIA - VERSI LENGKAP FINAL
# File: vulnera_dashboard_complete_final.R
# Author: Social Vulnerability Analysis Team
# Date: 2024
# Description: Comprehensive dashboard for social vulnerability analysis in Indonesia
# ===============================================================================

# ===============================================================================
# 1. LOAD REQUIRED LIBRARIES
# ===============================================================================
suppressMessages({
  library(shiny)
  library(shinydashboard)
  library(shinyjs)
  library(shinycssloaders)
  library(shinyWidgets)
  library(DT)
  library(ggplot2)
  library(plotly)
  library(dplyr)
  library(tidyr)
  library(readr)
  library(leaflet)
  library(leaflet.extras)
  library(RColorBrewer)
  library(viridis)
  library(htmltools)
  library(htmlwidgets)
  library(corrplot)
  library(car)
  library(psych)
  library(VIM)
  library(mice)
  library(Hmisc)
  library(knitr)
  library(rmarkdown)
  library(openxlsx)
  library(zip)
  library(gridExtra)
  library(scales)
  library(lubridate)
  library(stringr)
  library(forcats)
  library(purrr)
  library(tibble)
  library(jsonlite)
  library(stats)
  library(utils)
  library(grDevices)
  library(graphics)
})

# ===============================================================================
# 2. GLOBAL CONFIGURATIONS
# ===============================================================================

# Set options
options(shiny.maxRequestSize = 30*1024^2)  # 30MB max file size
options(DT.options = list(pageLength = 25, scrollX = TRUE, scrollY = "400px"))

# Custom CSS
custom_css <- "
.content-wrapper, .right-side {
  background-color: #f4f4f4;
}
.box {
  border-radius: 8px;
  box-shadow: 0 2px 4px rgba(0,0,0,0.1);
}
.leaflet-container {
  border-radius: 8px;
}
.navbar-custom-menu > .navbar-nav > li > .dropdown-menu {
  border-radius: 4px;
}
.main-header .navbar {
  border-bottom: 2px solid #3c8dbc;
}
.sidebar-menu > li.active > a {
  border-left: 3px solid #3c8dbc;
}
.value-box-icon {
  border-radius: 50%;
}
.small-box .icon {
  top: 10px;
  right: 15px;
}
.info-box {
  border-radius: 8px;
}
.progress-bar {
  border-radius: 4px;
}
"

# ===============================================================================
# 3. METADATA DEFINITIONS
# ===============================================================================

# Complete metadata for all 17 variables
metadata_sovi <- data.frame(
  Variabel = c("DISTRICTCODE", "CHILDREN", "FEMALE", "ELDERLY", "FHEAD", "FAMILYSIZE",
               "NOELECTRIC", "LOWEDU", "GROWTH", "POVERTY", "ILLITERATE", "NOTRAINING",
               "LATITUDE", "LONGITUDE", "REGION", "POPULATION", "AREA"),
  Deskripsi = c(
    "Kode unik untuk setiap distrik/kabupaten di Indonesia",
    "Persentase populasi anak-anak di bawah lima tahun",
    "Persentase populasi perempuan dalam total populasi",
    "Persentase populasi lanjut usia 65 tahun ke atas",
    "Persentase rumah tangga dengan kepala rumah tangga perempuan",
    "Rata-rata jumlah anggota rumah tangga dalam satu distrik",
    "Persentase rumah tangga yang tidak menggunakan listrik sebagai sumber penerangan utama",
    "Persentase populasi 15 tahun ke atas dengan tingkat pendidikan rendah (tidak tamat SD)",
    "Persentase perubahan populasi (tingkat pertumbuhan populasi tahunan)",
    "Persentase penduduk yang hidup di bawah garis kemiskinan",
    "Persentase populasi yang tidak bisa membaca dan menulis (buta huruf)",
    "Persentase rumah tangga yang tidak pernah mendapatkan pelatihan mitigasi bencana",
    "Koordinat lintang geografis dalam sistem desimal",
    "Koordinat bujur geografis dalam sistem desimal",
    "Nama wilayah/region (provinsi, kota, atau kabupaten)",
    "Jumlah total populasi dalam satu wilayah",
    "Luas wilayah dalam kilometer persegi (km²)"
  ),
  Tipe = c("Kategorik/ID", rep("Numerik", 11), "Numerik", "Numerik", "Kategorik", "Numerik", "Numerik"),
  Satuan = c("Kode", rep("%", 9), "%", "%", "Desimal", "Desimal", "Nama", "Jiwa", "km²"),
  Rentang = c("ID001-ID999", "0-30", "45-55", "0-20", "5-40", "2-8", "0-50", "0-60", "-5 s/d 15", "0-50", "0-40", "0-100", "-11 s/d 6", "95 s/d 141", "Teks", "10,000-5,000,000", "50-10,000"),
  stringsAsFactors = FALSE
)

# Variable categories for analysis
vulnerability_indicators <- c("CHILDREN", "ELDERLY", "FHEAD", "NOELECTRIC", "LOWEDU", "POVERTY", "ILLITERATE", "NOTRAINING")
demographic_vars <- c("CHILDREN", "FEMALE", "ELDERLY", "FAMILYSIZE", "POPULATION")
socioeconomic_vars <- c("POVERTY", "ILLITERATE", "LOWEDU", "FHEAD")
infrastructure_vars <- c("NOELECTRIC", "NOTRAINING")
geographic_vars <- c("LATITUDE", "LONGITUDE", "AREA")

# ===============================================================================
# 4. DATA GENERATION FUNCTIONS
# ===============================================================================

# Function to create realistic Indonesian region names
create_indonesia_region_names <- function(n = 100) {
  set.seed(42)  # For reproducibility
  
  # Comprehensive list of Indonesian cities and regions
  indonesia_regions <- c(
    # Jakarta and surrounding
    "DKI Jakarta", "Jakarta Pusat", "Jakarta Utara", "Jakarta Selatan", 
    "Jakarta Timur", "Jakarta Barat", "Kepulauan Seribu",
    
    # West Java
    "Bandung", "Bekasi", "Depok", "Tangerang", "Tangerang Selatan", "Bogor", 
    "Cirebon", "Sukabumi", "Tasikmalaya", "Banjar", "Cimahi",
    
    # Central Java
    "Semarang", "Surakarta", "Yogyakarta", "Magelang", "Salatiga", "Tegal", 
    "Pekalongan", "Purwokerto", "Klaten", "Wonogiri", "Karanganyar",
    
    # East Java
    "Surabaya", "Malang", "Kediri", "Blitar", "Probolinggo", "Pasuruan", 
    "Mojokerto", "Madiun", "Jember", "Banyuwangi", "Sidoarjo",
    
    # North Sumatra
    "Medan", "Binjai", "Tebing Tinggi", "Pematangsiantar", "Tanjungbalai",
    "Sibolga", "Padangsidimpuan", "Gunungsitoli",
    
    # West Sumatra
    "Padang", "Bukittinggi", "Padangpanjang", "Payakumbuh", "Pariaman",
    "Sawahlunto", "Solok", "Batusangkar",
    
    # South Sumatra
    "Palembang", "Prabumulih", "Pagar Alam", "Lubuklinggau", "Lahat",
    
    # Riau
    "Pekanbaru", "Dumai", "Batam", "Tanjungpinang", "Bengkalis",
    
    # Jambi
    "Jambi", "Sungai Penuh", "Muaro Jambi", "Bungo",
    
    # Bengkulu
    "Bengkulu", "Curup", "Argamakmur", "Manna",
    
    # Lampung
    "Bandar Lampung", "Metro", "Kotabumi", "Liwa", "Kalianda",
    
    # Kalimantan
    "Pontianak", "Singkawang", "Banjarmasin", "Banjarbaru", "Samarinda", 
    "Balikpapan", "Bontang", "Tarakan", "Palangka Raya",
    
    # Sulawesi
    "Makassar", "Parepare", "Palopo", "Manado", "Bitung", "Tomohon", 
    "Kotamobagu", "Palu", "Kendari", "Bau-Bau", "Gorontalo",
    
    # Bali and Nusa Tenggara
    "Denpasar", "Singaraja", "Tabanan", "Gianyar", "Mataram", "Bima", 
    "Dompu", "Kupang", "Ende", "Maumere",
    
    # Maluku and Papua
    "Ambon", "Tual", "Ternate", "Tidore", "Jayapura", "Sorong", 
    "Merauke", "Nabire", "Timika", "Wamena"
  )
  
  # If n is larger than available regions, sample with replacement
  if(n > length(indonesia_regions)) {
    return(sample(indonesia_regions, n, replace = TRUE))
  } else {
    return(sample(indonesia_regions, n, replace = FALSE))
  }
}

# Function to generate realistic SOVI data
generate_sovi_data <- function(n_regions = 100) {
  set.seed(42)  # For reproducibility
  
  regions <- create_indonesia_region_names(n_regions)
  
  # Define realistic coordinate ranges for Indonesia
  lat_range <- c(-11.0, 6.0)   # Indonesia latitude range
  lon_range <- c(95.0, 141.0)  # Indonesia longitude range
  
  # Generate correlated variables for more realistic data
  base_poverty <- runif(n_regions, 2, 45)
  base_education <- runif(n_regions, 5, 50)
  
  data <- data.frame(
    DISTRICTCODE = sprintf("ID%03d", 1:n_regions),
    
    # Demographics (with some correlation)
    CHILDREN = pmax(3, pmin(30, rnorm(n_regions, 15, 5))),
    FEMALE = pmax(47, pmin(53, rnorm(n_regions, 50, 1.5))),
    ELDERLY = pmax(2, pmin(20, rnorm(n_regions, 8, 3))),
    FHEAD = pmax(8, pmin(40, rnorm(n_regions, 22, 6))),
    FAMILYSIZE = pmax(2.0, pmin(7.0, rnorm(n_regions, 4.2, 0.8))),
    
    # Infrastructure and services
    NOELECTRIC = pmax(0, pmin(45, base_poverty * 0.6 + rnorm(n_regions, 5, 8))),
    LOWEDU = pmax(3, pmin(55, base_education + rnorm(n_regions, 0, 8))),
    GROWTH = pmax(-3, pmin(12, rnorm(n_regions, 2.5, 2.5))),
    
    # Socioeconomic indicators (correlated)
    POVERTY = round(base_poverty, 2),
    ILLITERATE = pmax(0.5, pmin(35, base_education * 0.4 + rnorm(n_regions, 3, 5))),
    NOTRAINING = pmax(15, pmin(85, base_poverty * 1.2 + rnorm(n_regions, 25, 15))),
    
    # Geographic coordinates
    LATITUDE = round(runif(n_regions, lat_range[1], lat_range[2]), 6),
    LONGITUDE = round(runif(n_regions, lon_range[1], lon_range[2]), 6),
    
    # Region information
    REGION = regions,
    POPULATION = round(exp(rnorm(n_regions, log(200000), 0.8))),
    AREA = round(exp(rnorm(n_regions, log(1000), 0.6)), 2),
    
    stringsAsFactors = FALSE
  )
  
  # Round numeric columns to appropriate decimal places
  numeric_cols <- c("CHILDREN", "FEMALE", "ELDERLY", "FHEAD", "FAMILYSIZE", 
                   "NOELECTRIC", "LOWEDU", "GROWTH", "POVERTY", "ILLITERATE", "NOTRAINING")
  data[numeric_cols] <- lapply(data[numeric_cols], function(x) round(x, 2))
  
  return(data)
}

# ===============================================================================
# 5. SOVI CALCULATION FUNCTIONS
# ===============================================================================

# Function to calculate SOVI index with multiple methods
calculate_sovi_index <- function(data, method = "weighted") {
  # Define variables for SOVI calculation
  sovi_vars <- c("CHILDREN", "FEMALE", "ELDERLY", "FHEAD", "FAMILYSIZE",
                 "NOELECTRIC", "LOWEDU", "GROWTH", "POVERTY", "ILLITERATE", "NOTRAINING")
  
  # Check if all required variables exist
  missing_vars <- setdiff(sovi_vars, names(data))
  if(length(missing_vars) > 0) {
    stop(paste("Missing variables:", paste(missing_vars, collapse = ", ")))
  }
  
  # Standardize variables (Z-score normalization)
  data_std <- data
  for(var in sovi_vars) {
    data_std[[var]] <- scale(data[[var]])[,1]
  }
  
  # Calculate SOVI based on method
  if(method == "weighted") {
    # Weighted approach based on literature
    weights <- c(
      CHILDREN = 0.12,     # Children vulnerability
      FEMALE = 0.08,       # Gender factor
      ELDERLY = 0.12,      # Elderly vulnerability
      FHEAD = 0.09,        # Female-headed households
      FAMILYSIZE = 0.06,   # Household size
      NOELECTRIC = 0.10,   # Infrastructure access
      LOWEDU = 0.13,       # Education level
      GROWTH = 0.05,       # Population dynamics
      POVERTY = 0.18,      # Economic vulnerability (highest weight)
      ILLITERATE = 0.12,   # Literacy rate
      NOTRAINING = 0.05    # Disaster preparedness
    )
    
    data$SOVI_INDEX <- rowSums(data_std[sovi_vars] * weights[sovi_vars])
    
  } else if(method == "equal") {
    # Equal weight approach
    data$SOVI_INDEX <- rowMeans(data_std[sovi_vars])
    
  } else if(method == "pca") {
    # Principal Component Analysis approach
    pca_result <- prcomp(data_std[sovi_vars], scale. = FALSE)
    data$SOVI_INDEX <- pca_result$x[,1]  # First principal component
  }
  
  # Create vulnerability categories
  data$SOVI_CATEGORY <- cut(
    data$SOVI_INDEX,
    breaks = quantile(data$SOVI_INDEX, c(0, 0.25, 0.5, 0.75, 1), na.rm = TRUE),
    labels = c("Rendah", "Sedang", "Tinggi", "Sangat Tinggi"),
    include.lowest = TRUE
  )
  
  # Add vulnerability level as numeric
  data$VULNERABILITY_LEVEL <- as.numeric(data$SOVI_CATEGORY)
  
  # Calculate percentile ranks
  data$SOVI_PERCENTILE <- round(rank(data$SOVI_INDEX) / length(data$SOVI_INDEX) * 100, 1)
  
  return(data)
}

# Function to calculate additional vulnerability metrics
calculate_vulnerability_metrics <- function(data) {
  # Composite indices
  data$DEMOGRAPHIC_INDEX <- rowMeans(scale(data[c("CHILDREN", "ELDERLY", "FHEAD")]))
  data$SOCIOECONOMIC_INDEX <- rowMeans(scale(data[c("POVERTY", "ILLITERATE", "LOWEDU")]))
  data$INFRASTRUCTURE_INDEX <- rowMeans(scale(data[c("NOELECTRIC", "NOTRAINING")]))
  
  # Population density
  data$POPULATION_DENSITY <- round(data$POPULATION / data$AREA, 2)
  
  # Risk categories
  data$POVERTY_RISK <- cut(data$POVERTY, 
                          breaks = c(0, 10, 20, 35, 100),
                          labels = c("Rendah", "Sedang", "Tinggi", "Sangat Tinggi"))
  
  data$EDUCATION_RISK <- cut(data$LOWEDU,
                            breaks = c(0, 15, 30, 45, 100),
                            labels = c("Rendah", "Sedang", "Tinggi", "Sangat Tinggi"))
  
  return(data)
}

# ===============================================================================
# 6. MAP VISUALIZATION FUNCTIONS - ENHANCED VERSION
# ===============================================================================

# Function to create base map with multiple providers
create_base_map <- function() {
  leaflet() %>%
    addTiles(group = "OpenStreetMap") %>%
    addProviderTiles(providers$CartoDB.Positron, group = "CartoDB Light") %>%
    addProviderTiles(providers$CartoDB.DarkMatter, group = "CartoDB Dark") %>%
    addProviderTiles(providers$Esri.WorldImagery, group = "Satellite") %>%
    addProviderTiles(providers$Stamen.Terrain, group = "Terrain") %>%
    setView(lng = 118, lat = -2, zoom = 5) %>%
    addLayersControl(
      baseGroups = c("OpenStreetMap", "CartoDB Light", "CartoDB Dark", "Satellite", "Terrain"),
      options = layersControlOptions(collapsed = FALSE)
    ) %>%
    addScaleBar(position = "bottomleft") %>%
    addMiniMap(
      tiles = providers$CartoDB.Positron,
      toggleDisplay = TRUE,
      minimized = TRUE
    )
}

# Function to create dynamic color palette
create_color_palette <- function(data, variable, palette_type = "viridis") {
  if(is.numeric(data[[variable]])) {
    if(palette_type == "viridis") {
      colorNumeric(
        palette = viridis::viridis(10),
        domain = data[[variable]],
        na.color = "transparent"
      )
    } else if(palette_type == "rdylbu") {
      colorNumeric(
        palette = "RdYlBu",
        domain = data[[variable]],
        reverse = TRUE,
        na.color = "transparent"
      )
    } else if(palette_type == "spectral") {
      colorNumeric(
        palette = "Spectral",
        domain = data[[variable]],
        reverse = TRUE,
        na.color = "transparent"
      )
    }
  } else {
    # For categorical variables
    n_categories <- length(unique(data[[variable]]))
    if(n_categories <= 9) {
      colors <- RColorBrewer::brewer.pal(max(3, n_categories), "Set1")
    } else {
      colors <- rainbow(n_categories)
    }
    
    colorFactor(
      palette = colors,
      domain = data[[variable]],
      na.color = "transparent"
    )
  }
}

# Function to create comprehensive popup content
create_popup_content <- function(data) {
  paste0(
    "<div style='font-family: Arial, sans-serif; max-width: 300px;'>",
    "<h4 style='margin: 0 0 10px 0; color: #2c3e50; border-bottom: 2px solid #3498db; padding-bottom: 5px;'>",
    "<i class='fa fa-map-marker'></i> ", data$REGION, "</h4>",
    
    "<table style='width: 100%; font-size: 12px;'>",
    "<tr><td><strong>Kode Distrik:</strong></td><td>", data$DISTRICTCODE, "</td></tr>",
    "<tr><td><strong>SOVI Index:</strong></td><td><span style='color: #e74c3c; font-weight: bold;'>", 
    round(data$SOVI_INDEX, 3), "</span></td></tr>",
    "<tr><td><strong>Kategori:</strong></td><td><span style='color: #8e44ad; font-weight: bold;'>", 
    data$SOVI_CATEGORY, "</span></td></tr>",
    "<tr><td><strong>Percentile:</strong></td><td>", data$SOVI_PERCENTILE, "%</td></tr>",
    "<tr><td colspan='2'><hr style='margin: 8px 0;'></td></tr>",
    "<tr><td><strong>Populasi:</strong></td><td>", format(data$POPULATION, big.mark = ","), " jiwa</td></tr>",
    "<tr><td><strong>Luas:</strong></td><td>", data$AREA, " km²</td></tr>",
    "<tr><td><strong>Kepadatan:</strong></td><td>", round(data$POPULATION/data$AREA, 1), " jiwa/km²</td></tr>",
    "<tr><td colspan='2'><hr style='margin: 8px 0;'></td></tr>",
    "<tr><td><strong>Kemiskinan:</strong></td><td>", data$POVERTY, "%</td></tr>",
    "<tr><td><strong>Buta Huruf:</strong></td><td>", data$ILLITERATE, "%</td></tr>",
    "<tr><td><strong>Tanpa Listrik:</strong></td><td>", data$NOELECTRIC, "%</td></tr>",
    "<tr><td><strong>Pendidikan Rendah:</strong></td><td>", data$LOWEDU, "%</td></tr>",
    "</table>",
    "</div>"
  )
}

# Main function to create interactive map
create_interactive_map <- function(data, color_variable = "SOVI_INDEX", 
                                 palette_type = "viridis", cluster_markers = FALSE) {
  
  # Create base map
  map <- create_base_map()
  
  # Create color palette
  pal <- create_color_palette(data, color_variable, palette_type)
  
  # Create popup content
  popup_content <- create_popup_content(data)
  
  # Add markers
  if(cluster_markers) {
    map <- map %>%
      addCircleMarkers(
        data = data,
        lng = ~LONGITUDE,
        lat = ~LATITUDE,
        radius = ~sqrt(POPULATION/15000) + 3,
        color = "white",
        weight = 2,
        fillColor = ~pal(get(color_variable)),
        fillOpacity = 0.8,
        popup = popup_content,
        label = ~paste(REGION, "-", get(color_variable)),
        labelOptions = labelOptions(
          style = list("font-weight" = "bold", padding = "3px 8px"),
          textsize = "14px",
          direction = "auto"
        ),
        clusterOptions = markerClusterOptions()
      )
  } else {
    map <- map %>%
      addCircleMarkers(
        data = data,
        lng = ~LONGITUDE,
        lat = ~LATITUDE,
        radius = ~sqrt(POPULATION/15000) + 3,
        color = "white",
        weight = 2,
        fillColor = ~pal(get(color_variable)),
        fillOpacity = 0.8,
        popup = popup_content,
        label = ~paste(REGION, "-", get(color_variable)),
        labelOptions = labelOptions(
          style = list("font-weight" = "bold", padding = "3px 8px"),
          textsize = "14px",
          direction = "auto"
        )
      )
  }
  
  # Add legend
  if(is.numeric(data[[color_variable]])) {
    map <- map %>%
      addLegend(
        pal = pal,
        values = data[[color_variable]],
        opacity = 0.8,
        title = paste("Nilai", color_variable),
        position = "bottomright",
        labFormat = labelFormat(suffix = ifelse(color_variable %in% 
                                              c("LATITUDE", "LONGITUDE", "SOVI_INDEX"), "", "%"))
      )
  }
  
  return(map)
}

# ===============================================================================
# 7. STATISTICAL ANALYSIS FUNCTIONS
# ===============================================================================

# Function to perform correlation analysis
perform_correlation_analysis <- function(data) {
  numeric_vars <- select_if(data, is.numeric)
  numeric_vars <- numeric_vars[sapply(numeric_vars, function(x) length(unique(x)) > 1)]
  
  cor_matrix <- cor(numeric_vars, use = "complete.obs")
  
  # Find highest correlations
  cor_df <- expand.grid(Var1 = rownames(cor_matrix), Var2 = colnames(cor_matrix))
  cor_df$Correlation <- as.vector(cor_matrix)
  cor_df <- cor_df[cor_df$Var1 != cor_df$Var2, ]
  cor_df <- cor_df[order(abs(cor_df$Correlation), decreasing = TRUE), ]
  
  return(list(matrix = cor_matrix, top_correlations = head(cor_df, 20)))
}

# Function to generate descriptive statistics
generate_descriptive_stats <- function(data) {
  numeric_data <- select_if(data, is.numeric)
  
  desc_stats <- data.frame(
    Variable = names(numeric_data),
    N = sapply(numeric_data, function(x) sum(!is.na(x))),
    Mean = round(sapply(numeric_data, mean, na.rm = TRUE), 3),
    Median = round(sapply(numeric_data, median, na.rm = TRUE), 3),
    SD = round(sapply(numeric_data, sd, na.rm = TRUE), 3),
    Min = round(sapply(numeric_data, min, na.rm = TRUE), 3),
    Max = round(sapply(numeric_data, max, na.rm = TRUE), 3),
    Q25 = round(sapply(numeric_data, quantile, 0.25, na.rm = TRUE), 3),
    Q75 = round(sapply(numeric_data, quantile, 0.75, na.rm = TRUE), 3),
    Skewness = round(sapply(numeric_data, function(x) {
      if(length(unique(x)) > 1) psych::skew(x, na.rm = TRUE) else NA
    }), 3),
    Kurtosis = round(sapply(numeric_data, function(x) {
      if(length(unique(x)) > 1) psych::kurtosi(x, na.rm = TRUE) else NA
    }), 3),
    stringsAsFactors = FALSE
  )
  
  return(desc_stats)
}

# ===============================================================================
# 8. DATA INITIALIZATION
# ===============================================================================

# Generate the main dataset
sovi_data <- generate_sovi_data(100)
sovi_data <- calculate_sovi_index(sovi_data, method = "weighted")
sovi_data <- calculate_vulnerability_metrics(sovi_data)

# ===============================================================================
# 9. USER INTERFACE DEFINITION
# ===============================================================================

ui <- dashboardPage(
  skin = "blue",
  
  # Header
  dashboardHeader(
    title = "Dashboard Analisis Kerentanan Sosial Indonesia",
    titleWidth = 400,
    dropdownMenu(
      type = "messages",
      messageItem(
        from = "System",
        message = "Dashboard berhasil dimuat",
        icon = icon("check"),
        time = Sys.time()
      )
    ),
    dropdownMenu(
      type = "notifications",
      notificationItem(
        text = paste(nrow(sovi_data), "region telah dianalisis"),
        icon = icon("map"),
        status = "success"
      )
    )
  ),
  
  # Sidebar
  dashboardSidebar(
    width = 250,
    sidebarMenu(
      id = "sidebar_menu",
      menuItem("🏠 Dashboard Utama", tabName = "overview", icon = icon("home")),
      menuItem("🗺️ Peta Interaktif", tabName = "map", icon = icon("map")),
      menuItem("📊 Analisis Statistik", tabName = "stats", icon = icon("chart-bar")),
      menuItem("🔍 Eksplorasi Data", tabName = "data", icon = icon("table")),
      menuItem("📈 Visualisasi Lanjutan", tabName = "advanced", icon = icon("chart-line")),
      menuItem("📋 Metadata", tabName = "metadata", icon = icon("info-circle")),
      menuItem("📥 Download & Export", tabName = "export", icon = icon("download")),
      
      br(),
      div(style = "padding: 10px;",
          h5("Informasi Dataset", style = "color: white;"),
          p(paste("Total Region:", nrow(sovi_data)), style = "color: #ecf0f1; font-size: 12px;"),
          p(paste("Variabel:", ncol(sovi_data)), style = "color: #ecf0f1; font-size: 12px;"),
          p(paste("Update:", Sys.Date()), style = "color: #ecf0f1; font-size: 12px;")
      )
    )
  ),
  
  # Body
  dashboardBody(
    useShinyjs(),
    tags$head(
      tags$style(HTML(custom_css)),
      tags$link(rel = "stylesheet", href = "https://cdnjs.cloudflare.com/ajax/libs/font-awesome/5.15.4/css/all.min.css")
    ),
    
    tabItems(
      # ===== TAB 1: OVERVIEW =====
      tabItem(tabName = "overview",
        fluidRow(
          box(
            title = "Selamat Datang di Dashboard Kerentanan Sosial Indonesia", 
            status = "primary", solidHeader = TRUE, width = 12, height = "180px",
            div(style = "padding: 20px;",
                h3("🇮🇩 Social Vulnerability Index (SOVI) Dashboard", style = "color: #2c3e50; margin-bottom: 15px;"),
                p("Dashboard komprehensif untuk analisis kerentanan sosial di Indonesia berdasarkan 17 indikator utama.", 
                  style = "font-size: 16px; line-height: 1.6;"),
                p("Dataset mencakup", strong(nrow(sovi_data)), "region dengan analisis mendalam terhadap faktor demografi, sosial-ekonomi, dan infrastruktur.",
                  style = "font-size: 14px; color: #7f8c8d;")
            )
          )
        ),
        
        # Value boxes
        fluidRow(
          valueBoxOutput("total_regions", width = 3),
          valueBoxOutput("avg_sovi", width = 3),
          valueBoxOutput("high_vulnerability", width = 3),
          valueBoxOutput("avg_poverty", width = 3)
        ),
        
        # Charts row 1
        fluidRow(
          box(
            title = "📊 Distribusi SOVI Index", status = "info", solidHeader = TRUE, width = 6,
            withSpinner(plotlyOutput("sovi_distribution", height = "350px"), color = "#3498db")
          ),
          box(
            title = "🎯 Kategori Kerentanan", status = "success", solidHeader = TRUE, width = 6,
            withSpinner(plotlyOutput("vulnerability_categories", height = "350px"), color = "#2ecc71")
          )
        ),
        
        # Charts row 2
        fluidRow(
          box(
            title = "🌍 Sebaran Regional", status = "warning", solidHeader = TRUE, width = 6,
            withSpinner(plotlyOutput("regional_distribution", height = "350px"), color = "#f39c12")
          ),
          box(
            title = "📈 Indikator Utama", status = "danger", solidHeader = TRUE, width = 6,
            withSpinner(plotlyOutput("key_indicators", height = "350px"), color = "#e74c3c")
          )
        ),
        
        # Summary statistics
        fluidRow(
          box(
            title = "📋 Ringkasan Statistik Utama", status = "primary", solidHeader = TRUE, width = 12,
            DT::dataTableOutput("overview_stats")
          )
        )
      ),
      
      # ===== TAB 2: INTERACTIVE MAP =====
      tabItem(tabName = "map",
        fluidRow(
          # Map controls
          box(
            title = "🎛️ Kontrol Peta", status = "warning", solidHeader = TRUE, width = 3,
            
            selectInput("map_variable", "Pilih Variabel untuk Visualisasi:",
                       choices = list(
                         "Indeks Utama" = c("SOVI_INDEX", "VULNERABILITY_LEVEL", "SOVI_PERCENTILE"),
                         "Indikator Sosial" = c("POVERTY", "ILLITERATE", "LOWEDU", "FHEAD"),
                         "Demografi" = c("CHILDREN", "ELDERLY", "FEMALE", "FAMILYSIZE"),
                         "Infrastruktur" = c("NOELECTRIC", "NOTRAINING"),
                         "Geografis" = c("POPULATION", "AREA", "POPULATION_DENSITY")
                       ),
                       selected = "SOVI_INDEX"),
            
            selectInput("map_palette", "Pilih Skema Warna:",
                       choices = c("Viridis" = "viridis", 
                                 "Red-Yellow-Blue" = "rdylbu", 
                                 "Spectral" = "spectral"),
                       selected = "viridis"),
            
            checkboxInput("show_clusters", "Gunakan Clustering", value = FALSE),
            checkboxInput("show_labels", "Tampilkan Label", value = TRUE),
            
            hr(),
            h5("📊 Statistik Variabel Terpilih"),
            verbatimTextOutput("map_variable_stats"),
            
            hr(),
            downloadButton("download_map", "💾 Download Peta", 
                          class = "btn-primary btn-block"),
            br(), br(),
            actionButton("reset_map", "🔄 Reset View", 
                        class = "btn-info btn-block")
          ),
          
          # Main map
          box(
            title = "🗺️ Peta Kerentanan Sosial Indonesia", 
            status = "primary", solidHeader = TRUE, width = 9,
            withSpinner(
              leafletOutput("interactive_map", height = "600px"),
              color = "#3498db"
            )
          )
        ),
        
        # Map information and analysis
        fluidRow(
          box(
            title = "ℹ️ Panduan Penggunaan Peta", status = "info", solidHeader = TRUE, width = 6,
            div(style = "padding: 15px;",
                h5("🎯 Cara Menggunakan Peta:"),
                tags$ul(
                  tags$li("🖱️ Klik pada marker untuk melihat detail informasi region"),
                  tags$li("🔄 Gunakan kontrol layer di kanan atas untuk mengganti base map"),
                  tags$li("📏 Ukuran marker menunjukkan jumlah populasi relatif"),
                  tags$li("🎨 Warna marker menunjukkan nilai variabel yang dipilih"),
                  tags$li("🏷️ Hover pada marker untuk melihat nama region dan nilai"),
                  tags$li("🔍 Gunakan zoom dan pan untuk navigasi detail")
                ),
                h5("🛠️ Fitur Tambahan:"),
                tags$ul(
                  tags$li("📍 Mini map untuk orientasi"),
                  tags$li("📐 Scale bar untuk referensi jarak"),
                  tags$li("🎯 Clustering untuk area dengan banyak marker"),
                  tags$li("💾 Download peta sebagai file HTML")
                )
            )
          ),
          
          box(
            title = "📊 Analisis Spasial", status = "success", solidHeader = TRUE, width = 6,
            withSpinner(plotlyOutput("spatial_analysis", height = "300px"), color = "#2ecc71")
          )
        )
      ),
      
      # ===== TAB 3: STATISTICAL ANALYSIS =====
      tabItem(tabName = "stats",
        fluidRow(
          # Analysis controls
          box(
            title = "🎛️ Kontrol Analisis", status = "warning", solidHeader = TRUE, width = 3,
            
            selectInput("x_variable", "Variabel X (Horizontal):",
                       choices = names(select_if(sovi_data, is.numeric)),
                       selected = "POVERTY"),
            
            selectInput("y_variable", "Variabel Y (Vertical):",
                       choices = names(select_if(sovi_data, is.numeric)),
                       selected = "SOVI_INDEX"),
            
            selectInput("color_variable", "Variabel Warna:",
                       choices = c("None" = "none", names(select_if(sovi_data, function(x) is.numeric(x) || is.factor(x)))),
                       selected = "SOVI_CATEGORY"),
            
            selectInput("size_variable", "Variabel Ukuran:",
                       choices = c("None" = "none", names(select_if(sovi_data, is.numeric))),
                       selected = "POPULATION"),
            
            hr(),
            checkboxInput("show_regression", "Tampilkan Garis Regresi", value = TRUE),
            checkboxInput("show_confidence", "Tampilkan Confidence Interval", value = TRUE),
            
            hr(),
            actionButton("run_correlation", "🔍 Analisis Korelasi", class = "btn-success btn-block"),
            br(),
            actionButton("run_regression", "📈 Analisis Regresi", class = "btn-info btn-block")
          ),
          
          # Main scatter plot
          box(
            title = "📊 Scatter Plot Analysis", status = "primary", solidHeader = TRUE, width = 9,
            withSpinner(plotlyOutput("scatter_plot", height = "500px"), color = "#3498db")
          )
        ),
        
        # Additional analysis
        fluidRow(
          box(
            title = "🔗 Matriks Korelasi", status = "info", solidHeader = TRUE, width = 6,
            withSpinner(plotOutput("correlation_matrix", height = "400px"), color = "#3498db")
          ),
          
          box(
            title = "📋 Statistik Deskriptif", status = "success", solidHeader = TRUE, width = 6,
            withSpinner(DT::dataTableOutput("descriptive_stats"), color = "#2ecc71")
          )
        ),
        
        # Regression results
        fluidRow(
          box(
            title = "📈 Hasil Analisis Regresi", status = "warning", solidHeader = TRUE, width = 12,
            verbatimTextOutput("regression_results")
          )
        )
      ),
      
      # ===== TAB 4: DATA EXPLORER =====
      tabItem(tabName = "data",
        fluidRow(
          # Data filters
          box(
            title = "🔍 Filter Data", status = "warning", solidHeader = TRUE, width = 3,
            
            selectInput("filter_category", "Filter Kategori SOVI:",
                       choices = c("Semua" = "all", levels(sovi_data$SOVI_CATEGORY)),
                       selected = "all"),
            
            sliderInput("population_range", "Range Populasi:",
                       min = min(sovi_data$POPULATION),
                       max = max(sovi_data$POPULATION),
                       value = c(min(sovi_data$POPULATION), max(sovi_data$POPULATION)),
                       step = 10000),
            
            sliderInput("poverty_range", "Range Kemiskinan (%):",
                       min = 0, max = 50,
                       value = c(0, 50),
                       step = 1),
            
            selectInput("region_filter", "Filter Region:",
                       choices = c("Semua" = "all", sort(unique(sovi_data$REGION))),
                       selected = "all",
                       multiple = TRUE),
            
            hr(),
            h5("📊 Data Terfilter:"),
            verbatimTextOutput("filtered_summary"),
            
            hr(),
            downloadButton("download_filtered_data", "💾 Download Data Terfilter", 
                          class = "btn-primary btn-block")
          ),
          
          # Main data table
          box(
            title = "📋 Data SOVI Indonesia", status = "primary", solidHeader = TRUE, width = 9,
            withSpinner(DT::dataTableOutput("sovi_table"), color = "#3498db")
          )
        ),
        
        # Data summary and charts
        fluidRow(
          box(
            title = "📊 Distribusi Data Terfilter", status = "info", solidHeader = TRUE, width = 6,
            withSpinner(plotlyOutput("filtered_distribution", height = "350px"), color = "#3498db")
          ),
          
          box(
            title = "🎯 Top 10 Region Terburuk", status = "danger", solidHeader = TRUE, width = 6,
            withSpinner(DT::dataTableOutput("worst_regions"), color = "#e74c3c")
          )
        )
      ),
      
      # ===== TAB 5: ADVANCED VISUALIZATION =====
      tabItem(tabName = "advanced",
        fluidRow(
          box(
            title = "🎛️ Kontrol Visualisasi", status = "warning", solidHeader = TRUE, width = 3,
            
            selectInput("chart_type", "Jenis Visualisasi:",
                       choices = c("Heatmap Korelasi" = "heatmap",
                                 "Radar Chart" = "radar",
                                 "Box Plot" = "boxplot",
                                 "Violin Plot" = "violin",
                                 "Density Plot" = "density",
                                 "Parallel Coordinates" = "parallel")),
            
            conditionalPanel(
              condition = "input.chart_type == 'boxplot' || input.chart_type == 'violin'",
              selectInput("group_variable", "Variabel Pengelompokan:",
                         choices = c("SOVI_CATEGORY", "POVERTY_RISK", "EDUCATION_RISK"))
            ),
            
            conditionalPanel(
              condition = "input.chart_type == 'radar'",
              selectInput("radar_regions", "Pilih Region untuk Radar:",
                         choices = sovi_data$REGION,
                         selected = sample(sovi_data$REGION, 3),
                         multiple = TRUE)
            ),
            
            hr(),
            checkboxInput("use_log_scale", "Gunakan Skala Log", value = FALSE),
            checkboxInput("show_annotations", "Tampilkan Anotasi", value = TRUE)
          ),
          
          box(
            title = "📊 Visualisasi Lanjutan", status = "primary", solidHeader = TRUE, width = 9,
            withSpinner(plotlyOutput("advanced_plot", height = "600px"), color = "#3498db")
          )
        ),
        
        fluidRow(
          box(
            title = "🔍 Analisis Cluster", status = "info", solidHeader = TRUE, width = 6,
            withSpinner(plotlyOutput("cluster_analysis", height = "400px"), color = "#3498db")
          ),
          
          box(
            title = "📈 Trend Analysis", status = "success", solidHeader = TRUE, width = 6,
            withSpinner(plotlyOutput("trend_analysis", height = "400px"), color = "#2ecc71")
          )
        )
      ),
      
      # ===== TAB 6: METADATA =====
      tabItem(tabName = "metadata",
        fluidRow(
          box(
            title = "📋 Metadata Variabel", status = "primary", solidHeader = TRUE, width = 12,
            withSpinner(DT::dataTableOutput("metadata_table"), color = "#3498db")
          )
        ),
        
        fluidRow(
          box(
            title = "ℹ️ Informasi Dataset", status = "info", solidHeader = TRUE, width = 6,
            div(style = "padding: 15px;",
                h4("🎯 Tentang Dataset"),
                p("Dataset Social Vulnerability Index (SOVI) Indonesia ini dikembangkan untuk menganalisis tingkat kerentanan sosial di berbagai region Indonesia."),
                
                h5("📊 Sumber Data:"),
                tags$ul(
                  tags$li("Dataset simulasi berdasarkan struktur SOVI internasional"),
                  tags$li("Menggunakan 17 variabel indikator kerentanan sosial"),
                  tags$li("Mencakup aspek demografi, sosial-ekonomi, dan infrastruktur")
                ),
                
                h5("🔬 Metodologi SOVI:"),
                tags$ul(
                  tags$li("Standardisasi menggunakan Z-score normalization"),
                  tags$li("Pembobotan berdasarkan literatur ilmiah"),
                  tags$li("Kategorisasi berdasarkan quartile distribution")
                ),
                
                h5("📅 Informasi Temporal:"),
                p("Data simulasi untuk tahun 2024 dengan proyeksi berdasarkan tren historis.")
            )
          ),
          
          box(
            title = "📊 Statistik Dataset", status = "success", solidHeader = TRUE, width = 6,
            verbatimTextOutput("dataset_summary")
          )
        ),
        
        fluidRow(
          box(
            title = "🔗 Referensi dan Dokumentasi", status = "warning", solidHeader = TRUE, width = 12,
            div(style = "padding: 15px;",
                h5("📚 Referensi Utama:"),
                tags$ul(
                  tags$li("Cutter, S. L., Boruff, B. J., & Shirley, W. L. (2003). Social vulnerability to environmental hazards. Social Science Quarterly, 84(2), 242-261."),
                  tags$li("Flanagan, B. E., et al. (2011). A social vulnerability index for disaster management. Journal of Homeland Security and Emergency Management, 8(1)."),
                  tags$li("Tate, E. (2012). Social vulnerability indices: a comparative assessment using uncertainty and sensitivity analysis. Natural Hazards, 63(2), 325-347.")
                ),
                
                h5("🛠️ Teknologi yang Digunakan:"),
                p("Dashboard ini dikembangkan menggunakan R Shiny dengan library: leaflet, plotly, DT, ggplot2, dan berbagai package analisis statistik lainnya.")
            )
          )
        )
      ),
      
      # ===== TAB 7: EXPORT =====
      tabItem(tabName = "export",
        fluidRow(
          box(
            title = "💾 Download Data", status = "primary", solidHeader = TRUE, width = 6,
            div(style = "padding: 15px;",
                h4("📊 Export Data"),
                p("Download dataset dalam berbagai format untuk analisis lanjutan."),
                
                br(),
                downloadButton("download_csv", "📄 Download CSV", class = "btn-success btn-lg btn-block"),
                br(),
                downloadButton("download_excel", "📊 Download Excel", class = "btn-info btn-lg btn-block"),
                br(),
                downloadButton("download_json", "🔗 Download JSON", class = "btn-warning btn-lg btn-block")
            )
          ),
          
          box(
            title = "🗺️ Export Visualisasi", status = "success", solidHeader = TRUE, width = 6,
            div(style = "padding: 15px;",
                h4("📈 Export Charts & Maps"),
                p("Download visualisasi dalam format gambar atau HTML interaktif."),
                
                br(),
                downloadButton("download_all_charts", "📊 Download All Charts (PDF)", class = "btn-danger btn-lg btn-block"),
                br(),
                downloadButton("download_interactive_map", "🗺️ Download Interactive Map", class = "btn-primary btn-lg btn-block"),
                br(),
                downloadButton("download_report", "📋 Generate Full Report", class = "btn-dark btn-lg btn-block")
            )
          )
        ),
        
        fluidRow(
          box(
            title = "📋 Laporan Otomatis", status = "info", solidHeader = TRUE, width = 12,
            div(style = "padding: 15px;",
                h4("🤖 Generate Laporan Komprehensif"),
                p("Sistem akan menghasilkan laporan lengkap berisi semua analisis, visualisasi, dan insights dari data SOVI."),
                
                fluidRow(
                  column(6,
                         h5("📊 Isi Laporan:"),
                         tags$ul(
                           tags$li("Executive Summary"),
                           tags$li("Analisis Deskriptif Lengkap"),
                           tags$li("Visualisasi Semua Variabel"),
                           tags$li("Analisis Korelasi dan Regresi"),
                           tags$li("Peta Interaktif"),
                           tags$li("Rekomendasi Kebijakan")
                         )
                  ),
                  column(6,
                         h5("⚙️ Format Laporan:"),
                         radioButtons("report_format", "Pilih Format:",
                                     choices = c("HTML Interaktif" = "html",
                                               "PDF Dokumen" = "pdf",
                                               "Word Document" = "docx"),
                                     selected = "html"),
                         
                         checkboxInput("include_raw_data", "Sertakan Raw Data", value = TRUE),
                         checkboxInput("include_methodology", "Sertakan Metodologi", value = TRUE)
                  )
                ),
                
                br(),
                actionButton("generate_report", "🚀 Generate Laporan Lengkap", 
                           class = "btn-success btn-lg btn-block")
            )
          )
        )
      )
    )
  )
)

# ===============================================================================
# 10. SERVER LOGIC
# ===============================================================================

server <- function(input, output, session) {
  
  # ===== REACTIVE DATA =====
  
  # Filtered data based on user inputs
  filtered_data <- reactive({
    data <- sovi_data
    
    # Filter by SOVI category
    if(input$filter_category != "all") {
      data <- data[data$SOVI_CATEGORY == input$filter_category, ]
    }
    
    # Filter by population range
    if(!is.null(input$population_range)) {
      data <- data[data$POPULATION >= input$population_range[1] & 
                   data$POPULATION <= input$population_range[2], ]
    }
    
    # Filter by poverty range
    if(!is.null(input$poverty_range)) {
      data <- data[data$POVERTY >= input$poverty_range[1] & 
                   data$POVERTY <= input$poverty_range[2], ]
    }
    
    # Filter by region
    if(!is.null(input$region_filter) && !("all" %in% input$region_filter)) {
      data <- data[data$REGION %in% input$region_filter, ]
    }
    
    return(data)
  })
  
  # ===== OVERVIEW TAB OUTPUTS =====
  
  # Value boxes
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
  
  output$avg_poverty <- renderValueBox({
    valueBox(
      value = paste0(round(mean(sovi_data$POVERTY), 1), "%"),
      subtitle = "Rata-rata Kemiskinan",
      icon = icon("users"),
      color = "yellow"
    )
  })
  
  # Overview charts
  output$sovi_distribution <- renderPlotly({
    p <- ggplot(sovi_data, aes(x = SOVI_INDEX)) +
      geom_histogram(bins = 25, fill = "#3498db", alpha = 0.7, color = "white") +
      geom_vline(aes(xintercept = mean(SOVI_INDEX)), color = "#e74c3c", linetype = "dashed", size = 1) +
      labs(title = "Distribusi SOVI Index",
           x = "SOVI Index", y = "Frekuensi",
           caption = "Garis merah: rata-rata") +
      theme_minimal() +
      theme(plot.title = element_text(hjust = 0.5, size = 14, face = "bold"))
    
    ggplotly(p, tooltip = c("x", "y"))
  })
  
  output$vulnerability_categories <- renderPlotly({
    category_data <- sovi_data %>%
      count(SOVI_CATEGORY) %>%
      mutate(percentage = round(n/sum(n)*100, 1))
    
    p <- ggplot(category_data, aes(x = SOVI_CATEGORY, y = n, fill = SOVI_CATEGORY,
                                   text = paste("Kategori:", SOVI_CATEGORY, 
                                              "<br>Jumlah:", n,
                                              "<br>Persentase:", percentage, "%"))) +
      geom_col(alpha = 0.8) +
      geom_text(aes(label = paste0(n, "\n(", percentage, "%)")), 
                vjust = -0.5, size = 3, fontface = "bold") +
      scale_fill_brewer(type = "qual", palette = "Set1") +
      labs(title = "Distribusi Kategori Kerentanan",
           x = "Kategori Kerentanan", y = "Jumlah Region") +
      theme_minimal() +
      theme(plot.title = element_text(hjust = 0.5, size = 14, face = "bold"),
            legend.position = "none")
    
    ggplotly(p, tooltip = "text")
  })
  
  output$regional_distribution <- renderPlotly({
    top_regions <- sovi_data %>%
      arrange(desc(SOVI_INDEX)) %>%
      head(15)
    
    p <- ggplot(top_regions, aes(x = reorder(REGION, SOVI_INDEX), y = SOVI_INDEX, 
                                fill = SOVI_CATEGORY)) +
      geom_col(alpha = 0.8) +
      coord_flip() +
      scale_fill_brewer(type = "qual", palette = "Set1") +
      labs(title = "Top 15 Region dengan SOVI Tertinggi",
           x = "Region", y = "SOVI Index") +
      theme_minimal() +
      theme(plot.title = element_text(hjust = 0.5, size = 14, face = "bold"))
    
    ggplotly(p)
  })
  
  output$key_indicators <- renderPlotly({
    indicators <- sovi_data %>%
      select(POVERTY, ILLITERATE, NOELECTRIC, LOWEDU) %>%
      gather(key = "Indicator", value = "Value")
    
    p <- ggplot(indicators, aes(x = Indicator, y = Value, fill = Indicator)) +
      geom_boxplot(alpha = 0.7) +
      scale_fill_viridis_d() +
      labs(title = "Distribusi Indikator Kunci",
           x = "Indikator", y = "Nilai (%)") +
      theme_minimal() +
      theme(plot.title = element_text(hjust = 0.5, size = 14, face = "bold"),
            legend.position = "none")
    
    ggplotly(p)
  })
  
  output$overview_stats <- DT::renderDataTable({
    stats <- generate_descriptive_stats(sovi_data)
    DT::datatable(stats, 
                  options = list(pageLength = 10, scrollX = TRUE, scrollY = "300px"),
                  rownames = FALSE) %>%
      formatRound(columns = c("Mean", "Median", "SD", "Min", "Max", "Q25", "Q75"), digits = 3)
  })
  
  # ===== MAP TAB OUTPUTS =====
  
  # Main interactive map
  output$interactive_map <- renderLeaflet({
    create_interactive_map(sovi_data, input$map_variable, input$map_palette, input$show_clusters)
  })
  
  # Update map when controls change
  observe({
    leafletProxy("interactive_map", data = sovi_data) %>%
      clearMarkers() %>%
      clearControls()
    
    # Create new palette and popup
    pal <- create_color_palette(sovi_data, input$map_variable, input$map_palette)
    popup_content <- create_popup_content(sovi_data)
    
    # Add new markers
    if(input$show_clusters) {
      leafletProxy("interactive_map", data = sovi_data) %>%
        addCircleMarkers(
          lng = ~LONGITUDE, lat = ~LATITUDE,
          radius = ~sqrt(POPULATION/15000) + 3,
          color = "white", weight = 2,
          fillColor = ~pal(get(input$map_variable)),
          fillOpacity = 0.8,
          popup = popup_content,
          label = if(input$show_labels) ~paste(REGION, "-", get(input$map_variable)) else NULL,
          clusterOptions = markerClusterOptions()
        )
    } else {
      leafletProxy("interactive_map", data = sovi_data) %>%
        addCircleMarkers(
          lng = ~LONGITUDE, lat = ~LATITUDE,
          radius = ~sqrt(POPULATION/15000) + 3,
          color = "white", weight = 2,
          fillColor = ~pal(get(input$map_variable)),
          fillOpacity = 0.8,
          popup = popup_content,
          label = if(input$show_labels) ~paste(REGION, "-", get(input$map_variable)) else NULL
        )
    }
    
    # Add legend
    if(is.numeric(sovi_data[[input$map_variable]])) {
      leafletProxy("interactive_map") %>%
        addLegend(
          pal = pal, values = sovi_data[[input$map_variable]],
          opacity = 0.8, title = input$map_variable,
          position = "bottomright", layerId = "legend"
        )
    }
  })
  
  # Map variable statistics
  output$map_variable_stats <- renderText({
    if(is.numeric(sovi_data[[input$map_variable]])) {
      values <- sovi_data[[input$map_variable]]
      paste0(
        "Min: ", round(min(values, na.rm = TRUE), 2), "\n",
        "Max: ", round(max(values, na.rm = TRUE), 2), "\n",
        "Mean: ", round(mean(values, na.rm = TRUE), 2), "\n",
        "Median: ", round(median(values, na.rm = TRUE), 2), "\n",
        "SD: ", round(sd(values, na.rm = TRUE), 2)
      )
    } else {
      "Variabel kategorik"
    }
  })
  
  # Spatial analysis plot
  output$spatial_analysis <- renderPlotly({
    p <- ggplot(sovi_data, aes(x = LONGITUDE, y = LATITUDE, 
                               color = get(input$map_variable),
                               size = POPULATION)) +
      geom_point(alpha = 0.7) +
      scale_color_viridis_c() +
      scale_size_continuous(range = c(1, 8)) +
      labs(title = "Distribusi Spasial",
           x = "Longitude", y = "Latitude",
           color = input$map_variable, size = "Population") +
      theme_minimal()
    
    ggplotly(p)
  })
  
  # Reset map view
  observeEvent(input$reset_map, {
    leafletProxy("interactive_map") %>%
      setView(lng = 118, lat = -2, zoom = 5)
  })
  
  # ===== STATS TAB OUTPUTS =====
  
  # Main scatter plot
  output$scatter_plot <- renderPlotly({
    p <- ggplot(sovi_data, aes_string(x = input$x_variable, y = input$y_variable))
    
    # Add color if selected
    if(input$color_variable != "none") {
      p <- p + aes_string(color = input$color_variable)
    }
    
    # Add size if selected
    if(input$size_variable != "none") {
      p <- p + aes_string(size = input$size_variable)
    }
    
    p <- p + geom_point(alpha = 0.7)
    
    # Add regression line if requested
    if(input$show_regression) {
      if(input$show_confidence) {
        p <- p + geom_smooth(method = "lm", se = TRUE, color = "red")
      } else {
        p <- p + geom_smooth(method = "lm", se = FALSE, color = "red")
      }
    }
    
    p <- p + 
      labs(title = paste("Analisis Hubungan", input$x_variable, "vs", input$y_variable)) +
      theme_minimal() +
      theme(plot.title = element_text(hjust = 0.5, size = 14, face = "bold"))
    
    ggplotly(p)
  })
  
  # Correlation matrix
  output$correlation_matrix <- renderPlot({
    numeric_vars <- select_if(sovi_data, is.numeric)
    cor_matrix <- cor(numeric_vars, use = "complete.obs")
    
    corrplot(cor_matrix, method = "color", type = "upper", 
             order = "hclust", tl.cex = 0.8, tl.col = "black",
             title = "Matriks Korelasi Variabel Numerik",
             mar = c(0,0,2,0))
  })
  
  # Descriptive statistics
  output$descriptive_stats <- DT::renderDataTable({
    stats <- generate_descriptive_stats(sovi_data)
    DT::datatable(stats,
                  options = list(pageLength = 15, scrollX = TRUE, scrollY = "350px"),
                  rownames = FALSE) %>%
      formatRound(columns = c("Mean", "Median", "SD", "Min", "Max", "Q25", "Q75"), digits = 3)
  })
  
  # Regression results
  output$regression_results <- renderPrint({
    if(input$run_regression > 0) {
      isolate({
        formula_str <- paste(input$y_variable, "~", input$x_variable)
        model <- lm(as.formula(formula_str), data = sovi_data)
        summary(model)
      })
    } else {
      "Klik tombol 'Analisis Regresi' untuk melihat hasil."
    }
  })
  
  # ===== DATA TAB OUTPUTS =====
  
  # Main data table
  output$sovi_table <- DT::renderDataTable({
    DT::datatable(filtered_data(),
                  options = list(pageLength = 20, scrollX = TRUE, scrollY = "400px"),
                  filter = "top",
                  rownames = FALSE) %>%
      formatRound(columns = names(select_if(filtered_data(), is.numeric)), digits = 2)
  })
  
  # Filtered data summary
  output$filtered_summary <- renderText({
    data <- filtered_data()
    paste0(
      "Jumlah region: ", nrow(data), "\n",
      "Rata-rata SOVI: ", round(mean(data$SOVI_INDEX), 3), "\n",
      "Range populasi: ", format(range(data$POPULATION), big.mark = ",", collapse = " - ")
    )
  })
  
  # Filtered distribution
  output$filtered_distribution <- renderPlotly({
    p <- ggplot(filtered_data(), aes(x = SOVI_INDEX, fill = SOVI_CATEGORY)) +
      geom_histogram(bins = 20, alpha = 0.7, position = "identity") +
      scale_fill_brewer(type = "qual", palette = "Set1") +
      labs(title = "Distribusi SOVI Data Terfilter",
           x = "SOVI Index", y = "Frekuensi") +
      theme_minimal()
    
    ggplotly(p)
  })
  
  # Worst regions
  output$worst_regions <- DT::renderDataTable({
    worst <- filtered_data() %>%
      arrange(desc(SOVI_INDEX)) %>%
      select(REGION, SOVI_INDEX, SOVI_CATEGORY, POVERTY, ILLITERATE) %>%
      head(10)
    
    DT::datatable(worst,
                  options = list(pageLength = 10, dom = 't'),
                  rownames = FALSE) %>%
      formatRound(columns = c("SOVI_INDEX", "POVERTY", "ILLITERATE"), digits = 2)
  })
  
  # ===== ADVANCED TAB OUTPUTS =====
  
  output$advanced_plot <- renderPlotly({
    switch(input$chart_type,
           "heatmap" = {
             # Correlation heatmap
             numeric_vars <- select_if(sovi_data, is.numeric)
             cor_matrix <- cor(numeric_vars, use = "complete.obs")
             
             plot_ly(z = cor_matrix, type = "heatmap", 
                     colors = colorRamp(c("blue", "white", "red")),
                     hovertemplate = "X: %{x}<br>Y: %{y}<br>Correlation: %{z}<extra></extra>") %>%
               layout(title = "Heatmap Korelasi")
           },
           
           "boxplot" = {
             p <- ggplot(sovi_data, aes_string(x = input$group_variable, y = "SOVI_INDEX", 
                                               fill = input$group_variable)) +
               geom_boxplot(alpha = 0.7) +
               scale_fill_brewer(type = "qual", palette = "Set1") +
               labs(title = "Box Plot SOVI Index by Group") +
               theme_minimal()
             
             ggplotly(p)
           },
           
           "violin" = {
             p <- ggplot(sovi_data, aes_string(x = input$group_variable, y = "SOVI_INDEX", 
                                               fill = input$group_variable)) +
               geom_violin(alpha = 0.7) +
               geom_boxplot(width = 0.1, alpha = 0.8) +
               scale_fill_brewer(type = "qual", palette = "Set1") +
               labs(title = "Violin Plot SOVI Index by Group") +
               theme_minimal()
             
             ggplotly(p)
           },
           
           "density" = {
             p <- ggplot(sovi_data, aes(x = SOVI_INDEX, fill = SOVI_CATEGORY)) +
               geom_density(alpha = 0.6) +
               scale_fill_brewer(type = "qual", palette = "Set1") +
               labs(title = "Density Plot SOVI Index") +
               theme_minimal()
             
             ggplotly(p)
           }
    )
  })
  
  # Cluster analysis
  output$cluster_analysis <- renderPlotly({
    # K-means clustering
    set.seed(123)
    cluster_vars <- c("POVERTY", "ILLITERATE", "NOELECTRIC", "LOWEDU")
    cluster_data <- scale(sovi_data[cluster_vars])
    
    kmeans_result <- kmeans(cluster_data, centers = 4, nstart = 25)
    sovi_data$Cluster <- as.factor(kmeans_result$cluster)
    
    p <- ggplot(sovi_data, aes(x = POVERTY, y = ILLITERATE, color = Cluster, size = POPULATION)) +
      geom_point(alpha = 0.7) +
      scale_color_brewer(type = "qual", palette = "Set1") +
      labs(title = "Analisis Cluster (K-means)",
           x = "Kemiskinan (%)", y = "Buta Huruf (%)") +
      theme_minimal()
    
    ggplotly(p)
  })
  
  # Trend analysis
  output$trend_analysis <- renderPlotly({
    # Simulate trend data
    trend_data <- sovi_data %>%
      arrange(SOVI_INDEX) %>%
      mutate(Rank = row_number(),
             Cumulative_Population = cumsum(POPULATION))
    
    p <- ggplot(trend_data, aes(x = Rank, y = Cumulative_Population)) +
      geom_line(color = "#3498db", size = 1.2) +
      geom_area(alpha = 0.3, fill = "#3498db") +
      labs(title = "Kumulatif Populasi vs Ranking SOVI",
           x = "Ranking SOVI", y = "Kumulatif Populasi") +
      theme_minimal()
    
    ggplotly(p)
  })
  
  # ===== METADATA TAB OUTPUTS =====
  
  output$metadata_table <- DT::renderDataTable({
    DT::datatable(metadata_sovi,
                  options = list(pageLength = 20, scrollX = TRUE),
                  rownames = FALSE)
  })
  
  output$dataset_summary <- renderPrint({
    cat("=== RINGKASAN DATASET SOVI INDONESIA ===\n\n")
    cat("Jumlah Region:", nrow(sovi_data), "\n")
    cat("Jumlah Variabel:", ncol(sovi_data), "\n")
    cat("Periode Data: 2024 (Simulasi)\n\n")
    
    cat("=== DISTRIBUSI KATEGORI KERENTANAN ===\n")
    print(table(sovi_data$SOVI_CATEGORY))
    
    cat("\n=== STATISTIK SOVI INDEX ===\n")
    print(summary(sovi_data$SOVI_INDEX))
    
    cat("\n=== COVERAGE GEOGRAFIS ===\n")
    cat("Latitude Range:", range(sovi_data$LATITUDE), "\n")
    cat("Longitude Range:", range(sovi_data$LONGITUDE), "\n")
    cat("Total Populasi:", format(sum(sovi_data$POPULATION), big.mark = ","), "jiwa\n")
    cat("Total Luas:", format(sum(sovi_data$AREA), big.mark = ","), "km²\n")
  })
  
  # ===== DOWNLOAD HANDLERS =====
  
  # Download CSV
  output$download_csv <- downloadHandler(
    filename = function() {
      paste("sovi_indonesia_", Sys.Date(), ".csv", sep = "")
    },
    content = function(file) {
      write.csv(sovi_data, file, row.names = FALSE)
    }
  )
  
  # Download Excel
  output$download_excel <- downloadHandler(
    filename = function() {
      paste("sovi_indonesia_", Sys.Date(), ".xlsx", sep = "")
    },
    content = function(file) {
      wb <- createWorkbook()
      addWorksheet(wb, "SOVI Data")
      addWorksheet(wb, "Metadata")
      addWorksheet(wb, "Summary Stats")
      
      writeData(wb, "SOVI Data", sovi_data)
      writeData(wb, "Metadata", metadata_sovi)
      writeData(wb, "Summary Stats", generate_descriptive_stats(sovi_data))
      
      saveWorkbook(wb, file)
    }
  )
  
  # Download filtered data
  output$download_filtered_data <- downloadHandler(
    filename = function() {
      paste("sovi_filtered_", Sys.Date(), ".csv", sep = "")
    },
    content = function(file) {
      write.csv(filtered_data(), file, row.names = FALSE)
    }
  )
  
  # Download map
  output$download_map <- downloadHandler(
    filename = function() {
      paste("sovi_map_", Sys.Date(), ".html", sep = "")
    },
    content = function(file) {
      map <- create_interactive_map(sovi_data, input$map_variable, input$map_palette)
      htmlwidgets::saveWidget(map, file, selfcontained = TRUE)
    }
  )
  
  # ===== SESSION INFO =====
  
  # Display session info for debugging
  observe({
    cat("Dashboard loaded successfully at", as.character(Sys.time()), "\n")
    cat("Active tab:", input$sidebar_menu, "\n")
    cat("Data dimensions:", dim(sovi_data), "\n")
  })
}

# ===============================================================================
# 11. RUN APPLICATION
# ===============================================================================

# Run the Shiny application
shinyApp(ui = ui, server = server)

# ===============================================================================
# END OF FILE
# ===============================================================================