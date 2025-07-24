# ===============================================================================
# ENHANCED SPATIAL DASHBOARD - INTEGRASI RDS + JSON
# Menggabungkan data point (RDS) dengan polygon provinsi (JSON)
# ===============================================================================

# ===============================================================================
# 1. LOAD LIBRARIES WITH SPATIAL CAPABILITIES
# ===============================================================================
suppressMessages({
  library(shiny)
  library(shinydashboard)
  library(leaflet)
  library(sf)           # Untuk handling spatial data
  library(geojsonio)    # Untuk membaca GeoJSON
  library(spdep)        # Untuk spatial weights
  library(DT)
  library(ggplot2)
  library(plotly)
  library(dplyr)
  library(RColorBrewer)
  library(htmltools)
  library(viridis)
})

# ===============================================================================
# 2. SPATIAL DATA LOADING FUNCTIONS
# ===============================================================================

# Fungsi untuk load data RDS (point data)
load_point_data <- function(rds_path) {
  tryCatch({
    data <- readRDS(rds_path)
    
    # Validasi apakah data memiliki koordinat
    if(!all(c("LATITUDE", "LONGITUDE") %in% names(data))) {
      stop("Data RDS harus memiliki kolom LATITUDE dan LONGITUDE")
    }
    
    return(data)
  }, error = function(e) {
    message("Error loading RDS file: ", e$message)
    return(NULL)
  })
}

# Fungsi untuk load data JSON (polygon data)
load_polygon_data <- function(json_path) {
  tryCatch({
    # Membaca GeoJSON menggunakan sf
    provinces_sf <- st_read(json_path, quiet = TRUE)
    
    # Pastikan CRS konsisten (WGS84)
    if(st_crs(provinces_sf)$input != "EPSG:4326") {
      provinces_sf <- st_transform(provinces_sf, crs = 4326)
    }
    
    return(provinces_sf)
  }, error = function(e) {
    message("Error loading JSON file: ", e$message)
    return(NULL)
  })
}

# ===============================================================================
# 3. SPATIAL ANALYSIS FUNCTIONS
# ===============================================================================

# Fungsi untuk membuat spatial weights
create_spatial_weights <- function(sf_data, method = "queen") {
  tryCatch({
    # Buat neighbors berdasarkan contiguity
    if(method == "queen") {
      neighbors <- poly2nb(sf_data, queen = TRUE)
    } else {
      neighbors <- poly2nb(sf_data, queen = FALSE)  # rook
    }
    
    # Convert ke spatial weights
    weights <- nb2listw(neighbors, style = "W", zero.policy = TRUE)
    
    return(list(neighbors = neighbors, weights = weights))
  }, error = function(e) {
    message("Error creating spatial weights: ", e$message)
    return(NULL)
  })
}

# Fungsi untuk spatial join point ke polygon
spatial_join_point_to_polygon <- function(point_data, polygon_data) {
  tryCatch({
    # Convert point data ke sf object
    points_sf <- st_as_sf(point_data, 
                         coords = c("LONGITUDE", "LATITUDE"), 
                         crs = 4326)
    
    # Spatial join
    joined_data <- st_join(points_sf, polygon_data)
    
    # Convert back ke data frame dengan koordinat
    result <- joined_data %>%
      mutate(
        LONGITUDE = st_coordinates(.)[,1],
        LATITUDE = st_coordinates(.)[,2]
      ) %>%
      st_drop_geometry()
    
    return(result)
  }, error = function(e) {
    message("Error in spatial join: ", e$message)
    return(point_data)
  })
}

# Fungsi untuk agregasi data per provinsi
aggregate_by_province <- function(point_data, value_column = "SOVI_INDEX") {
  if(!"province_name" %in% names(point_data)) {
    return(NULL)
  }
  
  province_summary <- point_data %>%
    group_by(province_name) %>%
    summarise(
      count_regions = n(),
      avg_sovi = mean(get(value_column), na.rm = TRUE),
      median_sovi = median(get(value_column), na.rm = TRUE),
      min_sovi = min(get(value_column), na.rm = TRUE),
      max_sovi = max(get(value_column), na.rm = TRUE),
      total_population = sum(POPULATION, na.rm = TRUE),
      avg_poverty = mean(POVERTY, na.rm = TRUE),
      .groups = 'drop'
    )
  
  return(province_summary)
}

# ===============================================================================
# 4. ENHANCED MAP FUNCTIONS
# ===============================================================================

# Fungsi untuk membuat peta dengan polygon + points
create_enhanced_map <- function(point_data, polygon_data = NULL, 
                               color_variable = "SOVI_INDEX",
                               show_provinces = TRUE) {
  
  # Base map
  map <- leaflet() %>%
    addTiles(group = "OpenStreetMap") %>%
    addProviderTiles(providers$CartoDB.Positron, group = "CartoDB") %>%
    setView(lng = 118, lat = -2, zoom = 5)
  
  # Tambahkan polygon provinsi jika ada
  if(show_provinces && !is.null(polygon_data)) {
    
    # Jika ada data agregat per provinsi, gunakan untuk coloring
    if("province_summary" %in% names(point_data)) {
      # Join polygon dengan summary data
      polygon_with_data <- polygon_data %>%
        left_join(point_data$province_summary, 
                 by = c("NAME_1" = "province_name"))  # Sesuaikan nama kolom
      
      # Color palette untuk provinsi
      pal_province <- colorNumeric(
        palette = "YlOrRd",
        domain = polygon_with_data$avg_sovi,
        na.color = "transparent"
      )
      
      # Tambahkan polygon dengan warna
      map <- map %>%
        addPolygons(
          data = polygon_with_data,
          fillColor = ~pal_province(avg_sovi),
          fillOpacity = 0.6,
          color = "white",
          weight = 2,
          popup = ~paste0(
            "<strong>Provinsi: </strong>", NAME_1, "<br>",
            "<strong>Rata-rata SOVI: </strong>", round(avg_sovi, 3), "<br>",
            "<strong>Jumlah Region: </strong>", count_regions, "<br>",
            "<strong>Total Populasi: </strong>", format(total_population, big.mark = ",")
          ),
          group = "Provinsi"
        ) %>%
        addLegend(
          pal = pal_province,
          values = polygon_with_data$avg_sovi,
          title = "Rata-rata SOVI<br>per Provinsi",
          position = "topright"
        )
    } else {
      # Tampilkan polygon tanpa data
      map <- map %>%
        addPolygons(
          data = polygon_data,
          fillColor = "lightblue",
          fillOpacity = 0.3,
          color = "white",
          weight = 2,
          popup = ~paste0("<strong>Provinsi: </strong>", NAME_1),
          group = "Provinsi"
        )
    }
  }
  
  # Tambahkan point markers
  if(!is.null(point_data)) {
    pal_points <- colorNumeric(
      palette = viridis::viridis(10),
      domain = point_data[[color_variable]]
    )
    
    map <- map %>%
      addCircleMarkers(
        data = point_data,
        lng = ~LONGITUDE,
        lat = ~LATITUDE,
        radius = ~sqrt(POPULATION/20000) + 4,
        color = "white",
        weight = 2,
        fillColor = ~pal_points(get(color_variable)),
        fillOpacity = 0.8,
        popup = ~paste0(
          "<strong>", REGION, "</strong><br>",
          "SOVI Index: ", round(get(color_variable), 3), "<br>",
          "Populasi: ", format(POPULATION, big.mark = ","), "<br>",
          "Kemiskinan: ", POVERTY, "%"
        ),
        group = "Data Points"
      ) %>%
      addLegend(
        pal = pal_points,
        values = point_data[[color_variable]],
        title = color_variable,
        position = "bottomright"
      )
  }
  
  # Layer control
  map <- map %>%
    addLayersControl(
      baseGroups = c("OpenStreetMap", "CartoDB"),
      overlayGroups = c("Provinsi", "Data Points"),
      options = layersControlOptions(collapsed = FALSE)
    )
  
  return(map)
}

# ===============================================================================
# 5. SPATIAL STATISTICS FUNCTIONS
# ===============================================================================

# Moran's I untuk spatial autocorrelation
calculate_morans_i <- function(values, spatial_weights) {
  tryCatch({
    moran_test <- moran.test(values, spatial_weights, zero.policy = TRUE)
    
    return(list(
      statistic = moran_test$statistic,
      p_value = moran_test$p.value,
      expected = moran_test$estimate[2],
      interpretation = ifelse(moran_test$p.value < 0.05, 
                            "Signifikan spatial clustering", 
                            "Tidak ada spatial clustering")
    ))
  }, error = function(e) {
    return(list(error = e$message))
  })
}

# Local Moran's I (LISA)
calculate_local_morans <- function(values, spatial_weights) {
  tryCatch({
    lisa <- localmoran(values, spatial_weights, zero.policy = TRUE)
    
    # Klasifikasi LISA
    lisa_categories <- rep("Not Significant", length(values))
    lisa_categories[lisa[,5] < 0.05 & values > mean(values) & 
                   lag.listw(spatial_weights, values) > mean(values)] <- "High-High"
    lisa_categories[lisa[,5] < 0.05 & values < mean(values) & 
                   lag.listw(spatial_weights, values) < mean(values)] <- "Low-Low"
    lisa_categories[lisa[,5] < 0.05 & values > mean(values) & 
                   lag.listw(spatial_weights, values) < mean(values)] <- "High-Low"
    lisa_categories[lisa[,5] < 0.05 & values < mean(values) & 
                   lag.listw(spatial_weights, values) > mean(values)] <- "Low-High"
    
    return(list(
      statistics = lisa,
      categories = lisa_categories
    ))
  }, error = function(e) {
    return(list(error = e$message))
  })
}

# ===============================================================================
# 6. SAMPLE DATA GENERATION (JIKA DIPERLUKAN)
# ===============================================================================

# Fungsi untuk generate sample data yang realistis dengan provinsi
generate_realistic_indonesia_data <- function(n_regions = 100, use_real_provinces = TRUE) {
  set.seed(42)
  
  # Daftar provinsi Indonesia yang real
  real_provinces <- c(
    "DKI Jakarta", "Jawa Barat", "Jawa Tengah", "Jawa Timur", "DI Yogyakarta",
    "Banten", "Sumatera Utara", "Sumatera Barat", "Riau", "Jambi",
    "Sumatera Selatan", "Bengkulu", "Lampung", "Kepulauan Bangka Belitung",
    "Kepulauan Riau", "Kalimantan Barat", "Kalimantan Tengah", "Kalimantan Selatan",
    "Kalimantan Timur", "Kalimantan Utara", "Sulawesi Utara", "Sulawesi Tengah",
    "Sulawesi Selatan", "Sulawesi Tenggara", "Gorontalo", "Sulawesi Barat",
    "Bali", "Nusa Tenggara Barat", "Nusa Tenggara Timur", "Maluku",
    "Maluku Utara", "Papua", "Papua Barat", "Papua Selatan", "Papua Tengah", "Papua Pegunungan"
  )
  
  # Generate data dengan distribusi realistis per provinsi
  provinces <- sample(real_provinces, n_regions, replace = TRUE)
  
  # Coordinate ranges per major region
  coord_mapping <- list(
    "Sumatera" = list(lat = c(-6, 6), lon = c(95, 106)),
    "Jawa" = list(lat = c(-9, -6), lon = c(106, 115)),
    "Kalimantan" = list(lat = c(-4, 4), lon = c(109, 119)),
    "Sulawesi" = list(lat = c(-6, 2), lon = c(119, 125)),
    "Papua" = list(lat = c(-9, -1), lon = c(130, 141)),
    "Bali_Nusa" = list(lat = c(-11, -8), lon = c(115, 125)),
    "Maluku" = list(lat = c(-9, 3), lon = c(125, 135))
  )
  
  # Generate coordinates based on province
  coordinates <- t(sapply(provinces, function(prov) {
    if(grepl("Sumatera|Riau|Jambi|Bengkulu|Lampung|Bangka|Kepulauan Riau", prov)) {
      region <- "Sumatera"
    } else if(grepl("Jawa|Jakarta|Banten|Yogyakarta", prov)) {
      region <- "Jawa"
    } else if(grepl("Kalimantan", prov)) {
      region <- "Kalimantan"
    } else if(grepl("Sulawesi|Gorontalo", prov)) {
      region <- "Sulawesi"
    } else if(grepl("Papua", prov)) {
      region <- "Papua"
    } else if(grepl("Bali|Nusa", prov)) {
      region <- "Bali_Nusa"
    } else {
      region <- "Maluku"
    }
    
    coords <- coord_mapping[[region]]
    c(runif(1, coords$lat[1], coords$lat[2]), 
      runif(1, coords$lon[1], coords$lon[2]))
  }))
  
  # Generate correlated socioeconomic data
  base_poverty <- runif(n_regions, 2, 45)
  base_education <- runif(n_regions, 5, 50)
  
  data <- data.frame(
    DISTRICTCODE = sprintf("ID%03d", 1:n_regions),
    REGION = paste0("Kab/Kota ", sample(c("A", "B", "C", "D", "E"), n_regions, replace = TRUE), 
                   " - ", provinces),
    PROVINCE = provinces,
    
    # Demographics
    CHILDREN = pmax(3, pmin(30, rnorm(n_regions, 15, 5))),
    FEMALE = pmax(47, pmin(53, rnorm(n_regions, 50, 1.5))),
    ELDERLY = pmax(2, pmin(20, rnorm(n_regions, 8, 3))),
    FHEAD = pmax(8, pmin(40, rnorm(n_regions, 22, 6))),
    FAMILYSIZE = pmax(2.0, pmin(7.0, rnorm(n_regions, 4.2, 0.8))),
    
    # Socioeconomic
    NOELECTRIC = pmax(0, pmin(45, base_poverty * 0.6 + rnorm(n_regions, 5, 8))),
    LOWEDU = pmax(3, pmin(55, base_education + rnorm(n_regions, 0, 8))),
    GROWTH = pmax(-3, pmin(12, rnorm(n_regions, 2.5, 2.5))),
    POVERTY = round(base_poverty, 2),
    ILLITERATE = pmax(0.5, pmin(35, base_education * 0.4 + rnorm(n_regions, 3, 5))),
    NOTRAINING = pmax(15, pmin(85, base_poverty * 1.2 + rnorm(n_regions, 25, 15))),
    
    # Geographic
    LATITUDE = round(coordinates[,1], 6),
    LONGITUDE = round(coordinates[,2], 6),
    POPULATION = round(exp(rnorm(n_regions, log(200000), 0.8))),
    AREA = round(exp(rnorm(n_regions, log(1000), 0.6)), 2),
    
    stringsAsFactors = FALSE
  )
  
  # Calculate SOVI
  sovi_vars <- c("CHILDREN", "FEMALE", "ELDERLY", "FHEAD", "FAMILYSIZE",
                 "NOELECTRIC", "LOWEDU", "GROWTH", "POVERTY", "ILLITERATE", "NOTRAINING")
  
  # Standardize and calculate SOVI
  data_std <- data
  for(var in sovi_vars) {
    data_std[[var]] <- scale(data[[var]])[,1]
  }
  
  weights <- c(0.12, 0.08, 0.12, 0.09, 0.06, 0.10, 0.13, 0.05, 0.18, 0.12, 0.05)
  data$SOVI_INDEX <- rowSums(data_std[sovi_vars] * weights)
  
  data$SOVI_CATEGORY <- cut(data$SOVI_INDEX,
                           breaks = quantile(data$SOVI_INDEX, c(0, 0.25, 0.5, 0.75, 1)),
                           labels = c("Rendah", "Sedang", "Tinggi", "Sangat Tinggi"),
                           include.lowest = TRUE)
  
  return(data)
}

# ===============================================================================
# 7. MAIN INTEGRATION FUNCTION
# ===============================================================================

# Fungsi utama untuk mengintegrasikan semua data spasial
integrate_spatial_data <- function(rds_path = NULL, json_path = NULL, 
                                 generate_sample = TRUE) {
  
  result <- list()
  
  # Load point data
  if(!is.null(rds_path) && file.exists(rds_path)) {
    result$point_data <- load_point_data(rds_path)
    message("✓ RDS data loaded successfully")
  } else if(generate_sample) {
    result$point_data <- generate_realistic_indonesia_data(100)
    message("✓ Sample point data generated")
  }
  
  # Load polygon data
  if(!is.null(json_path) && file.exists(json_path)) {
    result$polygon_data <- load_polygon_data(json_path)
    message("✓ JSON polygon data loaded successfully")
    
    # Spatial join if both data exist
    if(!is.null(result$point_data) && !is.null(result$polygon_data)) {
      result$point_data <- spatial_join_point_to_polygon(
        result$point_data, result$polygon_data
      )
      
      # Aggregate by province
      result$province_summary <- aggregate_by_province(result$point_data)
      message("✓ Spatial join completed")
    }
    
    # Create spatial weights for provinces
    result$spatial_weights <- create_spatial_weights(result$polygon_data)
    message("✓ Spatial weights created")
    
  } else {
    message("! JSON file not found, using point data only")
  }
  
  return(result)
}

# ===============================================================================
# 8. EXAMPLE USAGE
# ===============================================================================

# Contoh penggunaan dengan file Anda
# spatial_data <- integrate_spatial_data(
#   rds_path = "data/indonesia511_simplified.rds",
#   json_path = "data/indonesia-province-simple.json"
# )

# Untuk testing tanpa file
spatial_data <- integrate_spatial_data(generate_sample = TRUE)

# ===============================================================================
# 9. ENHANCED UI COMPONENTS
# ===============================================================================

spatial_analysis_ui <- fluidPage(
  titlePanel("Enhanced Spatial Analysis Dashboard"),
  
  sidebarLayout(
    sidebarPanel(
      h4("Spatial Controls"),
      
      checkboxInput("show_provinces", "Tampilkan Batas Provinsi", value = TRUE),
      checkboxInput("show_points", "Tampilkan Data Points", value = TRUE),
      
      selectInput("map_variable", "Variabel untuk Visualisasi:",
                 choices = c("SOVI_INDEX", "POVERTY", "ILLITERATE", "POPULATION"),
                 selected = "SOVI_INDEX"),
      
      hr(),
      h5("Spatial Analysis Options"),
      actionButton("run_morans", "Hitung Moran's I", class = "btn-primary"),
      br(), br(),
      actionButton("run_lisa", "Analisis LISA", class = "btn-info"),
      
      hr(),
      h5("Export Options"),
      downloadButton("download_spatial_data", "Download Spatial Data", class = "btn-success")
    ),
    
    mainPanel(
      tabsetPanel(
        tabPanel("Interactive Map", 
                leafletOutput("spatial_map", height = "600px")),
        
        tabPanel("Spatial Statistics",
                fluidRow(
                  column(6, 
                         h4("Global Moran's I"),
                         verbatimTextOutput("morans_result")),
                  column(6,
                         h4("LISA Analysis"),
                         plotOutput("lisa_plot"))
                )),
        
        tabPanel("Province Summary",
                DT::dataTableOutput("province_table"))
      )
    )
  )
)

# ===============================================================================
# 10. ENHANCED SERVER LOGIC
# ===============================================================================

spatial_analysis_server <- function(input, output, session) {
  
  # Reactive data
  current_data <- reactive({
    if(input$show_points) spatial_data$point_data else NULL
  })
  
  current_polygons <- reactive({
    if(input$show_provinces) spatial_data$polygon_data else NULL
  })
  
  # Main map
  output$spatial_map <- renderLeaflet({
    create_enhanced_map(
      point_data = current_data(),
      polygon_data = current_polygons(),
      color_variable = input$map_variable,
      show_provinces = input$show_provinces
    )
  })
  
  # Moran's I calculation
  observeEvent(input$run_morans, {
    if(!is.null(spatial_data$spatial_weights) && !is.null(spatial_data$point_data)) {
      
      # Aggregate data by province first
      province_data <- aggregate_by_province(spatial_data$point_data, input$map_variable)
      
      if(!is.null(province_data)) {
        values <- province_data[[paste0("avg_", tolower(gsub("_", "", input$map_variable)))]]
        
        morans_result <- calculate_morans_i(values, spatial_data$spatial_weights$weights)
        
        output$morans_result <- renderText({
          if("error" %in% names(morans_result)) {
            paste("Error:", morans_result$error)
          } else {
            paste0(
              "Moran's I Statistic: ", round(morans_result$statistic, 4), "\n",
              "P-value: ", round(morans_result$p_value, 4), "\n",
              "Expected: ", round(morans_result$expected, 4), "\n",
              "Interpretation: ", morans_result$interpretation
            )
          }
        })
      }
    }
  })
  
  # Province summary table
  output$province_table <- DT::renderDataTable({
    if(!is.null(spatial_data$province_summary)) {
      DT::datatable(spatial_data$province_summary,
                    options = list(pageLength = 15, scrollX = TRUE)) %>%
        formatRound(columns = c("avg_sovi", "median_sovi", "avg_poverty"), digits = 2) %>%
        formatCurrency(columns = "total_population", currency = "", digits = 0)
    }
  })
  
  # Download handler
  output$download_spatial_data <- downloadHandler(
    filename = function() {
      paste("spatial_analysis_", Sys.Date(), ".xlsx", sep = "")
    },
    content = function(file) {
      wb <- createWorkbook()
      
      if(!is.null(spatial_data$point_data)) {
        addWorksheet(wb, "Point Data")
        writeData(wb, "Point Data", spatial_data$point_data)
      }
      
      if(!is.null(spatial_data$province_summary)) {
        addWorksheet(wb, "Province Summary")
        writeData(wb, "Province Summary", spatial_data$province_summary)
      }
      
      saveWorkbook(wb, file)
    }
  )
}

# ===============================================================================
# 11. RUN ENHANCED APPLICATION
# ===============================================================================

# shinyApp(ui = spatial_analysis_ui, server = spatial_analysis_server)

message("✓ Enhanced spatial analysis functions loaded successfully!")
message("✓ Ready to integrate RDS + JSON data")
message("✓ Spatial weights and analysis capabilities available")