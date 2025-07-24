# ===============================================================================
# IMPLEMENTASI PRAKTIS - INTEGRASI FILE RDS + JSON ANDA
# Modifikasi dashboard untuk menggunakan file existing
# ===============================================================================

# ===============================================================================
# 1. MODIFIKASI UNTUK FILE ANDA
# ===============================================================================

# Ganti bagian data loading di dashboard utama dengan ini:
load_your_actual_data <- function() {
  
  message("🔄 Loading your actual files...")
  
  # 1. Load data RDS Anda
  tryCatch({
    sovi_data <- readRDS("data/indonesia511_simplified.rds")
    message("✅ RDS data loaded successfully")
  }, error = function(e) {
    message("❌ Error loading RDS: ", e$message)
    message("🔄 Generating sample data instead...")
    sovi_data <- generate_realistic_indonesia_data(100)
  })
  
  # 2. Load polygon JSON Anda
  tryCatch({
    provinces_sf <- st_read("data/indonesia-province-simple.json", quiet = TRUE)
    
    # Pastikan CRS konsisten
    if(!is.na(st_crs(provinces_sf))) {
      if(st_crs(provinces_sf)$input != "EPSG:4326") {
        provinces_sf <- st_transform(provinces_sf, crs = 4326)
      }
    } else {
      st_crs(provinces_sf) <- 4326
    }
    
    message("✅ JSON polygon data loaded successfully")
    message("📊 Provinces found: ", nrow(provinces_sf))
    
    # 3. Spatial join - gabungkan point data dengan polygon
    if(all(c("LATITUDE", "LONGITUDE") %in% names(sovi_data))) {
      
      # Convert point data ke sf object
      points_sf <- st_as_sf(sovi_data, 
                           coords = c("LONGITUDE", "LATITUDE"), 
                           crs = 4326)
      
      # Spatial join
      joined_data <- st_join(points_sf, provinces_sf)
      
      # Convert back ke data frame dengan koordinat
      sovi_data <- joined_data %>%
        mutate(
          LONGITUDE = st_coordinates(.)[,1],
          LATITUDE = st_coordinates(.)[,2]
        ) %>%
        st_drop_geometry()
      
      message("✅ Spatial join completed")
      
      # 4. Buat agregasi per provinsi
      province_summary <- sovi_data %>%
        group_by(NAME_1) %>%  # Sesuaikan dengan nama kolom di JSON Anda
        summarise(
          count_regions = n(),
          avg_sovi = mean(SOVI_INDEX, na.rm = TRUE),
          median_sovi = median(SOVI_INDEX, na.rm = TRUE),
          total_population = sum(POPULATION, na.rm = TRUE),
          avg_poverty = mean(POVERTY, na.rm = TRUE),
          .groups = 'drop'
        )
      
      message("✅ Province aggregation completed")
      
      # 5. Buat spatial weights untuk analisis lanjutan
      spatial_weights <- create_spatial_weights(provinces_sf)
      message("✅ Spatial weights calculated")
      
    } else {
      message("⚠️ No coordinate columns found, skipping spatial join")
      province_summary <- NULL
      spatial_weights <- NULL
    }
    
  }, error = function(e) {
    message("❌ Error loading JSON: ", e$message)
    provinces_sf <- NULL
    province_summary <- NULL
    spatial_weights <- NULL
  })
  
  return(list(
    point_data = sovi_data,
    polygon_data = provinces_sf,
    province_summary = province_summary,
    spatial_weights = spatial_weights
  ))
}

# ===============================================================================
# 2. ENHANCED MAP FUNCTION UNTUK DATA ANDA
# ===============================================================================

create_your_enhanced_map <- function(spatial_data, color_variable = "SOVI_INDEX", 
                                   show_provinces = TRUE, show_points = TRUE) {
  
  # Base map
  map <- leaflet() %>%
    addTiles(group = "OpenStreetMap") %>%
    addProviderTiles(providers$CartoDB.Positron, group = "CartoDB Light") %>%
    addProviderTiles(providers$Esri.WorldImagery, group = "Satellite") %>%
    setView(lng = 118, lat = -2, zoom = 5)
  
  # Tambahkan polygon provinsi
  if(show_provinces && !is.null(spatial_data$polygon_data)) {
    
    if(!is.null(spatial_data$province_summary)) {
      # Join polygon dengan summary data
      polygon_with_data <- spatial_data$polygon_data %>%
        left_join(spatial_data$province_summary, by = "NAME_1")
      
      # Color palette untuk provinsi
      pal_province <- colorNumeric(
        palette = "YlOrRd",
        domain = polygon_with_data$avg_sovi,
        na.color = "lightgray"
      )
      
      # Tambahkan polygon dengan warna berdasarkan rata-rata SOVI
      map <- map %>%
        addPolygons(
          data = polygon_with_data,
          fillColor = ~pal_province(avg_sovi),
          fillOpacity = 0.6,
          color = "white",
          weight = 2,
          popup = ~paste0(
            "<div style='font-family: Arial; max-width: 250px;'>",
            "<h4 style='margin: 0; color: #2c3e50;'>", NAME_1, "</h4>",
            "<hr style='margin: 5px 0;'>",
            "<strong>Jumlah Region:</strong> ", count_regions, "<br>",
            "<strong>Rata-rata SOVI:</strong> ", round(avg_sovi, 3), "<br>",
            "<strong>Median SOVI:</strong> ", round(median_sovi, 3), "<br>",
            "<strong>Total Populasi:</strong> ", format(total_population, big.mark = ","), "<br>",
            "<strong>Rata-rata Kemiskinan:</strong> ", round(avg_poverty, 1), "%",
            "</div>"
          ),
          group = "Batas Provinsi",
          highlightOptions = highlightOptions(
            weight = 3,
            color = "#666",
            dashArray = "",
            fillOpacity = 0.8,
            bringToFront = TRUE
          )
        ) %>%
        addLegend(
          pal = pal_province,
          values = polygon_with_data$avg_sovi,
          title = "Rata-rata SOVI<br>per Provinsi",
          position = "topright",
          opacity = 0.8
        )
    } else {
      # Tampilkan polygon tanpa data
      map <- map %>%
        addPolygons(
          data = spatial_data$polygon_data,
          fillColor = "lightblue",
          fillOpacity = 0.3,
          color = "white",
          weight = 2,
          popup = ~paste0("<strong>Provinsi:</strong> ", NAME_1),
          group = "Batas Provinsi"
        )
    }
  }
  
  # Tambahkan point markers
  if(show_points && !is.null(spatial_data$point_data)) {
    pal_points <- colorNumeric(
      palette = viridis::viridis(10),
      domain = spatial_data$point_data[[color_variable]],
      na.color = "gray"
    )
    
    map <- map %>%
      addCircleMarkers(
        data = spatial_data$point_data,
        lng = ~LONGITUDE,
        lat = ~LATITUDE,
        radius = ~pmax(3, pmin(15, sqrt(POPULATION/20000) + 4)),
        color = "white",
        weight = 2,
        fillColor = ~pal_points(get(color_variable)),
        fillOpacity = 0.8,
        popup = ~paste0(
          "<div style='font-family: Arial; max-width: 300px;'>",
          "<h4 style='margin: 0; color: #2c3e50;'>", REGION, "</h4>",
          if_else(!is.na(NAME_1), paste0("<p style='margin: 5px 0; color: #7f8c8d;'>", NAME_1, "</p>"), ""),
          "<hr style='margin: 5px 0;'>",
          "<strong>", color_variable, ":</strong> ", round(get(color_variable), 3), "<br>",
          "<strong>Kategori SOVI:</strong> ", SOVI_CATEGORY, "<br>",
          "<strong>Populasi:</strong> ", format(POPULATION, big.mark = ","), " jiwa<br>",
          "<strong>Kemiskinan:</strong> ", POVERTY, "%<br>",
          "<strong>Buta Huruf:</strong> ", ILLITERATE, "%",
          "</div>"
        ),
        label = ~paste(REGION, "-", round(get(color_variable), 2)),
        group = "Data Points"
      ) %>%
      addLegend(
        pal = pal_points,
        values = spatial_data$point_data[[color_variable]],
        title = color_variable,
        position = "bottomright",
        opacity = 0.8
      )
  }
  
  # Layer control
  overlay_groups <- c()
  if(show_provinces) overlay_groups <- c(overlay_groups, "Batas Provinsi")
  if(show_points) overlay_groups <- c(overlay_groups, "Data Points")
  
  if(length(overlay_groups) > 0) {
    map <- map %>%
      addLayersControl(
        baseGroups = c("OpenStreetMap", "CartoDB Light", "Satellite"),
        overlayGroups = overlay_groups,
        options = layersControlOptions(collapsed = FALSE)
      )
  }
  
  return(map)
}

# ===============================================================================
# 3. SPATIAL ANALYSIS FUNCTIONS UNTUK DATA ANDA
# ===============================================================================

analyze_spatial_autocorrelation <- function(spatial_data, variable = "SOVI_INDEX") {
  
  if(is.null(spatial_data$spatial_weights) || is.null(spatial_data$province_summary)) {
    return(list(error = "Spatial weights atau province summary tidak tersedia"))
  }
  
  # Ambil nilai variabel per provinsi
  values <- spatial_data$province_summary[[paste0("avg_", tolower(variable))]]
  
  if(is.null(values) || all(is.na(values))) {
    return(list(error = "Data variabel tidak tersedia"))
  }
  
  # Hitung Moran's I
  tryCatch({
    moran_result <- moran.test(values, spatial_data$spatial_weights$weights, zero.policy = TRUE)
    
    # Hitung Local Moran's I (LISA)
    lisa_result <- localmoran(values, spatial_data$spatial_weights$weights, zero.policy = TRUE)
    
    # Klasifikasi LISA
    mean_val <- mean(values, na.rm = TRUE)
    lag_val <- lag.listw(spatial_data$spatial_weights$weights, values)
    
    lisa_categories <- rep("Not Significant", length(values))
    significant <- lisa_result[,5] < 0.05
    
    lisa_categories[significant & values > mean_val & lag_val > mean_val] <- "High-High"
    lisa_categories[significant & values < mean_val & lag_val < mean_val] <- "Low-Low"
    lisa_categories[significant & values > mean_val & lag_val < mean_val] <- "High-Low"
    lisa_categories[significant & values < mean_val & lag_val > mean_val] <- "Low-High"
    
    return(list(
      global_morans = list(
        statistic = moran_result$statistic,
        p_value = moran_result$p.value,
        expected = moran_result$estimate[2],
        interpretation = ifelse(moran_result$p.value < 0.05, 
                              "Ada spatial clustering yang signifikan", 
                              "Tidak ada spatial clustering")
      ),
      local_morans = list(
        statistics = lisa_result,
        categories = lisa_categories,
        provinces = spatial_data$province_summary$NAME_1
      )
    ))
    
  }, error = function(e) {
    return(list(error = paste("Error in spatial analysis:", e$message)))
  })
}

# ===============================================================================
# 4. MODIFIKASI DASHBOARD UTAMA
# ===============================================================================

# Ganti bagian data initialization dengan:
# Load data Anda yang sebenarnya
actual_spatial_data <- load_your_actual_data()

# Ganti di server function, bagian map output dengan:
output$interactive_map <- renderLeaflet({
  create_your_enhanced_map(
    spatial_data = actual_spatial_data,
    color_variable = input$map_variable,
    show_provinces = input$show_provinces %||% TRUE,
    show_points = input$show_points %||% TRUE
  )
})

# Tambahkan spatial analysis tab
output$spatial_analysis_results <- renderPrint({
  if(!is.null(input$run_spatial_analysis) && input$run_spatial_analysis > 0) {
    isolate({
      results <- analyze_spatial_autocorrelation(actual_spatial_data, input$map_variable)
      
      if("error" %in% names(results)) {
        cat("Error:", results$error)
      } else {
        cat("=== GLOBAL MORAN'S I ===\n")
        cat("Statistic:", round(results$global_morans$statistic, 4), "\n")
        cat("P-value:", round(results$global_morans$p_value, 4), "\n")
        cat("Interpretation:", results$global_morans$interpretation, "\n\n")
        
        cat("=== LOCAL MORAN'S I (LISA) ===\n")
        lisa_summary <- table(results$local_morans$categories)
        print(lisa_summary)
      }
    })
  } else {
    cat("Klik tombol 'Run Spatial Analysis' untuk melihat hasil.")
  }
})

# ===============================================================================
# 5. TAMBAHAN UI UNTUK SPATIAL ANALYSIS
# ===============================================================================

# Tambahkan di UI, pada tab peta:
box(
  title = "🌍 Analisis Spasial Lanjutan", status = "info", solidHeader = TRUE, width = 12,
  
  fluidRow(
    column(4,
           h5("Kontrol Analisis"),
           checkboxInput("show_provinces", "Tampilkan Batas Provinsi", value = TRUE),
           checkboxInput("show_points", "Tampilkan Data Points", value = TRUE),
           actionButton("run_spatial_analysis", "🔍 Run Spatial Analysis", class = "btn-primary")
    ),
    
    column(8,
           h5("Hasil Analisis Spatial Autocorrelation"),
           verbatimTextOutput("spatial_analysis_results")
    )
  )
)

message("✅ Implementasi praktis untuk file RDS + JSON Anda sudah siap!")
message("📝 Langkah selanjutnya:")
message("   1. Pastikan file RDS dan JSON ada di folder 'data/'")
message("   2. Sesuaikan nama kolom di JSON (NAME_1, dll) dengan file Anda")
message("   3. Jalankan load_your_actual_data() untuk testing")
message("   4. Integrasikan ke dashboard utama")