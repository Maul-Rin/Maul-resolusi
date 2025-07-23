# PERBAIKAN BAGIAN PETA CHOROPLETH - COPY BAGIAN INI SAJA
# Ganti bagian peta di server function dengan kode ini

# Di bagian atas file (setelah generate data), tambahkan ini:
# Buat data GeoJSON sederhana untuk Indonesia (alternatif jika file tidak tersedia)
create_indonesia_geojson <- function() {
  # Data koordinat sederhana untuk beberapa wilayah Indonesia
  indonesia_bounds <- list(
    list(
      type = "Feature",
      properties = list(name = "Jakarta", id = "ID001"),
      geometry = list(
        type = "Polygon",
        coordinates = list(list(
          c(106.6, -6.3), c(107.0, -6.3), c(107.0, -5.9), c(106.6, -5.9), c(106.6, -6.3)
        ))
      )
    ),
    list(
      type = "Feature", 
      properties = list(name = "Bandung", id = "ID002"),
      geometry = list(
        type = "Polygon",
        coordinates = list(list(
          c(107.4, -7.0), c(107.8, -7.0), c(107.8, -6.6), c(107.4, -6.6), c(107.4, -7.0)
        ))
      )
    ),
    list(
      type = "Feature",
      properties = list(name = "Surabaya", id = "ID003"), 
      geometry = list(
        type = "Polygon",
        coordinates = list(list(
          c(112.5, -7.5), c(112.9, -7.5), c(112.9, -7.1), c(112.5, -7.1), c(112.5, -7.5)
        ))
      )
    )
  )
  
  geojson_data <- list(
    type = "FeatureCollection",
    features = indonesia_bounds
  )
  
  return(geojson_data)
}

# Fungsi untuk membaca GeoJSON (dengan fallback)
load_indonesia_geojson <- function() {
  tryCatch({
    # Coba baca file lokal jika ada
    if (file.exists("indonesia511.geojson")) {
      geojson_sf <- sf::st_read("indonesia511.geojson", quiet = TRUE)
      return(geojson_sf)
    } else {
      # Fallback: buat data sederhana
      warning("File GeoJSON tidak ditemukan, menggunakan data alternatif")
      return(NULL)
    }
  }, error = function(e) {
    warning("Error loading GeoJSON: ", e$message)
    return(NULL)
  })
}

# GANTI bagian output$interactive_map dengan ini:
output$interactive_map <- renderLeaflet({
  data <- filtered_data()
  
  # Load GeoJSON data
  indonesia_geojson <- load_indonesia_geojson()
  
  # Color palette untuk choropleth
  pal <- colorNumeric(
    palette = "RdYlBu", 
    domain = data[[input$map_var]], 
    reverse = TRUE
  )
  
  # Base map
  map <- leaflet() %>%
    addTiles() %>%
    setView(lng = 118, lat = -2, zoom = 5)
  
  if (!is.null(indonesia_geojson)) {
    # Jika GeoJSON tersedia, buat choropleth map
    tryCatch({
      # Merge data dengan GeoJSON berdasarkan nama wilayah
      merged_data <- merge(indonesia_geojson, data, 
                          by.x = "NAME_2", by.y = "Wilayah", 
                          all.x = TRUE)
      
      map <- map %>%
        addPolygons(
          data = merged_data,
          fillColor = ~pal(get(input$map_var)),
          weight = 1,
          opacity = 1,
          color = "white",
          dashArray = "3",
          fillOpacity = 0.7,
          highlight = highlightOptions(
            weight = 3,
            color = "#666",
            dashArray = "",
            fillOpacity = 0.7,
            bringToFront = TRUE
          ),
          popup = ~paste0(
            "<strong>", NAME_2, "</strong><br/>",
            input$map_var, ": ", round(get(input$map_var), 2), "<br/>",
            "SOVI Score: ", round(SOVI_Score, 3), "<br/>",
            "Kategori: ", SOVI_Category
          )
        )
    }, error = function(e) {
      # Jika merge gagal, fallback ke circle markers
      map <- map %>%
        addCircleMarkers(
          data = data,
          lng = ~LONGITUDE, lat = ~LATITUDE,
          radius = ~sqrt(get(input$map_size)) / 100,
          color = ~pal(get(input$map_var)),
          fillOpacity = 0.7,
          stroke = TRUE,
          weight = 1,
          popup = ~paste0(
            "<b>", Wilayah, "</b><br/>",
            "SOVI Score: ", round(SOVI_Score, 3), "<br/>",
            "Kategori: ", SOVI_Category, "<br/>",
            input$map_var, ": ", round(get(input$map_var), 2)
          )
        )
    })
  } else {
    # Fallback: gunakan circle markers
    size_var <- data[[input$map_size]]
    sizes <- sqrt(size_var / max(size_var, na.rm = TRUE)) * 20 + 5
    
    map <- map %>%
      addCircleMarkers(
        data = data,
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
      )
  }
  
  # Tambahkan legend
  map %>%
    addLegend(
      pal = pal,
      values = data[[input$map_var]],
      title = input$map_var,
      position = "bottomright",
      opacity = 0.7
    )
})

# UNTUK DEPLOYMENT: Tambahkan ini di bagian atas setelah load libraries
# Install sf package jika belum ada (untuk membaca GeoJSON)
if (!requireNamespace("sf", quietly = TRUE)) {
  tryCatch({
    install.packages("sf")
    library(sf)
  }, error = function(e) {
    cat("Warning: sf package tidak bisa diinstall. Menggunakan fallback method.\n")
  })
} else {
  library(sf)
}

# ALTERNATIF: Jika ingin embed GeoJSON langsung (untuk deployment)
# Ganti bagian create_indonesia_geojson dengan data yang lebih lengkap:
create_full_indonesia_geojson <- function() {
  # Data GeoJSON sederhana untuk beberapa provinsi Indonesia
  provinces <- list(
    list(
      type = "Feature",
      properties = list(name = "DKI Jakarta", id = "ID-JK"),
      geometry = list(type = "Polygon", coordinates = list(list(
        c(106.482, -6.165), c(107.175, -6.165), c(107.175, -5.880), c(106.482, -5.880), c(106.482, -6.165)
      )))
    ),
    list(
      type = "Feature", 
      properties = list(name = "Jawa Barat", id = "ID-JB"),
      geometry = list(type = "Polygon", coordinates = list(list(
        c(105.0, -7.5), c(108.5, -7.5), c(108.5, -5.5), c(105.0, -5.5), c(105.0, -7.5)
      )))
    ),
    list(
      type = "Feature",
      properties = list(name = "Jawa Tengah", id = "ID-JT"), 
      geometry = list(type = "Polygon", coordinates = list(list(
        c(108.5, -8.0), c(111.5, -8.0), c(111.5, -6.0), c(108.5, -6.0), c(108.5, -8.0)
      )))
    ),
    list(
      type = "Feature",
      properties = list(name = "Jawa Timur", id = "ID-JI"),
      geometry = list(type = "Polygon", coordinates = list(list(
        c(111.5, -8.5), c(114.5, -8.5), c(114.5, -6.5), c(111.5, -6.5), c(111.5, -8.5)
      )))
    )
  )
  
  return(list(type = "FeatureCollection", features = provinces))
}