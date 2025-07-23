# Dashboard Analisis Kerentanan Sosial Indonesia

Dashboard interaktif untuk analisis kerentanan sosial menggunakan indeks SOVI (Social Vulnerability Index) dengan data representatif Indonesia.

## Fitur Utama

- **Data Explorer**: Eksplorasi data interaktif dengan 17 variabel
- **Analisis SOVI**: Visualisasi dan analisis indeks kerentanan sosial
- **Peta Interaktif**: Analisis spasial dengan peta Indonesia
- **Analisis Korelasi**: Hubungan antar variabel kerentanan
- **Metadata**: Dokumentasi lengkap untuk setiap variabel

## Deployment

### Untuk ShinyApps.io

1. Install paket yang diperlukan:
```r
install.packages(c("rsconnect", "shiny", "shinydashboard", "DT", 
                   "ggplot2", "dplyr", "leaflet", "plotly", "corrplot"))
```

2. Setup akun ShinyApps.io:
```r
library(rsconnect)
rsconnect::setAccountInfo(name='your-account', 
                         token='your-token', 
                         secret='your-secret')
```

3. Deploy aplikasi:
```r
rsconnect::deployApp(appDir = ".", appName = "dashboard-kerentanan-sosial")
```

### Untuk Shiny Server

1. Copy semua file ke direktori Shiny Server
2. Pastikan file `app.R` berada di root direktori
3. Restart Shiny Server

### Untuk RStudio Connect

1. Gunakan tombol "Publish" di RStudio
2. Pilih RStudio Connect sebagai target
3. Deploy dengan konfigurasi default

## Struktur File

```
├── app.R                 # Aplikasi Shiny utama
├── manifest.json         # Konfigurasi deployment
├── rsconnect-python.txt  # Dependencies Python (opsional)
└── README.md            # Dokumentasi ini
```

## Troubleshooting Deployment

### Error "Package not found"
- Pastikan semua paket terinstall di environment deployment
- Gunakan `install.packages()` untuk install paket yang missing

### Error "Data not found"
- Aplikasi menggunakan data sintetis yang di-generate otomatis
- Tidak memerlukan file data eksternal

### Error "Memory limit"
- Reduce jumlah region di `generate_realistic_sovi_data(50)` menjadi angka lebih kecil
- Optimasi plot dengan mengurangi data points

### Error "Timeout"
- Aplikasi membutuhkan waktu loading untuk generate data
- Tunggu hingga loading selesai (biasanya 30-60 detik)

## Penggunaan

1. **Beranda**: Overview dan statistik umum
2. **Data Explorer**: Filter dan eksplorasi data mentah
3. **Analisis SOVI**: Distribusi dan ranking kerentanan
4. **Peta Interaktif**: Visualisasi geografis
5. **Korelasi**: Analisis hubungan antar variabel
6. **Metadata**: Dokumentasi variabel

## Kontribusi

Untuk kontribusi atau laporan bug, silakan buat issue atau pull request.

## Lisensi

MIT License - Silakan gunakan untuk keperluan akademis dan penelitian.
