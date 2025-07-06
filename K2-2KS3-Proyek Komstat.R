# =============================================================================
# DASHBOARD ANALISIS IKLIM & PRODUKTIVITAS PADI JAWA TIMUR - FINAL COMPLETE
# Menggunakan Data Real dengan Format Semicolon-Separated CSV
# =============================================================================

# Load required libraries
library(shiny)
library(shinydashboard)
library(DT)
library(plotly)
library(leaflet)
library(dplyr)
library(ggplot2)
library(RColorBrewer)
library(tidyr)
library(sf)
library(readr)
library(Kendall)
library(corrplot)
library(biscale)
library(cowplot)

# =============================================================================
# DATA LOADING (VERSI BARU - DARI FILE CSV LOKAL)
# =============================================================================

cat("=== LOADING DATA FROM LOCAL CSV FILES ===\n")

# 1. Tentukan path ke file CSV lokal Anda di dalam folder DATA
# Pastikan nama file ini sama persis dengan yang ada di folder Anda
path_produktivitas <- "DATA/rev-final-produktivitas-padi-jatim.csv"
path_curah_hujan <- "DATA/rev-final-rata-rata-curah-hujan.csv"
path_temperatur <- "DATA/rev-final-rata-rata-temperatur-suhu.csv"

# 2. Muat data menggunakan readr::read_csv2 (untuk file dengan separator ';')
# Fungsi ini lebih modern dan efisien daripada read.csv
tryCatch({
  # Menggunakan read_delim untuk presisi
  # delim=";" -> pemisah kolom adalah titik koma
  # locale=locale(decimal_mark = ".") -> penanda desimal adalah titik
  raw_produktivitas <- readr::read_delim("DATA/rev-final-produktivitas-padi-jatim.csv", 
                                         delim = ";", 
                                         locale = locale(decimal_mark = "."))
  cat("File produktivitas berhasil dimuat.\n")
}, error = function(e) {
  cat("Gagal memuat file produktivitas:", e$message, "\n")
  raw_produktivitas <- NULL
})

tryCatch({
  raw_curah_hujan <- readr::read_delim("DATA/rev-final-rata-rata-curah-hujan.csv", 
                                       delim = ";", 
                                       locale = locale(decimal_mark = "."))
  cat("File curah hujan berhasil dimuat.\n")
}, error = function(e) {
  cat("Gagal memuat file curah hujan:", e$message, "\n")
  raw_curah_hujan <- NULL
})

tryCatch({
  raw_temperatur <- readr::read_delim("DATA/rev-final-rata-rata-temperatur-suhu.csv", 
                                      delim = ";", 
                                      locale = locale(decimal_mark = "."))
  cat("File temperatur berhasil dimuat.\n\n")
}, error = function(e) {
  cat("Gagal memuat file temperatur:", e$message, "\n")
  raw_temperatur <- NULL
})

# Load all datasets
# MEMUAT DATA SPASIAL (SHAPEFILE) HANYA UNTUK PETA
# =============================================================================
cat("=== MEMUAT SHAPEFILE UNTUK VISUALISASI PETA ===\n")
path_shp <- "DATA/2-shp-project-komstat.shp"

tryCatch({
  shp_data_peta <- st_read(path_shp, quiet = TRUE) %>%
    # Buat kolom join yang sudah distandardisasi (UPPERCASE)
    # agar cocok dengan kolom 'kabkota' dari data CSV Anda.
    mutate(kabkota_join = toupper(nmkab))
  
  cat("Shapefile untuk peta berhasil dimuat.\n\n")
}, error = function(e) {
  cat("Gagal memuat Shapefile untuk peta:", e$message, "\n")
  shp_data_peta <- NULL
})
cat("=== LOADING DATA FROM REVISED CSV FILES ===\n")
raw_produktivitas <- readr::read_csv2(path_produktivitas)
raw_curah_hujan <- readr::read_csv2(path_curah_hujan)
raw_temperatur <- readr::read_csv2(path_temperatur)

# =============================================================================
# DATA PROCESSING FUNCTIONS FOR WIDE FORMAT
# =============================================================================

# Function to process wide format data to long format
process_wide_to_long <- function(data, value_name, data_type) {
  if(is.null(data)) return(NULL)
  
  cat("Processing", data_type, "data from wide to long format...\n")
  
  # Get kabkota column (first column)
  kabkota_col <- names(data)[1]
  
  # Get year columns (should be tahun_2019, tahun_2020, etc.)
  year_cols <- names(data)[-1]  # All columns except first
  cat("Year columns found:", paste(year_cols, collapse = ", "), "\n")
  
  # Convert to long format
  processed <- data %>%
    pivot_longer(cols = all_of(year_cols), 
                 names_to = "tahun", 
                 values_to = value_name) %>%
    mutate(
      # Extract year from column name (tahun_2019 -> 2019)
      tahun = as.numeric(gsub("tahun_", "", tahun)),
      # Ensure value is numeric
      !!value_name := as.numeric(.data[[value_name]])
    ) %>%
    # Rename kabkota column to standard name
    rename(kabkota = all_of(kabkota_col)) %>%
    # Remove rows with missing values
    filter(!is.na(.data[[value_name]]), !is.na(tahun)) %>%
    # Clean kabkota names (remove extra spaces, standardize case)
    mutate(kabkota = trimws(toupper(kabkota)))
  
  cat("Processed", data_type, "data:", nrow(processed), "rows\n")
  cat("Years:", paste(sort(unique(processed$tahun)), collapse = ", "), "\n")
  cat("Sample kabkota:", paste(head(unique(processed$kabkota), 3), collapse = ", "), "\n\n")
  
  return(processed)
}

# =============================================================================
# PROCESS ALL DATA
# =============================================================================

cat("=== PROCESSING DATA TO LONG FORMAT ===\n")
data_produktivitas <- process_wide_to_long(raw_produktivitas, "produktivitas", "Produktivitas")
data_curah_hujan <- process_wide_to_long(raw_curah_hujan, "curah_hujan", "Curah Hujan")
data_temperatur <- process_wide_to_long(raw_temperatur, "temperatur", "Temperatur")

# Print summary of processed data
if(!is.null(data_produktivitas)) {
  cat("PRODUKTIVITAS DATA SUMMARY:\n")
  cat("- Total rows:", nrow(data_produktivitas), "\n")
  cat("- Unique kabkota:", length(unique(data_produktivitas$kabkota)), "\n")
  cat("- Years range:", paste(range(data_produktivitas$tahun), collapse = " - "), "\n")
  cat("- Produktivitas range:", paste(round(range(data_produktivitas$produktivitas, na.rm = TRUE), 2), collapse = " - "), "ku/ha\n\n")
}

if(!is.null(data_curah_hujan)) {
  cat("CURAH HUJAN DATA SUMMARY:\n")
  cat("- Total rows:", nrow(data_curah_hujan), "\n")
  cat("- Unique kabkota:", length(unique(data_curah_hujan$kabkota)), "\n")
  cat("- Years range:", paste(range(data_curah_hujan$tahun), collapse = " - "), "\n")
  cat("- Curah hujan range:", paste(round(range(data_curah_hujan$curah_hujan, na.rm = TRUE), 2), collapse = " - "), "mm\n\n")
}

if(!is.null(data_temperatur)) {
  cat("TEMPERATUR DATA SUMMARY:\n")
  cat("- Total rows:", nrow(data_temperatur), "\n")
  cat("- Unique kabkota:", length(unique(data_temperatur$kabkota)), "\n")
  cat("- Years range:", paste(range(data_temperatur$tahun), collapse = " - "), "\n")
  cat("- Temperatur range:", paste(round(range(data_temperatur$temperatur, na.rm = TRUE), 2), collapse = " - "), "°C\n\n")
}

# =============================================================================
# UTILITY FUNCTIONS
# =============================================================================

# Trend analysis using Mann-Kendall test
trend_analysis <- function(data_vector) {
  if(length(data_vector) < 3) {
    return(list(
      trend = "Insufficient data",
      significance = "N/A",
      tau = NA,
      p_value = NA
    ))
  }
  
  tryCatch({
    mk_test <- MannKendall(data_vector)
    list(
      trend = ifelse(mk_test$tau > 0, "Increasing", "Decreasing"),
      significance = ifelse(mk_test$sl < 0.05, "Significant", "Not Significant"),
      tau = mk_test$tau,
      p_value = mk_test$sl
    )
  }, error = function(e) {
    list(
      trend = "Error in calculation",
      significance = "N/A",
      tau = NA,
      p_value = NA
    )
  })
}

# Correlation analysis
correlation_analysis <- function(x, y) {
  if(length(x) < 3 || length(y) < 3) {
    return(list(
      correlation = NA,
      p_value = NA,
      significance = "Insufficient data",
      confidence_interval = c(NA, NA)
    ))
  }
  
  tryCatch({
    cor_test <- cor.test(x, y, method = "pearson")
    list(
      correlation = as.numeric(cor_test$estimate),
      p_value = cor_test$p.value,
      confidence_interval = cor_test$conf.int,
      significance = ifelse(cor_test$p.value < 0.05, "Signifikan", "Tidak Signifikan")
    )
  }, error = function(e) {
    list(
      correlation = NA,
      p_value = NA,
      significance = "Error in calculation",
      confidence_interval = c(NA, NA)
    )
  })
}

# Fungsi untuk menghitung korelasi per kabupaten
calculate_correlation_table <- function(variabel_iklim) {
  # Pastikan data yang dibutuhkan ada
  if (is.null(data_produktivitas) || is.null(data_temperatur) || is.null(data_curah_hujan)) {
    return(data.frame(Message = "Data tidak lengkap untuk perhitungan."))
  }
  
  correlation_results <- data.frame()
  
  for(kab in kabkota_iklim) {
    # Data produktivitas untuk kabupaten ini
    prod_data <- data_produktivitas %>% 
      filter(kabkota == toupper(kab)) %>% 
      arrange(tahun) %>% 
      pull(produktivitas)
    
    # Data iklim untuk kabupaten ini
    if(variabel_iklim == "temperatur") {
      iklim_data <- data_temperatur %>% 
        filter(kabkota == toupper(kab)) %>% 
        arrange(tahun) %>% 
        pull(temperatur)
    } else {
      iklim_data <- data_curah_hujan %>% 
        filter(kabkota == toupper(kab)) %>% 
        arrange(tahun) %>% 
        pull(curah_hujan)
    }
    
    # Hitung korelasi hanya jika data cukup
    if(length(prod_data) >= 3 && length(iklim_data) >= 3) {
      cor_result <- correlation_analysis(iklim_data, prod_data)
      
      # Kategori kekuatan korelasi
      kekuatan <- case_when(
        is.na(cor_result$correlation) ~ "N/A",
        abs(cor_result$correlation) >= 0.8 ~ "Sangat Kuat",
        abs(cor_result$correlation) >= 0.6 ~ "Kuat",
        abs(cor_result$correlation) >= 0.4 ~ "Sedang",
        abs(cor_result$correlation) >= 0.2 ~ "Lemah",
        TRUE ~ "Sangat Lemah"
      )
      
      # Kategori produktivitas rata-rata
      avg_prod <- mean(prod_data, na.rm = TRUE)
      kategori_prod <- case_when(
        avg_prod >= 60 ~ "Tinggi",
        avg_prod >= 50 ~ "Sedang",
        TRUE ~ "Rendah"
      )
      
      correlation_results <- rbind(correlation_results, data.frame(
        Kabupaten = kab,
        Rata_rata_Iklim = round(mean(iklim_data, na.rm = TRUE), 2),
        Rata_rata_Produktivitas = round(avg_prod, 2),
        Korelasi = round(cor_result$correlation, 3),
        P_Value = round(cor_result$p_value, 4),
        Kekuatan_Korelasi = kekuatan,
        Kategori_Produktivitas = kategori_prod,
        Signifikansi = cor_result$significance
      ))
    }
  }
  
  # Urutkan berdasarkan nilai absolut korelasi
  if(nrow(correlation_results) > 0) {
    correlation_results <- correlation_results %>%
      arrange(desc(abs(Korelasi)))
  }
  
  return(correlation_results)
}

# Get available data with safe handling
kabkota_produktivitas <- if(!is.null(data_produktivitas)) sort(unique(data_produktivitas$kabkota)) else c()
kabkota_iklim <- if(!is.null(data_temperatur)) sort(unique(data_temperatur$kabkota)) else c()
years <- if(!is.null(data_produktivitas)) sort(unique(data_produktivitas$tahun)) else 2019:2024

# Data untuk analisis korelasi - with safe join
data_korelasi <- NULL
if(!is.null(data_produktivitas) && !is.null(data_temperatur) && !is.null(data_curah_hujan)) {
  # Find common kabkota between datasets
  common_kabkota <- intersect(intersect(unique(data_produktivitas$kabkota), 
                                        unique(data_temperatur$kabkota)), 
                              unique(data_curah_hujan$kabkota))
  
  if(length(common_kabkota) > 0) {
    data_korelasi <- data_produktivitas %>%
      filter(kabkota %in% common_kabkota) %>%
      left_join(data_temperatur %>% filter(kabkota %in% common_kabkota), 
                by = c("kabkota", "tahun")) %>%
      left_join(data_curah_hujan %>% filter(kabkota %in% common_kabkota), 
                by = c("kabkota", "tahun")) %>%
      filter(!is.na(produktivitas), !is.na(temperatur), !is.na(curah_hujan))
    
    cat("CORRELATION DATA CREATED:\n")
    cat("- Total rows:", nrow(data_korelasi), "\n")
    cat("- Common kabkota:", length(common_kabkota), "\n")
    cat("- Sample kabkota:", paste(head(common_kabkota, 5), collapse = ", "), "\n\n")
  }
}


# =============================================================================
# UI DEFINITION
# =============================================================================

ui <- dashboardPage(
  dashboardHeader(
    title = "Analisis Iklim & Produktivitas Padi di Jawa Timur", 
    titleWidth = 450 
  ),
  
  dashboardSidebar(
    width = 280,
    sidebarMenu(
      # --- Grup Menu Utama ---
      menuItem("Dashboard Overview", tabName = "overview", icon = icon("tachometer-alt")),
      menuItem("Produktivitas Padi", tabName = "produktivitas", icon = icon("seedling")),
      menuItem("Curah Hujan", tabName = "curah_hujan", icon = icon("cloud-rain")),
      menuItem("Temperatur", tabName = "temperatur", icon = icon("thermometer-half")),
      menuItem("Analisis Korelasi", tabName = "korelasi", icon = icon("chart-line")),
      menuItem("Data Info", tabName = "info", icon = icon("info-circle")),
      
      # --- Pemisah Otomatis ---
      tags$li(class = "mt-auto"),
      
      # --- Grup Menu Bawah ---
      menuItem("User Guide", tabName = "user_guide", icon = icon("book-reader")),
      menuItem("Profil Tim", tabName = "profil_tim", icon = icon("users"))
    )  
  ),
  
  
  dashboardBody(
    tags$head(
      tags$style(HTML("
      .main-header .navbar-brand {
      padding-left: 15px !important; /* Jarak kiri untuk judul */
    }
    .sidebar-toggle {
      float: left; /* Paksa menu ke kiri */
      margin-left: 15px; /* Beri sedikit jarak */
    }
      
    /* Memberi sentuhan modern pada font header */
    .main-header .logo {
      font-family: 'Segoe UI', Roboto, 'Helvetica Neue', Arial, sans-serif;
      font-weight: 600; /* Sedikit lebih tebal */
      font-size: 18px;
    }

    /* Memberi background yang soft pada body */
    .content-wrapper {
      background-color: #f4f6f9; /* Warna abu-abu yang lebih cerah */
    }

    /* Membuat box lebih modern dengan shadow halus */
    .box {
      border-radius: 0.5rem; /* Sudut lebih melengkung */
      box-shadow: 0 0 1px rgba(0,0,0,.125), 0 1px 3px rgba(0,0,0,.2);
      border: none; /* Hapus border default */
    }
    
    /* Tombol filter yang lebih rapi */
    .btn-primary {
      background-color: #007bff;
      border-color: #007bff;
    }

    /* Konsistensi warna header dan sidebar */
    .main-header .navbar { background-color: #3c8dbc !important; }
    .main-sidebar { background-color: #2c3e50 !important; }

    /* Mengatur menu di sidebar agar menempel di bawah */
    .sidebar-menu {
      display: flex;
      flex-direction: column;
      height: 90%;
    }
    .sidebar-menu > .mt-auto {
      margin-top: auto;
    }
  "))
    ),
    
    tabItems(
      # Tab Overview
      tabItem(tabName = "overview",
              fluidRow(
                box(
                  title = "Dashboard Overview - Data Real Jawa Timur (Complete Version)", 
                  status = "primary", solidHeader = TRUE, width = 12,
                  h4("Analisis Berdasarkan Data CSV Semicolon-Separated Format"),
                  p("Dashboard ini menggunakan data real dari CSV dengan format semicolon-separated yang telah diproses dari wide format ke long format."),
                  hr()
                )
              ),
              
              # Value Boxes Row
              fluidRow(
                valueBoxOutput("total_kabkota", width = 3),
                valueBoxOutput("avg_produktivitas", width = 3),
                valueBoxOutput("avg_temperatur", width = 3),
                valueBoxOutput("avg_curah_hujan", width = 3)
              ),
              
              # Summary Statistics Row
              fluidRow(
                box(
                  title = "Summary Statistik Produktivitas Padi", 
                  status = "primary", solidHeader = TRUE, width = 4,
                  tableOutput("summary_produktivitas")
                ),
                box(
                  title = "Summary Statistik Temperatur", 
                  status = "warning", solidHeader = TRUE, width = 4,
                  tableOutput("summary_temperatur")
                ),
                box(
                  title = "Summary Statistik Curah Hujan", 
                  status = "success", solidHeader = TRUE, width = 4,
                  tableOutput("summary_curah_hujan")
                )
              ),
              
              # Trend Analysis Row
              fluidRow(
                box(
                  title = "Trend Analysis - Produktivitas Padi", 
                  status = "info", solidHeader = TRUE, width = 4,
                  verbatimTextOutput("trend_produktivitas")
                ),
                box(
                  title = "Trend Analysis - Temperatur", 
                  status = "info", solidHeader = TRUE, width = 4,
                  verbatimTextOutput("trend_temperatur")
                ),
                box(
                  title = "Trend Analysis - Curah Hujan", 
                  status = "info", solidHeader = TRUE, width = 4,
                  verbatimTextOutput("trend_curah_hujan")
                )
              ),
              
              # Charts Row
              fluidRow(
                box(
                  title = "Trend Produktivitas Padi per Tahun", 
                  status = "primary", solidHeader = TRUE, width = 4,
                  plotlyOutput("overview_chart_produktivitas", height = "300px")
                ),
                box(
                  title = "Trend Temperatur per Tahun", 
                  status = "warning", solidHeader = TRUE, width = 4,
                  plotlyOutput("overview_chart_temperatur", height = "300px")
                ),
                box(
                  title = "Trend Curah Hujan per Tahun", 
                  status = "success", solidHeader = TRUE, width = 4,
                  plotlyOutput("overview_chart_curah_hujan", height = "300px")
                )
              ),
              
              # Comparative Analysis Row
              fluidRow(
                box(
                  title = "Perbandingan Trend Semua Variabel", 
                  status = "danger", solidHeader = TRUE, width = 8,
                  plotlyOutput("overview_combined_chart", height = "400px")
                ),
                box(
                  title = "Matriks Korelasi Antar Variabel", 
                  status = "danger", solidHeader = TRUE, width = 4,
                  plotOutput("overview_correlation_plot", height = "350px"),
                  br(),
                  h5("Interpretasi Korelasi:"),
                  p("Biru: korelasi positif", br(),
                    "Merah: korelasi negatif", br(), 
                    "Intensitas warna menunjukkan kekuatan korelasi", 
                    style = "font-size: 11px; color: #666;")
                )
              )
      ),
      
      # Tab Produktivitas Padi
      tabItem(tabName = "produktivitas",
              fluidRow(
                box(
                  title = "Filter Data - Produktivitas Padi", 
                  status = "primary", solidHeader = TRUE, width = 12,
                  column(4,
                         selectInput("prod_scope", "Pilih Scope:",
                                     choices = c("Seluruh Jawa Timur" = "all", 
                                                 "Kabupaten/Kota Tertentu" = "specific"),
                                     selected = "all")
                  ),
                  column(4,
                         conditionalPanel(
                           condition = "input.prod_scope == 'specific'",
                           selectInput("prod_kabkota", "Pilih Kabupaten/Kota:",
                                       choices = kabkota_produktivitas, 
                                       selected = if(length(kabkota_produktivitas) > 0) kabkota_produktivitas[1] else NULL)
                         )
                  ),
                  column(4,
                         conditionalPanel(
                           condition = "input.prod_scope == 'all'",
                           selectInput("prod_tahun", "Pilih Tahun:",
                                       choices = years, 
                                       selected = if(length(years) > 0) max(years) else NULL)
                         )
                  )
                )
              ),
              
              fluidRow(
                box(
                  title = "Trend Produktivitas Padi", status = "info", solidHeader = TRUE, width = 6,
                  plotlyOutput("prod_line_chart", height = "300px")
                ),
                box(
                  title = "Perbandingan Produktivitas Padi", status = "info", solidHeader = TRUE, width = 6,
                  plotlyOutput("prod_bar_chart", height = "300px")
                )
              ),
              
              fluidRow(
                box(
                  title = "Distribusi Kategori Produktivitas", status = "info", solidHeader = TRUE, width = 6,
                  plotlyOutput("prod_pie_chart", height = "300px")
                ),
                box(
                  title = "Peta Produktivitas Padi", status = "info", solidHeader = TRUE, width = 6,
                  leafletOutput("prod_map", height = "300px")
                )
              )
      ),
      
      # Tab Curah Hujan
      tabItem(tabName = "curah_hujan",
              fluidRow(
                box(
                  title = "Filter Data - Curah Hujan", 
                  status = "primary", solidHeader = TRUE, width = 12,
                  column(6,
                         selectInput("hujan_kabkota", "Pilih Kabupaten/Kota:",
                                     choices = c("Semua Kabupaten/Kota" = "all", kabkota_iklim),
                                     selected = "all")
                  ),
                  column(6,
                         selectInput("hujan_tahun", "Pilih Tahun:",
                                     choices = years, 
                                     selected = if(length(years) > 0) max(years) else NULL)
                  )
                )
              ),
              
              fluidRow(
                box(
                  title = "Trend Curah Hujan", status = "success", solidHeader = TRUE, width = 6,
                  plotlyOutput("hujan_line_chart", height = "300px")
                ),
                box(
                  title = "Perbandingan Curah Hujan", status = "success", solidHeader = TRUE, width = 6,
                  plotlyOutput("hujan_bar_chart", height = "300px")
                )
              ),
              
              fluidRow(
                box(
                  title = "Distribusi Curah Hujan", status = "success", solidHeader = TRUE, width = 6,
                  plotlyOutput("hujan_pie_chart", height = "300px")
                ),
                box(
                  title = "Peta Curah Hujan", status = "success", solidHeader = TRUE, width = 6,
                  leafletOutput("hujan_map", height = "300px")
                )
              )
      ),
      
      # Tab Temperatur
      tabItem(tabName = "temperatur",
              fluidRow(
                box(
                  title = "Filter Data - Temperatur", 
                  status = "primary", solidHeader = TRUE, width = 12,
                  column(6,
                         selectInput("temp_kabkota", "Pilih Kabupaten/Kota:",
                                     choices = c("Semua Kabupaten/Kota" = "all", kabkota_iklim),
                                     selected = "all")
                  ),
                  column(6,
                         selectInput("temp_tahun", "Pilih Tahun:",
                                     choices = years, 
                                     selected = if(length(years) > 0) max(years) else NULL)
                  )
                )
              ),
              
              fluidRow(
                box(
                  title = "Trend Temperatur", status = "warning", solidHeader = TRUE, width = 6,
                  plotlyOutput("temp_line_chart", height = "300px")
                ),
                box(
                  title = "Perbandingan Temperatur", status = "warning", solidHeader = TRUE, width = 6,
                  plotlyOutput("temp_bar_chart", height = "300px")
                )
              ),
              
              fluidRow(
                box(
                  title = "Distribusi Temperatur", status = "warning", solidHeader = TRUE, width = 6,
                  plotlyOutput("temp_pie_chart", height = "300px")
                ),
                box(
                  title = "Peta Temperatur", status = "warning", solidHeader = TRUE, width = 6,
                  leafletOutput("temp_map", height = "300px")
                )
              )
      ),
      
      # Tab Analisis Korelasi
      tabItem(tabName = "korelasi",
              fluidRow(
                box(
                  title = "Analisis Korelasi Iklim vs Produktivitas Padi", 
                  status = "primary", solidHeader = TRUE, width = 12,
                  column(3,
                         selectInput("korel_var", "Pilih Variabel Iklim:",
                                     choices = c("Temperatur" = "temperatur", "Curah Hujan" = "curah_hujan"),
                                     selected = "temperatur")
                  ),
                  column(3,
                         selectInput("korel_tahun", "Pilih Tahun:",
                                     choices = years, 
                                     selected = if(length(years) > 0) max(years) else NULL)
                  ),
                  column(3,
                         selectInput("korel_kabkota", "Pilih Kabupaten/Kota:",
                                     choices = c("Semua" = "all", if(!is.null(data_korelasi)) sort(unique(data_korelasi$kabkota)) else c()),
                                     selected = "all")
                  ),
                  column(3,
                         actionButton("hitung_korelasi", "Hitung Korelasi", 
                                      class = "btn-primary", style = "margin-top: 25px; width: 100%;")
                  )
                )
              ),
              
              fluidRow(
                box(
                  title = "Peta Bivariat: Iklim vs Produktivitas", status = "danger", solidHeader = TRUE, width = 8,
                  # Output untuk peta Leaflet
                  leafletOutput("bivariate_map", height = "400px")
                ),
                box(
                  title = "Legenda Bivariat", status = "danger", solidHeader = TRUE, width = 4,
                  # Output untuk legenda yang dibuat terpisah
                  plotOutput("bivariate_legend", height = "400px")
                )
              ),
              
              fluidRow(
                box(
                  title = "Scatter Plot: Iklim vs Produktivitas", status = "danger", solidHeader = TRUE, width = 8,
                  plotlyOutput("korelasi_scatter", height = "400px")
                ),
                box(
                  title = "Hasil Analisis Statistik", status = "danger", solidHeader = TRUE, width = 4,
                  verbatimTextOutput("korelasi_hasil")
                )
              ),
              
              fluidRow(
                box(
                  title = "Tabel Data Korelasi", status = "danger", solidHeader = TRUE, width = 12,
                  DT::dataTableOutput("korelasi_table")
                )
              )
      ),
      
      # Tab Data Info - UPDATED WITHOUT IMPORT FUNCTIONALITY
      tabItem(tabName = "info",
              
              # Export Data Section
              fluidRow(
                box(
                  title = "Export Data", status = "success", solidHeader = TRUE, width = 12,
                  h4("Download Processed Data:"),
                  p("Download data yang telah diproses dalam format CSV untuk analisis lebih lanjut."),
                  br(),
                  fluidRow(
                    column(3,
                           downloadButton("download_produktivitas", "Download Produktivitas", 
                                          class = "btn-success", style = "width: 100%; margin-bottom: 10px;")
                    ),
                    column(3,
                           downloadButton("download_temperatur", "Download Temperatur", 
                                          class = "btn-warning", style = "width: 100%; margin-bottom: 10px;")
                    ),
                    column(3,
                           downloadButton("download_curah_hujan", "Download Curah Hujan", 
                                          class = "btn-info", style = "width: 100%; margin-bottom: 10px;")
                    ),
                    column(3,
                           downloadButton("download_all_data", "Download All Data (Combined)", 
                                          class = "btn-primary", style = "width: 100%; margin-bottom: 10px;")
                    )
                  ),
                  br(),
                  h5("Format Export:"),
                  p("• CSV format dengan separator koma", br(),
                    "• Long format (kabkota, tahun, nilai)", br(),
                    "• Encoding UTF-8", br(),
                    "• Header included",
                    style = "font-size: 12px; color: #666;")
                )
              ),
              
              fluidRow(
                box(
                  title = "Preview Data Produktivitas", status = "info", solidHeader = TRUE, width = 4,
                  DT::dataTableOutput("preview_produktivitas")
                ),
                box(
                  title = "Preview Data Temperatur", status = "warning", solidHeader = TRUE, width = 4,
                  DT::dataTableOutput("preview_temperatur")
                ),
                box(
                  title = "Preview Data Curah Hujan", status = "success", solidHeader = TRUE, width = 4,
                  DT::dataTableOutput("preview_curah_hujan")
                )
              )
      ),
      # Tab User Guide
      tabItem(tabName = "user_guide",
              h2("Panduan Penggunaan Dashboard"),
              hr(),
              
              fluidRow(
                box(
                  title = "Video Tutorial",
                  status = "danger", solidHeader = TRUE, width = 12,
                  p("Berikut adalah video tutorial singkat mengenai cara menggunakan dashboard ini."),
                  a(href = "https://youtu.be/UgNTweJlRDM",
                    target = "_blank",
                    class = "btn btn-primary btn-lg",
                    style = "width:100%; font-size:18px;background-color:#c4302b; border-color:#c4302b; color:white;",
                    icon("youtube"),
                    "Buka Video Tutorial di Tab Baru"
                  )
                )
              ),
              
              fluidRow(
                box(
                  title = "Dokumen Panduan (PDF)",
                  status = "success", solidHeader = TRUE, width = 12,
                  p("Untuk panduan yang lebih detail, silakan unduh file PDF di bawah ini."),
                  downloadButton("download_pdf", "Unduh Panduan Pengguna (PDF)", 
                                 class = "btn-lg btn-success", style="width:100%; font-size:18px;")
                )
              ),
              
              fluidRow(
                box(
                  title = "Akses Kode Sumber (GitHub)",
                  status = "primary", solidHeader = TRUE, width = 12,
                  p("Untuk melihat kode sumber dan dokumentasi proyek ini lebih lanjut, silakan kunjungi repository GitHub berikut:"),
                  a(href = "https://github.com/muhammadludviarg/Dashboard-Analisis-Iklim-dan-Produktivitas-Padi-Jawa-Timur-2019-2024", target = "_blank",
                    class = "btn btn-info btn-lg", 
                    style="width:100%; font-size:18px;background-color:#367fa9; border-color:#367fa9; color:white",
                    icon("github"), "Kunjungi GitHub Repository")
                )
              )
      ),
      
      # Tab Profil Tim
      tabItem(tabName = "profil_tim",
              h2("Profil Tim Pengembang"),
              hr(),
              
              fluidRow(
                
                # --- Anggota 1 ---
                column(width = 4,
                       box(
                         title = "Amanda Tri Hapsari", 
                         status = "primary", solidHeader = TRUE, width = NULL,
                         style = "text-align: center;",
                         tags$img(src = "REFERENSI/Amanda.JPG", 
                                  width = "250px", height="250px",
                                  style = "border-radius: 50%; object-fit: cover; border: 3px solid #f4f4f4; margin-bottom: 10px;"),
                         br(),
                         strong("NIM:"), "222312966"
                       )
                ),
                
                # --- Anggota 2 ---
                column(width = 4,
                       box(
                         title = "Faizal Eka Setiawan", 
                         status = "primary", solidHeader = TRUE, width = NULL,
                         style = "text-align: center;",
                         tags$img(src = "REFERENSI/Faizal.jpg", 
                                  width = "250px", height="250px",
                                  style = "border-radius: 50%; object-fit: cover; border: 3px solid #f4f4f4; margin-bottom: 10px;"),
                         br(),
                         strong("NIM:"), "222313073"
                       )
                ),
                
                # --- Anggota 3 ---
                column(width = 4,
                       box(
                         title = "Muhammad Ludvi Argorahayu", 
                         status = "primary", solidHeader = TRUE, width = NULL,
                         style = "text-align: center;",
                         tags$img(src = "REFERENSI/Ludvi.jpg", 
                                  width = "250px", height="250px",
                                  style = "border-radius: 50%; object-fit: cover; border: 3px solid #f4f4f4; margin-bottom: 10px;"),
                         br(),
                         strong("NIM:"), "222313248"
                       )
                )
                
              ) # Akhir dari fluidRow
      )
    )
  )
)


# =============================================================================
# SERVER LOGIC
# =============================================================================

server <- function(input, output, session) {
  
  # Update choices when data is loaded
  observe({
    if(!is.null(data_produktivitas)) {
      updateSelectInput(session, "prod_kabkota", choices = kabkota_produktivitas)
    }
    if(!is.null(data_temperatur)) {
      updateSelectInput(session, "hujan_kabkota", choices = c("Semua Kabupaten/Kota" = "all", kabkota_iklim))
      updateSelectInput(session, "temp_kabkota", choices = c("Semua Kabupaten/Kota" = "all", kabkota_iklim))
    }
    if(!is.null(data_korelasi)) {
      updateSelectInput(session, "korel_kabkota", choices = c("Semua" = "all", sort(unique(data_korelasi$kabkota))))
    }
    
    # Update year choices
    updateSelectInput(session, "prod_tahun", choices = years, selected = if(length(years) > 0) max(years) else NULL)
    updateSelectInput(session, "hujan_tahun", choices = years, selected = if(length(years) > 0) max(years) else NULL)
    updateSelectInput(session, "temp_tahun", choices = years, selected = if(length(years) > 0) max(years) else NULL)
    updateSelectInput(session, "korel_tahun", choices = years, selected = if(length(years) > 0) max(years) else NULL)
  })
  
  # Observer untuk menangani klik pada peta produktivitas
  observeEvent(input$prod_map_shape_click, {
    clicked_kab <- input$prod_map_shape_click$id
    
    # Update input dropdown
    updateSelectInput(session, "prod_scope", selected = "specific")
    updateSelectInput(session, "prod_kabkota", selected = clicked_kab)
  })
  
  # Logika untuk tombol download PDF
  output$download_pdf <- downloadHandler(
    filename = function() { "Panduan_Pengguna_Dashboard.pdf" },
    content = function(file) { file.copy("www/REFERENSI/User Guide Dashboard.pdf", file) },
    contentType = "application/pdf"
  )
  # =============================================================================
  # OVERVIEW TAB
  # =============================================================================
  
  output$total_kabkota <- renderValueBox({
    valueBox(
      value = length(kabkota_produktivitas),
      subtitle = "Total Kabupaten/Kota",
      icon = icon("map"),
      color = "blue"
    )
  })
  
  output$avg_produktivitas <- renderValueBox({
    if(!is.null(data_produktivitas)) {
      avg_prod <- round(mean(data_produktivitas$produktivitas, na.rm = TRUE), 2)
      valueBox(
        value = paste(avg_prod, "ku/ha"),
        subtitle = "Rata-rata Produktivitas",
        icon = icon("seedling"),
        color = "green"
      )
    } else {
      valueBox(
        value = "N/A",
        subtitle = "Rata-rata Produktivitas",
        icon = icon("seedling"),
        color = "red"
      )
    }
  })
  
  output$avg_temperatur <- renderValueBox({
    if(!is.null(data_temperatur)) {
      avg_temp <- round(mean(data_temperatur$temperatur, na.rm = TRUE), 2)
      valueBox(
        value = paste(avg_temp, "°C"),
        subtitle = "Rata-rata Temperatur",
        icon = icon("thermometer-half"),
        color = "orange"
      )
    } else {
      valueBox(
        value = "N/A",
        subtitle = "Rata-rata Temperatur",
        icon = icon("thermometer-half"),
        color = "red"
      )
    }
  })
  
  output$avg_curah_hujan <- renderValueBox({
    if(!is.null(data_curah_hujan)) {
      avg_hujan <- round(mean(data_curah_hujan$curah_hujan, na.rm = TRUE), 2)
      valueBox(
        value = paste(avg_hujan, "mm"),
        subtitle = "Rata-rata Curah Hujan",
        icon = icon("cloud-rain"),
        color = "light-blue"
      )
    } else {
      valueBox(
        value = "N/A",
        subtitle = "Rata-rata Curah Hujan",
        icon = icon("cloud-rain"),
        color = "red"
      )
    }
  })
  
  # Summary Statistics Tables
  output$summary_produktivitas <- renderTable({
    if(!is.null(data_produktivitas)) {
      summary_stats <- data_produktivitas %>%
        group_by(tahun) %>%
        summarise(
          `Rata-rata` = round(mean(produktivitas, na.rm = TRUE), 2),
          `Median` = round(median(produktivitas, na.rm = TRUE), 2),
          `Min` = round(min(produktivitas, na.rm = TRUE), 2),
          `Max` = round(max(produktivitas, na.rm = TRUE), 2),
          .groups = 'drop'
        ) %>%
        mutate(tahun = as.integer(tahun))
      summary_stats
    } else {
      data.frame(Message = "Data tidak tersedia")
    }
  }, striped = TRUE, hover = TRUE)
  
  output$summary_temperatur <- renderTable({
    if(!is.null(data_temperatur)) {
      summary_stats <- data_temperatur %>%
        group_by(tahun) %>%
        summarise(
          `Rata-rata` = round(mean(temperatur, na.rm = TRUE), 2),
          `Median` = round(median(temperatur, na.rm = TRUE), 2),
          `Min` = round(min(temperatur, na.rm = TRUE), 2),
          `Max` = round(max(temperatur, na.rm = TRUE), 2),
          .groups = 'drop'
        ) %>%
        mutate(tahun = as.integer(tahun))
      summary_stats
    } else {
      data.frame(Message = "Data tidak tersedia")
    }
  }, striped = TRUE, hover = TRUE)
  
  output$summary_curah_hujan <- renderTable({
    if(!is.null(data_curah_hujan)) {
      summary_stats <- data_curah_hujan %>%
        group_by(tahun) %>%
        summarise(
          `Rata-rata` = round(mean(curah_hujan, na.rm = TRUE), 2),
          `Median` = round(median(curah_hujan, na.rm = TRUE), 2),
          `Min` = round(min(curah_hujan, na.rm = TRUE), 2),
          `Max` = round(max(curah_hujan, na.rm = TRUE), 2),
          .groups = 'drop'
        ) %>%
        mutate(tahun = as.integer(tahun))
      summary_stats
    } else {
      data.frame(Message = "Data tidak tersedia")
    }
  }, striped = TRUE, hover = TRUE)
  
  # Trend Analysis
  output$trend_produktivitas <- renderText({
    if(!is.null(data_produktivitas)) {
      avg_by_year <- data_produktivitas %>%
        group_by(tahun) %>%
        summarise(avg_prod = mean(produktivitas, na.rm = TRUE), .groups = 'drop')
      
      trend_prod <- trend_analysis(avg_by_year$avg_prod)
      
      paste("Trend Produktivitas Padi:\n",
            "Arah Trend:", trend_prod$trend, "\n",
            "Signifikansi:", trend_prod$significance, "\n",
            "Tau (Kendall):", round(trend_prod$tau, 4), "\n",
            "P-value:", round(trend_prod$p_value, 4), "\n\n",
            "Interpretasi:\n",
            ifelse(trend_prod$significance == "Significant",
                   paste("Ada", tolower(trend_prod$trend), "yang signifikan"),
                   "Tidak ada trend yang signifikan"))
    } else {
      "Data produktivitas tidak tersedia"
    }
  })
  
  output$trend_temperatur <- renderText({
    if(!is.null(data_temperatur)) {
      avg_by_year <- data_temperatur %>%
        group_by(tahun) %>%
        summarise(avg_temp = mean(temperatur, na.rm = TRUE), .groups = 'drop')
      
      trend_temp <- trend_analysis(avg_by_year$avg_temp)
      
      paste("Trend Temperatur:\n",
            "Arah Trend:", trend_temp$trend, "\n",
            "Signifikansi:", trend_temp$significance, "\n",
            "Tau (Kendall):", round(trend_temp$tau, 4), "\n",
            "P-value:", round(trend_temp$p_value, 4), "\n\n",
            "Interpretasi:\n",
            ifelse(trend_temp$significance == "Significant",
                   paste("Ada", tolower(trend_temp$trend), "yang signifikan"),
                   "Tidak ada trend yang signifikan"))
    } else {
      "Data temperatur tidak tersedia"
    }
  })
  
  output$trend_curah_hujan <- renderText({
    if(!is.null(data_curah_hujan)) {
      avg_by_year <- data_curah_hujan %>%
        group_by(tahun) %>%
        summarise(avg_hujan = mean(curah_hujan, na.rm = TRUE), .groups = 'drop')
      
      trend_hujan <- trend_analysis(avg_by_year$avg_hujan)
      
      paste("Trend Curah Hujan:\n",
            "Arah Trend:", trend_hujan$trend, "\n",
            "Signifikansi:", trend_hujan$significance, "\n",
            "Tau (Kendall):", round(trend_hujan$tau, 4), "\n",
            "P-value:", round(trend_hujan$p_value, 4), "\n\n",
            "Interpretasi:\n",
            ifelse(trend_hujan$significance == "Significant",
                   paste("Ada", tolower(trend_hujan$trend), "yang signifikan"),
                   "Tidak ada trend yang signifikan"))
    } else {
      "Data curah hujan tidak tersedia"
    }
  })
  
  # Charts
  output$overview_chart_produktivitas <- renderPlotly({
    if(!is.null(data_produktivitas)) {
      summary_data <- data_produktivitas %>%
        group_by(tahun) %>%
        summarise(
          rata_rata = mean(produktivitas, na.rm = TRUE),
          q25 = quantile(produktivitas, 0.25, na.rm = TRUE),
          q75 = quantile(produktivitas, 0.75, na.rm = TRUE),
          .groups = 'drop'
        )
      
      p <- ggplot(summary_data, aes(x = tahun)) +
        geom_ribbon(aes(ymin = q25, ymax = q75), alpha = 0.3, fill = "#3c8dbc") +
        geom_line(aes(y = rata_rata), color = "#3c8dbc", size = 1.2) +
        geom_point(aes(y = rata_rata), color = "#3c8dbc", size = 2.5) +
        labs(title = "Trend Produktivitas Padi",
             x = "Tahun", y = "Produktivitas (ku/ha)") +
        theme_minimal() +
        scale_x_continuous(breaks = years)
      
      ggplotly(p, tooltip = c("x", "y"))
    } else {
      plotly_empty() %>% layout(title = "Data tidak tersedia")
    }
  })
  
  output$overview_chart_temperatur <- renderPlotly({
    if(!is.null(data_temperatur)) {
      summary_data <- data_temperatur %>%
        group_by(tahun) %>%
        summarise(
          rata_rata = mean(temperatur, na.rm = TRUE),
          q25 = quantile(temperatur, 0.25, na.rm = TRUE),
          q75 = quantile(temperatur, 0.75, na.rm = TRUE),
          .groups = 'drop'
        )
      
      p <- ggplot(summary_data, aes(x = tahun)) +
        geom_ribbon(aes(ymin = q25, ymax = q75), alpha = 0.3, fill = "#f39c12") +
        geom_line(aes(y = rata_rata), color = "#f39c12", size = 1.2) +
        geom_point(aes(y = rata_rata), color = "#f39c12", size = 2.5) +
        labs(title = "Trend Temperatur",
             x = "Tahun", y = "Temperatur (°C)") +
        theme_minimal() +
        scale_x_continuous(breaks = years)
      
      ggplotly(p, tooltip = c("x", "y"))
    } else {
      plotly_empty() %>% layout(title = "Data tidak tersedia")
    }
  })
  
  output$overview_chart_curah_hujan <- renderPlotly({
    if(!is.null(data_curah_hujan)) {
      summary_data <- data_curah_hujan %>%
        group_by(tahun) %>%
        summarise(
          rata_rata = mean(curah_hujan, na.rm = TRUE),
          q25 = quantile(curah_hujan, 0.25, na.rm = TRUE),
          q75 = quantile(curah_hujan, 0.75, na.rm = TRUE),
          .groups = 'drop'
        )
      
      p <- ggplot(summary_data, aes(x = tahun)) +
        geom_ribbon(aes(ymin = q25, ymax = q75), alpha = 0.3, fill = "#00a65a") +
        geom_line(aes(y = rata_rata), color = "#00a65a", size = 1.2) +
        geom_point(aes(y = rata_rata), color = "#00a65a", size = 2.5) +
        labs(title = "Trend Curah Hujan",
             x = "Tahun", y = "Curah Hujan (mm)") +
        theme_minimal() +
        scale_x_continuous(breaks = years)
      
      ggplotly(p, tooltip = c("x", "y"))
    } else {
      plotly_empty() %>% layout(title = "Data tidak tersedia")
    }
  })
  
  # Combined Chart
  output$overview_combined_chart <- renderPlotly({
    if(!is.null(data_produktivitas) && !is.null(data_temperatur) && !is.null(data_curah_hujan)) {
      # Normalize data untuk perbandingan
      prod_summary <- data_produktivitas %>%
        group_by(tahun) %>%
        summarise(value = mean(produktivitas, na.rm = TRUE), .groups = 'drop') %>%
        mutate(variable = "Produktivitas Padi",
               normalized = scale(value)[,1])
      
      temp_summary <- data_temperatur %>%
        group_by(tahun) %>%
        summarise(value = mean(temperatur, na.rm = TRUE), .groups = 'drop') %>%
        mutate(variable = "Temperatur",
               normalized = scale(value)[,1])
      
      hujan_summary <- data_curah_hujan %>%
        group_by(tahun) %>%
        summarise(value = mean(curah_hujan, na.rm = TRUE), .groups = 'drop') %>%
        mutate(variable = "Curah Hujan",
               normalized = scale(value)[,1])
      
      combined_data <- bind_rows(prod_summary, temp_summary, hujan_summary)
      
      p <- ggplot(combined_data, aes(x = tahun, y = normalized, color = variable)) +
        geom_line(size = 1.2) +
        geom_point(size = 3) +
        scale_color_manual(values = c("Produktivitas Padi" = "#3c8dbc", 
                                      "Temperatur" = "#f39c12", 
                                      "Curah Hujan" = "#00a65a")) +
        labs(title = "Perbandingan Trend Semua Variabel (Normalized)",
             x = "Tahun", y = "Nilai Normalized", color = "Variabel") +
        theme_minimal() +
        scale_x_continuous(breaks = years) +
        theme(legend.position = "bottom")
      
      ggplotly(p)
    } else {
      plotly_empty() %>% layout(title = "Data tidak lengkap untuk perbandingan")
    }
  })
  
  # Correlation Plot
  output$overview_correlation_plot <- renderPlot({
    if(!is.null(data_korelasi)) {
      cor_data <- data_korelasi %>%
        select(produktivitas, temperatur, curah_hujan) %>%
        filter(complete.cases(.))
      
      if(nrow(cor_data) > 0) {
        cor_matrix <- cor(cor_data, use = "complete.obs")
        
        corrplot(cor_matrix, 
                 method = "color", 
                 type = "upper",
                 order = "hclust",
                 tl.cex = 0.8,
                 tl.col = "black",
                 addCoef.col = "black",
                 number.cex = 0.8,
                 title = "Korelasi Antar Variabel",
                 mar = c(0,0,2,0))
      } else {
        plot.new()
        text(0.5, 0.5, "Data tidak cukup untuk analisis korelasi", 
             cex = 1.2, col = "red")
      }
    } else {
      plot.new()
      text(0.5, 0.5, "Data korelasi tidak tersedia", 
           cex = 1.2, col = "red")
    }
  })
  
  # =============================================================================
  # PRODUKTIVITAS PADI TAB
  # =============================================================================
  
  output$prod_line_chart <- renderPlotly({
    if(!is.null(data_produktivitas)) {
      if(input$prod_scope == "all") {
        data_summary <- data_produktivitas %>%
          group_by(tahun) %>%
          summarise(
            rata_rata = mean(produktivitas, na.rm = TRUE),
            median = median(produktivitas, na.rm = TRUE),
            .groups = 'drop'
          )
        
        p <- ggplot(data_summary, aes(x = tahun)) +
          geom_line(aes(y = rata_rata, color = "Rata-rata"), size = 1.2) +
          geom_point(aes(y = rata_rata, color = "Rata-rata"), size = 3) +
          geom_line(aes(y = median, color = "Median"), size = 1.2) +
          geom_point(aes(y = median, color = "Median"), size = 3) +
          scale_color_manual(values = c("Rata-rata" = "#3c8dbc", "Median" = "#00a65a")) +
          labs(title = "Trend Produktivitas Padi Jawa Timur",
               x = "Tahun", y = "Produktivitas (ku/ha)", color = "Statistik") +
          theme_minimal() +
          scale_x_continuous(breaks = years)
      } else {
        data_filtered <- data_produktivitas %>% 
          filter(kabkota == input$prod_kabkota)
        
        p <- ggplot(data_filtered, aes(x = tahun, y = produktivitas)) +
          geom_line(color = "#3c8dbc", size = 1.2) +
          geom_point(color = "#3c8dbc", size = 3) +
          geom_smooth(method = "lm", se = TRUE, color = "red", alpha = 0.3) +
          labs(title = paste("Trend Produktivitas Padi -", input$prod_kabkota),
               x = "Tahun", y = "Produktivitas (ku/ha)") +
          theme_minimal() +
          scale_x_continuous(breaks = years)
      }
      
      ggplotly(p)
    } else {
      plotly_empty() %>% layout(title = "Data tidak tersedia")
    }
  })
  
  output$prod_bar_chart <- renderPlotly({
    if(!is.null(data_produktivitas)) {
      if(input$prod_scope == "all") {
        data_filtered <- data_produktivitas %>% 
          filter(tahun == input$prod_tahun) %>%
          arrange(desc(produktivitas)) %>%
          head(15)
        
        p <- ggplot(data_filtered, aes(x = reorder(kabkota, produktivitas), y = produktivitas)) +
          geom_col(fill = "#3c8dbc", alpha = 0.8) +
          coord_flip() +
          labs(title = paste("Top 15 Produktivitas Padi Tahun", input$prod_tahun),
               x = "Kabupaten/Kota", y = "Produktivitas (ku/ha)") +
          theme_minimal() +
          theme(axis.text.y = element_text(size = 8))
      } else {
        data_filtered <- data_produktivitas %>% 
          filter(kabkota == input$prod_kabkota)
        
        p <- ggplot(data_filtered, aes(x = factor(tahun), y = produktivitas)) +
          geom_col(fill = "#3c8dbc", alpha = 0.8) +
          labs(title = paste("Produktivitas Padi per Tahun -", input$prod_kabkota),
               x = "Tahun", y = "Produktivitas (ku/ha)") +
          theme_minimal()
      }
      
      ggplotly(p)
    } else {
      plotly_empty() %>% layout(title = "Data tidak tersedia")
    }
  })
  
  output$prod_pie_chart <- renderPlotly({
    if(!is.null(data_produktivitas)) {
      if(input$prod_scope == "all") {
        data_filtered <- data_produktivitas %>% 
          filter(tahun == input$prod_tahun) %>%
          mutate(kategori = case_when(
            produktivitas >= 60 ~ "Tinggi (≥60)",
            produktivitas >= 50 ~ "Sedang (50-59)",
            TRUE ~ "Rendah (<50)"
          )) %>%
          count(kategori)
      } else {
        data_filtered <- data_produktivitas %>% 
          filter(kabkota == input$prod_kabkota) %>%
          mutate(kategori = case_when(
            produktivitas >= 60 ~ "Tinggi (≥60)",
            produktivitas >= 50 ~ "Sedang (50-59)",
            TRUE ~ "Rendah (<50)"
          )) %>%
          count(kategori)
      }
      
      plot_ly(data_filtered, labels = ~kategori, values = ~n, type = 'pie',
              textposition = 'inside', textinfo = 'label+percent',
              marker = list(colors = c("#4575b4", "#fee08b", "#d73027"))) %>%
        layout(title = "Distribusi Kategori Produktivitas")
    } else {
      plotly_empty() %>% layout(title = "Data tidak tersedia")
    }
  })
  
  output$prod_map <- renderLeaflet({
    req(data_produktivitas, shp_data_peta, input$prod_scope)
    
    # Logika untuk peta "Seluruh Jawa Timur"
    if (input$prod_scope == "all") {
      req(input$prod_tahun)
      peta_data_gabungan <- shp_data_peta %>%
        left_join(
          data_produktivitas %>% filter(tahun == input$prod_tahun),
          by = c("kabkota_join" = "kabkota")
        ) %>%
        ungroup()
      
      pal <- colorNumeric(palette = "YlGn", domain = peta_data_gabungan$produktivitas, na.color = "white")
      
      leaflet(data = peta_data_gabungan) %>%
        setView(lng = 112.5, lat = -7.5, zoom = 8) %>%
        addPolygons(
          fillColor = ~pal(produktivitas),
          weight = 1, color = "black", fillOpacity = 1,
          highlightOptions = highlightOptions(weight = 3, color = "red", bringToFront = TRUE),
          
          # <<< PERUBAHAN 1: Label saat hover HANYA NAMA KABUPATEN >>>
          label = ~nmkab,
          
          # <<< PERUBAHAN 2: Menambahkan layerId agar bisa di-klik >>>
          layerId = ~kabkota_join
          
        ) %>%
        addLegend(pal = pal, values = ~produktivitas, title = "Produktivitas (ku/ha)", position = "bottomright", na.label = "Tidak Tersedia")
      
    } else {
      # Logika untuk highlight satu wilayah
      req(input$prod_kabkota)
      selected_shp <- shp_data_peta %>% filter(kabkota_join == input$prod_kabkota)
      
      prod_data_kab <- data_produktivitas %>% 
        filter(kabkota == input$prod_kabkota) %>%
        summarise(Rata2 = round(mean(produktivitas, na.rm = TRUE), 2))
      
      label_text <- paste0("<strong>", selected_shp$nmkab, "</strong><br/>Rata-rata Produktivitas: ", prod_data_kab$Rata2, " ku/ha")
      
      leaflet() %>%
        addPolygons(data = shp_data_peta, fillColor = "#EAEAEA", fillOpacity = 1, weight = 0.5, color = "white") %>%
        addPolygons(
          data = selected_shp, 
          fillColor = "#ffc107", 
          weight = 2, 
          color = "red", 
          # <<< PERUBAHAN 3: Diubah menjadi label (hover) agar konsisten >>>
          label = HTML(label_text),
          labelOptions = labelOptions(noHide = T, textOnly = FALSE, direction = 'auto'),
          fillOpacity = 1
        ) %>%
        setView(lng = st_coordinates(st_centroid(selected_shp$geometry))[1], 
                lat = st_coordinates(st_centroid(selected_shp$geometry))[2], 
                zoom = 9)
    }
  })
  
  
  # 1. Membuat data reaktif untuk peta bivariat
  #    Kode ini akan berjalan setiap kali input tahun atau variabel iklim berubah.
  bivariate_data <- reactive({
    # Membutuhkan input dan data yang relevan
    req(input$korel_tahun, input$korel_var, data_korelasi, shp_data_peta)
    
    # Filter data korelasi (yang hanya berisi 9 kab/kota) sesuai tahun
    data_filtered <- data_korelasi %>%
      filter(tahun == input$korel_tahun)
    
    # Hentikan jika data tidak cukup
    if (nrow(data_filtered) < 3) return(NULL)
    
    # Pilih variabel iklim berdasarkan input
    if (input$korel_var == "temperatur") {
      data_biv <- data_filtered %>% select(kabkota, produktivitas, var_iklim = temperatur)
      attr(data_biv, "var_label") <- "Temperatur"
    } else {
      data_biv <- data_filtered %>% select(kabkota, produktivitas, var_iklim = curah_hujan)
      attr(data_biv, "var_label") <- "Curah Hujan"
    }
    
    # 2. Membuat kelas bivariat (3x3)
    #    Membagi setiap variabel menjadi 3 kelompok (Low, Medium, High)
    #    dan membuat kolom 'bi_class' (misal: "1-1", "3-2", dst)
    data_biv_class <- bi_class(data_biv, x = var_iklim, y = produktivitas, style = "quantile", dim = 3)
    
    # 3. Gabungkan data bivariat dengan data geospasial (SHP)
    map_data <- shp_data_peta %>%
      left_join(data_biv_class, by = c("kabkota_join" = "kabkota"))
    
    return(map_data)
  })
  
  # 4. Membuat output untuk peta Leaflet
  output$bivariate_map <- renderLeaflet({
    map_data <- bivariate_data()
    req(map_data)
    
    biv_palette <- bi_pal(pal = "DkViolet", dim = 3, preview = FALSE)
    pal <- colorFactor(palette = biv_palette, domain = map_data$bi_class)
    
    # Pisahkan data NA dan yang punya nilai
    map_na <- map_data %>% filter(is.na(bi_class))
    map_value <- map_data %>% filter(!is.na(bi_class))
    
    var_label <- attr(map_data, "var_label")
    popup_content <- paste0(
      "<strong>", map_value$nmkab, "</strong><br/>",
      "Produktivitas: ", round(map_value$produktivitas, 2), " ku/ha<br/>",
      var_label, ": ", round(map_value$var_iklim, 2)
    )
    
    leaflet() %>%
      setView(lng = 112.5, lat = -7.5, zoom = 8) %>%
      # LAPISAN 1: Wilayah tanpa data
      addPolygons(
        data = map_na,
        fillColor = "white",
        fillOpacity = 1,
        weight = 1,
        color = "black",
        label = ~nmkab
      ) %>%
      # LAPISAN 2: Wilayah dengan data bivariat
      addPolygons(
        data = map_value,
        fillColor = ~pal(bi_class),
        weight = 1,
        opacity = 1,
        color = "white",
        fillOpacity = 1,
        label = ~lapply(popup_content, HTML)
      )
  })
  
  # 5. Membuat output untuk legenda bivariat
  output$bivariate_legend <- renderPlot({
    map_data <- bivariate_data()
    req(map_data)
    
    # Ambil nama variabel iklim yang dipilih
    var_label <- attr(map_data, "var_label")
    
    # Buat legenda menggunakan fungsi dari paket 'biscale'
    legend <- bi_legend(pal = "DkViolet",
                        dim = 3,
                        xlab = paste(">", var_label, "Tinggi"),
                        ylab = "> Produktivitas Tinggi") +
      theme(
        panel.background = element_rect(fill = "transparent", colour = NA),
        plot.background = element_rect(fill = "transparent", colour = NA)
      )
    
    # Tampilkan legenda
    legend
  }, bg = "transparent")
  
  # =============================================================================
  # CURAH HUJAN TAB
  # =============================================================================
  
  output$hujan_line_chart <- renderPlotly({
    if(!is.null(data_curah_hujan)) {
      if(input$hujan_kabkota == "all") {
        # Show average trend for all kabkota
        data_summary <- data_curah_hujan %>%
          group_by(tahun) %>%
          summarise(
            rata_rata = mean(curah_hujan, na.rm = TRUE),
            median = median(curah_hujan, na.rm = TRUE),
            .groups = 'drop'
          )
        
        p <- ggplot(data_summary, aes(x = tahun)) +
          geom_line(aes(y = rata_rata, color = "Rata-rata"), size = 1.2) +
          geom_point(aes(y = rata_rata, color = "Rata-rata"), size = 3) +
          geom_line(aes(y = median, color = "Median"), size = 1.2) +
          geom_point(aes(y = median, color = "Median"), size = 3) +
          scale_color_manual(values = c("Rata-rata" = "#00a65a", "Median" = "#3c8dbc")) +
          labs(title = "Trend Curah Hujan Jawa Timur",
               x = "Tahun", y = "Curah Hujan (mm)", color = "Statistik") +
          theme_minimal() +
          scale_x_continuous(breaks = years)
      } else {
        data_filtered <- data_curah_hujan %>% 
          filter(kabkota == input$hujan_kabkota)
        
        p <- ggplot(data_filtered, aes(x = tahun, y = curah_hujan)) +
          geom_line(color = "#00a65a", size = 1.2) +
          geom_point(color = "#00a65a", size = 3) +
          geom_smooth(method = "lm", se = TRUE, color = "red", alpha = 0.3) +
          labs(title = paste("Trend Curah Hujan -", input$hujan_kabkota),
               x = "Tahun", y = "Curah Hujan (mm)") +
          theme_minimal() +
          scale_x_continuous(breaks = years)
      }
      
      ggplotly(p)
    } else {
      plotly_empty() %>% layout(title = "Data tidak tersedia")
    }
  })
  
  output$hujan_bar_chart <- renderPlotly({
    if(!is.null(data_curah_hujan)) {
      data_filtered <- data_curah_hujan %>% 
        filter(tahun == input$hujan_tahun) %>%
        arrange(desc(curah_hujan))
      
      p <- ggplot(data_filtered, aes(x = reorder(kabkota, curah_hujan), y = curah_hujan)) +
        geom_col(fill = "#00a65a", alpha = 0.8) +
        coord_flip() +
        labs(title = paste("Curah Hujan Tahun", input$hujan_tahun),
             x = "Kabupaten/Kota", y = "Curah Hujan (mm)") +
        theme_minimal() +
        theme(axis.text.y = element_text(size = 8))
      
      ggplotly(p)
    } else {
      plotly_empty() %>% layout(title = "Data tidak tersedia")
    }
  })
  
  output$hujan_pie_chart <- renderPlotly({
    if(!is.null(data_curah_hujan)) {
      data_filtered <- data_curah_hujan %>% 
        filter(tahun == input$hujan_tahun) %>%
        mutate(kategori = case_when(
          curah_hujan >= 10 ~ "Tinggi (≥10mm)",
          curah_hujan >= 5 ~ "Sedang (5-9mm)",
          TRUE ~ "Rendah (<5mm)"
        )) %>%
        count(kategori)
      
      plot_ly(data_filtered, labels = ~kategori, values = ~n, type = 'pie',
              textposition = 'inside', textinfo = 'label+percent',
              marker = list(colors = c("#2166ac", "#5aae61", "#d73027"))) %>%
        layout(title = paste("Distribusi Kategori Curah Hujan Tahun", input$hujan_tahun))
    } else {
      plotly_empty() %>% layout(title = "Data tidak tersedia")
    }
  })
  
  output$hujan_map <- renderLeaflet({
    req(data_curah_hujan, shp_data_peta)
    
    # Logika baru untuk merespon pilihan "Semua" atau spesifik
    if (input$hujan_kabkota == "all") {
      req(input$hujan_tahun)
      data_tahunan <- data_curah_hujan %>% filter(tahun == input$hujan_tahun)
      peta_data_gabungan <- shp_data_peta %>% left_join(data_tahunan, by = c("kabkota_join" = "kabkota"))
      
      pal <- colorNumeric(palette = "YlGnBu", domain = peta_data_gabungan$curah_hujan)
      
      peta_na <- peta_data_gabungan %>% filter(is.na(curah_hujan))
      peta_value <- peta_data_gabungan %>% filter(!is.na(curah_hujan))
      
      popup_content <- paste0("<strong>", peta_value$nmkab, "</strong><br/>Curah Hujan: ", round(peta_value$curah_hujan, 2), " mm")
      
      leaflet() %>%
        setView(112.5, -7.5, 8) %>%
        addPolygons(data = peta_na, fillColor = "white", fillOpacity = 1, weight = 1, color = "black", label = ~nmkab) %>%
        addPolygons(
          data = peta_value, fillColor = ~pal(curah_hujan), weight = 1, color = "white", fillOpacity = 1,
          label = ~lapply(popup_content, HTML)
        ) %>%
        addLegend(pal = pal, values = peta_value$curah_hujan, opacity = 1, title = "Curah Hujan (mm)", position = "bottomright")
      
    } else {
      # Logika untuk highlight satu wilayah
      req(input$hujan_kabkota)
      selected_shp <- shp_data_peta %>% filter(kabkota_join == input$hujan_kabkota)
      
      hujan_data_kab <- data_curah_hujan %>% 
        filter(kabkota == input$hujan_kabkota) %>%
        summarise(Rata2 = round(mean(curah_hujan, na.rm = TRUE), 2))
      
      label_text <- paste0("<strong>", selected_shp$nmkab, "</strong><br/>Rata-rata Curah Hujan: ", hujan_data_kab$Rata2, " mm")
      
      leaflet() %>%
        addPolygons(data = shp_data_peta, fillColor = "#EAEAEA", fillOpacity = 1, weight = 0.5, color = "white") %>%
        addPolygons(
          data = selected_shp, 
          fillColor = "#28a745", 
          weight = 2, 
          color = "red", 
          
          # <<< INI PERBAIKANNYA: Mengganti 'popup' menjadi 'label' >>>
          label = HTML(label_text),
          
          # Menambahkan labelOptions agar label selalu terlihat dan tidak hilang saat mouse digeser sedikit
          labelOptions = labelOptions(noHide = TRUE, textOnly = FALSE, direction = 'auto'),
          fillOpacity = 1
        ) %>%
        setView(lng = st_coordinates(st_centroid(selected_shp$geometry))[1], 
                lat = st_coordinates(st_centroid(selected_shp$geometry))[2], 
                zoom = 9)
    }
  })
  
  # =============================================================================
  # TEMPERATUR TAB
  # =============================================================================
  
  output$temp_line_chart <- renderPlotly({
    if(!is.null(data_temperatur)) {
      if(input$temp_kabkota == "all") {
        # Show average trend for all kabkota
        data_summary <- data_temperatur %>%
          group_by(tahun) %>%
          summarise(
            rata_rata = mean(temperatur, na.rm = TRUE),
            median = median(temperatur, na.rm = TRUE),
            .groups = 'drop'
          )
        
        p <- ggplot(data_summary, aes(x = tahun)) +
          geom_line(aes(y = rata_rata, color = "Rata-rata"), size = 1.2) +
          geom_point(aes(y = rata_rata, color = "Rata-rata"), size = 3) +
          geom_line(aes(y = median, color = "Median"), size = 1.2) +
          geom_point(aes(y = median, color = "Median"), size = 3) +
          scale_color_manual(values = c("Rata-rata" = "#f39c12", "Median" = "#3c8dbc")) +
          labs(title = "Trend Temperatur Jawa Timur",
               x = "Tahun", y = "Temperatur (°C)", color = "Statistik") +
          theme_minimal() +
          scale_x_continuous(breaks = years)
      } else {
        data_filtered <- data_temperatur %>% 
          filter(kabkota == input$temp_kabkota)
        
        p <- ggplot(data_filtered, aes(x = tahun, y = temperatur)) +
          geom_line(color = "#f39c12", size = 1.2) +
          geom_point(color = "#f39c12", size = 3) +
          geom_smooth(method = "lm", se = TRUE, color = "red", alpha = 0.3) +
          labs(title = paste("Trend Temperatur -", input$temp_kabkota),
               x = "Tahun", y = "Temperatur (°C)") +
          theme_minimal() +
          scale_x_continuous(breaks = years)
      }
      
      ggplotly(p)
    } else {
      plotly_empty() %>% layout(title = "Data tidak tersedia")
    }
  })
  
  output$temp_bar_chart <- renderPlotly({
    if(!is.null(data_temperatur)) {
      data_filtered <- data_temperatur %>% 
        filter(tahun == input$temp_tahun) %>%
        arrange(desc(temperatur))
      
      p <- ggplot(data_filtered, aes(x = reorder(kabkota, temperatur), y = temperatur)) +
        geom_col(fill = "#f39c12", alpha = 0.8) +
        coord_flip() +
        labs(title = paste("Temperatur Tahun", input$temp_tahun),
             x = "Kabupaten/Kota", y = "Temperatur (°C)") +
        theme_minimal() +
        theme(axis.text.y = element_text(size = 8))
      
      ggplotly(p)
    } else {
      plotly_empty() %>% layout(title = "Data tidak tersedia")
    }
  })
  
  output$temp_pie_chart <- renderPlotly({
    if(!is.null(data_temperatur)) {
      data_filtered <- data_temperatur %>% 
        filter(tahun == input$temp_tahun) %>%
        mutate(kategori = case_when(
          temperatur >= 28 ~ "Panas (≥28°C)",
          temperatur >= 25 ~ "Sedang (25-27°C)",
          TRUE ~ "Sejuk (<25°C)"
        )) %>%
        count(kategori)
      
      plot_ly(data_filtered, labels = ~kategori, values = ~n, type = 'pie',
              textposition = 'inside', textinfo = 'label+percent',
              marker = list(colors = c("#d73027", "#fee08b", "#4575b4"))) %>%
        layout(title = paste("Distribusi Kategori Temperatur Tahun", input$temp_tahun))
    } else {
      plotly_empty() %>% layout(title = "Data tidak tersedia")
    }
  })
  
  output$temp_map <- renderLeaflet({
    req(data_temperatur, shp_data_peta)
    
    if (input$temp_kabkota == "all") {
      req(input$temp_tahun)
      data_tahunan <- data_temperatur %>% filter(tahun == input$temp_tahun)
      peta_data_gabungan <- shp_data_peta %>% left_join(data_tahunan, by = c("kabkota_join" = "kabkota"))
      
      pal <- colorNumeric(palette = "YlOrRd", domain = peta_data_gabungan$temperatur)
      
      peta_na <- peta_data_gabungan %>% filter(is.na(temperatur))
      peta_value <- peta_data_gabungan %>% filter(!is.na(temperatur))
      
      popup_content <- paste0("<strong>", peta_value$nmkab, "</strong><br/>Suhu: ", round(peta_value$temperatur, 2), " °C")
      
      leaflet() %>%
        setView(112.5, -7.5, 8) %>%
        addPolygons(data = peta_na, fillColor = "white", fillOpacity = 1, weight = 1, color = "black", label = ~nmkab) %>%
        addPolygons(
          data = peta_value, fillColor = ~pal(temperatur), weight = 1, color = "white", fillOpacity = 1,
          label = ~lapply(popup_content, HTML)
        ) %>%
        addLegend(pal = pal, values = peta_value$temperatur, opacity = 1, title = "Suhu (°C)", position = "bottomright")
      
    } else {
      # Logika untuk highlight satu wilayah
      req(input$temp_kabkota)
      selected_shp <- shp_data_peta %>% filter(kabkota_join == input$temp_kabkota)
      
      temp_data_kab <- data_temperatur %>% 
        filter(kabkota == input$temp_kabkota) %>%
        summarise(Rata2 = round(mean(temperatur, na.rm = TRUE), 2))
      
      label_text <- paste0("<strong>", selected_shp$nmkab, "</strong><br/>Rata-rata Suhu: ", temp_data_kab$Rata2, " °C")
      
      leaflet() %>%
        addPolygons(data = shp_data_peta, fillColor = "#EAEAEA", fillOpacity = 1, weight = 0.5, color = "white") %>%
        addPolygons(
          data = selected_shp, 
          fillColor = "#fd7e14", 
          weight = 2, 
          color = "red", 
          
          # <<< INI PERBAIKANNYA: Mengganti 'popup' menjadi 'label' >>>
          label = HTML(label_text),
          
          # Menambahkan labelOptions agar label selalu terlihat
          labelOptions = labelOptions(noHide = TRUE, textOnly = FALSE, direction = 'auto'),
          fillOpacity = 1
        ) %>%
        setView(lng = st_coordinates(st_centroid(selected_shp$geometry))[1], 
                lat = st_coordinates(st_centroid(selected_shp$geometry))[2], 
                zoom = 9)
    }
  })
  
  # =============================================================================
  # ANALISIS KORELASI TAB
  # =============================================================================
  
  correlation_table_data <- eventReactive(input$hitung_korelasi, {
    # Tampilkan notifikasi saat proses dimulai (opsional, tapi membantu)
    showNotification("Menghitung korelasi untuk semua kabupaten...", type = "message", duration = 3)
    
    # Panggil fungsi yang sudah ada untuk menghitung tabel
    calculate_correlation_table(input$korel_var)
  })
  
  output$korelasi_scatter <- renderPlotly({
    if(!is.null(data_korelasi)) {
      data_plot <- data_korelasi %>%
        filter(tahun == input$korel_tahun)
      
      if(input$korel_kabkota != "all") {
        data_plot <- data_plot %>% filter(kabkota == input$korel_kabkota)
      }
      
      if(nrow(data_plot) > 0) {
        if(input$korel_var == "temperatur") {
          p <- ggplot(data_plot, aes(x = temperatur, y = produktivitas, color = kabkota)) +
            geom_point(size = 3, alpha = 0.7) +
            geom_smooth(method = "lm", se = TRUE, color = "red", linetype = "dashed") +
            labs(title = paste("Korelasi Temperatur vs Produktivitas Padi - Tahun", input$korel_tahun),
                 x = "Temperatur (°C)", y = "Produktivitas (ku/ha)", color = "Kabupaten/Kota") +
            theme_minimal()
        } else {
          p <- ggplot(data_plot, aes(x = curah_hujan, y = produktivitas, color = kabkota)) +
            geom_point(size = 3, alpha = 0.7) +
            geom_smooth(method = "lm", se = TRUE, color = "red", linetype = "dashed") +
            labs(title = paste("Korelasi Curah Hujan vs Produktivitas Padi - Tahun", input$korel_tahun),
                 x = "Curah Hujan (mm)", y = "Produktivitas (ku/ha)", color = "Kabupaten/Kota") +
            theme_minimal()
        }
        
        ggplotly(p)
      } else {
        plotly_empty() %>% layout(title = "Data tidak tersedia untuk analisis")
      }
    } else {
      plotly_empty() %>% layout(title = "Data korelasi tidak tersedia")
    }
  })
  
  # Menampilkan hasil analisis statistik di kotak verbatim
  output$korelasi_hasil <- renderPrint({
    # Membutuhkan input dari filter
    req(input$korel_var, input$korel_kabkota, data_korelasi)
    
    # Filter data berdasarkan pilihan pengguna
    data_analisis <- data_korelasi %>%
      filter(
        # Jika bukan "Semua", filter berdasarkan kab/kota
        if (input$korel_kabkota != "all") kabkota == input$korel_kabkota else TRUE
      )
    
    # Hentikan jika data tidak cukup
    if (nrow(data_analisis) < 3) {
      cat("Data tidak cukup untuk analisis statistik.")
      return()
    }
    
    # Pilih variabel iklim
    variabel_x <- if (input$korel_var == "temperatur") data_analisis$temperatur else data_analisis$curah_hujan
    label_x <- if (input$korel_var == "temperatur") "Temperatur" else "Curah Hujan"
    
    # Lakukan analisis korelasi
    hasil_tes <- correlation_analysis(variabel_x, data_analisis$produktivitas)
    
    # Tampilkan hasil dengan format yang rapi
    cat(
      "=== Hasil Analisis Korelasi ===\n\n",
      "Variabel X:", label_x, "\n",
      "Variabel Y: Produktivitas Padi\n",
      "Kabupaten/Kota:", input$korel_kabkota, "\n\n",
      "----------------------------------\n",
      "Koefisien Korelasi (r):", round(hasil_tes$correlation, 4), "\n",
      "P-Value:", round(hasil_tes$p_value, 4), "\n",
      "Signifikansi:", hasil_tes$significance, "\n",
      "----------------------------------\n\n",
      "Interpretasi:\n",
      "Hubungan antara", tolower(label_x), "dan produktivitas padi di",
      input$korel_kabkota, "adalah", tolower(hasil_tes$significance),
      "dengan nilai p-value", round(hasil_tes$p_value, 4), "."
    )
  })
  
  output$korelasi_table <- DT::renderDataTable({
    corr_data <- correlation_table_data()
    
    # Jika data kosong
    if (is.null(corr_data) || nrow(corr_data) == 0) {
      return(DT::datatable(data.frame(Pesan = "Klik 'Hitung Korelasi' untuk menampilkan data."), rownames = FALSE))
    }
    
    # Tentukan nama kolom iklim secara dinamis
    iklim_col_name <- ifelse(
      input$korel_var == "temperatur", 
      "Rata-rata Temp (°C)", 
      "Rata-rata Hujan (mm)"
    )
    
    # Render datatable
    DT::datatable(
      corr_data,
      options = list(
        scrollX = TRUE,
        scrollY = "400px",
        autoWidth = TRUE,
        dom = 'frtip',
        pageLength = 10,
        columnDefs = list(
          list(width = '150px', targets = "_all")
        )
      ),
      rownames = FALSE,
      colnames = c(
        "Kabupaten/Kota",
        iklim_col_name,
        "Rata-rata Prod (ku/ha)",
        "Korelasi",
        "P-Value",
        "Kekuatan",
        "Kategori Prod",
        "Signifikansi"
      ),
      class = 'display nowrap stripe hover'
    ) %>%
      formatRound(columns = c("Rata_rata_Iklim", "Rata_rata_Produktivitas"), digits = 2) %>%
      formatRound(columns = "Korelasi", digits = 3) %>%
      formatStyle(
        "Korelasi",
        backgroundColor = styleInterval(
          cuts = c(-0.6, -0.4, -0.2, 0.2, 0.4, 0.6),
          values = c('#d7191c', '#fdae61', '#ffffbf', '#ffffff', '#ffffbf', '#a6d96a', '#1a9641')
        )
      ) %>%
      formatStyle(
        "Kekuatan_Korelasi",
        backgroundColor = styleEqual(
          c("Sangat Kuat", "Kuat", "Sedang", "Lemah", "Sangat Lemah"),
          c("#d73027", "#fc8d59", "#fee08b", "#d9d9d9", "#f7f7f7")
        )
      ) %>%
      formatStyle(
        "Signifikansi",
        backgroundColor = styleEqual("Signifikan", "#abdda4")
      )
  })
  
  
  # =============================================================================
  # DATA INFO TAB
  # =============================================================================
  
  output$data_info <- renderText({
    info_text <- paste(
      "=== INFORMASI DATASET ===\n\n",
      "1. DATA PRODUKTIVITAS PADI:\n",
      if(!is.null(data_produktivitas)) {
        paste("   - Total Records:", nrow(data_produktivitas), "\n",
              "   - Kabupaten/Kota:", length(unique(data_produktivitas$kabkota)), "\n",
              "   - Periode:", paste(range(data_produktivitas$tahun), collapse = " - "), "\n",
              "   - Range Produktivitas:", paste(round(range(data_produktivitas$produktivitas, na.rm = TRUE), 2), collapse = " - "), "ku/ha\n")
      } else {
        "   - Data tidak berhasil dimuat\n"
      },
      "\n2. DATA TEMPERATUR:\n",
      if(!is.null(data_temperatur)) {
        paste("   - Total Records:", nrow(data_temperatur), "\n",
              "   - Kabupaten/Kota:", length(unique(data_temperatur$kabkota)), "\n",
              "   - Periode:", paste(range(data_temperatur$tahun), collapse = " - "), "\n",
              "   - Range Temperatur:", paste(round(range(data_temperatur$temperatur, na.rm = TRUE), 2), collapse = " - "), "°C\n")
      } else {
        "   - Data tidak berhasil dimuat\n"
      },
      "\n3. DATA CURAH HUJAN:\n",
      if(!is.null(data_curah_hujan)) {
        paste("   - Total Records:", nrow(data_curah_hujan), "\n",
              "   - Kabupaten/Kota:", length(unique(data_curah_hujan$kabkota)), "\n",
              "   - Periode:", paste(range(data_curah_hujan$tahun), collapse = " - "), "\n",
              "   - Range Curah Hujan:", paste(round(range(data_curah_hujan$curah_hujan, na.rm = TRUE), 2), collapse = " - "), "mm\n")
      } else {
        "   - Data tidak berhasil dimuat\n"
      },
      "\n=== STATUS LOADING ===\n",
      "Produktivitas:", ifelse(!is.null(data_produktivitas), "✓ Berhasil", "✗ Gagal"), "\n",
      "Temperatur:", ifelse(!is.null(data_temperatur), "✓ Berhasil", "✗ Gagal"), "\n",
      "Curah Hujan:", ifelse(!is.null(data_curah_hujan), "✓ Berhasil", "✗ Gagal"), "\n",
      "\n=== FORMAT DATA ===\n",
      "- Format: Semicolon-separated CSV\n",
      "- Struktur: Wide format (years as columns) converted to long format\n",
      "- Kabupaten/Kota names: Standardized to uppercase\n",
      "- Data processing: Automatic detection and conversion\n"
    )
    
    return(info_text)
  })
  
  # =============================================================================
  # EXPORT FUNCTIONALITY
  # =============================================================================
  
  # Download handlers
  output$download_produktivitas <- downloadHandler(
    filename = function() {
      paste("produktivitas_padi_jatim_", Sys.Date(), ".csv", sep = "")
    },
    content = function(file) {
      if(!is.null(data_produktivitas)) {
        write.csv(data_produktivitas, file, row.names = FALSE, fileEncoding = "UTF-8")
      }
    }
  )
  
  output$download_temperatur <- downloadHandler(
    filename = function() {
      paste("temperatur_jatim_", Sys.Date(), ".csv", sep = "")
    },
    content = function(file) {
      if(!is.null(data_temperatur)) {
        write.csv(data_temperatur, file, row.names = FALSE, fileEncoding = "UTF-8")
      }
    }
  )
  
  output$download_curah_hujan <- downloadHandler(
    filename = function() {
      paste("curah_hujan_jatim_", Sys.Date(), ".csv", sep = "")
    },
    content = function(file) {
      if(!is.null(data_curah_hujan)) {
        write.csv(data_curah_hujan, file, row.names = FALSE, fileEncoding = "UTF-8")
      }
    }
  )
  
  output$download_all_data <- downloadHandler(
    filename = function() {
      paste("all_data_jatim_", Sys.Date(), ".csv", sep = "")
    },
    content = function(file) {
      if(!is.null(data_korelasi)) {
        write.csv(data_korelasi, file, row.names = FALSE, fileEncoding = "UTF-8")
      } else if(!is.null(data_produktivitas)) {
        # If correlation data not available, combine available data
        combined_data <- data_produktivitas
        if(!is.null(data_temperatur)) {
          combined_data <- combined_data %>%
            left_join(data_temperatur, by = c("kabkota", "tahun"), suffix = c("", "_temp"))
        }
        if(!is.null(data_curah_hujan)) {
          combined_data <- combined_data %>%
            left_join(data_curah_hujan, by = c("kabkota", "tahun"), suffix = c("", "_hujan"))
        }
        write.csv(combined_data, file, row.names = FALSE, fileEncoding = "UTF-8")
      }
    }
  )
  
  # Preview tables
  output$preview_produktivitas <- DT::renderDataTable({
    if(!is.null(data_produktivitas)) {
      DT::datatable(head(data_produktivitas, 100), 
                    options = list(pageLength = 10, scrollX = TRUE),
                    rownames = FALSE) %>%
        formatRound(columns = c("produktivitas"), digits = 2)
    } else {
      DT::datatable(data.frame(Message = "Data tidak tersedia"))
    }
  })
  
  output$preview_temperatur <- DT::renderDataTable({
    if(!is.null(data_temperatur)) {
      DT::datatable(head(data_temperatur, 100), 
                    options = list(pageLength = 10, scrollX = TRUE),
                    rownames = FALSE) %>%
        formatRound(columns = c("temperatur"), digits = 2)
    } else {
      DT::datatable(data.frame(Message = "Data tidak tersedia"))
    }
  })
  
  output$preview_curah_hujan <- DT::renderDataTable({
    if(!is.null(data_curah_hujan)) {
      DT::datatable(head(data_curah_hujan, 100), 
                    options = list(pageLength = 10, scrollX = TRUE),
                    rownames = FALSE) %>%
        formatRound(columns = c("curah_hujan"), digits = 2)
    } else {
      DT::datatable(data.frame(Message = "Data tidak tersedia"))
    }
  })
}

# Run the application
shinyApp(ui = ui, server = server)
