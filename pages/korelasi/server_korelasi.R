# Load required libraries
library(ggplot2)
library(dplyr)
library(reshape2)
library(polycor)

# Read the data (assuming you have the CSV file)
aspek_sosial <- read.csv("data/AspekSosial.csv")

# Convert to factors with appropriate levels
aspek_sosial$`Apakah.Anda.aktif.dalam.kegiatan.sosial.kemasyarakatan.di.desa.ini` <- 
  factor(aspek_sosial$`Apakah.Anda.aktif.dalam.kegiatan.sosial.kemasyarakatan.di.desa.ini`, 
         levels = c(1, 2), labels = c("Ya", "Tidak"))

aspek_sosial$`Seberapa.sering.Anda.berpartisipasi.dalam.pertemuan.warga.atau.komunitas.di.desa` <- 
  factor(aspek_sosial$`Seberapa.sering.Anda.berpartisipasi.dalam.pertemuan.warga.atau.komunitas.di.desa`, 
         levels = 1:4, labels = c("Tidak pernah", "Jarang", "Kadang-kadang", "Sering"))

aspek_sosial$`Apakah.anda.merasa.adanya.dukungan.sosial.yang.memadai.di.desa.ini` <- 
  factor(aspek_sosial$`Apakah.anda.merasa.adanya.dukungan.sosial.yang.memadai.di.desa.ini`, 
         levels = c(1, 2), labels = c("Ya", "Tidak"))

# Calculate polychoric correlation matrix
cor_matrix <- polycor::hetcor(aspek_sosial[, c("Apakah.Anda.aktif.dalam.kegiatan.sosial.kemasyarakatan.di.desa.ini",
                                               "Seberapa.sering.Anda.berpartisipasi.dalam.pertemuan.warga.atau.komunitas.di.desa",
                                               "Apakah.anda.merasa.adanya.dukungan.sosial.yang.memadai.di.desa.ini")])$correlations

# Melt the correlation matrix
cor_melted <- melt(cor_matrix)

# Function to generate explanation based on correlation values
generate_correlation_explanation <- function(cor_matrix) {
  # Extract correlation values
  cor_aktif_frekuensi <- cor_matrix[1, 2]
  cor_aktif_dukungan <- cor_matrix[1, 3]
  cor_frekuensi_dukungan <- cor_matrix[2, 3]
  
  # Function to interpret correlation strength
  interpret_correlation <- function(cor) {
    if (abs(cor) < 0.3) return("lemah")
    else if (abs(cor) < 0.5) return("sedang")
    else if (abs(cor) < 0.7) return("kuat")
    else return("sangat kuat")
  }
  
  # Generate explanation
  explanation <- paste(
    "Interpretasi Korelasi Antar Aspek Sosial:",
    "\n\n1. Hubungan antara Keaktifan dan Frekuensi Partisipasi:",
    sprintf("\n   Korelasi: %.2f - %s", cor_aktif_frekuensi, interpret_correlation(cor_aktif_frekuensi)),
    "\n   Interpretasi: ", 
    if(cor_aktif_frekuensi > 0) {
      "Orang yang aktif dalam kegiatan sosial cenderung lebih sering berpartisipasi dalam pertemuan warga."
    } else {
      "Tidak ada hubungan yang jelas antara keaktifan dalam kegiatan sosial dan frekuensi partisipasi dalam pertemuan warga."
    },
    
    "\n\n2. Hubungan antara Keaktifan dan Persepsi Dukungan Sosial:",
    sprintf("\n   Korelasi: %.2f - %s", cor_aktif_dukungan, interpret_correlation(cor_aktif_dukungan)),
    "\n   Interpretasi: ",
    if(cor_aktif_dukungan > 0) {
      "Orang yang aktif dalam kegiatan sosial cenderung merasa adanya dukungan sosial yang memadai di desa."
    } else {
      "Tidak ada hubungan yang jelas antara keaktifan dalam kegiatan sosial dan persepsi dukungan sosial."
    },
    
    "\n\n3. Hubungan antara Frekuensi Partisipasi dan Persepsi Dukungan Sosial:",
    sprintf("\n   Korelasi: %.2f - %s", cor_frekuensi_dukungan, interpret_correlation(cor_frekuensi_dukungan)),
    "\n   Interpretasi: ",
    if(cor_frekuensi_dukungan > 0) {
      "Orang yang lebih sering berpartisipasi dalam pertemuan warga cenderung merasa adanya dukungan sosial yang memadai di desa."
    } else {
      "Tidak ada hubungan yang jelas antara frekuensi partisipasi dalam pertemuan warga dan persepsi dukungan sosial."
    },
    
    "\n\nKesimpulan:",
    "\n   Berdasarkan analisis korelasi, ",
    if(mean(c(cor_aktif_frekuensi, cor_aktif_dukungan, cor_frekuensi_dukungan)) > 0) {
      "terdapat hubungan positif antara ketiga aspek sosial yang diukur. Ini menunjukkan bahwa keaktifan dalam kegiatan sosial, frekuensi partisipasi dalam pertemuan warga, dan persepsi dukungan sosial saling terkait dan cenderung bergerak dalam arah yang sama."
    } else {
      "hubungan antara ketiga aspek sosial yang diukur tidak konsisten atau lemah. Ini menunjukkan bahwa keaktifan dalam kegiatan sosial, frekuensi partisipasi dalam pertemuan warga, dan persepsi dukungan sosial mungkin tidak selalu bergerak dalam arah yang sama atau memiliki hubungan yang kuat satu sama lain."
    }
  )
  
  return(explanation)
}

# Create the heatmap
output$korelasi1Plot <- renderPlot({
  ggplot(cor_melted, aes(x = Var1, y = Var2, fill = value)) +
    geom_tile() +
    geom_text(aes(label = sprintf("%.2f", value)), color = "white", size = 5) +
    scale_fill_gradient2(low = "blue", high = "red", mid = "white", midpoint = 0, limit = c(-1,1), name = "Korelasi") +
    theme_minimal() +
    theme(
      axis.text.x = element_text(angle = 45, hjust = 1, size = 10),
      axis.text.y = element_text(size = 10),
      plot.title = element_text(hjust = 0.5, face = "bold", size = 14),
      legend.position = "bottom"
    ) +
    labs(
      title = "Korelasi Antar Aspek Sosial",
      x = "",
      y = ""
    ) +
    scale_x_discrete(labels = c("Keaktifan", "Frekuensi", "Dukungan")) +
    scale_y_discrete(labels = c("Keaktifan", "Frekuensi", "Dukungan"))
}, height = function() {
  session$clientData$output_korelasi1Plot_width
}, width = function() {
  session$clientData$output_korelasi1Plot_width
})

# Generate and display explanation
output$korelasi1Explanation <- renderText({
  generate_correlation_explanation(cor_matrix)
})
# Fungsi untuk menghitung korelasi
calculate_correlation <- function(data, var1, var2) {
  cor(data[[var1]], data[[var2]], use = "pairwise.complete.obs")
}

# Fungsi untuk membuat plot korelasi
create_correlation_plot <- function(data, var1, var2, title) {
  ggplot(data, aes_string(x = var1, y = var2)) +
    geom_point() +
    geom_smooth(method = "lm", se = FALSE) +
    theme_minimal()
}

# Fungsi untuk menghasilkan penjelasan dinamis
create_correlation_explanation <- function(data, var1, var2) {
  # Menghitung koefisien korelasi
  correlation <- cor(data[[var1]], data[[var2]], use = "complete.obs")
  
  # Tentukan arah korelasi
  direction <- if (correlation > 0) {
    "positif"
  } else if (correlation < 0) {
    "negatif"
  } else {
    "tidak ada korelasi"
  }
  
  # Tentukan kekuatan korelasi
  strength <- if (abs(correlation) > 0.7) {
    "kuat"
  } else if (abs(correlation) > 0.3) {
    "sedang"
  } else {
    "lemah"
  }
  
  # Buat penjelasan teks
  explanation <- paste(
    "Hasil analisis menunjukkan bahwa korelasi antara",
    var1,
    "dan",
    var2,
    "adalah",
    direction,
    if (strength == "lemah")
      ". Namun, ",
    "dengan kekuatan yang",
    strength,
    "(",
    round(correlation, 2),
    ").",
    "Ini menunjukkan bahwa adanya dana desa",
    if (direction == "positif")
      "berpotensi besar untuk membuka"
    else
      "mungkin kurang efektif dalam membuka",
    "peluang usaha ekonomi rakyat."
  )
  
  return(explanation)
}

# Server logic for correlation


output$korelasi2Plot <- renderPlot({
  # Korelasi 2: Peluang Ekonomi dan Bantuan Modal
  create_correlation_plot(peningkatan_perekonomian, 
                          "Terbukanya.usaha.ekonomi.rakyat.karena.adanya.dana.desa", 
                          "Adanya.Dana.desa.membantu.mengembangkan.modal.untuk.rakyat",
                          "Korelasi 2: Peluang Ekonomi dan Bantuan Modal")
})

output$korelasi2Explanation <- renderText({
  var1 <- "Terbukanya.usaha.ekonomi.rakyat.karena.adanya.dana.desa"
  var2 <- "Adanya.Dana.desa.membantu.mengembangkan.modal.untuk.rakyat"
  data <- peningkatan_perekonomian
  correlation <- cor(data[[var1]], data[[var2]], use = "complete.obs")
  
  # Tentukan arah korelasi
  direction <- if (correlation > 0) {
    "positif"
  } else if (correlation < 0) {
    "negatif"
  } else {
    "tidak ada korelasi"
  }
  
  # Tentukan kekuatan korelasi
  strength <- if (abs(correlation) > 0.7) {
    "kuat"
  } else if (abs(correlation) > 0.3) {
    "sedang"
  } else {
    "lemah"
  }
  
  # Buat penjelasan teks
  explanation <- paste(
    "Hasil analisis menunjukkan bahwa korelasi antara",
    var1,
    "dan",
    var2,
    "adalah",
    direction,
    if (strength == "lemah")
      ". Namun, ",
    "dengan kekuatan yang",
    strength,
    "(",
    round(correlation, 2),
    ").",
    "Ini menunjukkan bahwa adanya dana desa",
    if (direction == "positif")
      "berpotensi besar untuk membuka"
    else
      "mungkin kurang efektif dalam membuka",
    "peluang usaha ekonomi rakyat."
  )
})

output$korelasi3Plot <- renderPlot({
  # Korelasi 3: Stabilitas Harga Pangan dan Pendapatan Masyarakat
  # Karena kita tidak memiliki data tentang stabilitas harga pangan, kita akan menggunakan data yang tersedia
  create_correlation_plot(peningkatan_perekonomian, 
                          "Dana.desa.menambah.penghasilan.masyarakat", 
                          "Adanya.Dana.desa.membantu.mengembangkan.modal.untuk.rakyat",
                          "Korelasi 3: Penghasilan dan Modal Masyarakat")
})

output$korelasi3Explanation <- renderText({
  var1 <- "Dana.desa.menambah.penghasilan.masyarakat"
  var2 <- "Adanya.Dana.desa.membantu.mengembangkan.modal.untuk.rakyat"
  data <- peningkatan_perekonomian
  
  correlation <- cor(data[[var1]], data[[var2]], use = "complete.obs")
  
  # Tentukan arah korelasi
  direction <- if (correlation > 0) {
    "positif"
  } else if (correlation < 0) {
    "negatif"
  } else {
    "tidak ada korelasi"
  }
  
  # Tentukan kekuatan korelasi
  strength <- if (abs(correlation) > 0.7) {
    "kuat"
  } else if (abs(correlation) > 0.3) {
    "sedang"
  } else {
    "lemah"
  }
  
  # Membuat penjelasan teks yang lebih mendalam
  explanation <- paste(
    "Analisis korelasi antara variabel 'Dana Desa Menambah Penghasilan Masyarakat' dan",
    "'Adanya Dana Desa Membantu Mengembangkan Modal untuk Rakyat' menunjukkan adanya korelasi yang",
    direction,
    "dengan kekuatan yang",
    strength,
    "(nilai korelasi:",
    round(correlation, 2),
    ").",
    "Hal ini mengindikasikan bahwa",
    if (direction == "positif") {
      "peningkatan modal yang diperoleh melalui dana desa berhubungan erat dengan peningkatan penghasilan masyarakat. Ini menunjukkan bahwa ketika masyarakat memiliki akses lebih besar terhadap modal, mereka cenderung meningkatkan pendapatan mereka melalui berbagai usaha ekonomi."
    } else if (direction == "negatif") {
      "meskipun masyarakat menerima modal tambahan melalui dana desa, hal tersebut tidak serta-merta meningkatkan penghasilan mereka. Mungkin ada faktor lain, seperti kemampuan pengelolaan modal atau kondisi pasar lokal, yang mempengaruhi hasil tersebut secara negatif."
    } else {
      "tidak ada hubungan yang signifikan antara dana desa yang digunakan untuk modal dan peningkatan penghasilan masyarakat. Ini mungkin menunjukkan bahwa faktor-faktor lain, seperti pendidikan atau akses pasar, memiliki peran yang lebih dominan dalam menentukan penghasilan masyarakat."
    },
    "Penemuan ini memberikan wawasan yang penting bagi pembuat kebijakan untuk menilai bagaimana dana desa dapat lebih efektif digunakan untuk meningkatkan kesejahteraan masyarakat melalui pengembangan modal."
  )
})

output$korelasi4Plot <- renderPlot({
  # Korelasi 4: Ketersediaan Lapangan Pekerjaan dan Dana Pembangunan
  create_correlation_plot(peningkatan_perekonomian, 
                          "Terbukanya.usaha.ekonomi.rakyat.karena.adanya.dana.desa", 
                          "Dana.desa.menambah.penghasilan.masyarakat",
                          "Korelasi 4: Ketersediaan Lapangan Pekerjaan dan Dana Pembangunan")
})

output$korelasi4Explanation <- renderText({
  var1 <- "Terbukanya.usaha.ekonomi.rakyat.karena.adanya.dana.desa"
  var2 <- "Dana.desa.menambah.penghasilan.masyarakat"
  data <- peningkatan_perekonomian
  correlation <- cor(data[[var1]], data[[var2]], use = "complete.obs")
  
  # Tentukan arah korelasi
  direction <- if (correlation > 0) {
    "positif"
  } else if (correlation < 0) {
    "negatif"
  } else {
    "tidak ada korelasi"
  }
  
  # Tentukan kekuatan korelasi
  strength <- if (abs(correlation) > 0.7) {
    "kuat"
  } else if (abs(correlation) > 0.3) {
    "sedang"
  } else {
    "lemah"
  }
  
  # Membuat penjelasan teks yang lebih mendalam
  explanation <- paste(
    "Analisis korelasi antara variabel 'Terbukanya Usaha Ekonomi Rakyat karena Adanya Dana Desa' dan",
    "'Dana Desa Menambah Penghasilan Masyarakat' menunjukkan bahwa hubungan antara kedua variabel ini",
    direction,
    "dengan kekuatan yang",
    strength,
    "(nilai korelasi:",
    round(correlation, 2),
    ").",
    "Ini mengindikasikan bahwa",
    if (direction == "positif") {
      "dana pembangunan desa yang efektif dapat membuka lapangan pekerjaan baru dan meningkatkan penghasilan masyarakat secara signifikan. Dengan dana yang tersedia, masyarakat memiliki kesempatan lebih besar untuk mengembangkan usaha ekonomi mereka, yang pada gilirannya berkontribusi pada peningkatan kesejahteraan."
    } else if (direction == "negatif") {
      "meskipun ada dana pembangunan, hal itu tidak selalu berkontribusi pada peningkatan ketersediaan lapangan pekerjaan atau penghasilan. Ini bisa disebabkan oleh faktor lain seperti distribusi dana yang tidak merata atau kurangnya keterampilan dan pengetahuan dalam mengelola usaha ekonomi."
    } else {
      "tidak ada hubungan yang signifikan antara ketersediaan dana pembangunan dan terbukanya lapangan pekerjaan atau peningkatan penghasilan masyarakat. Faktor-faktor lain mungkin memiliki pengaruh yang lebih besar, seperti kondisi ekonomi lokal atau tingkat pendidikan masyarakat."
    },
    "Penemuan ini penting untuk mengevaluasi efektivitas program pembangunan desa dan memastikan bahwa alokasi dana benar-benar digunakan untuk meningkatkan ketersediaan lapangan pekerjaan dan penghasilan masyarakat."
  )
})

output$korelasi5Plot <- renderPlot({
  # Korelasi 5: Peluang Ekonomi dan Bantuan Perusahaan
  create_correlation_plot(peningkatan_perekonomian, 
                          "Terbukanya.usaha.ekonomi.rakyat.karena.adanya.dana.CSR", 
                          "Dana.CSR.menambah.penghasilan.masyarakat",
                          "Korelasi 5: Peluang Ekonomi dan Bantuan Perusahaan")
})

output$korelasi5Explanation <- renderText({
  var1 <- "Terbukanya.usaha.ekonomi.rakyat.karena.adanya.dana.CSR"
  var2 <- "Dana.CSR.menambah.penghasilan.masyarakat"
  data <- peningkatan_perekonomian
  correlation <- cor(data[[var1]], data[[var2]], use = "complete.obs")
  
  # Tentukan arah korelasi
  direction <- if (correlation > 0) {
    "positif"
  } else if (correlation < 0) {
    "negatif"
  } else {
    "tidak ada korelasi"
  }
  
  # Tentukan kekuatan korelasi
  strength <- if (abs(correlation) > 0.7) {
    "kuat"
  } else if (abs(correlation) > 0.3) {
    "sedang"
  } else {
    "lemah"
  }
  
  # Membuat penjelasan teks yang lebih mendalam
  explanation <- paste(
    "Analisis korelasi antara variabel 'Terbukanya Usaha Ekonomi Rakyat karena Adanya Dana CSR' dan",
    "'Dana CSR Menambah Penghasilan Masyarakat' menunjukkan hubungan yang",
    direction,
    "dengan kekuatan yang",
    strength,
    "(nilai korelasi:",
    round(correlation, 2),
    ").",
    "Ini mengindikasikan bahwa",
    if (direction == "positif") {
      "bantuan perusahaan melalui dana CSR dapat membuka peluang ekonomi baru yang pada akhirnya meningkatkan penghasilan masyarakat. Dana CSR yang efektif bisa menjadi katalisator penting dalam mengembangkan usaha-usaha lokal, yang berkontribusi pada kesejahteraan ekonomi masyarakat."
    } else if (direction == "negatif") {
      "meskipun perusahaan menyediakan dana CSR, hal tersebut tidak serta-merta meningkatkan penghasilan masyarakat. Ini bisa disebabkan oleh distribusi dana yang tidak tepat sasaran atau kurangnya kesiapan masyarakat dalam memanfaatkan bantuan tersebut untuk usaha ekonomi mereka."
    } else {
      "tidak ada hubungan yang signifikan antara bantuan perusahaan melalui dana CSR dan peningkatan penghasilan masyarakat. Hal ini menunjukkan bahwa faktor-faktor lain seperti dukungan teknis, pelatihan, atau akses ke pasar mungkin lebih penting dalam mendukung usaha ekonomi rakyat."
    },
    "Penemuan ini memberikan wawasan penting bagi perusahaan dan pembuat kebijakan dalam mengevaluasi efektivitas program CSR untuk memberdayakan masyarakat lokal dan meningkatkan ekonomi mereka."
  )
})

output$korelasi6Plot <- renderPlot({
  # Korelasi 6: Infrastruktur dan Aksesibilitas Fasilitas Umum
  # Karena kita tidak memiliki data spesifik tentang infrastruktur, kita akan menggunakan visualisasi alternatif
  ggplot(karakteristik, aes(x = Pekerjaan.Utama, y = Usia)) +
    geom_boxplot() +
    ggtitle("Korelasi 6: Distribusi Usia berdasarkan Pekerjaan Utama") +
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))
})

output$korelasi6Explanation <- renderText({
  
  data <- karakteristik
  x_var <- "Pekerjaan.Utama"
  y_var <- "Usia"
  
  summary_stats <- data %>%
    group_by(!!sym(x_var)) %>%
    summarise(
      min_age = min(!!sym(y_var), na.rm = TRUE),
      q1_age = quantile(!!sym(y_var), 0.25, na.rm = TRUE),
      median_age = median(!!sym(y_var), na.rm = TRUE),
      q3_age = quantile(!!sym(y_var), 0.75, na.rm = TRUE),
      max_age = max(!!sym(y_var), na.rm = TRUE)
    )
  
  explanation <- paste(
    "Visualisasi distribusi usia berdasarkan pekerjaan utama menunjukkan perbedaan yang signifikan di antara berbagai kelompok pekerjaan. Berikut adalah ringkasan statistik untuk masing-masing kelompok:",
    paste0(
      "1. ",
      x_var,
      " '",
      summary_stats[[x_var]][1],
      "': Usia berkisar dari ",
      summary_stats$min_age[1],
      " hingga ",
      summary_stats$max_age[1],
      " dengan median usia ",
      summary_stats$median_age[1],
      "."
    ),
    paste0(
      "2. ",
      x_var,
      " '",
      summary_stats[[x_var]][2],
      "': Usia berkisar dari ",
      summary_stats$min_age[2],
      " hingga ",
      summary_stats$max_age[2],
      " dengan median usia ",
      summary_stats$median_age[2],
      "."
    ),
    paste0(
      "3. ",
      x_var,
      " '",
      summary_stats[[x_var]][3],
      "': Usia berkisar dari ",
      summary_stats$min_age[3],
      " hingga ",
      summary_stats$max_age[3],
      " dengan median usia ",
      summary_stats$median_age[3],
      "."
    ),
    "Dari hasil ini, kita dapat mengamati bahwa median usia dalam setiap kelompok pekerjaan utama berbeda, yang menunjukkan bahwa beberapa pekerjaan mungkin lebih banyak diambil oleh kelompok usia tertentu. Ini dapat memberikan wawasan tentang bagaimana distribusi usia dapat mempengaruhi aksesibilitas ke fasilitas umum, serta kebutuhan akan infrastruktur yang sesuai dengan kelompok usia tersebut.",
    sep = "\n"
  )
})

output$korelasi7Plot <- renderPlot({
  # Korelasi 7: Dampak Lingkungan dan Kualitas Hidup
  # Karena kita tidak memiliki data spesifik tentang dampak lingkungan, kita akan menggunakan visualisasi alternatif
  
  data_filtered <- karakteristik %>%
    filter(!is.na(Pendidikan), !is.na(Usia), !is.na(Jenis.Usaha))
  
  ggplot(data_filtered, aes(x = Pendidikan, y = Usia, color = Jenis.Usaha)) +
    geom_point() +
    theme_minimal()
})

# output$korelasi7Explanation <- renderText({
# 
#   data <- karakteristik
#   
#   # Filter data to remove rows with NA in specified columns
#   data <- data %>%
#     drop_na(Pendidikan, Usia, Jenis.Usaha)
#   
#   # Define the column names
#   color_var <- "Jenis.Usaha"
#   x_var <- "Pendidikan"
#   y_var <- "Usia"
#   
#   # Group and summarise the data
#   summary_data <- data %>%
#     group_by(.data[[color_var]]) %>%
#     summarise(
#       avg_usia = mean(.data[[y_var]], na.rm = TRUE),
#       median_pendidikan = median(.data[[x_var]], na.rm = TRUE)
#     )
#   
#   # Generate the explanation text
#   explanation <- paste(
#     "Visualisasi hubungan antara Pendidikan, Usia, dan Jenis Usaha menunjukkan pola-pola menarik yang memberikan wawasan tentang interaksi antara faktor-faktor ini. Berikut adalah beberapa temuan utama:",
#     paste0(
#       "1. Jenis Usaha '",
#       summary_data[[color_var]][1],
#       "': Rata-rata usia pekerja adalah ",
#       round(summary_data$avg_usia[1], 2),
#       " tahun, dengan median tingkat pendidikan '",
#       summary_data$median_pendidikan[1],
#       "'."
#     ),
#     paste0(
#       "2. Jenis Usaha '",
#       summary_data[[color_var]][2],
#       "': Rata-rata usia pekerja adalah ",
#       round(summary_data$avg_usia[2], 2),
#       " tahun, dengan median tingkat pendidikan '",
#       summary_data$median_pendidikan[2],
#       "'."
#     ),
#     paste0(
#       "3. Jenis Usaha '",
#       summary_data[[color_var]][3],
#       "': Rata-rata usia pekerja adalah ",
#       round(summary_data$avg_usia[3], 2),
#       " tahun, dengan median tingkat pendidikan '",
#       summary_data$median_pendidikan[3],
#       "'."
#     ),
#     "Dari hasil ini, kita dapat mengamati bahwa terdapat variasi yang signifikan dalam tingkat pendidikan dan usia pekerja berdasarkan jenis usaha yang mereka geluti. Misalnya, beberapa jenis usaha mungkin lebih banyak menarik tenaga kerja yang lebih tua dengan pendidikan yang lebih tinggi, sementara jenis usaha lain mungkin didominasi oleh pekerja muda dengan pendidikan lebih rendah.",
#     "Penemuan ini memberikan wawasan tentang bagaimana dampak lingkungan sosial dan ekonomi dapat memengaruhi kualitas hidup. Kualitas hidup dapat terkait erat dengan jenis usaha yang tersedia, tingkat pendidikan yang diperlukan, dan usia tenaga kerja. Jika usia pekerja lebih tua dalam usaha yang membutuhkan pendidikan lebih tinggi, hal ini mungkin menunjukkan kestabilan ekonomi tetapi juga tantangan dalam adaptasi teknologi atau inovasi. Sebaliknya, usaha yang didominasi oleh pekerja muda dengan pendidikan lebih rendah mungkin mencerminkan dinamika pasar tenaga kerja yang lebih fleksibel tetapi juga lebih rentan terhadap fluktuasi ekonomi.",
#     sep = "\n"
#   )
#   
#   explanation
# })

# output$korelasi7Explanation <- renderText({
#   
#   # Filter data untuk menghilangkan nilai NA
#   data_filtered <- karakteristik %>%
#     filter(!is.na(Pendidikan), !is.na(Usia), !is.na(Jenis.Usaha))
#   
#   # Buat ringkasan data
#   summary_data <- data_filtered %>%
#     group_by(Jenis.Usaha) %>%
#     summarise(
#       avg_usia = mean(Usia, na.rm = TRUE),
#       median_pendidikan = median(Pendidikan, na.rm = TRUE)
#     )
#   
#   # Bangun narasi
#   explanation <- paste(
#     "Analisis korelasi antara Pendidikan, Usia, dan Jenis Usaha menunjukkan beberapa temuan penting:",
#     paste0(
#       "1. Jenis Usaha '", summary_data$Jenis.Usaha[1], 
#       "': Rata-rata usia pekerja adalah ", round(summary_data$avg_usia[1], 2), 
#       " tahun, dengan median tingkat pendidikan '", summary_data$median_pendidikan[1], "'."
#     ),
#     paste0(
#       "2. Jenis Usaha '", summary_data$Jenis.Usaha[2], 
#       "': Rata-rata usia pekerja adalah ", round(summary_data$avg_usia[2], 2), 
#       " tahun, dengan median tingkat pendidikan '", summary_data$median_pendidikan[2], "'."
#     ),
#     paste0(
#       "3. Jenis Usaha '", summary_data$Jenis.Usaha[3], 
#       "': Rata-rata usia pekerja adalah ", round(summary_data$avg_usia[3], 2), 
#       " tahun, dengan median tingkat pendidikan '", summary_data$median_pendidikan[3], "'."
#     ),
#     "Temuan ini menunjukkan variasi yang signifikan dalam usia dan tingkat pendidikan berdasarkan jenis usaha yang digeluti. Misalnya, jenis usaha dengan usia pekerja yang lebih tua cenderung memiliki tingkat pendidikan yang lebih tinggi, menunjukkan pengalaman dan stabilitas dalam usaha tersebut.",
#     sep = "\n"
#   )
#   
#   return(explanation)
# })

# output$korelasi7Explanation <- renderText({
#   data_filtered <- karakteristik %>%
#     mutate(
#       Pendidikan = as.numeric(Pendidikan),
#       Usia = as.numeric(Usia)
#     )
#   
#   # Hitung korelasi dengan mengabaikan pasangan yang memiliki NA
#   correlation_value <- cor(data_filtered$Pendidikan, data_filtered$Usia, method = "pearson", use = "pairwise.complete.obs")
#   
#   # Buat penjelasan dinamis
#   if (is.na(correlation_value)) {
#     explanation <- "Data tidak cukup untuk menghitung korelasi yang valid."
#   } else if (correlation_value > 0.5) {
#     explanation <- "Terdapat korelasi positif yang kuat antara tingkat pendidikan dan usia."
#   } else if (correlation_value < -0.5) {
#     explanation <- "Terdapat korelasi negatif yang kuat antara tingkat pendidikan dan usia."
#   } else {
#     explanation <- "Korelasi antara tingkat pendidikan dan usia tidak signifikan."
#   }
#   
#   paste("Nilai korelasi antara tingkat pendidikan dan usia adalah", round(correlation_value, 2), ".", explanation)
# })

output$korelasi7Explanation <- renderText({
  data_filtered <- karakteristik %>%
    mutate(
      Pendidikan = as.numeric(Pendidikan),
      Usia = as.numeric(Usia)
    )
  
  # Hitung korelasi dengan mengabaikan pasangan yang memiliki NA
  correlation_value <- cor(data_filtered$Pendidikan, data_filtered$Usia, method = "pearson", use = "pairwise.complete.obs")
  
  if (is.na(correlation_value)) {
    explanation <- "Berdasarkan data yang ada, tidak dapat dihitung korelasi yang valid antara tingkat pendidikan dan usia. Hal ini kemungkinan disebabkan oleh jumlah data yang tidak mencukupi atau karena adanya terlalu banyak nilai yang hilang (NA). Untuk mendapatkan hasil yang lebih akurat, pertimbangkan untuk melengkapi data yang hilang atau menggunakan metode imputasi untuk memperkirakan nilai yang hilang."
  } else if (correlation_value > 0.5) {
    explanation <- paste(
      "Terdapat korelasi positif yang kuat antara tingkat pendidikan dan usia, dengan nilai korelasi sebesar", 
      round(correlation_value, 2), ".",
      "Artinya, secara umum, semakin tinggi tingkat pendidikan seseorang, semakin tua usianya. Ini mungkin menunjukkan bahwa dalam data yang dianalisis, individu yang lebih tua cenderung memiliki pendidikan yang lebih tinggi, mungkin karena mereka telah menyelesaikan pendidikan pada tahap yang lebih lanjut dalam hidup mereka."
    )
  } else if (correlation_value < -0.5) {
    explanation <- paste(
      "Terdapat korelasi negatif yang kuat antara tingkat pendidikan dan usia, dengan nilai korelasi sebesar", 
      round(correlation_value, 2), ".",
      "Ini berarti bahwa seiring bertambahnya usia, tingkat pendidikan cenderung menurun dalam populasi yang dianalisis. Fenomena ini bisa terjadi dalam situasi di mana individu yang lebih tua mungkin tidak memiliki akses atau kesempatan untuk melanjutkan pendidikan yang lebih tinggi, berbeda dengan generasi yang lebih muda."
    )
  } else {
    explanation <- paste(
      "Korelasi antara tingkat pendidikan dan usia tidak signifikan, dengan nilai korelasi sebesar", 
      round(correlation_value, 2), ".",
      "Ini menunjukkan bahwa dalam populasi yang dianalisis, tidak ada hubungan yang jelas antara usia dan tingkat pendidikan. Artinya, tingkat pendidikan seseorang tidak dapat diprediksi berdasarkan usia mereka, dan faktor lain mungkin lebih berpengaruh dalam menentukan tingkat pendidikan."
    )
  }
  
  paste(explanation)
})

