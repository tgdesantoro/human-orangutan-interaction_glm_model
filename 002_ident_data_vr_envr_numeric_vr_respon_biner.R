
install.packages("Hmisc")

library(readxl)
library(writexl)
library(car)
library(Hmisc)
library(ggplot2)

#membaca data interaksi manusia dengan orangutan
#penggunaan data 1:1 (pa_interaksi)
imo <- readxl::read_excel("D:/006_pascasarjana/008_thesis/004_thesis/data/data-sebaran-interaksi/data_produce_r/007_compile_data_imo_edit.xlsx",sheet = "pa_onebyone")
View(imo)

#pemeriksaan struktur data
#untuk menjelaskan tipe data, numeric atau character
str(imo)

#melihat panjang, rentang data dan kondisi data
summary(imo) 

#hasil summary data imo
#sebaran data banyak 0, terlalu skewed, dan variabel menghasilkan outlier yang berlebihan 
#specific: rentang nilai hotsopt sangt jauh, alternatif membuat binary nilai hotpsot (pa_hotspot)
#potensial terjadinya karena ukuran desa tidak seimbang. ada yang sangat besar dan sangat kecil

#treatment awal
# membuat kolom baru pa_hotspot (0 = tidak ada hotspot, 1 = ada hotspot)
imo$pa_hotspot <- ifelse(imo$hotspot > 0, 1, 0)


#cek missing data & missing value
sapply(imo, class)
colSums(is.na(imo))

#ubah kolom tahun menjadi data factor bukan character
imo$tahun <- as.factor(imo$tahun)

#uji normalitas data dengan shapiro-wilk
# --- 1. Ambil kolom numeric ---
numeric_vars <- imo[, sapply(imo, is.numeric)]

# --- 2. Lakukan uji Shapiro-Wilk untuk setiap variabel ---
shapiro_results <- lapply(numeric_vars, function(x) {
  # Shapiro sensitif terhadap n besar → gunakan sample max 5000
  samp <- sample(x, min(length(x), 5000))
  shapiro.test(samp)
})

# --- 3. Ringkas hasil ke dalam dataframe ---
shapiro_summary <- data.frame(
  variable = names(shapiro_results),
  W = round(sapply(shapiro_results, function(x) x$statistic), 4),
  p_value = round(sapply(shapiro_results, function(x) x$p.value), 6)
)

# --- 4. Keterangan normal / tidak normal ---
shapiro_summary$normalitas <- ifelse(
  shapiro_summary$p_value > 0.05,
  "Normal",
  "Tidak Normal"
)

# --- 5. Tampilkan hasil ---
print(shapiro_summary)
View(shapiro_summary)
write_xlsx(shapiro_summary, "D:/006_pascasarjana/008_thesis/004_thesis/data/data-sebaran-interaksi/data_produce_r/007_hasil_uji_shapiro_imo.xlsx")


#cek distribusi data
hist(imo$luas_desa)
hist(imo$def_luas)
hist(imo$luas_gambut)
hist(imo$luas_phva)

hist(imo$hotspot)

#hasil: histogram
#rata-rata miring ke kanan, ada data yang besar sekali di luar (outlier) dan banyak data bernilai 0 dari histogram
#perlu dilakukan perlakukan agar tidak terlalu condong ke kanan

#lakukan log-trasnformasi
imo$luas_desa_log <- log1p(imo$luas_desa)
imo$def_luas_log <- log1p(imo$def_luas)
imo$hotspot_log <- log1p(imo$hotspot)
imo$luas_gambut_log <- log1p(imo$luas_gambut)
imo$luas_phva_log <- log1p(imo$luas_phva)

#visualisasai & diagnostik kembali pasca transform histogram dan Q-Q

#membuat histogram
# Atur layout panel (3 baris, 2 kolom)
par(mfrow = c(3, 2))

hist(imo$hotspot_log, main = "Distribusi Hotspot (log)",
     xlab = "Nilai hotspot (log)",
     ylab = "Frekuensi",
     col = "lightblue",
     border = "black")

hist(imo$luas_desa_log, main = "Distribusi Luas Desa (log)",
     xlab = "Nilai luas desa (log)",
     ylab = "Frekuensi",
     col = "lightblue",
     border = "black")

hist(imo$def_luas_log, main = "Distribusi luas Deforestasi (log)",
     xlab = "Nilai luas deforestasi (log)",
     ylab = "Frekuensi",
     col = "lightblue",
     border = "black")

hist(imo$luas_gambut_log, main = "Distribusi Luas Gambut (log)",
     xlab = "Nilai luas gambut log",
     ylab = "Frekuensi",
     col = "lightblue",
     border = "black")

hist(imo$luas_phva_log, main = "Distribusi Luas PHVA (log)",
     xlab = "Nilai luas PHVA (log)",
     ylab = "Frekuensi",
     col = "lightblue",
     border = "black")

#mwmbuat Q-Q plot
# Atur layout panel (3 baris, 2 kolom)
par(mfrow = c(3, 2))

# 1. hotspot_log
qqnorm(
  imo$hotspot_log,
  main = "QQ Plot Hotspot (log)",
  xlab = "Theoretical Quantiles",
  ylab = "Sample Quantiles",
  pch = 19,
  col = "blue"
)
qqline(imo$hotspot_log, col = "red", lwd = 2)

# 2. luas_desa_log
qqnorm(
  imo$luas_desa_log,
  main = "QQ Plot Luas Desa (log)",
  xlab = "Theoretical Quantiles",
  ylab = "Sample Quantiles",
  pch = 19,
  col = "blue"
)
qqline(imo$luas_desa_log, col = "red", lwd = 2)

# 3. def_luas_log
qqnorm(
  imo$def_luas_log,
  main = "QQ Plot Deforestasi (log)",
  xlab = "Theoretical Quantiles",
  ylab = "Sample Quantiles",
  pch = 19,
  col = "blue"
)
qqline(imo$def_luas_log, col = "red", lwd = 2)

# 4. luas_gambut_log
qqnorm(
  imo$luas_gambut_log,
  main = "QQ Plot Luas Gambut (log)",
  xlab = "Theoretical Quantiles",
  ylab = "Sample Quantiles",
  pch = 19,
  col = "blue"
)
qqline(imo$luas_gambut_log, col = "red", lwd = 2)

# 5. luas_phva_log
qqnorm(
  imo$luas_phva_log,
  main = "QQ Plot Luas PHVA (log)",
  xlab = "Theoretical Quantiles",
  ylab = "Sample Quantiles",
  pch = 19,
  col = "blue"
)
qqline(imo$luas_phva_log, col = "red", lwd = 2)

# Kembalikan layout default
par(mfrow = c(1, 1))


#karena data respon (imo) dan data lingkungan juga memiliki nilai binary (1-0)
#maka perlu dilihat kembali profil data tersebut

#periksa proporsi binary tiap data
library(dplyr)
library(tidyr)
library(purrr)

# daftar variabel biner yang ingin dianalisis
vars <- c("pa_interaksi", "pa_phva", "pa_hotspot", "pa_def", "pa_gambut")

# membuat fungsi untuk mengubah table menjadi data frame
convert_to_df <- function(x, varname) {
  tab <- table(x)
  prop <- round(prop.table(tab) * 100, 2)
  
  data.frame(
    variabel = varname,
    kategori = names(tab),
    jumlah = as.numeric(tab),
    persen = as.numeric(prop),
    row.names = NULL
  )
}

# proses semua variabel menjadi satu data frame
df_kesimpulan <- map_dfr(vars, ~ convert_to_df(imo[[.x]], .x))

df_kesimpulan


#periksa overlap prediktor per kelas
#Boxplot antara prediktor dan respon biner
par(mfrow=c(3,2))  # 5 grafik dalam 1 panel

boxplot(hotspot_log ~ pa_interaksi, data = imo,
        main = "Hotspot (log) vs Interaksi",
        xlab = "presence-absence imo",
        ylab = "Hotspot (log)",
        pch = 19)
#hasil 1: hotspot merupakan prediktor kuat terhadap terjadinya interaksi.
#hasil 2: Nilai hotspot 0 terlalu banyak → berpotensi complete separation pada model logit.


boxplot(luas_gambut_log ~ pa_interaksi, data = imo,
        main = "Luas Gambut (log) vs Interaksi",
        xlab = "presence-absence imo",
        ylab = "Luas gambut (log)",
        pch = 19)
#hasil 1: Ada overlap, tetapi pola naik jelas → prediktor potensial.
#hasil 2: bagus sebagai prediktor, tidak terlalu ekstrem seperti hotspot.


boxplot(luas_phva_log ~ pa_interaksi, data = imo,
        main = "Luas PHVA (log) vs Interaksi",
        xlab = "presence-absence imo",
        ylab = "Luas PHVA (log)",
        pch = 19)
#hasil 1: Outlier kecil di kelas 0 menunjukkan variasi besar.
#hasil 2: Ini menjadi predictor yang baik, tapi perlu diperiksa multikolinearitasnya dengan luas desa atau variabel area lain.


boxplot(def_luas_log ~ pa_interaksi, data = imo,
        main = "Deforestasi (log) vs Interaksi",
        xlab = "presence-absence imo",
        ylab = "Luas deforestasi (log)",
        pch = 19)
#hasil 1: hubungan nya lemah, Tidak sekuat hotspot atau gambut.
#hasil 2: Prediktor moderat, signifikan tapi tidak dominan.


boxplot(luas_desa_log ~ pa_interaksi, data = imo,
        main = "Luas Desa (log) vs Interaksi",
        xlab = "presence-absence imo",
        ylab = "Luas desa (log)",
        pch = 19)
#hasil 1: lebih cocok jadi variabel offset bukan variabel prediktor
#hasil 2: perlu diperhatikan dengan prediktor lain, potensial multikolinearitas kalau dijadiin variabel prediktor

par(mfrow=c(1,1))  # reset


#melihat hubungan variabel prediktor dengan variabel imo
#karena data merupakan nilai numeric predictor, dan nilai binary (imo)
#uji yang digunakan kruskal-wallis/wilcoxon 
#uji hubungan antara variabel prediktor dengan variabel yang dipengaruhi

### ==========================================
### WILCOXON TEST + AUTOMATIC INTERPRETATION
### ==========================================

# Target biner
target <- "pa_interaksi"

# Ambil semua prediktor numerik (kecuali target)
prediktor <- names(imo)[sapply(imo, is.numeric)]
prediktor <- prediktor[prediktor != target]

# Data frame hasil
hasil <- data.frame(
  variabel = character(),
  W_stat = numeric(),
  p_value = numeric(),
  effect_size = numeric(),
  kategori_effect = character(),
  interpretasi = character(),
  stringsAsFactors = FALSE
)

# Fungsi klasifikasi effect size
kategori_effect_size <- function(r){
  if (abs(r) < 0.1) return("sangat kecil")
  if (abs(r) < 0.3) return("kecil")
  if (abs(r) < 0.5) return("sedang")
  return("besar")
}

# Loop semua prediktor
for (v in prediktor) {
  
  # Wilcoxon rank-sum test
  test <- wilcox.test(imo[[v]] ~ imo[[target]], exact = FALSE)
  
  # Hitung effect size (rank-biserial)
  n1 <- sum(imo[[target]] == 0)
  n2 <- sum(imo[[target]] == 1)
  rbc <- (test$statistic / (n1 * n2)) * 2 - 1
  
  # Kategori effect size
  kategori <- kategori_effect_size(rbc)
  
  # Interpretasi otomatis
  if (test$p.value < 0.05) {
    signif_txt <- "ADA perbedaan signifikan"
  } else {
    signif_txt <- "TIDAK ada perbedaan signifikan"
  }
  
  arah <- ifelse(rbc < 0,
                 "nilai median pada desa dengan interaksi (IMO=1) lebih tinggi",
                 "nilai median pada desa tanpa interaksi (IMO=0) lebih tinggi")
  
  interpretasi <- paste0(
    signif_txt, 
    " antara kelompok IMO dan non-IMO untuk variabel ", v, ". ",
    "Effect size termasuk kategori ", kategori, 
    ", menunjukkan bahwa ", arah, "."
  )
  
  # Simpan hasil
  hasil <- rbind(
    hasil,
    data.frame(
      variabel = v,
      W_stat = as.numeric(test$statistic),
      p_value = test$p.value,
      effect_size = rbc,
      kategori_effect = kategori,
      interpretasi = interpretasi,
      stringsAsFactors = FALSE
    )
  )
}

# Urutkan berdasarkan p-value
hasil <- hasil[order(hasil$p_value), ]

# Print hasil
print(hasil)
View(hasil)
write_xlsx(hasil, "D:/006_pascasarjana/008_thesis/004_thesis/data/data-sebaran-interaksi/data_produce_r/007_hasil_uji_wilcoxon_imo.xlsx")


#analisa hubungan spearman rank dan multikolinearitas antar prediktor untuk data numeric (Variance Inflation Factor)

#Variabel prediktor (log_transformed)
prediktor <- imo[, c("luas_phva_log","luas_gambut_log", "luas_desa_log", "def_luas_log","hotspot_log")]

#membuat label
labels_new <- c(
  expression(bar(x)[Luas_PHVA]),
  expression(bar(x)[Luas_Gambut]),
  expression(bar(x)[Luas_Desa]),
  expression(bar(x)[Luas_Deforestasi]),
  expression(bar(x)[Jumlah_Hotspot])
)


#Pairs plot
pairs(prediktor,
      main = "Pairs Plot - Korelasi Antar Prediktor",
      pch = 19, cex = 0.6, labels = labels_new)

#Korelasi Spearman (nilai korelasi)
cor_spearman <- cor(prediktor, method = "spearman", use = "pairwise.complete.obs")
print(cor_spearman)

#label heatmap lebih ringkas 
colnames(cor_spearman) <- c("Luas PHVA", "Luas Gambut", "Luas Desa", "Luas Deforestasi", "Jumlah Hotspot")
rownames(cor_spearman) <- colnames(cor_spearman)

#Heatmap Spearman
corrplot(cor_spearman, 
         method = "color", 
         type = "upper",
         tl.col = "black",
         tl.srt = 45,
         addCoef.col = "black",
         number.cex = 0.9)


#Spearman p-value lengkap ---
rcorr_spear <- rcorr(as.matrix(prediktor), type = "spearman")
print(rcorr_spear$P)

#Deteksi Multikolinearitas Berdasarkan r > 0.7
threshold <- 0.7
multi_pairs <- which(abs(cor_spearman) > threshold & abs(cor_spearman) < 1, arr.ind = TRUE)

if (nrow(multi_pairs) == 0) {
  message("Tidak ada pasangan variabel dengan korelasi > 0.7 (aman dari multikolinearitas).")
} else {
  message("Variabel dengan multikolinearitas tinggi (|rho| > 0.7):")
  for (i in 1:nrow(multi_pairs)) {
    cat(
      rownames(cor_spearman)[multi_pairs[i, 1]], " <-> ",
      colnames(cor_spearman)[multi_pairs[i, 2]], 
      " | rho = ",
      round(cor_spearman[multi_pairs[i, 1], multi_pairs[i, 2]], 3),
      "\n"
    )
  }
}


#menyimpan hasil spearman untuk data numeric
install.packages("openxlsx")
library(openxlsx)

wb <- createWorkbook()

addWorksheet(wb, "Spearman Correlation")
writeData(wb, "Spearman Correlation", cor_spearman)

addWorksheet(wb, "Spearman P Values")
writeData(wb, "Spearman P Values", rcorr_spear$P)

saveWorkbook(wb,"D:/006_pascasarjana/008_thesis/004_thesis/data/data-sebaran-interaksi/data_produce_r/007_hasil_korelasi_spearman_rv.xlsx", overwrite = TRUE) # simpan file



#Variance Inflation Factor
library(car)
model_vif <- glm(pa_interaksi ~ hotspot_log + luas_gambut + luas_phva + def_luas + luas_desa,
                 data = imo,
                 family = binomial)
vif_values <- vif(model_vif)
print(vif_values)

#hasilnya
#hotspot_log luas_gambut   luas_phva    def_luas   luas_desa 
#1.141171    1.325892    3.566044    1.484563    4.598364
#VIF < 5 → aman (tidak ada masalah)

#Interpretasi otomatis
if(any(vif_values > 5)){
  print("⚠️ Ada multikolinearitas serius (VIF > 5).")
} else if(any(vif_values > 3)){
  print("⚠️ Ada indikasi multikolinearitas (VIF 3–5).")
} else {
  print("✔ Tidak ada multikolinearitas berarti (VIF < 3).")
}


#uji korelasi antar variabel untuk data binary terhadap numeric atau sebaliknya
#Identifikasi kolom binary dan numeric
### ---------------------------------------

# Binary = nilai hanya 0 dan 1
binary_vars <- names(imo)[sapply(imo, function(x) all(unique(x) %in% c(0,1)))]

# Numeric = numeric tetapi bukan binary
numeric_vars <- names(imo)[sapply(imo, is.numeric)]
numeric_vars <- setdiff(numeric_vars, binary_vars)

binary_vars
numeric_vars

### ---------------------------------------
### 2. Buat fungsi analisis untuk satu pasangan
### ---------------------------------------

analyse_numeric_binary <- function(data, num_var, bin_var) {
  
  # Mann–Whitney test
  w_test <- wilcox.test(data[[num_var]] ~ data[[bin_var]])
  
  # Spearman (optional)
  spear <- cor.test(data[[num_var]], data[[bin_var]], method = "spearman")
  
  # Point-biserial (Pearson)
  pear <- cor.test(data[[num_var]], data[[bin_var]], method = "pearson")
  
  return(data.frame(
    numeric = num_var,
    binary  = bin_var,
    mann_whitney_p = w_test$p.value,
    pearson_r = pear$estimate,
    pearson_p = pear$p.value,
    spearman_r = spear$estimate,
    spearman_p = spear$p.value
  ))
}


### ---------------------------------------
### 3. Loop otomatis semua kombinasi
### ---------------------------------------

results <- do.call(rbind,
                   lapply(binary_vars, function(bv) {
                     do.call(rbind, lapply(numeric_vars, function(nv) {
                       analyse_numeric_binary(imo, nv, bv)
                     }))
                   }))

### ---------------------------------------
### 4. Lihat hasil lengkap
### ---------------------------------------

print(results)
View(results)

# Jika ingin diurutkan berdasarkan p-value terkecil:
results_sorted <- results[order(results$mann_whitney_p), ]
results_sorted
write_xlsx(results, "D:/006_pascasarjana/008_thesis/004_thesis/data/data-sebaran-interaksi/data_produce_r/007_hasil_uji_wilcoxon_imo_lingkungan.xlsx")



