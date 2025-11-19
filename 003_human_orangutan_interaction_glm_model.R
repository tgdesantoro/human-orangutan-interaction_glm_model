#proses pembuatan model GLM
#seluruh data digunakan untuk pembuatan model ini
#jika berhasil, lanjut dengan K-fold cross validation

#mengaktifkan package yang digunakan
install.packages("patchwork") #untuk menggabungkan beberapa plot
install.packages("pROC")
install.packages("caret")
install.packages("broom")
install.packages("openxlsx")

library(readxl) # membaca file Excel (.xls / .xlsx) ke dalam R, sangat berguna untuk import data mentah
library(lme4)   # untuk membangun model linear mixed-effects dan generalized linear mixed-effects (GLMM)
library(dplyr)  # grammar of data manipulation: select, filter, mutate, summarise, arrange, group_by, dll.
library(ggplot2) #  visualisasi berbasis grammar of graphics; membuat grafik statis yang fleksibel dan elegan
library(patchwork) # menggabungkan beberapa plot ggplot2 menjadi layout grid (horizontal, vertikal, wrap)
library(tidyr) # untuk merapikan data (data tidying): pivot_longer, pivot_wider, separate, unite, drop_na, dll.
library(magrittr) # menyediakan operator pipe %>% dan %<>% (pipe assignment) untuk alur kode yang lebih bersih
library(pROC) #untuk menghitung dan memvisualisasikan ROC curve serta nilai AUC (Area Under the Curve)
library(caret) # paket komprehensif untuk machine learning: training & tuning model, cross-validation, data splitting, preprocessing (scaling, centering, imputasi), dan evaluasi performa
library(broom) #menyimpan hasil model
library(writexl) #menulis/meyimpan excel
library(openxlsx) 

#membaca atau memanggil data yang sudah diuji multikolinearitas
#membaca dataset
imo <- readxl::read_excel("G:/My Drive/006_school/007_compile_data_imo.xlsx",sheet = "pa_onebyone")
View(imo)

#periksa data NA
sum(is.na(imo))   # total seluruh NA

# --- 1️⃣ Buat versi asli dan versi scaled ---
imo_scaled <- imo %>%
  mutate(
    luas_phva   = scale(luas_phva),
    luas_gambut = scale(luas_gambut),
    def_luas    = scale(def_luas),
    hotspot     = scale(hotspot)
  )

# --- 2️⃣ Gabungkan data sebelum dan sesudah untuk perbandingan ---
imo_compare <- bind_rows(
  imo %>% mutate(type = "Sebelum Scaling"),
  imo_scaled %>% mutate(type = "Sesudah Scaling")
)

# --- 3️⃣ Ubah ke format long untuk memudahkan plotting ---
imo_long <- imo_compare %>%
  pivot_longer(cols = c(luas_phva, luas_gambut, def_luas, hotspot),
               names_to = "Variabel",
               values_to = "Nilai")

# --- 4️⃣ Plot distribusi (density plot) ---
ggplot(imo_long, aes(x = Nilai, fill = type)) +
  geom_density(alpha = 0.5) +
  facet_grid(type ~ Variabel, scales = "free") +
  scale_fill_manual(values = c("#E69F00", "#56B4E9")) +
  theme_minimal(base_size = 14) +
  labs(
    title = "Distribusi Variabel Sebelum vs Sesudah Scaling",
    x = "Nilai", y = "Kerapatan", fill = "Tipe Data"
  )


#cek proporsi kelas
table(imo_scaled$pa_interaksi)
prop.table(table(imo_scaled$pa_interaksi))


#Transformasi log variabel luas dan hotspot
imo_log <- imo %>%
  mutate(
    phva_log   = log1p(luas_phva),
    gambut_log = log1p(luas_gambut),
    def_log    = log1p(def_luas),
    hotspot_log     = log1p(hotspot),
    desa_log = log1p(luas_desa)
  )

colSums(is.na(imo_log))
View(imo_log)


#visualisasai distribusi log
library(ggplot2)
library(patchwork)

p1 <- ggplot(imo_log, aes(phva_log)) +
  geom_histogram(bins = 30, color = "black", fill = "skyblue") +
  labs(title = "Luas PHVA") +
  theme_minimal()

p2 <- ggplot(imo_log, aes(gambut_log)) +
  geom_histogram(bins = 30, color = "black", fill = "skyblue") +
  labs(title = "Luas Gambut") +
  theme_minimal()

p3 <- ggplot(imo_log, aes(def_log)) +
  geom_histogram(bins = 30, color = "black", fill = "skyblue") +
  labs(title = "Luas Deforestasi") +
  theme_minimal()

p4 <- ggplot(imo_log, aes(hotspot_log)) +
  geom_histogram(bins = 30, color = "black", fill = "skyblue") +
  labs(title = "Titik Hotspot") +
  theme_minimal()

p5 <- ggplot(imo_log, aes(desa_log)) +
  geom_histogram(bins = 30, color = "black", fill = "skyblue") +
  labs(title = "Luas Desa") +
  theme_minimal()

# gabungkan 2 kolom × 2 baris
(p1 | p2 | p3) / (p4 | p5)




#jalankan ulang GLM dengan variable log
glm_log <- glm(
  pa_interaksi ~ phva_log + gambut_log + def_log + hotspot_log,
  family = binomial(link = "logit"),
  data = imo_log
)

summary(glm_log)

#Simpan hasil GLM
output <- capture.output(summary(glm_log))
writeLines(output, "G:/My Drive/006_school/glm_log_summary_full.txt")


#lanjut membuat model glm dengan memisahkan data training dan data test
# ==============================================
# LOGISTIC REGRESSION dengan PEMBAGIAN 70:30
# ==============================================


# 1️⃣ Split data menjadi training dan testing (70:30)
set.seed(123)
index <- sample(1:nrow(imo_log), 0.70 * nrow(imo_log))
train_data <- imo_log[index, ]
test_data  <- imo_log[-index, ]

# 2️⃣ Buat wadah hasil evaluasi
results_glm <- data.frame(Accuracy = numeric(), AUC = numeric())

# 3️⃣ Bangun model regresi logistik
model <- glm(
  pa_interaksi ~ phva_log + gambut_log + def_log + hotspot_log,
  family = binomial(link = "logit"),
  data = train_data
)

# 4️⃣ Prediksi probabilitas pada data testing
pred_prob <- predict(model, newdata = test_data, type = "response")

# 5️⃣ Konversi probabilitas ke kelas (threshold 0.5)
pred_class <- ifelse(pred_prob > 0.5, 1, 0)

# 6️⃣ Pastikan variabel observasi dalam bentuk numerik 0/1
obs <- as.numeric(as.character(test_data$pa_interaksi))

# 7️⃣ Hitung metrik evaluasi
acc <- mean(pred_class == obs)                # Akurasi
auc_val <- pROC::auc(obs, pred_prob)          # AUC dari ROC Curve

# 8️⃣ Simpan hasil evaluasi ke tabel
results_glm <- rbind(results_glm, data.frame(Accuracy = acc, AUC = auc_val))

# 9️⃣ Tampilkan hasil
print(results_glm)

# 10️⃣ (Opsional) Lihat ringkasan model
summary(model)

# Prediksi probabilitas dari data uji
pred_prob <- predict(model, newdata = test_data, type = "response")

# Pastikan variabel observasi adalah numerik biner (0 dan 1)
obs <- as.numeric(as.character(test_data$pa_interaksi))

# Buat objek ROC
roc_obj <- roc(obs, pred_prob)

# Cetak nilai AUC
auc_value <- auc(roc_obj)
cat("AUC =", auc_value, "\n")

# Plot ROC Curve
plot(roc_obj, col = "#1c61b6", lwd = 3, main = "ROC Curve - GLM Logistic Model")
abline(a = 0, b = 1, lty = 2, col = "gray")  # garis diagonal acuan

# Tambahkan teks nilai AUC di grafik
text(0.6, 0.2, paste("AUC =", round(auc_value, 3)), col = "black", cex = 1.2)

#menyimpan hasil GLM
# ============================
# 1. PATH PENYIMPANAN
# ============================
save_path <- "G:/My Drive/006_school/glm_70_30_phva_deforestasi_gambut_hotspot/"


# ============================
# 2. SIMPAN RINGKASAN MODEL (summary)
# ============================
glm_summary <- capture.output(summary(model))
writeLines(glm_summary, paste0(save_path, "GLM_summary.txt"))


# ============================
# 3. SIMPAN KOEFISIEN RAPI (pakai broom)
# ============================
library(broom)
coef_table <- tidy(model)
write.csv(coef_table, paste0(save_path, "GLM_coefficients.csv"), row.names = FALSE)


# ============================
# 4. SIMPAN HASIL EVALUASI (Accuracy & AUC)
# ============================
write.csv(results_glm,
          paste0(save_path, "GLM_evaluation_accuracy_auc.csv"),
          row.names = FALSE)


# ============================
# 5. SIMPAN OBJEK ROC DAN NILAI AUC
# ============================
auc_text <- paste("AUC =", auc_value)
writeLines(auc_text, paste0(save_path, "GLM_AUC_value.txt"))


# Jika ingin menyimpan objek ROC (untuk plot nanti)
save(roc_obj, file = paste0(save_path, "ROC_object.Rdata"))


# ============================
# 6. SIMPAN MODEL GLM SECARA UTUH (.Rdata)
# ============================
save(model, file = paste0(save_path, "GLM_model_full.Rdata"))


# ============================
# 7. SIMPAN PREDIKSI
# ============================
pred_df <- data.frame(
  Observed = obs,
  Predicted_Prob = pred_prob,
  Predicted_Class = ifelse(pred_prob > 0.5, 1, 0)
)

write.csv(pred_df,
          paste0(save_path, "GLM_predictions.csv"),
          row.names = FALSE)


# ============================
# INFORMASI SELESAI
# ============================
cat("Semua hasil GLM berhasil disimpan ke folder:\n", save_path)






##proses model GLM dengan k-fold = 5
# ============================================================
# 1. LOAD LIBRARY
# ============================================================
library(caret)
library(pROC)
library(dplyr)
library(openxlsx)
library(MuMIn)  # untuk AICc

# ============================================================
# 2. PERSIAPAN DATA
# ============================================================
imo_log$pa_interaksi <- factor(
  imo_log$pa_interaksi,
  levels = c(0,1),
  labels = c("absence","presence")
)

table(imo_log$pa_interaksi)

# ============================================================
# 3. CROSS VALIDATION 80–20 SEPERTI JURNAL (LGOCV, 5 kali)
# ============================================================
ctrl <- trainControl(
  method = "LGOCV",      # Leave-group-out CV
  p = 0.80,              # 80% data training
  number = 5,            # 5 kali pengulangan
  classProbs = TRUE,
  summaryFunction = twoClassSummary,
  savePredictions = "final"
)

# ============================================================
# 4. JALANKAN MODEL GLM
# ============================================================
glm_cv <- train(
  pa_interaksi ~ phva_log + gambut_log + def_log + hotspot_log,
  data = imo_log,
  method = "glm",
  family = binomial(link = "logit"),
  trControl = ctrl,
  metric = "ROC"
)

print(glm_cv)

# ============================================================
# 5. AUC DARI 5x SPLIT
# ============================================================
auc_caret <- glm_cv$results$ROC
cat("AUC caret:", auc_caret, "\n")

# ============================================================
# 6. BANGUN ROC DARI PREDIKSI
# ============================================================
roc_data <- glm_cv$pred
prob_col <- roc_data$presence

roc_curve <- roc(
  response  = roc_data$obs,
  predictor = prob_col,
  levels = c("absence","presence"),
  direction = "<"
)

auc_value <- auc(roc_curve)
cat("AUC ROC:", auc_value, "\n")

# ============================================================
# 7. HITUNG AIC DAN AICc DARI MODEL FINAL
# ============================================================
glm_final <- glm_cv$finalModel
aic_value <- AIC(glm_final)
aicc_value <- AICc(glm_final)

cat("AIC :", aic_value, "\n")
cat("AICc:", aicc_value, "\n")

# ============================================================
# 8. PLOT ROC
# ============================================================
plot(
  roc_curve,
  col = "blue",
  lwd = 2,
  main = "ROC Curve – GLM (5x Random 80–20 CV)"
)
text(0.6, 0.2, labels = paste("AUC =", round(auc_value, 3)), cex = 1.4)

# ============================================================
# 9. BUAT FOLDER OUTPUT
# ============================================================
output_dir <- "G:/My Drive/006_school/glm_k_fold_80-20_phva_deforestasi_gambut_hotspot/"
if (!dir.exists(output_dir)) dir.create(output_dir)

# ============================================================
# 10. SIMPAN MODEL (.rds)
# ============================================================
saveRDS(glm_cv, file.path(output_dir, "glm_kfold_80_20.rds"))
cat("✔ Model disimpan: glm_kfold_80_20.rds\n")

# ============================================================
# 11. SIMPAN DATA PREDIKSI KE XLSX
# ============================================================
openxlsx::write.xlsx(
  roc_data,
  file.path(output_dir, "roc_data_kfold.xlsx")
)
cat("✔ Prediksi disimpan: roc_data_kfold.xlsx\n")

# ============================================================
# 12. SIMPAN AUC KE FILE TXT
# ============================================================
sink(file.path(output_dir, "AUC_kfold_80_20.txt"))
cat("AUC caret (mean 5x CV):", auc_caret, "\n")
cat("AUC ROC (gabungan 5 split):", auc_value, "\n")
sink()
cat("✔ AUC disimpan: AUC_kfold_80_20.txt\n")

# ============================================================
# 13. SIMPAN AIC & AICc KE FILE TXT
# ============================================================
sink(file.path(output_dir, "AIC_AICc.txt"))
cat("AIC  :", aic_value, "\n")
cat("AICc :", aicc_value, "\n")
sink()
cat("✔ AIC & AICc disimpan: AIC_AICc.txt\n")

# ============================================================
# 14. SIMPAN SUMMARY MODEL KE TXT
# ============================================================
sink(file.path(output_dir, "summary_glm_model.txt"))
cat("===== SUMMARY caret (GLM with 80–20 CV) =====\n")
print(glm_cv)
cat("\n\n===== SUMMARY MODEL GLM FINAL =====\n")
print(summary(glm_final))
sink()
cat("✔ Summary disimpan: summary_glm_model.txt\n")

# ============================================================
# 15. SIMPAN KOEFISIEN GLM KE EXCEL
# ============================================================
coef_df <- as.data.frame(coef(summary(glm_final)))
openxlsx::write.xlsx(
  coef_df,
  file.path(output_dir, "glm_coefficients.xlsx")
)
cat("✔ Koefisien disimpan: glm_coefficients.xlsx\n")

# ============================================================
# 16. SIMPAN ROC PLOT (.PNG)
# ============================================================
png(
  filename = file.path(output_dir, "ROC_curve_kfold.png"),
  width = 1600, height = 1200, res = 200
)
plot(
  roc_curve,
  col = "blue",
  lwd = 2,
  main = "ROC Curve – GLM (5x Random 80–20 CV)"
)
text(0.6, 0.2, labels = paste("AUC =", round(auc_value, 3)), cex = 1.4)
dev.off()

cat("✔ ROC plot disimpan: ROC_curve_kfold.png\n")

# ============================================================
# DONE
# ============================================================
cat("\nSEMUA FILE BERHASIL DISIMPAN DI FOLDER:\n", output_dir, "\n")


#K-Fold GLM dengan looping Variabel (PHVA [utama] + gambut + deforest + hotspot)
# ============================================================
# 1. LOAD LIBRARY
# ============================================================
install.packages("MuMIn")
library(caret)
library(pROC)
library(dplyr)
library(openxlsx)
library(MuMIn)

# ============================================================
# 2. PERSIAPAN DATA
# ============================================================
imo_log$pa_interaksi <- factor(
  imo_log$pa_interaksi,
  levels = c(0,1),
  labels = c("absence","presence")
)
View(imo_log)

# Predictors
base_var <- "phva_log"
opt_vars  <- c("gambut_log", "def_log", "hotspot_log")

# Buat semua kombinasi variabel
comb_list <- list()
idx <- 1
for (i in 0:length(opt_vars)) {
  sets <- combn(opt_vars, i, simplify = FALSE)
  for (s in sets) {
    comb_list[[idx]] <- c(base_var, s)
    idx <- idx + 1
  }
}

# ============================================================
# 3. SETUP CROSS VALIDATION
# ============================================================
ctrl <- trainControl(
  method = "LGOCV",
  p = 0.80,
  number = 5,
  classProbs = TRUE,
  summaryFunction = twoClassSummary,
  savePredictions = "final"
)

# ============================================================
# 4. LOOPING SEMUA KOMBINASI MODEL
# ============================================================
results_all <- list()
comparison <- data.frame()

output_dir <- "G:/My Drive/006_school/glm_kfold_combination_models/"
if (!dir.exists(output_dir)) dir.create(output_dir)

model_id <- 1

for (vars in comb_list) {
  
  # Nama model
  model_name <- paste(vars, collapse = "_")
  cat("\n====================================================\n")
  cat("Menjalankan Model:", model_name, "\n")
  
  # Bangun formula
  formula_str <- as.formula(
    paste("pa_interaksi ~", paste(vars, collapse = " + "))
  )
  
  # Train model GLM
  glm_cv <- train(
    formula_str,
    data = imo_log,
    method = "glm",
    family = binomial(link = "logit"),
    trControl = ctrl,
    metric = "ROC"
  )
  
  # Ambil prediksi
  roc_data <- glm_cv$pred
  prob_col <- roc_data$presence
  
  # ROC
  roc_curve <- roc(
    response  = roc_data$obs,
    predictor = prob_col,
    levels = c("absence","presence"),
    direction = "<"
  )
  auc_value <- auc(roc_curve)
  
  # AIC dan AICc
  glm_final <- glm_cv$finalModel
  aic_value <- AIC(glm_final)
  aicc_value <- AICc(glm_final)
  
  # Simpan ke tabel perbandingan
  comparison <- rbind(
    comparison,
    data.frame(
      Model_ID = model_id,
      Variables = model_name,
      AUC = auc_value,
      AIC = aic_value,
      AICc = aicc_value
    )
  )
  
  # ---------------------
  # SIMPAN FILE
  # ---------------------
  model_folder <- file.path(output_dir, paste0("model_", model_id, "_", model_name))
  if (!dir.exists(model_folder)) dir.create(model_folder)
  
  # Simpan model .rds
  saveRDS(glm_cv, file.path(model_folder, "model_glm_kfold.rds"))
  
  # Simpan ROC data
  write.xlsx(roc_data, file.path(model_folder, "roc_data_kfold.xlsx"))
  
  # Simpan AUC
  sink(file.path(model_folder, "AUC.txt"))
  cat("AUC ROC:", auc_value, "\n")
  cat("AUC caret mean:", glm_cv$results$ROC, "\n")
  sink()
  
  # Simpan AIC & AICc
  sink(file.path(model_folder, "AIC_AICc.txt"))
  cat("AIC :", aic_value, "\n")
  cat("AICc:", aicc_value, "\n")
  sink()
  
  # Simpan koefisien
  coef_df <- as.data.frame(coef(summary(glm_final)))
  write.xlsx(coef_df, file.path(model_folder, "glm_coefficients.xlsx"))
  
  # Simpan summary
  sink(file.path(model_folder, "summary_glm_model.txt"))
  print(glm_cv)
  print(summary(glm_final))
  sink()
  
  # Simpan plot ROC
  png(
    filename = file.path(model_folder, "ROC_curve.png"),
    width = 1600, height = 1200, res = 200
  )
  plot(roc_curve, col = "blue", lwd = 2, main = paste("ROC Curve –", model_name))
  text(0.6, 0.2, labels = paste("AUC =", round(auc_value, 3)), cex = 1.4)
  dev.off()
  
  # simpan hasil ke list
  results_all[[model_id]] <- glm_cv
  model_id <- model_id + 1
}

# ============================================================
# 5. SIMPAN HASIL PERBANDINGAN KE EXCEL
# ============================================================
write.xlsx(
  comparison,
  file.path(output_dir, "PERBANDINGAN_SEMUA_MODEL.xlsx")
)

cat("\n====================================================\n")
cat("SEMUA MODEL SELESAI DIJALANKAN.\n")
cat("HASIL TERSIMPAN DI FOLDER:\n", output_dir, "\n")
cat("Tabel perbandingan: PERBANDINGAN_SEMUA_MODEL.xlsx\n")

# ============================================================
# PLOT PARTIAL EFFECT UNTUK 2 VARIABEL TERPENTING
# ============================================================

library(ggplot2)

glm_model <- glm_final   # dari script Anda

# Buat fungsi untuk partial effect plot
partial_plot <- function(model, data, var, n = 100) {
  
  # rentang nilai variabel
  var_seq <- seq(min(data[[var]], na.rm=TRUE),
                 max(data[[var]], na.rm=TRUE),
                 length.out = n)
  
  # buat data baru
  newdata <- as.data.frame(lapply(data, function(x) mean(x, na.rm=TRUE)))
  newdata <- newdata[rep(1, n), ]
  newdata[[var]] <- var_seq
  
  # prediksi probabilitas + SE
  pred <- predict(model, newdata, type="link", se.fit=TRUE)
  
  # transformasi ke probabilitas logit
  newdata$fit  <- plogis(pred$fit)
  newdata$lower <- plogis(pred$fit - 1.96 * pred$se.fit)
  newdata$upper <- plogis(pred$fit + 1.96 * pred$se.fit)
  
  return(newdata)
}

# Generate dataset plot
plot1_data <- partial_plot(glm_model, imo_log, "phva_log")
plot2_data <- partial_plot(glm_model, imo_log, "gambut_log")

# ============================================================
# PLOT 1 — pengaruh PHVA
# ============================================================
p1 <- ggplot(plot1_data, aes(x = phva_log, y = fit)) +
  geom_line(size=1.2) +
  geom_ribbon(aes(ymin = lower, ymax = upper), alpha = 0.25, fill="skyblue") +
  labs(
    x = "Luas PHVA",
    y = "Probabilitis yang diprediksi (± SE)",
    title = "Pengaruh PHVA terhadap interaksi yang diprediksi"
  ) +
  theme_bw(base_size = 14)

# ============================================================
# PLOT 2 — pengaruh Gambut
# ============================================================
p2 <- ggplot(plot2_data, aes(x = gambut_log, y = fit)) +
  geom_line(size=1.2) +
  geom_ribbon(aes(ymin = lower, ymax = upper), alpha = 0.25, fill="skyblue") +
  labs(
    x = "Luas Gambut",
    y = "Probabilitas yang diprediksi (± SE)",
    title = "Pengaruh gambut terhadap interaksi yang diprediksi"
  ) +
  theme_bw(base_size = 14)

# Tampilkan
p1
p2






