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

#cek proporsi kelas
table(imo$pa_interaksi)
prop.table(table(imo$pa_interaksi))

#Transformasi log variabel luas dan hotspot
imo_log <- imo %>%
  mutate(
    phva_log   = log1p(luas_phva),
    gambut_log = log1p(luas_gambut),
    def_log    = log1p(def_luas),
    hotspot_log     = log1p(hotspot),
    desa_log = log1p(luas_desa)
  )

#catatan: efek acak level desa terlalu detail (kelas terlalu banyak)
#sedangkan data presence IMO tidak banyak terjadi
#pengoperasian gagal
#alternatif, ganti ke level kecamatan
#glmm gagal diaplikasikan, buat alternatif dengan luas desa sebagai nilai offset dengan variabel respon binary

# ============================================================
# 0. Load library
# ============================================================
library(dplyr)

# ============================================================
# 1. Pastikan variabel sudah siap
# ============================================================
# Cek apakah kolom luas_desa ada
if(!"luas_desa" %in% names(imo_log)){
  stop("Kolom 'luas_desa' tidak ditemukan di data.")
}

# Cek nilai 0 pada luas desa (tidak boleh untuk log offset)
if(any(imo_log$luas_desa <= 0)){
  stop("Terdapat nilai luas_desa <= 0. Offset harus > 0.")
}

# ============================================================
# 2. Fit GLM Dengan Offset Luas Desa
# ============================================================
glm_offset <- glm(
  pa_interaksi ~ phva_log + gambut_log + def_log + hotspot_log +
    offset(log(luas_desa)),
  family = binomial(link = "logit"),
  data = imo_log
)

# ============================================================
# 3. Output ringkas
# ============================================================
summary(glm_offset)

# ============================================================
# 4. Hitung AIC dan AICc
# ============================================================
library(MuMIn)
AIC(glm_offset)
AICc(glm_offset)

# ============================================================
# 5. Prediksi probabilitas IMO
# ============================================================
imo_log$pred_imo <- predict(glm_offset, type = "response")

head(imo_log)

# ==============================================
# LOGISTIC REGRESSION 70:30 dengan OFFSET luas_desa
# ==============================================

# 1️⃣ Split data menjadi training dan testing (70:30)
set.seed(123)
index <- sample(1:nrow(imo_log), 0.70 * nrow(imo_log))
train_data <- imo_log[index, ]
test_data  <- imo_log[-index, ]

# 2️⃣ Wadah evaluasi
results_glm <- data.frame(Accuracy = numeric(), AUC = numeric())

# 3️⃣ MODEL GLM DENGAN OFFSET
model <- glm(
  pa_interaksi ~ phva_log + gambut_log + def_log + hotspot_log +
    offset(log(luas_desa)),
  family = binomial(link = "logit"),
  data = train_data
)

# 4️⃣ Prediksi probabilitas pada data test
pred_prob <- predict(model, newdata = test_data, type = "response")

# 5️⃣ Probabilitas → kelas 0/1
pred_class <- ifelse(pred_prob > 0.5, 1, 0)

# 6️⃣ Observasi harus numerik biner
obs <- as.numeric(as.character(test_data$pa_interaksi))

# 7️⃣ Hitung metrik evaluasi
acc <- mean(pred_class == obs)
auc_val <- pROC::auc(obs, pred_prob)

# 8️⃣ Simpan hasil evaluasi
results_glm <- rbind(results_glm, data.frame(Accuracy = acc, AUC = auc_val))

# 9️⃣ Tampilkan hasil
print(results_glm)

# 10️⃣ Ringkasan model
summary(model)

# ============================
# ROC Curve
# ============================
pred_prob <- predict(model, newdata = test_data, type = "response")
obs <- as.numeric(as.character(test_data$pa_interaksi))

roc_obj <- roc(obs, pred_prob)
auc_value <- auc(roc_obj)
cat("AUC =", auc_value, "\n")

plot(roc_obj, col = "#1c61b6", lwd = 3,
     main = "ROC Curve - GLM Logistic Model (Offset luas_desa)")
abline(a = 0, b = 1, lty = 2, col = "gray")
text(0.6, 0.2, paste("AUC =", round(auc_value, 3)), cex = 1.2)

# ============================
# Penyimpanan hasil GLM
# ============================

save_path <- "G:/My Drive/006_school/glm_offset_luas_desa_70_30_phva_deforestasi_gambut_hotspot/"

# 1. Simpan summary
glm_summary <- capture.output(summary(model))
writeLines(glm_summary, paste0(save_path, "GLM_summary.txt"))

# 2. Simpan koefisien
library(broom)
coef_table <- tidy(model)
write.csv(coef_table, paste0(save_path, "GLM_coefficients.csv"), row.names = FALSE)

# 3. Simpan evaluasi
write.csv(results_glm,
          paste0(save_path, "GLM_evaluation_accuracy_auc.csv"),
          row.names = FALSE)

# 4. Simpan AUC
auc_text <- paste("AUC =", auc_value)
writeLines(auc_text, paste0(save_path, "GLM_AUC_value.txt"))

# 5. Simpan ROC object
save(roc_obj, file = paste0(save_path, "ROC_object.Rdata"))

# 6. Simpan model
save(model, file = paste0(save_path, "GLM_model_full.Rdata"))

# 7. Simpan prediksi
pred_df <- data.frame(
  Observed = obs,
  Predicted_Prob = pred_prob,
  Predicted_Class = ifelse(pred_prob > 0.5, 1, 0)
)

write.csv(pred_df,
          paste0(save_path, "GLM_predictions.csv"),
          row.names = FALSE)

cat("Semua hasil GLM dengan offset(luas_desa) telah disimpan ke folder:\n", save_path)


# ============================================================
# 1. LOAD LIBRARY
# ============================================================
library(readxl)
library(dplyr)
library(ggplot2)
library(broom)
library(pROC)

# ============================================================
# 2. PATH & IMPORT DATA DARI EXCEL
# ============================================================

# Path hanya STRING, bukan data!
data_path <- "G:/My Drive/006_school/007_compile_data_imo.xlsx"

# Pilih sheet
sheet_name <- "pa_onebyone"

# Baca data
imo <- read_excel(data_path, sheet = sheet_name)

#Transformasi log variabel luas dan hotspot
imo_log <- imo %>%
  mutate(
    phva_log   = log1p(luas_phva),
    gambut_log = log1p(luas_gambut),
    def_log    = log1p(def_luas),
    hotspot_log     = log1p(hotspot)
  )

# Lihat struktur
str(imo_log)

# ============================================================
# 3. CEK & KONVERSI DATA
# ============================================================

# Pastikan variabel respon adalah faktor biner 0/1
imo_log$pa_interaksi <- as.factor(imo_log$pa_interaksi)

# Pastikan luas desa tidak nol
imo_log <- imo_log %>% filter(luas_desa > 0)

# ============================================================
# 4. GLM TANPA OFFSET
# ============================================================

glm_no_offset <- glm(
  pa_interaksi ~ phva_log + gambut_log + def_log + hotspot_log,
  family = binomial(link = "logit"),
  data = imo_log
)

summary(glm_no_offset)

# ============================================================
# 5. GLM DENGAN OFFSET log(luas_desa)
# ============================================================

glm_offset <- glm(
  pa_interaksi ~ phva_log + gambut_log + def_log + hotspot_log +
    offset(log(luas_desa)),
  family = binomial(link = "logit"),
  data = imo_log
)

summary(glm_offset)

# ============================================================
# 6. BANDINKAN AIC & KOEFISIEN
# ============================================================

compare_table <- data.frame(
  Model = c("GLM_No_Offset", "GLM_Offset"),
  AIC = c(AIC(glm_no_offset), AIC(glm_offset)),
  Resid_Deviance = c(glm_no_offset$deviance, glm_offset$deviance)
)

print(compare_table)

# ============================================================
# 7. VISUALISASI PERBANDINGAN KOEFISIEN
# ============================================================

coef_no <- tidy(glm_no_offset) %>% 
  mutate(Model = "GLM_No_Offset")

coef_off <- tidy(glm_offset) %>% 
  mutate(Model = "GLM_Offset")

coef_all <- bind_rows(coef_no, coef_off) %>%
  filter(term != "(Intercept)")

ggplot(coef_all, aes(x = term, y = estimate, fill = Model)) +
  geom_col(position = "dodge") +
  theme_minimal(base_size = 14) +
  labs(title = "Perbandingan Koefisien: GLM vs GLM + Offset",
       x = "Variabel",
       y = "Estimasi Koefisien")

# ============================================================
# 8. BANDINKAN FITTING MODEL DENGAN AUC
# ============================================================

# Prediksi probabilitas
pred_no <- predict(glm_no_offset, type = "response")
pred_off <- predict(glm_offset, type = "response")

# Konversi pa_interaksi ke numeric 0/1
obs <- as.numeric(as.character(imo_log$pa_interaksi))

auc_no <- auc(obs, pred_no)
auc_off <- auc(obs, pred_off)

cat("\nAUC GLM tanpa offset =", auc_no)
cat("\nAUC GLM dengan offset =", auc_off)

# ============================================================
# 9. VISUALISASI PERBANDINGAN AUC ROC
# ============================================================

roc_no <- roc(obs, pred_no)
roc_off <- roc(obs, pred_off)

plot(roc_no, col = "blue", lwd = 3, main = "ROC: GLM vs GLM + Offset")
plot(roc_off, col = "red", lwd = 3, add = TRUE)
legend("bottomright",
       legend = c(paste0("GLM No Offset (AUC=", round(auc_no,3), ")"),
                  paste0("GLM Offset (AUC=", round(auc_off,3), ")")),
       col = c("blue", "red"), lwd = 3)






