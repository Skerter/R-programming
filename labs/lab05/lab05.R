# запуск проги Rscript --vanilla labs/lab05/lab05.R

cat("1. Гистограмма и boxplot\n")

# Учебная выборка; 5 и 100 добавлены как заведомые ошибки измерения.
set.seed(123)
values <- c(rnorm(50, mean = 50, sd = 5), 5, 100)

png("labs/lab05/distribution.png", width = 1400, height = 650, res = 120)
par(mfrow = c(1, 2), mar = c(4, 4, 3, 1))
hist(
  values, breaks = seq(0, 110, by = 5),
  main = "Распределение исходных данных",
  xlab = "Значение", ylab = "Частота", col = "lightblue"
)
boxplot(
  values, horizontal = TRUE, main = "Boxplot исходных данных",
  xlab = "Значение", col = "peachpuff"
)
invisible(dev.off())
cat("Гистограмма и boxplot: labs/lab05/distribution.png\n")

cat("\n2. Поиск выбросов по правилу 1,5 IQR\n")

q1 <- quantile(values, 0.25, names = FALSE)
q3 <- quantile(values, 0.75, names = FALSE)
iqr_value <- IQR(values)
lower_bound <- q1 - 1.5 * iqr_value
upper_bound <- q3 + 1.5 * iqr_value
outlier_mask <- values < lower_bound | values > upper_bound
print(c(Q1 = q1, Q3 = q3, IQR = iqr_value))
cat("Границы:", lower_bound, "и", upper_bound, "\n")
print(data.frame(index = which(outlier_mask), value = values[outlier_mask]))

# В этом примере удаление обосновано известным происхождением ошибок.
# В реальных данных выход за границу IQR сам по себе не доказывает ошибку.
# Исходный вектор сохраняем; пороги вычисляем один раз, без повторного удаления.
values_clean <- values[!outlier_mask]
cat("Удалено значений:", sum(outlier_mask), "\n")
comparison <- data.frame(
  sample = c("Исходная", "После обработки"),
  n = c(length(values), length(values_clean)),
  mean = c(mean(values), mean(values_clean)),
  median = c(median(values), median(values_clean)),
  sd = c(sd(values), sd(values_clean))
)
print(comparison)

# Учебная выборка генерируется из независимых наблюдений.
# H0: наблюдения происходят из нормального распределения.
# H1: распределение отличается от нормального. Уровень значимости — 0,05.
cat("\n3. Тест Шапиро–Уилка\n")

alpha <- 0.05
normality_original <- shapiro.test(values)
normality_clean <- shapiro.test(values_clean)
print(normality_original)
print(normality_clean)
normality_results <- data.frame(
  sample = c("Исходная", "После обработки"),
  p_value = c(normality_original$p.value, normality_clean$p.value)
)
normality_results$conclusion <- ifelse(
  normality_results$p_value < alpha,
  "H0 отвергается",
  "Нет оснований отвергнуть H0"
)
print(normality_results)

# Большое p-value не доказывает нормальность. После отбора по IQR тест
# служит описательным сравнением: отбор меняет распределение выборки.
# Нельзя удалять значения только ради получения p-value больше 0,05.
png("labs/lab05/normality.png", width = 1400, height = 650, res = 120)
par(mfrow = c(1, 2), mar = c(4, 4, 3, 1))
qqnorm(
  values, main = "Q–Q: исходные данные",
  xlab = "Теоретические квантили", ylab = "Выборочные квантили"
)
qqline(values, col = "red", lwd = 2)
qqnorm(
  values_clean, main = "Q–Q: после обработки",
  xlab = "Теоретические квантили", ylab = "Выборочные квантили"
)
qqline(values_clean, col = "red", lwd = 2)
invisible(dev.off())
cat("Q–Q-графики: labs/lab05/normality.png\n")
