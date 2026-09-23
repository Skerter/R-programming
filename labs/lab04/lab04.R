# запуск проги
# Rscript labs/lab04/lab04.R

cat("1. Основные статистики\n")

values <- c(10, NA, 20, 30, NA, 40)
print(values)

statistics <- c(
  mean = mean(values, na.rm = TRUE),
  median = median(values, na.rm = TRUE),
  min = min(values, na.rm = TRUE),
  max = max(values, na.rm = TRUE),
  variance = var(values, na.rm = TRUE),
  standard_deviation = sd(values, na.rm = TRUE),
  interquartile_range = IQR(values, na.rm = TRUE)
)
print(statistics)

cat("\nКвартили (25%, 50%, 75%):\n")
quartiles <- quantile(values, probs = c(0.25, 0.5, 0.75), na.rm = TRUE)
print(quartiles)

cat("\n2. Обработка пропусков\n")

missing_mask <- is.na(values)
print(missing_mask)
cat("Позиции пропусков:", which(missing_mask), "\n")
cat("Количество пропусков:", sum(missing_mask), "\n")

# Исключаем NA: оснований восстанавливать значения нет; исходник сохраняем.
values_clean <- values[!missing_mask]
print(values_clean)
cat("Число элементов до обработки:", length(values), "\n")
cat("Число элементов после обработки:", length(values_clean), "\n")
cat("Осталось пропусков:", sum(is.na(values_clean)), "\n")

cat("\n3. Сводка исходных данных (включая число NA)\n")

print(summary(values))
cat("\nСводка после удаления пропусков\n")
print(summary(values_clean))
