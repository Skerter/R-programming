# запуск проги Rscript --vanilla labs/lab07/lab07.R

cat("1. Корреляция между переменными\n")

# Данные в задании не указаны; для учебного примера выбран встроенный набор cars.
# speed — скорость (мили/час), dist — тормозной путь (футы).
car_data <- datasets::cars
print(head(car_data))
str(car_data)
stopifnot(
  is.numeric(car_data$speed),
  is.numeric(car_data$dist),
  !anyNA(car_data)
)

correlations <- c(
  pearson = cor(car_data$speed, car_data$dist, method = "pearson"),
  spearman = cor(car_data$speed, car_data$dist, method = "spearman"),
  kendall = cor(car_data$speed, car_data$dist, method = "kendall")
)
print(correlations)

cat("Пирсон оценивает линейную связь, Спирмен и Кендалл — ранговую.\n")
cat("Положительная корреляция означает, что большей скорости обычно\n",
    "соответствует больший тормозной путь; сама корреляция не доказывает причинность.\n")

cat("\n2. Линейная регрессионная модель\n")

model <- lm(dist ~ speed, data = car_data)
model_summary <- summary(model)
print(model_summary)

intercept <- unname(coef(model)[1])
slope <- unname(coef(model)[2])
cat(sprintf("Модель: dist = %.3f + %.3f * speed\n", intercept, slope))
cat(sprintf(
  "При увеличении скорости на 1 милю/час модель предсказывает увеличение пути на %.3f фута.\n",
  slope
))
cat(sprintf(
  "R² = %.3f: модель объясняет %.1f%% вариации тормозного пути в этих данных.\n",
  model_summary$r.squared, 100 * model_summary$r.squared
))
cat("Свободный член не интерпретируем как реальный путь при нулевой скорости:\n",
    "нулевая скорость находится вне диапазона наблюдений.\n")

# Двусторонняя проверка наклона: H0: beta_speed = 0; H1: beta_speed != 0.
# Для t-теста предполагаются линейность, независимость и нормальность ошибок
# с постоянной дисперсией. Здесь эти предпосылки не проверены; вывод условный.
alpha <- 0.05
p_value <- model_summary$coefficients["speed", "Pr(>|t|)"]
cat("Уровень значимости:", alpha, "\np-value для наклона:", p_value, "\n")
if (p_value < alpha) {
  cat("При выполнении предпосылок H0 отвергается: наклон статистически значим.\n")
} else {
  cat("Нет оснований отвергнуть H0 о нулевом наклоне.\n")
}
