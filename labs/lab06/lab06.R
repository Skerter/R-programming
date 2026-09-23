# запуск проги Rscript --vanilla labs/lab06/lab06.R

# Выборки из примера методички; предполагаем независимость внутри и между группами.
group1 <- c(5.1, 4.9, 5.0, 5.2, 5.3)
group2 <- c(4.8, 4.7, 4.9, 5.0, 4.8)
alpha <- 0.05

cat("Исходные данные и описательные статистики\n")
print(data.frame(group1 = group1, group2 = group2))
print(data.frame(
  group = c("Группа 1", "Группа 2"),
  n = c(length(group1), length(group2)),
  mean = c(mean(group1), mean(group2)),
  median = c(median(group1), median(group2)),
  sd = c(sd(group1), sd(group2))
))

# Для каждой группы H0: распределение нормальное; H1: не нормальное.
# Проверяем группы отдельно; уровень значимости — 0,05.
cat("\n1. Тесты Шапиро–Уилка\n")

normality_group1 <- shapiro.test(group1)
normality_group2 <- shapiro.test(group2)
print(normality_group1)
print(normality_group2)

normality_results <- data.frame(
  group = c("Группа 1", "Группа 2"),
  p_value = c(normality_group1$p.value, normality_group2$p.value)
)
normality_results$conclusion <- ifelse(
  normality_results$p_value < alpha,
  "H0 отвергается",
  "Нет оснований отвергнуть H0"
)
print(normality_results)
# При n = 5 мощность проверки мала; p >= 0,05 не доказывает нормальность.

cat("\n2. t-test и U-критерий Манна–Уитни\n")

# H0: средние генеральных совокупностей равны; H1: различаются.
# Тест Уэлча: независимые выборки, нормальность в группах; дисперсии могут различаться.
cat("\n2.1. t-test Уэлча\n")

t_result <- t.test(
  group1, group2,
  alternative = "two.sided",
  paired = FALSE,
  var.equal = FALSE,
  conf.level = 1 - alpha
)
print(t_result)
cat("Разность выборочных средних (группа 1 - группа 2):",
    mean(group1) - mean(group2), "\n")
cat("95%-й доверительный интервал разности средних:", t_result$conf.int, "\n")
if (t_result$p.value < alpha) {
  cat("H0 отвергается: средние статистически значимо различаются.\n")
} else {
  cat("Нет оснований отвергнуть H0 о равенстве средних.\n")
}

# H0: распределения групп одинаковы. В модели одинаковой формы и разброса
# проверяется отсутствие сдвига; H1: сдвиг отличается от нуля.
# Без предположения одинаковой формы это не просто тест равенства медиан.
# Метод использует ранги и не требует нормальности, но требует независимости.
# При совпадениях используем аппроксимацию с поправкой на непрерывность.
# При n = 5 приближённое p-value следует интерпретировать осторожно.
cat("\n2.2. Критерий Манна–Уитни\n")

mann_whitney_result <- wilcox.test(
  group1, group2,
  alternative = "two.sided",
  paired = FALSE,
  exact = FALSE,
  correct = TRUE
)
print(mann_whitney_result)
# Статистика W в выводе R равна U для первой группы.
cat("U для первой группы:", unname(mann_whitney_result$statistic), "\n")
if (mann_whitney_result$p.value < alpha) {
  cat("H0 отвергается: обнаружено различие по ранговому критерию.\n")
} else {
  cat("Нет оснований отвергнуть H0 по критерию Манна–Уитни.\n")
}
# Тесты проверяют разные гипотезы; значимость не определяет практическую важность.
