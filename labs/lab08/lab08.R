# запуск проги Rscript --vanilla labs/lab08/lab08.R

cat("1. Построение временного ряда\n")

# Данные не заданы; выбран учебный набор AirPassengers за 1949–1960 годы.
# Ежемесячное число международных авиапассажиров измеряется в тысячах.
ts_data <- ts(
  as.numeric(datasets::AirPassengers),
  start = c(1949, 1),
  frequency = 12
)
print(ts_data)
cat("Число наблюдений:", length(ts_data), "\n")
cat("Частота наблюдений в год:", frequency(ts_data), "\n")
stopifnot(!anyNA(ts_data), all(is.finite(ts_data)))

png("labs/lab08/time_series.png", width = 1400, height = 700, res = 120)
plot(
  ts_data, main = "Международные авиаперевозки, 1949–1960",
  xlab = "Год", ylab = "Пассажиры, тыс. человек", col = "steelblue", lwd = 2
)
invisible(dev.off())
cat("График сохранён в labs/lab08/time_series.png\n")

cat("\n2. Сглаживание временного ряда: MA и EWMA\n")

# MA: среднее текущего и четырёх предыдущих месяцев (окно направлено назад).
# Первые четыре значения NA: для полного окна ещё недостаточно наблюдений.
window_size <- 5
ts_ma <- stats::filter(
  ts_data, filter = rep(1 / window_size, window_size), sides = 1
)

# EWMA: S[t] = alpha * x[t] + (1 - alpha) * S[t - 1].
# Выбран alpha = 2 / (5 + 1); начальное сглаженное значение равно первому наблюдению.
alpha <- 2 / (window_size + 1)
ts_ewma <- ts_data
for (i in 2:length(ts_data)) {
  ts_ewma[i] <- alpha * ts_data[i] + (1 - alpha) * ts_ewma[i - 1]
}

comparison <- data.frame(
  month = format(
    seq(as.Date("1949-01-01"), by = "month", length.out = length(ts_data)),
    "%Y-%m"
  ),
  original = as.numeric(ts_data),
  MA = as.numeric(ts_ma),
  EWMA = as.numeric(ts_ewma)
)
cat("Первые 12 месяцев:\n")
print(head(comparison, 12), row.names = FALSE)
cat("Окно MA:", window_size, "месяцев; alpha EWMA:", alpha, "\n")

png("labs/lab08/smoothing.png", width = 1400, height = 1000, res = 120)
par(mfrow = c(2, 1), mar = c(4, 5, 3, 1))
plot(
  ts_data, main = "Скользящее среднее: MA, окно 5 месяцев",
  xlab = "Год", ylab = "Пассажиры, тыс. человек", col = "grey60"
)
lines(ts_ma, col = "firebrick", lwd = 2)
legend(
  "topleft", legend = c("Исходный ряд", "MA"),
  col = c("grey60", "firebrick"), lty = 1, lwd = c(1, 2), bty = "n"
)
plot(
  ts_data, main = "Экспоненциальное сглаживание: EWMA, alpha = 1/3",
  xlab = "Год", ylab = "Пассажиры, тыс. человек", col = "grey60"
)
lines(ts_ewma, col = "darkgreen", lwd = 2)
legend(
  "topleft", legend = c("Исходный ряд", "EWMA"),
  col = c("grey60", "darkgreen"), lty = 1, lwd = c(1, 2), bty = "n"
)
invisible(dev.off())
cat("Графики сохранены в labs/lab08/smoothing.png\n")
cat("В ряду видны рост и повторяющиеся сезонные колебания.\n")
cat("MA и EWMA сглаживают колебания и запаздывают при изменении уровня ряда.\n")
cat("MA даёт одинаковый вес пяти месяцам, EWMA — больший вес недавним данным.\n")
cat("Сглаживание не устраняет сезонность полностью и само по себе не является прогнозом.\n")
