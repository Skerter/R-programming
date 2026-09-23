# запуск проги 
# Rscript labs/lab03/lab03.R

cat("1. Импорт данных\n")

employees <- read.csv(
  "labs/lab03/data.csv",
  header = TRUE,
  sep = ",",
  fileEncoding = "UTF-8",
  stringsAsFactors = FALSE
)
print(employees)
str(employees)

stopifnot(
  all(c("Name", "Age", "Salary", "Department") %in% names(employees)),
  is.numeric(employees$Age),
  is.numeric(employees$Salary)
)

cat("\n2. Сотрудники старше 30 лет с зарплатой больше 50000\n")

selected_employees <- subset(
  employees,
  Age > 30 & Salary > 50000,
  select = c(Name, Age, Salary)
)
print(selected_employees)
cat("Отобрано строк:", nrow(selected_employees), "из", nrow(employees), "\n")

cat("\n3. Экспорт данных\n")

write.csv(
  selected_employees,
  "labs/lab03/filtered_data.csv",
  row.names = FALSE,
  fileEncoding = "UTF-8"
)
cat("Результат сохранён в labs/lab03/filtered_data.csv")
