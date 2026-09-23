# запуск проги 
# Rscript labs/lab03/lab03.R

cat("1. Объекты разных типов\n")

decimal_number <- 10.5
integer_number <- 5L
text_value <- "Hello, world!"
logical_value <- TRUE
complex_number <- 2 + 3i
level_factor <- factor(
  c("Низкий", "Средний", "Высокий", "Средний"),
  levels = c("Низкий", "Средний", "Высокий"),
  ordered = TRUE
)

str(decimal_number)
str(integer_number)
str(text_value)
str(logical_value)
str(complex_number)
str(level_factor)
print(levels(level_factor))

cat("\n2. Преобразования типов\n")

number_from_text <- as.numeric("12.5")
text_from_number <- as.character(decimal_number)
integer_from_decimal <- as.integer(decimal_number)
number_from_logical <- as.numeric(logical_value)
text_from_factor <- as.character(level_factor)

str(number_from_text)
str(text_from_number)
str(integer_from_decimal)
str(number_from_logical)
str(text_from_factor)

factor_codes <- as.numeric(level_factor)
print(factor_codes)

cat("\n3. Векторы, матрицы и списки\n")

numeric_vector <- c(10, 20, 30, 40)
character_vector <- c("Анна", "Иван", "Мария")
logical_vector <- c(TRUE, FALSE, TRUE)
print(numeric_vector)
print(character_vector)
print(logical_vector)

number_matrix <- matrix(1:6, nrow = 2, ncol = 3, byrow = TRUE)
print(number_matrix)
print(dim(number_matrix))

mixed_list <- list(
  name = "Анна",
  age = 20L,
  is_student = TRUE,
  scores = c(4, 5, 5)
)
str(mixed_list)
