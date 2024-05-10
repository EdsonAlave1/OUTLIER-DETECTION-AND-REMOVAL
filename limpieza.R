library(openxlsx)

# Leer los datos desde el archivo Excel
datos <- read.xlsx("Microdatos_2022_061_ESU.xlsx", sheet = 1)

# Borramos la primera columna que tiene ID y la columna de la edad
data <- datos[c(-1, -11)]
boxplot(data)

summary(data)
# Contar los valores faltantes en cada columna
num_na <- colSums(is.na(data))
num_na

# Imprimir el número de valores faltantes por columna
print(num_na)
# Definidata()# Definir una función para calcular la moda
getmode <- function(v) {
  uniqv <- unique(v)
  uniqv[which.max(tabulate(match(v, uniqv)))]
}

# Calcular la moda para cada columna
moda_columnas <- sapply(data, getmode)

# Imputar los valores faltantes con la moda correspondiente
for (i in 1:ncol(data)) {
  data[is.na(data[, i]), i] <- moda_columnas[i]
}

# Identificar valores atípicos (outliers)
outliers <- boxplot(data, plot = FALSE)$out
# Imprimir los valores atípicos
print(outliers)

# Reemplazar valores atípicos con la mediana de cada columna
for (i in 1:ncol(data)) {
  mediana_columna <- median(data[, i], na.rm = TRUE)
  data[data[, i] %in% outliers, i] <- mediana_columna
}

# Verificar los datos imputados
print(data)
num_na <- colSums(is.na(data))
num_na

# Graficar diagramas de caja para los datos imputados
boxplot(data)
outliers <- boxplot(data, plot = FALSE)$out

# Guardar los datos imputados en un nuevo archivo Excel
write.xlsx(data, "datos_imputados_y_limpios.xlsx")






