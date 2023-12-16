install.packages("readr")
install.packages("corrplot")
library(readr)
library(ggplot2)
library(dplyr)
library(corrplot)
library(caret)


# Leer la base de datos en local usando el paquete readr
df <- read_csv('supermarket_db.csv')

#head(df)
summary(df)
print(dim(df))

# Obtener los valores vacios por cada columna
missing_values <- colSums(is.na(df))
print(missing_values)
#no hay columnas vacias en el dataset :D


## Visualizacion de datos.

# Compradores segmentados por genero (Female-o-Male)
gender_counts <- table(df$Gender)
print(gender_counts)
ggplot(df, aes(x = Gender, fill = Gender)) +
  geom_bar() +
  geom_text(stat = 'count', aes(label = ..count..), vjust = -0.5) +
  scale_fill_manual(values = c("Male" = "blue", "Female" = "pink")) +
  labs(title = "Compradores por genero")
# asisten al mercado mas mujeres que hombres, solo por 2 muestras...

##graficamos por genero la linea de productos en la que gastan mas las mujeres y los hombres
df_female <- df %>%
  filter(Gender == "Female")

# Crear el gráfico de barras apiladas para la cantidad de productos cpmprados por categoria en mujeres
ggplot(df_female, aes(x = `Product line`, y = Quantity, fill = Gender)) +
  geom_bar(stat = "identity") +
  labs(title = "Cantidad Comprada por Mujeres en Cada Línea de Producto", x = "Línea de Producto", y = "Cantidad") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  scale_fill_manual(values = c("Male" = "blue", "Female" = "pink"))

# ahora para los hombres
df_male <- filter(df, Gender == "Male")
ggplot(df_male, aes(x = `Product line`, y = Quantity, fill = Gender)) +
  geom_bar(stat = "identity") +
  labs(title = "Cantidad Comprada por Hombres en Cada Línea de Producto", x = "Línea de Producto", y = "Cantidad") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  scale_fill_manual(values = c("Male" = "blue", "Female" = "pink")) 

# Clientes por ciudad
place_df <- df %>%
  group_by(City) %>%
  summarise(CustomerCount = n())
print(place_df)
# no se grafica porque los resultados son todos muy parecidos y no demuestra nada relevante que no se aprecie con solo ver las cantidades

## Tipos de pago
payment_df <- df %>%
  group_by(Payment) %>%
  summarise(Count = n())
print(payment_df)

ggplot(payment_df, aes(x = Payment, y = Count, fill = Payment, label = Count)) +
  geom_bar(stat = "identity") +
  geom_text(position = position_stack(vjust = 0.5), size = 3) +
  labs(title = "Cantidad de Transacciones por Tipo de Pago", x = "Tipo de Pago", y = "Cantidad") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  scale_fill_manual(values = c("Cash" = "blue", "Credit card" = "green", "Ewallet" = "orange"))

## Vizualizar que linea de produccion tiene mas ingreso bruto
ggplot(df, aes(x = `Product line`, y = `gross income`)) +
  geom_bar(stat = "identity", fill = "blue") +
  labs(title = "Ingreso Bruto por Línea de Producto", x = "Línea de Producto", y = "Ingreso Bruto") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))


# grafico de barra para ver cual linea de producto tiene mas rating
ggplot(df, aes(x = Rating, y = `Product line`)) +
  geom_bar(stat = "identity", fill = "blue") +
  labs(title = "Calificación por Línea de Producto", x = "Rating", y = "Product Line") +
  theme_minimal()


#Mapa de calor para las variables numericas
numeric_df <- df[, sapply(df, is.numeric)]

# Elimina las columnas con desviación estándar igual a cero
numeric_df <- numeric_df[, apply(numeric_df, 2, sd) > 0]

# Calcula la matriz de correlación
cor_matrix <- cor(numeric_df)

# Crea el mapa de calor de correlación
corrplot(cor_matrix, method = "color", type = "upper", order = "hclust", tl.col = "black", tl.srt = 45)


#--------------------Regresion Linal---------------------------------
# Convertir la columna Date a formato de fecha
df$Date <- as.Date(df$Date, format = "%m/%d/%Y")

# Visualizar el patrón de ventas a lo largo del tiempo
ggplot(df, aes(x = Date, y = Total)) +
  geom_line() +
  labs(title = "Patrón de Ventas Diarias", x = "Fecha", y = "Total Ventas") +
  theme_minimal()

# Crear una nueva columna con el día del mes
df$Day <- as.numeric(format(df$Date, "%d"))

# Realizar la regresión lineal
modelo <- lm(Total ~ Day, data = df)

# Imprimir resumen del modelo
summary(modelo)

# Crear un nuevo dataframe con los días para los cuales deseas hacer predicciones
nuevos_datos <- data.frame(Day = 1:31)  # Cambia según el rango de días que necesites

# Hacer predicciones
predicciones <- predict(modelo, nuevos_datos)

# Visualizar las predicciones
plot(df$Day, df$Total, pch = 16, col = "black", xlab = "Día", ylab = "Ventas Diarias", main = "Ventas Diarias y Predicciones")
lines(nuevos_datos$Day, predicciones, col = "blue", type = "b")
legend("topright", legend = c("Ventas reales", "Predicciones"), col = c("black", "blue"), pch = c(16, 1))

###### Otro intento
# Cargar librerías necesarias

# Suponiendo que tu dataframe se llama df
correlation_matrix <- cor(df[, c("Total", "Unit price", "Quantity", "Tax 5%", "cogs", "gross margin percentage", "gross income", "Rating")])

# Visualizar la matriz de correlación
print(correlation_matrix)

# Modelo de regresión lineal
modelo <- lm(Total ~ `Unit price` + Quantity + `Tax 5%`, data = df)

# Imprimir resumen del modelo
summary(modelo)

### uso del modelo para prediccion lineal
columnas_prediccion <- df[c("Unit price", "Quantity", "Tax 5%")]
# Crear una nueva base de datos con las columnas seleccionadas
nueva_data <- data.frame(
  `Unit price` = columnas_prediccion$`Unit price`,
  Quantity = columnas_prediccion$Quantity,
  `Tax 5%` = columnas_prediccion$`Tax 5%`
)


#### utilizamos caret para dividir el modelo en parte de entrenamiento y prueba
set.seed(123)
index_entrenamiento <- sample(1:nrow(df), 0.7 * nrow(df))
datos_entrenamiento <- df[index_entrenamiento, ]



# El 30% restante se utiliza para prueba
datos_prueba <- df[-index_entrenamiento, ]
modelo_entrenamiento <- lm(Total ~ `Unit price` + Quantity + `Tax 5%`, data = datos_entrenamiento)
predicciones_prueba <- predict(modelo_entrenamiento, newdata = datos_prueba)
summary(modelo_entrenamiento)





#--------------------Regresion Linal---------------------------------


modelo1 <- lm(Total~df$`Unit price` + Quantity + Rating, data=df, na.action = na.exclude)
summary(modelo1)

### graficando modelo de regresion lineal

grafica1 <- ggplot(df, aes(`Rating`, Total))
grafica1 + geom_point()
#pendiente de regresion lineal
grafica1 + geom_point() + geom_smooth(method = "lm", colour="red")
    
