library(dplyr)
library(ggplot2)

datos <- read_csv('supermarket_db.csv')
datos <- datos %>%
  mutate(isMemeber = ifelse(datos$`Customer type` == "Member", TRUE, FALSE))

ggplot(datos, aes(x = isMemeber, y = Total, fill = isMemeber)) +
  geom_bar(stat = "summary", fun = "mean") +
  labs(title = "Relación entre Membresía y Ventas",
       x = "Es Miembro", y = "Total de Ventas")

plot(datos$Total, datos$isMemeber, col="red", pch=20, ylab= "Total")
