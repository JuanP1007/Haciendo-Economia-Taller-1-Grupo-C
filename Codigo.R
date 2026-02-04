
library(haven)
library(dplyr)
library(ggplot2)

base <- read_dta("D:/Jose Luis/Descargas/TenderosFU03_Publica.dta")

#grafica pregunta 

grafica <- base %>%
  filter(Solic_Credito == 1) %>%
  filter(!is.na(Pres_Bancario)) %>%
  count(Pres_Bancario)

ggplot(grafica, aes(x = factor(Pres_Bancario), y = n, fill = factor(Pres_Bancario))) +
  geom_col() +
  labs(
    title = "Resultado de la solicitud de crédito bancario",
    subtitle = "Micronegocios que solicitaron crédito - últimos 12 meses",
    x = "",
    y = "Número de micronegocios",
    fill = ""
  ) +
  scale_fill_manual(
    values = c("#1FA6A6", "#4DB6AC", "#80CBC4", "#B2DFDB"),
    labels = c(
      "Sí, me lo otorgaron y lo acepté",
      "Sí, pero me lo negaron",
      "Sí, me lo otorgaron pero no lo utilicé",
      "No"
    )
  ) +
  theme_minimal()


#grafica pregunta 2

frecuencias <- c(
  sum(base$hipoteticaFunding__1 == 1, na.rm = TRUE),
  sum(base$hipoteticaFunding__2 == 1, na.rm = TRUE),
  sum(base$hipoteticaFunding__3 == 1, na.rm = TRUE),
  sum(base$hipoteticaFunding__4 == 1, na.rm = TRUE),
  sum(base$hipoteticaFunding__5 == 1, na.rm = TRUE),
  sum(base$hipoteticaFunding__6 == 1, na.rm = TRUE)
)

tipo <- c(
  Alta_conexion = frecuencias[1] + frecuencias[2],
  Relacional   = frecuencias[3] + frecuencias[6],
  Informal     = frecuencias[5],
  No_invertir  = frecuencias[4]
)

porc <- round(100 * tipo / sum(tipo), 1)

labels_porc <- paste0(names(tipo), "\n", porc, "%")

colores <- hcl.colors(4, "Teal")

par(mar = c(2, 2, 2, 8))

pie(tipo,
    col = colores,
    labels = NA,
    border = "white",
    radius = 0.8)

symbols(0, 0, circles = 0.50, inches = FALSE, add = TRUE,
        fg = "white", bg = "white")

legend("right",
       inset = -0.60,
       legend = paste0(names(tipo), " (", porc, "%)"),
       fill = colores,
       bty = "n",
       cex = 0.8,
       xpd = TRUE)

