# Cargamos la base de datos
datos <- read.csv("trabajo-estadistica/DatosGrupo_74.csv")

# Guardamos separamos cada variable
dure <- datos$dure
carb <- datos$carb
ferr <- datos$ferr
reco <- datos$reco
moli <- datos$moli

# Item a)

# Creamos una lista de variables
variables <- list(carb = carb, ferr = ferr, moli = moli, reco = reco, dure = dure)

# Creamos una matriz de pares de variables para analizar las combinaciones
combinaciones <- combn(names(variables), 2, simplify = FALSE)

# Nuestros resultados irán acá
resultados <- data.frame(Var1 = character(0), Var2 = character(0),
                Correlacion = numeric(0), Interpretacion = character(0))

for (combinacion in combinaciones) {
  # Extraer las dos variables para la combinación actual
  var1 <- variables[[combinacion[1]]]
  var2 <- variables[[combinacion[2]]]
  
  # Gráfico de dispersión
  plot(var1, var2, xlab = combinacion[1], ylab = combinacion[2],
       main = paste("Dispersión entre", combinacion[1], "y", combinacion[2]))
  
  # Calculamos el coeficiente de correlación de Pearson
  cor_pearson <- cor(var1, var2)
  
  # Guardamos el resultado
  resultados <- rbind(resultados, data.frame(Var1 = combinacion[1], Var2 = combinacion[2],
        Correlacion = cor_pearson, Interpretacion = ifelse(abs(cor_pearson) > 0.8, "Altamente correlacionados",
        ifelse(abs(cor_pearson) > 0.5, "Moderadamente correlacionados", "Baja correlación"))))
}
print(resultados)

# Item b)

# Modelo carb-dure
plot(carb, dure, xlab="carb", ylab="dure", main="Modelo de regresión lineal de carb-dure")
carb_dure <- lm(dure~carb)
abline(carb_dure,col="blue")
summary(carb_dure)

# Modelo reco-dure
plot(reco, dure, xlab="reco", ylab="dure", main="Modelo de regresión lineal de reco-dure")
reco_dure <- lm(dure~reco)
abline(reco_dure, col="blue")
summary(reco_dure)


# Item c)
# i)
# Test para carb-dure
# Linealidad
plot(carb_dure, wich=1, main="Valores ajustados vs residuales")

# Independencia de las observaciones
plot(residuals(carb_dure), type="o", main="Grafico de los residuos vs tiempo")

# Normalidad de los residuos
qqnorm(residuals(carb_dure), main="holamundo")
qqline(residuals(carb_dure), col="red")

# Shapiro-Wilk
shapiro.test(residuals(carb_dure))

# Test para reco-dure
# Linealidad
plot(reco_dure, wich=1)

# Independencia de las observaciones
plot(residuals(reco_dure), type="o", main="Grafico de los residuos vs tiempo")

# Normalidad de los residuos
qqnorm(residuals(reco_dure))
qqline(residuals(reco_dure), col="red")

# Shapiro-Wilk
shapiro.test(residuals(reco_dure))

