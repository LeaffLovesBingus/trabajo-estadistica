# Cargamos la base de datos
datos <- read.csv("trabajo-estadistica/DatosGrupo_74.csv")

# Guardamos separamos cada variable
dure <- datos$dure  # Variable independiente
carb <- datos$carb
ferr <- datos$ferr
reco <- datos$reco
moli <- datos$moli

# Item a)

# Creamos una lista de variables
variables <- list(dure = dure, carb = carb, ferr = ferr, moli = moli, reco = reco)

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
