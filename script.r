# Cargamos la base de datos
datos <- read.csv("trabajo-estadistica/DatosGrupo_74.csv")

# Guardamos separamos cada variable
dure <- datos$dure
carb <- datos$carb
ferr <- datos$ferr
reco <- datos$reco
moli <- datos$moli

# Item a)

# Estudio de datos
summary(datos)
pairs(datos,lower.panel = NULL,col='brown')
install.packages('corrplot')
#library(corrplot)
m_cor <- cor(datos,method='pearson')
corrplot(m_cor,method='color',type='upper',tl.col='black',tl.srt=45,addCoef.col = 'black')

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
plot(carb, dure, xlab="carb", ylab="dure")
carb_dure <- lm(dure~carb)
abline(carb_dure, col='red', lwd=2)
summary(carb_dure)

# Modelo reco-dure
plot(reco, dure, xlab="reco", ylab="dure")
reco_dure <- lm(dure~reco)
abline(reco_dure, col='red', lwd=2)
summary(reco_dure)


# Item c)

# Estudio de los modelos seleccionados
plot(datos$carb,datos$dure)
mean(m1$residuals)
hist(m1$residuals,breaks=25)
qqnorm(m1$residuals)
qqline(m1$residuals)
ks.test(m1$residuals,'pnorm',mean=mean(m1$residuals),sd=sd(m1$residuals))
#install.packages('lmtest')
library(lmtest)
bptest(m1)
dwtest(m1)
plot(datos$reco,datos$dure)
mean(m2$residuals)
hist(m2$residuals,breaks=25)
qqnorm(m2$residuals)
qqline(m2$residuals)
ks.test(m2$residuals,'pnorm',mean=mean(m2$residuals),sd=sd(m2$residuals))
bptest(m2)
dwtest(m2)
summary(m1)
summary(m2)

# Item d)
# Probando el mejor modelo
confint(m1, level = 0.95)
predict(m1, newdata = data.frame(carb=0.3), interval = "prediction", level = 0.95)

