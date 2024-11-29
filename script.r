# Cargamos la base de datos
datos <- read.csv("trabajo-estadistica/DatosGrupo_74.csv")

# Guardamos separamos cada variable
dure <- datos$dure  # Variable independiente
carb <- datos$carb
ferr <- datos$ferr
reco <- datos$reco
moli <- datos$moli

# Diagrama de dispersión para carbono
plot(dure, carb, xlab="Dureza del acero", ylab="Contenido de carbono")
# Modelo lineal
modcarb <- lm(carb~dure)
abline(modcarb, col="blue")
# Coeficiente de pearson
cor(dure, carb)
modcarb

# Diagrama de dispersión para ferrita
plot(dure, ferr, xlab="Dureza del acero", ylab="Contenido de ferrita")
# Modelo lineal
modferr <- lm(ferr~dure)
abline(modferr, col="blue")
# Coeficiente de pearson
cor(dure, ferr)
modferr

# Diagrama de dispersión para molibdeno
plot(dure, moli, xlab="Dureza del acero", ylab="Contenido de molibdeno")
# Modelo lineal
modmoli <- lm(moli~dure)
abline(modmoli, col="blue")
# Coeficiente de pearson
cor(dure, moli)
modmoli

# Diagrama de dispersión para la tasa de recocido
plot(dure, reco, xlab="Dureza del acero", ylab="Tasa de recocido")
# Modelo lineal
modreco <- lm(reco~dure)
abline(modreco, col="blue")
# Coeficiente de pearson
cor(dure, reco)
modreco
