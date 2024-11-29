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
mod1 <- lm(carb~dure)
abline(mod1, col="blue")

# Diagrama de dispersión para ferrita
plot(dure, ferr, xlab="Dureza del acero", ylab="Contenido de ferrita")
mod2 <- lm(ferr~dure)
abline(mod2, col="blue")

# Diagrama de dispersión para molibdeno
plot(dure, moli, xlab="Dureza del acero", ylab="Contenido de molibdeno")
mod3 <- lm(moli~dure)
abline(mod3, col="blue")

# Diagrama de dispersión para la tasa de recocido
plot(dure, reco, xlab="Dureza del acero", ylab="Tasa de recocido")
mod4 <- lm(reco~dure)
abline(mod4, col="blue")
