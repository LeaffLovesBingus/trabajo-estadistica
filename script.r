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
# Diagrama de dispersión para ferrita
plot(dure, ferr, xlab="Dureza del acero", ylab="Contenido de ferrita")
# Diagrama de dispersión para molibdeno
plot(dure, moli, xlab="Dureza del acero", ylab="Contenido de molibdeno")
# Diagrama de dispersión para la tasa de recocido
plot(dure, reco, xlab="Dureza del acero", ylab="Tasa de recocido")
