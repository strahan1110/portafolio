#######################################################################
####################    TRABAJO JEMIMA JANGE    #######################
#######################################################################

#PRIMERO INSTALAMOS TODAS LAS LIBRERÍAS
install.packages(c("readr", "tidyverse"))

#CARGAMOS LAS LIBRERÍAS
library(readr)
library(tidyverse)

#1. Descargue el archivo y lea su contenido en un data.frame.

#CARGAMOS LA BASE DE DATOS
games <- read_csv("games.csv")

#PROCEDEMOS CON LA OBSERVACIÓN DE LA DATA
view(games)
head(games)

#2. Despliegue el tipo de datos reconocido en cada variable del archivo.
#   Transforme en factores aquellas variables que considere lo ameritan.

#TIPO DE DATOS PRESENTES
str(games)

#TRANSFORMACIÓN DE VARIABLES
#PODEMOS VER LA PRESENCIA DE VARIABLES CATEGORICAS CHR Y LGL
#chr  (2): title, rating
#dbl  (6): app_id, positive_ratio, user_reviews, price_final, price_original, ...
#lgl  (4): win, mac, linux, steam_deck
#date (1): date_release

games$win <- as.factor(games$win)
games$mac <- as.factor(games$mac)
games$linux <- as.factor(games$linux)
games$steam_deck <- as.factor(games$steam_deck)

games$title <- as.factor(games$title)
games$rating <- as.factor(games$rating)

#3. Calcule la media, la varianza, la desviación estándar, el máximo, el mínimo, la
#   mediana y los cuartiles de cada variable numérica.
#   Comente sus hallazgos.

# Estadística descriptiva general
summary(games)

#Variables numericas app_id, positive_ratio, user_reviews, price_final,
#price_original, discount / NO ANALIZAREMOS app_id ya que es el codigo
#de cada app del juego / A continuación, los análisis por variable ->

# positive_ratio
media_positive_ratio <- mean(games$positive_ratio, na.rm = TRUE)
varianza_positive_ratio <- var(games$positive_ratio, na.rm = TRUE)
desviacion_estandar_positive_ratio <- sd(games$positive_ratio, na.rm = TRUE)
maximo_positive_ratio <- max(games$positive_ratio, na.rm = TRUE)
minimo_positive_ratio <- min(games$positive_ratio, na.rm = TRUE)
mediana_positive_ratio <- median(games$positive_ratio, na.rm = TRUE)
cuartiles_positive_ratio <- quantile(games$positive_ratio, probs = c(0, 0.25, 0.5, 0.75, 1), na.rm = TRUE)

# user_reviews
media_user_reviews <- mean(games$user_reviews, na.rm = TRUE)
varianza_user_reviews <- var(games$user_reviews, na.rm = TRUE)
desviacion_estandar_user_reviews <- sd(games$user_reviews, na.rm = TRUE)
maximo_user_reviews <- max(games$user_reviews, na.rm = TRUE)
minimo_user_reviews <- min(games$user_reviews, na.rm = TRUE)
mediana_user_reviews <- median(games$user_reviews, na.rm = TRUE)
cuartiles_user_reviews <- quantile(games$user_reviews, probs = c(0, 0.25, 0.5, 0.75, 1), na.rm = TRUE)

# price_final
media_price_final <- mean(games$price_final, na.rm = TRUE)
varianza_price_final <- var(games$price_final, na.rm = TRUE)
desviacion_estandar_price_final <- sd(games$price_final, na.rm = TRUE)
maximo_price_final <- max(games$price_final, na.rm = TRUE)
minimo_price_final <- min(games$price_final, na.rm = TRUE)
mediana_price_final <- median(games$price_final, na.rm = TRUE)
cuartiles_price_final <- quantile(games$price_final, probs = c(0, 0.25, 0.5, 0.75, 1), na.rm = TRUE)

# price_original
media_price_original <- mean(games$price_original, na.rm = TRUE)
varianza_price_original <- var(games$price_original, na.rm = TRUE)
desviacion_estandar_price_original <- sd(games$price_original, na.rm = TRUE)
maximo_price_original <- max(games$price_original, na.rm = TRUE)
minimo_price_original <- min(games$price_original, na.rm = TRUE)
mediana_price_original <- median(games$price_original, na.rm = TRUE)
cuartiles_price_original <- quantile(games$price_original, probs = c(0, 0.25, 0.5, 0.75, 1), na.rm = TRUE)

# discount
media_discount <- mean(games$discount, na.rm = TRUE)
varianza_discount <- var(games$discount, na.rm = TRUE)
desviacion_estandar_discount <- sd(games$discount, na.rm = TRUE)
maximo_discount <- max(games$discount, na.rm = TRUE)
minimo_discount <- min(games$discount, na.rm = TRUE)
mediana_discount <- median(games$discount, na.rm = TRUE)
cuartiles_discount <- quantile(games$discount, probs = c(0, 0.25, 0.5, 0.75, 1), na.rm = TRUE)

#Cabe resaltar que el uso de na.rm=TRUE hace referencia a
#la función de calcular estadísticas descriptivas
#utilizando solo los valores no faltantes,
#mientras que na.rm = FALSE calcula estadísticas descriptivas
#que pueden incluir valores NA si están presentes en los datos.

########COMENTARIOS###########

#La presencia de ofertas de descuento es muy comun en el mundo de
#la venta de videojuegos, los usuarios pudieron ser capaces de conseguir
#una oferta hasta con el 90% de descuento

#El costo maximo de un videojuego no supera los 299.99 dólares, en parte
#podemos ver que es un costo elevado, pero que de alguna forma puede
#tener un límite claro y no sobrepasará ese monto.

#Un precio final aproximado de venta
#repetito y usual es de $8, valor tentativo de compra que 
#puede ser aprovechado por nuevos lanzamientos de videojuegos
#vendiendo productos con precios cómodos y accesibles para el
#jugador común que suele comprar.

#4. Calcule la moda de cada variable no numérica.
#   Comente sus hallazgos.

moda_title <- names(sort(-table(games$title)))[1]
moda_date_release <- names(sort(-table(games$date_release)))[1]
moda_win <- names(sort(-table(games$win)))[1]
moda_mac <- names(sort(-table(games$mac)))[1]
moda_linux <- names(sort(-table(games$linux)))[1]
moda_rating <- names(sort(-table(games$rating)))[1]
moda_steam_deck <- names(sort(-table(games$steam_deck)))[1]

########COMENTARIOS###########

#Hay una preferencia por dejar una calificación de rating
#como POSITIVE, término de satisfacción prometedor, concentrando
#un público que disfruta el software del videojuego

#Instalaciones en sistemas operativos LINUX y el OS de MAC suelen
#no ser de las más comunes como WINDOWS. El rey de la compatibilidad
#es WINDOWS

#La mayoría de juegos pueden ser ejecutados en un STEAM_DECK
#existe compatibilidad

#Varios juegos fueron estrenados la fecha siguiente (2020-06-18)
#hubo coincidencia de fechas. Solo hay precedencia que durante
#esa temporada atravesábamos mundialmente el COVID

#5. Una de las variables del archivo se denomina price_final.
#Contiene el precio de cada juego de la tabla.
#Cree una nueva variable llamada price_cat que contenga las categorías “bajo”, “medio” y “alto”
#en términos de si el precio pertenece al cuartil 1, al 2 o 3, y al 4,
#respectivamente. Asegúrese de que dicha variable es entendida como un factor.

#LLAMAMOS A cuartiles_price_final

# Creamos las categorias
etiquetas <- c("bajo", "medio", "medio", "alto")

# Creamos la variable price_cat
games$price_cat <- cut(games$price_final, breaks = cuartiles_price_final, labels = etiquetas)

# Convertimos la variable en factor
games$price_cat <- factor(games$price_cat)


#6. Elabore y presente tablas de frecuencia apropiadas para las variables win, mac
#y linux. Comente sus hallazgos

table_win <- table(games$win)
table_mac <- table(games$mac)
table_linux <- table(games$linux)

# Tabla de frecuencia para la variable 'win'
print("Tabla de Frecuencia para la variable 'win':")
print(table_win)

# Tabla de frecuencia para la variable 'mac'
print("Tabla de Frecuencia para la variable 'mac':")
print(table_mac)

# Tabla de frecuencia para la variable 'linux'
print("Tabla de Frecuencia para la variable 'linux':")
print(table_linux)

########COMENTARIOS###########

#Windows resalta por su compatibilidad, su incompatibilidad es mínima.
#Linux y Mac tienen compatibilidad, pero su incompatibilidad supera
#a la compatibilidad. Importante medida de asegurar la mejor experiencia
#para el usuario en sus sistemas operativos está pendiente, junto al desarrollo
#óptimo de los desarrolladores de videojuegos para lograr compatibilidad.

#7. Elabore y presente tablas de frecuencia apropiadas para las variables
#positive_ratio, user_reviews y price_final. Comente sus hallazgos.

table_positive_ratio <- table(games$positive_ratio)
table_user_reviews <- table(games$user_reviews)
table_price_final <- table(games$price_final)

# Tabla de frecuencia para la variable 'positive_ratio' #
print("Tabla de Frecuencia para la variable 'positive_ratio':")
print(table_positive_ratio)
print(max(table_positive_ratio))

# Tabla de frecuencia para la variable 'user_reviews' #
print("Tabla de Frecuencia para la variable 'user_reviews':")
print(table_user_reviews)
print(max(table_user_reviews))

# Tabla de frecuencia para la variable 'price_final' #
print("Tabla de Frecuencia para la variable 'price_final':")
print(table_price_final)
print(max(table_price_final))

########COMENTARIOS###########
#Se observa que una suma consiste de videojuegos se ofrecen GRATIS
#más de 9000 juegos están libres al acceso de cualquier usuario.

#Menos de 50 juegos poseen más de 1000 reviews comentadas por los usuarios

#Calificación perfecta en el positive ratio es evidente
#en 3635 juegos, una gran suma, demostrando así, que lograr
#calificaciones perfectas es una realidad alcanzable

#8. Para las variables mencionadas en 7 y 8, elabore y despliegue gráficos
#apropiados que ilustren sus valores.

barplot(table_win, main = "Frecuencia de 'win'", xlab = "Valor", ylab = "Frecuencia")
barplot(table_mac, main = "Frecuencia de 'mac'", xlab = "Valor", ylab = "Frecuencia")
barplot(table_linux, main = "Frecuencia de 'linux'", xlab = "Valor", ylab = "Frecuencia")
barplot(table_positive_ratio, main = "Frecuencia de 'positive_ratio'", xlab = "Valor", ylab = "Frecuencia")
barplot(table_user_reviews, main = "Frecuencia de 'user_reviews'", xlab = "Valor", ylab = "Frecuencia")
barplot(table_price_final, main = "Frecuencia de 'price_final'", xlab = "Valor", ylab = "Frecuencia")

#9. Considerando la nueva variable creada price_cat y la variable
# positive_ratio, elabore y despliegue un gráfico de cajas que le permita
# comentar entre la relación del precio del juego con la opinión de los jugadores
# consultados.

boxplot(positive_ratio ~ price_cat, data = games, main = "Relación de 'positive_ratio' con 'price_cat'", xlab = "price_cat", ylab = "positive_ratio")

#10. Elabore y despliegue una serie de tiempo del precio (price_final) con
# relación a la fecha de lanzamiento (date_release). Comente sus hallazgos.

games$date_release <- as.Date(games$date_release)
plot(games$date_release, games$price_final, type = "l", main = "PRECIO FINAL SEGUN FECHAS", xlab = "FECHA DE LANZAMIENTO", ylab = "Precio final")

########COMENTARIOS###########
#Entre el 2013/2015 se observan picos de ventas de alto precio, caso
#parecido observado durante la pandemia.

#Los últimos 3 ańos han reflejado bajas de ventas en juegos de alto precio
#es probable que la gente opte por comprar ofertas de juegos y precios
#accesibles.

