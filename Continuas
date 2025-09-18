---
title: "Estatura en metros"
author: "Evelin Zavala"
date: "2025-09-17"
output: html_document
---

```{r setup, include=FALSE}
knitr::opts_chunk$set(echo = TRUE)
```

```{r}
estatura <- c(1.57, 1.97, 1.85, 1.78, 1.62, 1.67, 1.92, 1.96, 1.76,
              1.94, 1.85, 1.71, 1.65, 1.98, 1.56, 1.84, 1.91, 1.61, 1.8,
              1.58, 1.7, 1.62, 1.76, 1.84, 1.95, 1.67, 1.62, 1.65, 1.61,
              1.96, 1.63, 1.75, 1.9, 1.92, 1.79, 1.62, 1.79, 1.57, 1.74,
              1.55, 1.84, 1.65, 1.84, 1.79, 1.61, 1.54, 1.89, 1.84, 1.76,
              1.61, 1.59, 1.81, 1.52, 1.61, 1.91, 1.65, 1.51, 1.84, 1.72,
              1.92, 1.91, 1.93, 1.77, 1.62, 1.88, 1.82, 1.96, 1.66, 1.79,
              1.88, 1.78, 1.84, 1.69, 1.95, 1.99, 1.88, 1.51, 1.76, 1.16,
              1.69, 1.72, 1.87, 1.87, 1.66, 1.71, 1.64, 1.81, 1.52, 1.8,
              1.71, 1.52, 2, 1.81, 1.92, 1.7)

```

```{r}
# Número de clases (Sturges)
k <- ceiling(1 + 3.322 * log10(length(estatura)))

# Intervalos de clase
clases <- cut(estatura, breaks = k)

# Frecuencia absoluta
tabla_frecuencia <- table(clases)

# Frecuencia relativa
frecuencia_relativa <- prop.table(tabla_frecuencia) * 100

# Tabla
tabla <- data.frame(
  Intervalo = names(tabla_frecuencia),
  Frecuencia = as.numeric(tabla_frecuencia),
  Porcentaje = round(frecuencia_relativa, 2)
)

tabla
```



```{r}
# Media
media <- mean(estatura)

# Mediana
mediana <- median(estatura)

# Cuartiles
cuartiles <- quantile(estatura, probs = c(0.25, 0.5, 0.75))

# Moda como clase modal
moda_clase <- names(which.max(tabla_frecuencia))

cat("Media:", round(media, 2), "\n")
cat("Mediana:", round(mediana, 2), "\n")
cat("Moda (Clase Modal):", moda_clase, "\n")
cat("Cuartiles:\n")
print(cuartiles)
```

```{r}
hist(estatura,
     breaks = k,
     col = "purple",
     main = "Histograma de Estatura",
     xlab = "Estatura (m)",
     ylab = "Frecuencia")
```

```{r}
# Frecuencia acumulada
frecuencia_acumulada <- cumsum(tabla_frecuencia)

# Puntos medios de clase
puntos_clase <- (as.numeric(sub("\\((.+),.*", "\\1", levels(clases))) +
                 as.numeric(sub(".*,(.+)\\]", "\\1", levels(clases)))) / 2

# Gráfico
plot(puntos_clase, frecuencia_acumulada, type = "o",
     col = "blue", main = "Ojiva de Estatura",
     xlab = "Estatura (m)", ylab = "Frecuencia acumulada")
```


··##EJercicio 2___________________________-


---
title: "Tiempos de translados"
author: "Evelin Zavala"
date: "2025-09-17"
output: html_document
---

```{r setup, include=FALSE}
knitr::opts_chunk$set(echo = TRUE)
```

```{r}
Tiempo_traslado <- c( 1.5, 1.3, 1.1, 0.6, 0.9, 1.4, 0.8, 1.7, 1.7, 1.6, 1.7, 1.3,
 1.5, 0.5, 1.4, 0.6, 1.5, 1.9, 0.7, 1.9, 2, 1.6, 0.7, 1.6, 0.7, 0.5, 0.6, 1.6, 1.4, 1.9, 1.9,
 1, 1.7, 1.7, 1.8, 1.6, 1.4, 1.4, 0.7, 0.8, 1.5, 0.8, 1.9, 1.2, 1.9, 1.8, 1.7, 1.5, 1.5, 0.7,
 1.8, 1.3, 1.8, 0.6, 1.5, 1.4, 1.3, 1.3, 0.8, 1.4, 0.6, 1.1, 1.1, 0.6, 1.7, 0.5, 2, 1.7, 0.7,
 1, 0.6, 1.8, 1.1, 1.6, 1, 1.1, 1.8, 0.8, 1.1, 1.7, 1.4, 1.4, 1.2, 1.5, 1.6, 1.6, 1.5, 0.5,
 1.4, 1.1, 1, 0.9, 0.6, 1.7, 1.6, 1.2, 0.6, 1.2, 1.2, 1.8)
```

```{r}
summary(Tiempo_traslado)
```

```{r}
Tiempo_ord <- sort(Tiempo_traslado); (Tiempo_ord)
cte <- 9
```

```{r}
indices <- 1:10; (indices)
t <- cte*indices; (t)
```

```{r}
mis_deciles <- Tiempo_ord[t]; (mis_deciles)
cuantiles <- quantile(Tiempo_traslado); (cuantiles)
Q1 <- cuantiles[2]; (Q1)
Q2 <- cuantiles[3]; (Q2)
Q3 <- cuantiles[4]; (Q3)
```

```{r}
x_media <- mean(Tiempo_traslado);(x_media)
mi_min <- min(Tiempo_traslado); (mi_min)
mi_max <- max(Tiempo_traslado); (mi_max)
varianza <- var(Tiempo_traslado); (varianza)
DesvEst <- sd(Tiempo_traslado); (DesvEst)
```

```{r}
plot(
  Tiempo_traslado,
  main = "Gráfica Tiempo de Traslado",
  xlab = "Índice de Tiempo",
  ylab = "Numero (Horas)",
  col = "yellow",      
  pch = 19,          
  cex = 0.5,          
  cex.main = 1.5,    
  cex.lab = 1.2,      
  col.main = "red")
legend(
  "topright",                
  legend = c(mi_min,mi_max),    
  pch = 17,
  col=c("green","lightgreen"),
  pt.cex = 1.5,                
  cex = 0.7,
  bty = "n"                    
)
abline(h = x_media, col = "red", lwd = 1, lty = 2)    
abline(h = Q2, col = "yellow", lwd = 1, lty = 3)
abline(h = mis_deciles, col = "blue", lty = 1.5)
points(which.min(Tiempo_traslado), min(Tiempo_traslado), col = "purple", pch = 17, cex = 1.5)
points(which.max(Tiempo_traslado), max(Tiempo_traslado), col = "orange", pch = 17, cex = 1.5)
```

```{r}
nbreaks=10;
miscolores <- rainbow(25,0.85);
h <- hist(Tiempo_traslado, breaks = nbreaks, col= miscolores,
          main = 'Traslados',
          xlab="Tiempo",
          ylab="Frecuencias")
text(h$mids, h$counts, labels = h$counts, pos = 3, cex = 0.4,
     col = "black")
nl <- length(h$counts);
legend(
  "topright", # ubicación
  legend = h$counts, # etiquetas
  fill = miscolores,
  col = miscolores, # colores (uno por cada símbolo)
  pt.cex = 1.5, # tamaño de los símbolos
  cex = 0.7,
  bty = "n" # sin borde en la caja
)
```

```{r}
nbreaks=10;
h2 <- hist(Tiempo_traslado, breaks = nbreaks, col= miscolores,
           main = 'Traslados',
           xlab="Tiempo",
           ylab="Frecuencias",
           ylim=c(0,max(h$counts)*1.5))
text(h$mids, h$counts, labels = h$counts, pos = 3, cex = 0.8,
     col = "black")
nl <- length(h$counts);
legend(
  "topright",                
  legend = h$counts,    
  fill = miscolores,
  col = miscolores,    
  pt.cex = 1.5,              
  cex = 0.5,
  bty = "n"                    
)
```

```{r}
h3 <- hist(Tiempo_traslado,
           breaks = nbreaks,
           col= miscolores,
           main = 'Tiempo de Traslado',
           xlab="Tiempo",
           ylab="Porcentaje",
           ylim=c(0,max(h$density)*1.5),
           probability=TRUE)
porcentajes <- h3$counts/sum(h3$counts)*100

text(h3$mids,
     h3$density,
     labels = paste0(round(porcentajes,1),"%"),
     pos = 3,
     cex = 0.8,
     col = "black")

nl <- length(h3$counts); (nl)
legend(
  "topright",                
  legend = paste0(round(porcentajes,1),"%"),
  fill = miscolores,
  cex = 0.5,
  bty = "n"                  
)
```

```{r}
nbreaks <- 10
intervalos <- cut(Tiempo_traslado, breaks = nbreaks); (intervalos)
tabla <- table(intervalos); (tabla)
porcentajes <- round(prop.table(tabla) * 100, 1); (porcentajes)
pie(tabla,
    main = "Tiempo de Traslados",
    col = rainbow(length(tabla)),
    labels = paste0(porcentajes, "%"))
```

```{r}
##---- Grafica combinada ----
nbreaks <- 10
h <- hist(Tiempo_traslado, breaks = nbreaks, plot = FALSE)
hist(Tiempo_traslado,
     breaks = nbreaks,
     col = "pink",
     main = "Histograma y Ojiva (%)",
     xlab = "Numero (traslados)",
     ylab = "Frecuencia")
freq_acum_pct <- cumsum(h$counts) / sum(h$counts) * 100
x_vals <- h$breaks[-1]
par(new = TRUE)
plot(x_vals, freq_acum_pct,
     type = "o", pch = 19, lwd = 2, col = "red",
     axes = FALSE, xlab = "", ylab = "",
     xlim = range(h$breaks), ylim = c(0, 100))
axis(4)                          
```

··##Ejercicio 3 _____________________________________________________________



---
title: "Temperatura en grados Celsius"
author: "Evelin Zavala"
date: "2025-09-17"
output: html_document
---

```{r setup, include=FALSE}
knitr::opts_chunk$set(echo = TRUE)
```

```{r}
# Datos de temperatura en grados Celsius
temperatura <- c(20.1, 23.1, 30.6, 32.3, 31.7, 5.0, 21.1, 27.4, 21.3, 29.0,
                 19.8, 15.3, 28.7, 25.1, 14.3, 23.3, 28.7, 17.7, 30.3, 26.7,
                 26.2, 11.4, 25.5, 15.3, 28.3, 23.9, 16.2, 26.1, 12.7, 32.8,
                 19.2, 37.2, 10.3, 25.0, 14.9, 30.9, 19.0, 23.6, 24.1, 13.1,
                 17.6, 31.9, 25.7, 13.6, 21.6, 14.4, 29.5, 7.75, 33.7, 35.2)

length(temperatura)  # número de datos
```

```{r}
# Número de clases según la regla de Sturges
k <- ceiling(1 + 3.322 * log10(length(temperatura)))

# Intervalos de clase
clases <- cut(temperatura, breaks = k)

# Frecuencia absoluta
tabla_frecuencia <- table(clases)

# Frecuencia relativa (en %)
frecuencia_relativa <- prop.table(tabla_frecuencia) * 100

# Tabla de distribución de frecuencias
tabla <- data.frame(
  Intervalo = names(tabla_frecuencia),
  Frecuencia = as.numeric(tabla_frecuencia),
  Porcentaje = round(frecuencia_relativa, 2)
)

tabla
```

```{r}
# Medidas de tendencia central
media <- mean(temperatura)
mediana <- median(temperatura)
cuartiles <- quantile(temperatura, probs = c(0.25, 0.5, 0.75))
moda_clase <- names(which.max(tabla_frecuencia))

cat("Media:", round(media, 2), "\n")
cat("Mediana:", round(mediana, 2), "\n")
cat("Moda (Clase Modal):", moda_clase, "\n")
cat("Cuartiles:\n")
print(cuartiles)
```

```{r}
# Histograma
hist(temperatura,
     breaks = k,
     col = "purple",
     main = "Histograma de la Temperatura",
     xlab = "Temperatura (°C)",
     ylab = "Frecuencia")
```

```{r}
# Ojiva
frecuencia_acumulada <- cumsum(tabla_frecuencia)

# Puntos medios de clase
puntos_clase <- (as.numeric(sub("\\((.+),.*", "\\1", levels(clases))) +
                 as.numeric(sub(".*,(.+)\\]", "\\1", levels(clases)))) / 2

plot(puntos_clase, frecuencia_acumulada, type = "o",
     col = "blue",
     main = "Ojiva de la Temperatura",
     xlab = "Temperatura (°C)", ylab = "Frecuencia acumulada")
```


··##Ejercicio 4_______________________________________________


---
title: "Nivel de contaminaciòn"
author: "Evelin Zavala"
date: "2025-09-17"
output: html_document
---

```{r setup, include=FALSE}
knitr::opts_chunk$set(echo = TRUE)
```

```{r}
# Datos de nivel de contaminación en julio
contaminacion <- c(0.7, 0.4, 0.4, 0.5, 0.3, 0.3, 0.7, 0.5, 0.2, 0.2, 0.5,
                   0.8, 0.3, 0.9, 0.5, 0.2, 0.4, 0.4, 0.7, 0.6, 0.8, 0.3,
                   1.0, 0.7, 1.5, 0.1, 0.1, 0.9, 0.1, 0.3, 0.9, 0.3, 0.3,
                   1.2, 1.2, 1.2, 0.6, 0.7, 0.4, 1.8, 0.2, 0.6, 0.7, 1.7,
                   0.4, 0.9, 0.2, 0.8, 1.0, 0.7, 0.6, 0.7, 0.3, 0.5, 0.2,
                   1.2, 0.5, 0.8, 0.2, 0.4, 0.3, 0.4, 0.5, 0.5, 1.4, 0.4)

```

```{r}
# Regla de Sturges para número de clases
k <- ceiling(1 + 3.322 * log10(length(contaminacion)))

# Intervalos de clase
clases <- cut(contaminacion, breaks = k)

# Frecuencia absoluta
tabla_frecuencia <- table(clases)

# Frecuencia relativa (en %)
frecuencia_relativa <- prop.table(tabla_frecuencia) * 100

# Tabla de distribución de frecuencias
tabla <- data.frame(
  Intervalo = names(tabla_frecuencia),
  Frecuencia = as.numeric(tabla_frecuencia),
  Porcentaje = round(frecuencia_relativa, 2)
)

tabla
```

```{r}
# Medidas de tendencia central
media <- mean(contaminacion)
mediana <- median(contaminacion)
cuartiles <- quantile(contaminacion, probs = c(0.25, 0.5, 0.75))
moda_clase <- names(which.max(tabla_frecuencia))

cat("Media:", round(media, 2), "\n")
cat("Mediana:", round(mediana, 2), "\n")
cat("Moda (Clase Modal):", moda_clase, "\n")
cat("Cuartiles:\n")
print(cuartiles)
```

```{r}
# Histograma
hist(contaminacion,
     breaks = k,
     col = "purple",
     main = "Histograma del Nivel de Contaminación",
     xlab = "Nivel de contaminación",
     ylab = "Frecuencia")
```

```{r}
# Ojiva
frecuencia_acumulada <- cumsum(tabla_frecuencia)

# Puntos medios de clase
puntos_clase <- (as.numeric(sub("\\((.+),.*", "\\1", levels(clases))) +
                 as.numeric(sub(".*,(.+)\\]", "\\1", levels(clases)))) / 2

plot(puntos_clase, frecuencia_acumulada, type = "o",
     col = "blue", main = "Ojiva del Nivel de Contaminación",
     xlab = "Nivel de contaminación", ylab = "Frecuencia acumulada")
```


··##Ejercicio 5 ______________________________________··


---
title: "Peso corporal en kilogramos"
author: "Evelin Zavala"
date: "2025-09-17"
output: html_document
---

```{r setup, include=FALSE}
knitr::opts_chunk$set(echo = TRUE)
```

```{r}
peso <- c(100.5, 91.3, 60.4, 83.2, 49.6, 103.1, 60.3, 109.3, 83.5, 63.9,
          106, 50, 47.9, 108.5, 78.9, 82.7, 60.7, 98.7, 85.2, 48.7, 106.7,
          63.9, 84.1, 69.5, 53.3, 108.9, 91.8, 108.6, 54.5, 95.1, 90.6,
          115.9, 88.5, 67.7, 115.1, 108.3, 76.8, 81.4, 102.6, 63.9, 100.6,
          76.3, 113.7, 50.3, 105.8, 81.4, 67.9, 91.3, 68.9, 93.9, 113.7,
          87.7, 92.8, 76.2, 104.7, 109.7, 72.6, 111.2, 86.9, 79.8, 67.5,
          100.1, 94.6, 60.5, 117.1, 45.5, 112.7, 51.7, 107.8, 86.6, 100.5,
          96.7, 64.7, 48, 55.4, 52.9, 58.2, 117.1, 59.6, 69.9, 96.9, 97.6,
          66.5, 67.4, 77.2, 73.7, 113)
```

```{r}
# Regla de Sturges
k <- ceiling(1 + 3.322 * log10(length(peso)))

# Intervalos de clase
clases <- cut(peso, breaks = k)

# Frecuencia absoluta
tabla_frecuencia <- table(clases)

# Frecuencia relativa
frecuencia_relativa <- prop.table(tabla_frecuencia) * 100

# Tabla completa
tabla <- data.frame(
  Intervalo = names(tabla_frecuencia),
  Frecuencia = as.numeric(tabla_frecuencia),
  Porcentaje = round(frecuencia_relativa, 2)
)

tabla
```

```{r}
# Media
media <- mean(peso)

# Mediana
mediana <- median(peso)

# Cuartiles
cuartiles <- quantile(peso, probs = c(0.25, 0.5, 0.75))

# Moda (como clase modal)
moda_clase <- names(which.max(tabla_frecuencia))

cat("Media:", round(media, 2), "\n")
cat("Mediana:", round(mediana, 2), "\n")
cat("Moda (Clase Modal):", moda_clase, "\n")
cat("Cuartiles:\n")
print(cuartiles)
```

```{r}
hist(peso,
     breaks = k,
     col = "purple",
     main = "Histograma del Peso Corporal",
     xlab = "Peso (kg)",
     ylab = "Frecuencia")
```

```{r}
# Frecuencia acumulada
frecuencia_acumulada <- cumsum(tabla_frecuencia)

# Puntos medios de clase
puntos_clase <- (as.numeric(sub("\\((.+),.*", "\\1", levels(clases))) +
                 as.numeric(sub(".*,(.+)\\]", "\\1", levels(clases)))) / 2

# Gráfico de la ojiva
plot(puntos_clase, frecuencia_acumulada, type = "o",
     col = "blue", main = "Ojiva del Peso Corporal",
     xlab = "Peso (kg)", ylab = "Frecuencia acumulada")
```
