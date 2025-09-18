---
title: "Accidentes en una vialidad"
author: "Evelin Zavala"
date: "2025-09-17"
output: html_document
---

```{r setup, include=FALSE}
knitr::opts_chunk$set(echo = TRUE)
```

```{r}
Accidentes_vialidad <- c( 22, 24, 35, 8, 12, 33, 17, 31, 24, 12, 6, 14, 26, 8, 41,
          3, 15, 7, 3, 23, 10, 17, 3, 7, 1, 11, 19, 4, 33, 7, 10, 2, 2, 19, 1, 15, 7, 16, 13, 3, 26,
          4, 16, 18, 27, 18, 17, 32, 25, 22, 34, 40, 47, 35, 19, 27, 12, 44, 37, 27, 40, 18, 28,
          16, 31, 37, 32, 44, 29, 35, 25, 28, 27, 31, 33, 21, 41, 20, 39, 28, 22, 34, 27, 28, 37,
          26, 29, 37, 38, 26, 38, 53, 39, 41, 35, 57, 27, 41, 41, 72, 53, 64, 67, 46, 44, 31, 47,
          56, 56, 63, 47, 58, 44, 32, 70, 53, 59, 51, 70, 44, 51, 52, 46, 44, 46, 41, 54, 63, 46,
          47, 37, 60, 34, 49, 63, 42, 48, 44, 65, 63, 45, 63, 42, 58, 43, 54, 90, 55, 57, 54, 55,
          46, 46, 46, 55, 23, 39, 35, 44, 58, 68, 44, 11, 23, 24, 13, 47, 49.)
```

```{r}
summary(Accidentes_vialidad)
```

```{r}
accidentes_ord <- sort(Accidentes_vialidad); (accidentes_ord)
cte <- 9
```

```{r}
indices <- 1:10; (indices)
t <- cte*indices; (t)
```

```{r}
mis_deciles <- accidentes_ord[t]; (mis_deciles)
cuantiles <- quantile(Accidentes_vialidad); (cuantiles)
Q1 <- cuantiles[2]; (Q1)
Q2 <- cuantiles[3]; (Q2)
Q3 <- cuantiles[4]; (Q3)
```

```{r}
x_media <- mean(Accidentes_vialidad);(x_media)
mi_min <- min(Accidentes_vialidad); (mi_min)
mi_max <- max(Accidentes_vialidad); (mi_max)
varianza <- var(Accidentes_vialidad); (varianza) 
DesvEst <- sd(Accidentes_vialidad); (DesvEst)
```

```{r}
plot(
  Accidentes_vialidad,
  main = "Gráfica Accidentes",
  xlab = "Índice de Accidentes",
  ylab = "Numero (accidentes)",
  col = "blue",       
  pch = 19,           
  cex = 0.5,          
  cex.main = 1.5,     
  cex.lab = 1.2,      
  col.main = "red")
legend(
  "topright",                 
  legend = c(mi_min,mi_max),    
  pch = 17,
  col=c("purple","orange"),
  pt.cex = 1.5,                
  cex = 0.7,
  bty = "n"                    
)
abline(h = x_media, col = "red", lwd = 1, lty = 2)     
abline(h = Q2, col = "lightgreen", lwd = 1, lty = 3) 
abline(h = mis_deciles, col = "lightgray", lty = 1.5)
points(which.min(Accidentes_vialidad), min(Accidentes_vialidad), col = "purple", pch = 17, cex = 1.5)
points(which.max(Accidentes_vialidad), max(Accidentes_vialidad), col = "orange", pch = 17, cex = 1.5)
```

```{r}
nbreaks=10;
miscolores <- rainbow(25,0.85);
h <- hist(Accidentes_vialidad, breaks = nbreaks, col= miscolores, 
          main = 'Numero de Accidentes',
          xlab="Accidentes",
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
h2 <- hist(Accidentes_vialidad, breaks = nbreaks, col= miscolores, 
           main = 'Numero de Accidentes',
           xlab="Accidentes",
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
h3 <- hist(Accidentes_vialidad, 
           breaks = nbreaks, 
           col= miscolores, 
           main = 'Accidentes de Vialidad',
           xlab="Numero accidentes",
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
intervalos <- cut(Accidentes_vialidad, breaks = nbreaks); (intervalos)
tabla <- table(intervalos); (tabla)
porcentajes <- round(prop.table(tabla) * 100, 1); (porcentajes)
pie(tabla,
    main = "Accidentes ",
    col = rainbow(length(tabla)),
    labels = paste0(porcentajes, "%"))
```

```{r}
##---- Grafica combinada ----
nbreaks <- 10
h <- hist(Accidentes_vialidad, breaks = nbreaks, plot = FALSE)
hist(Accidentes_vialidad,
     breaks = nbreaks,
     col = "pink",
     main = "Histograma y Ojiva (%)",
     xlab = "Numero (Accidentes)",
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


··##EJercicio 2____________________________________________________________________··


---
title: "Goles en un partido"
author: "Evelin Zavala"
date: "2025-09-17"
output: html_document
---

```{r setup, include=FALSE}
knitr::opts_chunk$set(echo = TRUE)
```

```{r}
Goles_partido <- c( 0, 1, 1, 0, 0, 1, 2, 0, 4, 0, 0, 0, 3, 0, 2, 3, 1, 1, 0, 1, 3, 0, 0,
 4, 0, 1, 0, 4, 3, 3, 1, 2, 0, 0, 4, 4, 1, 2, 0, 0, 1, 0, 0, 4, 1, 3, 1, 0, 1, 0, 0, 3, 0, 0,
 0, 0, 0, 0, 3, 4, 3, 1, 1, 0, 0, 3, 0, 1, 1, 0, 1, 0, 1, 1, 0, 3, 0, 1, 3, 0, 0, 3, 1, 3, 1,
 3, 0, 1, 2, 0, 1, 1, 3, 0, 1, 0, 0, 1, 1, 2 )
```

```{r}
summary(Goles_partido)
```

```{r}
Goles_ord <- sort(Goles_partido); (Goles_ord)
cte <- 9
table(Goles_partido)
```

```{r}
indices <- 1:1; (indices)
t <- cte*indices; (t)
```

```{r}
mis_deciles <- Goles_ord[t]; (mis_deciles)
cuantiles <- quantile(Goles_partido); (cuantiles)
Q1 <- cuantiles[2]; (Q1)
Q2 <- cuantiles[3]; (Q2)
Q3 <- cuantiles[4]; (Q3)
```

```{r}
x_media <- mean(Goles_partido);(x_media)
mi_min <- min(Goles_partido); (mi_min)
mi_max <- max(Goles_partido); (mi_max)
varianza <- var(Goles_partido); (varianza) 
DesvEst <- sd(Goles_partido); (DesvEst)
```

```{r}
plot(
  Goles_partido,
  main = "Gráfica Goles por Partido",
  xlab = "Índice de Goles",
  ylab = "Numero (goles)",
  col = "blue",       
  pch = 19,           
  cex = 0.5,          
  cex.main = 1.5,     
  cex.lab = 1.2,      
  col.main = "red")
legend(
  "topright",                 
  legend = c(mi_min,mi_max),    
  pch = 17,
  col=c("purple","orange"),
  pt.cex = 1.5,                
  cex = 0.7,
  bty = "n"                    
)
abline(h = x_media, col = "red", lwd = 1, lty = 2)     
abline(h = Q2, col = "lightgreen", lwd = 1, lty = 3) 
abline(h = mis_deciles, col = "lightgray", lty = 1.5)
points(which.min(Goles_partido), min(Goles_partido), col = "purple", pch = 17, cex = 1.5)
points(which.max(Goles_partido), max(Goles_partido), col = "orange", pch = 17, cex = 1.5)
```

```{r}
nbreaks=10;
miscolores <- rainbow(25,0.85);
h <- hist(Goles_partido, breaks = nbreaks, col= miscolores, 
          main = 'Numero de Goles por Partido',
          xlab="Goles",
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
h2 <- hist(Goles_partido, breaks = nbreaks, col= miscolores, 
           main = 'Numero de Goles por Partido',
           xlab="Goles",
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
h3 <- hist(Goles_partido, 
           breaks = nbreaks, 
           col= miscolores, 
           main = ' Goles por partido',
           xlab="Numero Goles",
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
intervalos <- cut(Goles_partido, breaks = nbreaks); (intervalos)
tabla <- table(intervalos); (tabla)
porcentajes <- round(prop.table(tabla) * 100, 1); (porcentajes)
pie(tabla,
    main = "Goles Por Partido ",
    col = rainbow(length(tabla)),
    labels = paste0(porcentajes, "%"))
```

```{r}
##---- Grafica combinada ----
nbreaks <- 10
h <- hist(Goles_partido, breaks = nbreaks, plot = FALSE)
hist(Goles_partido,
     breaks = nbreaks,
     col = "pink",
     main = "Histograma y Ojiva (%)",
     xlab = "Numero (Goles)",
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

··##Ejercicio 3 ________________________________________________________________··

---
title: "Numero de autos"
author: "Evelin Zavala"
date: "2025-09-17"
output: html_document
---

```{r setup, include=FALSE}
knitr::opts_chunk$set(echo = TRUE)
```

```{r}
Autos_familia <- c( 2, 1, 0, 0, 1, 1, 0, 0, 3, 3, 1, 0, 1, 1, 1, 0, 1, 1, 3, 2,
 1, 2, 1, 0, 1, 1, 2, 1, 2, 1, 1, 2, 1, 1, 0, 1, 0, 1, 0, 0, 1, 1, 1, 2, 0, 0, 1, 0, 1, 2, 2,
 0, 0, 1, 1, 0, 1, 0, 1, 0, 1, 1, 2, 2, 1, 2, 1, 0, 0, 0, 0, 0, 3, 3, 1)
```

```{r}
summary(Autos_familia)
```

```{r}
Autos_ord <- sort(Autos_familia); (Autos_ord)
cte <- 9
```

```{r}
indices <- 1:3; (indices)
t <- cte*indices; (t)
```

```{r}
mis_deciles <- Autos_ord[t]; (mis_deciles)
cuantiles <- quantile(Autos_familia); (cuantiles)
Q1 <- cuantiles[2]; (Q1)
Q2 <- cuantiles[3]; (Q2)
Q3 <- cuantiles[4]; (Q3)
```

```{r}
x_media <- mean(Autos_familia);(x_media)
mi_min <- min(Autos_familia); (mi_min)
mi_max <- max(Autos_familia); (mi_max)
varianza <- var(Autos_familia); (varianza) 
DesvEst <- sd(Autos_familia); (DesvEst)
```

```{r}
plot(
  Autos_familia,
  main = "Gráfica Autos en una familia",
  xlab = "Índice de Autos",
  ylab = "Numero (autos)",
  col = "blue",       
  pch = 19,           
  cex = 0.5,          
  cex.main = 1.5,     
  cex.lab = 1.2,      
  col.main = "red")
legend(
  "topright",                 
  legend = c(mi_min,mi_max),    
  pch = 17,
  col=c("purple","orange"),
  pt.cex = 1.5,                
  cex = 0.7,
  bty = "n"                    
)
abline(h = x_media, col = "red", lwd = 1, lty = 2)     
abline(h = Q2, col = "lightgreen", lwd = 1, lty = 3) 
abline(h = mis_deciles, col = "lightgray", lty = 1.8)
points(which.min(Autos_familia), min(Autos_familia), col = "purple", pch = 17, cex = 1.5)
points(which.max(Autos_familia), max(Autos_familia), col = "orange", pch = 17, cex = 1.5)
```

```{r}
nbreaks=10;
miscolores <- rainbow(25,0.85);
h <- hist(Autos_familia, breaks = nbreaks, col= miscolores, 
          main = 'Numero de Autos en una Familia',
          xlab="Autos",
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
h2 <- hist(Autos_familia, breaks = nbreaks, col= miscolores, 
           main = 'Numero de Autos en una familia',
           xlab="Autos",
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
h3 <- hist(Autos_familia, 
           breaks = nbreaks, 
           col= miscolores, 
           main = 'Numero de Autos por Familia',
           xlab="Numero Autos",
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
intervalos <- cut(Autos_familia, breaks = nbreaks); (intervalos)
tabla <- table(intervalos); (tabla)
porcentajes <- round(prop.table(tabla) * 100, 1); (porcentajes)
pie(tabla,
    main = "Autos por Familia ",
    col = rainbow(length(tabla)),
    labels = paste0(porcentajes, "%"))
```

```{r}
##---- Grafica combinada ----
nbreaks <- 10
h <- hist(Autos_familia, breaks = nbreaks, plot = FALSE)
hist(Autos_familia,
     breaks = nbreaks,
     col = "pink",
     main = "Histograma y Ojiva (%)",
     xlab = "Numero (autos)",
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

··##Ejercicio 4 ________________________________________________________________···


---
title: "Nùmero de estudiantes"
author: "Evelin Zavala"
date: "2025-09-17"
output: html_document
---

```{r setup, include=FALSE}
knitr::opts_chunk$set(echo = TRUE)
```

```{r}
Numero_estudiantes <- c( 35, 37, 13, 6, 40, 42, 11, 24, 24, 33, 8, 34, 21,
 23, 34, 24, 45, 39, 38, 35, 9, 39, 38, 45, 7, 35, 22, 28, 38, 6, 5, 45, 36, 18, 37, 45,
 13, 31, 23, 43.)
```

```{r}
summary(Numero_estudiantes)
```

```{r}
Estudiantes_ord <- sort(Numero_estudiantes); (Estudiantes_ord)
cte <- 9
```

```{r}
indices <- 1:10; (indices)
t <- cte*indices; (t)
```

```{r}
mis_deciles <- Estudiantes_ord[t]; (mis_deciles)
cuantiles <- quantile(Numero_estudiantes); (cuantiles)
Q1 <- cuantiles[2]; (Q1)
Q2 <- cuantiles[3]; (Q2)
Q3 <- cuantiles[4]; (Q3)
```

```{r}
x_media <- mean(Numero_estudiantes);(x_media)
mi_min <- min(Numero_estudiantes); (mi_min)
mi_max <- max(Numero_estudiantes); (mi_max)
varianza <- var(Numero_estudiantes); (varianza) 
DesvEst <- sd(Numero_estudiantes); (DesvEst)
```

```{r}
plot(
  Numero_estudiantes,
  main = "Numero de Estudiantes en un Salon",
  xlab = "Estudiantes",
  ylab = "Numero (Estudiantes)",
  col = "blue",       
  pch = 19,           
  cex = 0.5,          
  cex.main = 1.5,     
  cex.lab = 1.2,      
  col.main = "darkred")
legend(
  "topright", # ubicación
  legend = c(mi_min,mi_max), # etiquetas
  pch = 17,
  col=c("purple","orange"),
  pt.cex = 1.5, # tamaño de los símbolos
  cex = 0.7,
  bty = "n" # sin borde en la caja
)
abline(h = x_media, col = "red", lwd = 1, lty = 2)     
abline(h = Q2, col = "lightgreen", lwd = 1, lty = 3) 
abline(h = mis_deciles, col = "lightgray", lty = 1.5)
points(which.min(Numero_estudiantes), min(Numero_estudiantes), col = "purple", pch = 17, cex = 1.5)
points(which.max(Numero_estudiantes), max(Numero_estudiantes), col = "orange", pch = 17, cex = 1.5)
```

```{r}
nbreaks=10;
miscolores <- rainbow(25,0.35);
h <- hist(Numero_estudiantes, breaks = nbreaks, col= miscolores, 
          main = 'Numero de Estudiantes',
          xlab="Estudiantes",
          ylab="Numero (estudiantes)")

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
##---- Mejora del grafico ----
h2 <- hist(Numero_estudiantes, breaks = nbreaks, col= miscolores, 
           main = 'Numero de Estudiantes',
           xlab="Estudiantes",
           ylab="Numero(estudiantes)",
           ylim=c(0,max(h$counts)*1.5))
text(h$mids, h$counts, labels = h$counts, pos = 3, cex = 0.8, 
     col = "black")
nl <- length(h$counts); 
legend(
  "topright", # ubicación
  legend = h$counts, # etiquetas
  fill = miscolores,
  col = miscolores, # colores (uno por cada símbolo)
  pt.cex = 1.5, # tamaño de los símbolos
  cex = 0.5,
  bty = "n" # sin borde en la caja
)
```

```{r}
##---- Grafico con porcentajes ----
h3 <- hist(Numero_estudiantes, 
           breaks = nbreaks, 
           col= miscolores, 
           main = 'Numero de Estudiantes',
           xlab="Numero Estudiantes",
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
  "topright", # ubicación
  legend = paste0(round(porcentajes,1),"%"),
  fill = miscolores,
  cex = 0.5,
  bty = "n" # sin borde en la caja
)
```

```{r}
#---- Graficos de pastel ----
# Tus datos
nbreaks <- 10
intervalos <- cut(Numero_estudiantes, breaks = nbreaks); (intervalos)
tabla <- table(intervalos); (tabla)
porcentajes <- round(prop.table(tabla) * 100, 1); (porcentajes)
pie(tabla,
    main = "Numero de Estudiantes",
    col = rainbow(length(tabla)),
    labels = paste0(porcentajes, "%"))
```

```{r}
##---- Grafica combinada ----
nbreaks <- 10
h <- hist(Numero_estudiantes, breaks = nbreaks, plot = FALSE)
hist(Numero_estudiantes,
     breaks = nbreaks,
     col = "pink",
     main = "Histograma y Ojiva (%)",
     xlab = "Numero (Estudiantes)",
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


··##Ejercicio 5 ___________________________________________________________________________···


---
title: "Nùmero de familia"
author: "Evelin Zavala"
date: "2025-09-17"
output: html_document
---
```{r}
Numero_familia <- c( 2,2,4,4,1,3,1,3,3,3,5,2,1,5,2,1,5,3,3,3,
                         3,1,3,3,2,5,5,2,5,2,3,1,3)
```

```{r}
summary(Numero_familia)
```

```{r}
Familia_ord <- sort(Numero_familia); (Familia_ord)
cte <- 9
```

```{r}
indices <- 1:10; (indices)
t <- cte*indices; (t)
```

```{r}
mis_deciles <- Familia_ord[t]; (mis_deciles)
cuantiles <- quantile(Numero_familia); (cuantiles)
Q1 <- cuantiles[2]; (Q1)
Q2 <- cuantiles[3]; (Q2)
Q3 <- cuantiles[4]; (Q3)
```

```{r}
x_media <- mean(Numero_familia);(x_media)
mi_min <- min(Numero_familia); (mi_min)
mi_max <- max(Numero_familia); (mi_max)
varianza <- var(Numero_familia); (varianza) 
DesvEst <- sd(Numero_familia); (DesvEst)
```

```{r}
plot(
  Numero_familia,
  main = "Numero de hijos en una Familia",
  xlab = "Hijos",
  ylab = "Numero (Hijos)",
  col = "pink",       
  pch = 19,           
  cex = 0.5,          
  cex.main = 1.5,     
  cex.lab = 1.2,      
  col.main = "red")
legend(
  "topright", # ubicación
  legend = c(mi_min,mi_max), # etiquetas
  pch = 18,
  col=c("purple","orange"),
  pt.cex = 1.8, # tamaño de los símbolos
  cex = 0.7,
  bty = "n" # sin borde en la caja
)
abline(h = x_media, col = "red", lwd = 1, lty = 2)     
abline(h = Q2, col = "lightgreen", lwd = 1, lty = 3) 
abline(h = mis_deciles, col = "lightgray", lty = 1.8)
points(which.min(Numero_familia), min(Numero_familia), col = "purple", pch = 18, cex = 1.8)
points(which.max(Numero_familia), max(Numero_familia), col = "orange", pch = 18, cex = 1.8)
```

```{r}
nbreaks=10;
miscolores <- rainbow(25,0.85);
h <- hist(Numero_familia, breaks = nbreaks, col= miscolores, 
          main = 'Numero de hijos en una familia',
          xlab="Hijos",
          ylab="Numero (Hijos)")

text(h$mids, h$counts, labels = h$counts, pos = 3, cex = 0.4, 
     col = "black")
nl <- length(h$counts); 
legend(
  "topright", # ubicación
  legend = h$counts, # etiquetas
  fill = miscolores,
  col = miscolores, # colores (uno por cada símbolo)
  pt.cex = 1.8, # tamaño de los símbolos
  cex = 0.7,
  bty = "n" # sin borde en la caja
)
```

```{r}
##---- Mejora del grafico ----
h2 <- hist(Numero_familia, breaks = nbreaks, col= miscolores, 
           main = 'Numero de hijos en una familia',
           xlab="Hijos",
           ylab="Numero(hijos)",
           ylim=c(0,max(h$counts)*1.8))
text(h$mids, h$counts, labels = h$counts, pos = 3, cex = 0.8, 
     col = "black")
nl <- length(h$counts); 
legend(
  "topright", # ubicación
  legend = h$counts, # etiquetas
  fill = miscolores,
  col = miscolores, # colores (uno por cada símbolo)
  pt.cex = 1.8, # tamaño de los símbolos
  cex = 0.5,
  bty = "n" # sin borde en la caja
)
```

```{r}
##---- Grafico con porcentajes ----
h3 <- hist(Numero_familia, 
           breaks = nbreaks, 
           col= miscolores, 
           main = 'Numero de hijos en una familia',
           xlab="Numero hijos",
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
  "topright", # ubicación
  legend = paste0(round(porcentajes,1),"%"),
  fill = miscolores,
  cex = 0.5,
  bty = "n" # sin borde en la caja
)
```

```{r}
#---- Graficos de pastel ----
# Tus datos
nbreaks <- 10
intervalos <- cut(Numero_familia, breaks = nbreaks); (intervalos)
tabla <- table(intervalos); (tabla)
porcentajes <- round(prop.table(tabla) * 100, 1); (porcentajes)
pie(tabla,
    main = "Numero de jijos en una familia",
    col = rainbow(length(tabla)),
    labels = paste0(porcentajes, "%"))
```

```{r}
##---- Grafica combinada ----
nbreaks <- 10
h <- hist(Numero_familia, breaks = nbreaks, plot = FALSE)
hist(Numero_familia,
     breaks = nbreaks,
     col = "pink",
     main = "Histograma y Ojiva (%)",
     xlab = "Numero (Hijos)",
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
