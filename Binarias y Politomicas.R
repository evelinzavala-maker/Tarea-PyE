---
title: "Licencia de conducir"
author: "Evelin Zavala"
date: "2025-09-17"
output: html_document
---

```{r setup, include=FALSE}
knitr::opts_chunk$set(echo = TRUE)
```

```{r}
Licencia <- c("No", "Si", "No", "Si", "Si", "No", "Si", "No", "No", "No", "Si", "Si", "No", "No", "Si",
 "No", "No", "Si", "No", "No", "Si", "Si", "Si", "No", "No", "Si", "No", "Si", "No", "No", "No", "No", "Si", "Si", "No", "Si",
 "Si", "No", "Si", "Si", "No", "Si", "No", "No", "Si", "No", "No", "No", "No", "Si", "Si", "Si", "No", "Si", "No", "No", "No",
 "No", "Si", "Si", "No", "Si", "No", "Si", "No", "No", "No", "Si", "No", "Si", "No", "Si", "Si", "No", "No")
               
```

```{r}
summary(Licencia)
```

```{r}
Licencia_ord <- sort(Licencia); (Licencia)
```

```{r}
table(Licencia) # Frecuencias
prop.table(table(Licencia)) # Proporciones
```

```{r}

freq <- table(Licencia)
porc <- round(prop.table(freq)*100, 1)
mi_min <- names(freq)[which.min(freq)]
(mi_min)
mi_max <- names(freq)[which.max(freq)]
(mi_max)
```

```{r}
bar_colors <- c("pink", "lightblue")

# Gráfico
bp <- barplot(freq,
              col = bar_colors,
              main = "Licencia de Conducir",
              xlab = "",
              ylab = "Frecuencia",
              ylim = c(0, 50), # << ahora llega hasta 50
              cex.names = 0.9,
              space = 0.5
)

# Agregar valores encima de cada barra
text(bp, freq, labels = freq, pos = 3, cex = 1, font = 2)
text(bp, freq, labels = freq, pos = 3, cex = 0.9, font = 2)
     
```

```{r}
pie(freq,
    labels = paste(names(freq), porc, "%"),
    col = rainbow(length(freq), 0.7),
    main = "Licencia de Conducir")
```
##Ejercicio 2 binarias_____________________________________________________________________##



---
title: "Examen"
author: "Evelin Zavala"
date: "2025-09-17"
output: html_document
---

```{r setup, include=FALSE}
knitr::opts_chunk$set(echo = TRUE)
```

```{r}
Examen <- c("Reprobado", "Aprobado", "Aprobado", "Aprobado", "Aprobado", "Reprobado", "Aprobado",
"Reprobado", "Reprobado", "Aprobado", "Reprobado", "Reprobado", "Reprobado", "Reprobado",
"Aprobado", "Reprobado", "Reprobado", "Aprobado", "Aprobado", "Reprobado", "Aprobado",
"Aprobado", "Aprobado", "Aprobado", "Reprobado", "Aprobado", "Reprobado", "Aprobado", "Reprobado",
"Reprobado", "Aprobado", "Reprobado", "Aprobado", "Reprobado", "Aprobado", "Reprobado",
"Reprobado", "Aprobado", "Reprobado", "Reprobado", "Reprobado", "Aprobado", "Aprobado",
 "Aprobado", "Aprobado", "Reprobado", "Reprobado", "Reprobado", "Aprobado", "Reprobado")
               
```

```{r}
summary(Examen)
```

```{r}
Examen_ord <- sort(Examen); (Examen_ord)
```

```{r}
table(Examen) # Frecuencias
prop.table(table(Examen)) # Proporciones
```

```{r}

freq_examen <- table(Examen)
porc_examen <- round(prop.table(freq_examen)*100, 1)
mi_min <- names(freq_examen)[which.min(freq_examen)]
(mi_min)
mi_max <- names(freq_examen)[which.max(freq_examen)]
(mi_max)
```

```{r}
bar_colors <- c("pink", "skyblue2")

# Gráfico
bp <- barplot(freq_examen,
              col = bar_colors,
              main = "Examen",
              xlab = "",
              ylab = "Frecuencia",
              ylim = c(0, 50), # << ahora llega hasta 50
              cex.names = 0.9,
              space = 0.5
)

# Agregar valores encima de cada barra
text(bp, freq_examen, labels = freq_examen, pos = 3, cex = 1, font = 2)
text(bp, freq_examen, labels = freq_examen, pos = 3, cex = 0.9, font = 2)
     
```

```{r}
pie(freq_examen,
    labels = paste(names(freq_examen), porc_examen, "%"),
    col = rainbow(length(freq_examen), 0.4),
    main = "Examen")
```

··##Ejercicio 3 Binarias_________________________________________________

---
title: "COVID"
author: "Evelin Zavala"
date: "2025-09-17"
output: html_document
---

```{r setup, include=FALSE}
knitr::opts_chunk$set(echo = TRUE)
```

```{r}
covid <- c("Negativo", "Positivo", "Negativo", "Negativo", "Negativo", "Positivo", "Positivo", "Negativo", "Negativo", "Positivo", "Negativo", "Positivo", "Positivo", "Negativo", "Negativo", "Negativo", "Negativo", "Negativo", "Negativo", "Negativo", "Positivo", "Positivo", "Negativo", "Negativo", "Positivo", "Negativo", "Positivo", "Negativo", "Negativo", "Positivo", "Positivo", "Negativo", "Positivo", "Positivo", "Negativo", "Positivo", "Negativo", "Negativo", "Negativo", "Negativo", "Positivo", "Negativo", "Positivo", "Negativo", "Negativo", "Negativo", "Negativo", "Negativo", "Negativo", "Negativo")


```

```{r}
summary(covid)
```

```{r}
covid_ord <- sort(covid); (covid_ord)
```

```{r}
table(covid) # Frecuencias
prop.table(table(covid)) # Proporciones
```

```{r}

freq_covid <- table(covid)
porc_covid <- round(prop.table(freq_covid)*100, 1)
mi_min <- names(freq_covid)[which.min(freq_covid)]
(mi_min)
mi_max <- names(freq_covid)[which.max(freq_covid)]
(mi_max)
```

```{r}
bar_colors <- c("pink","lightblue")

# Gráfico
bp <- barplot(freq_covid,
              col = bar_colors,
              main = "COVID",
              xlab = "",
              ylab = "Frecuencia",
              ylim = c(0, 50), # << ahora llega hasta 50
              cex.names = 0.9,
              space = 0.5
)

# Agregar valores encima de cada barra
text(bp, freq_covid, labels = freq_covid, pos = 3, cex = 1, font = 2)
text(bp, freq_covid, labels = freq_covid, pos = 3, cex = 0.9, font = 2)
     
```

```{r}
pie(freq_covid,
    labels = paste(names(freq_covid), porc_covid, "%"),
    col = rainbow(length(freq_covid), 0.4),
    main = "COVID")
```

··##Ejericio 4 Politomicas ____________________________________


---
title: "Tipo de sangre"
author: "Evelin Zavala"
date: "2025-09-17"
output: html_document
---

```{r setup, include=FALSE}
knitr::opts_chunk$set(echo = TRUE)
```

```{r}
sangre <- c("A-", "AB-", "AB+", "B+", "O-", "O+", "B+", "A-", "O-", "B-", "A-", "A-", "O+", "A+",
 "O+", "AB+", "B+", "AB+", "AB-", "B-", "O-", "O+", "B+", "O+", "O+", "O+", "A+", "O-", "AB-", "A-", "B+",
 "O-", "B-", "A-", "A-", "AB-", "B+", "AB-", "A-", "B+", "A-", "AB-", "O-", "AB+", "B+", "O+", "B-", "O-", "A-",
 "AB-", "O+", "AB+", "AB+", "B+", "O-", "AB+", "AB+", "AB-", "A-", "O-", "AB+", "AB+", "A+", "A+",
 "O+", "B+", "AB-", "O-", "A-", "A+", "A+", "B-", "B-", "B-", "A+", "A-", "AB-", "A-", "A-", "AB+")
               
```

```{r}
summary(sangre)
```

```{r}
sangre_ord <- sort(sangre); (sangre_ord)
```

```{r}
table(sangre) # Frecuencias
prop.table(table(sangre)) # Proporciones
```

```{r}

freq <- table(sangre)
porc <- round(prop.table(freq)*100, 1)
mi_min <- names(freq)[which.min(freq)]
(mi_min)
mi_max <- names(freq)[which.max(freq)]
(mi_max)
```

```{r}
bar_colors <- c("skyblue","pink","cyan",
            "orange","violet","green","yellow",
            "red")

# Gráfico
bp <- barplot(freq,
              col = bar_colors,
              main = "Tipo de Sangre",
              xlab = "",
              ylab = "Frecuencia",
              ylim = c(0, 50), # << ahora llega hasta 50
              cex.names = 0.9,
              space = 0.5
)

# Agregar valores encima de cada barra
text(bp, freq, labels = freq, pos = 3, cex = 1, font = 2)
text(bp, freq, labels = freq, pos = 3, cex = 0.9, font = 2)
     
```

```{r}
pie(freq,
    labels = paste(names(freq), porc, "%"),
    col = rainbow(length(freq), 0.9),
    main = "Tipo de Sangre")
```

··##Ejercicio 5 Politomicas_____________________________

---
title: "Ocopaciòn"
author: "Evelin Zavala"
date: "2025-09-17"
output: html_document
---

```{r setup, include=FALSE}
knitr::opts_chunk$set(echo = TRUE)
```

```{r}
Ocupacion <- c("Desempleado", "Estudiante", "Empleado", "Empleado", "Estudiante", "Empleado", "Empleado", "Empleado", "Jubilado", "Jubilado", "Jubilado", "Jubilado", "Jubilado", "Estudiante", "Desempleado", "Desempleado", "Jubilado", "Empleado", "Estudiante", "Estudiante", "Empleado", "Desempleado", "Estudiante", "Desempleado", "Jubilado", "Empleado", "Jubilado", "Desempleado", "Desempleado", "Desempleado")
               
```

```{r}
summary(Ocupacion)
```

```{r}
Ocupacion_ord <- sort(Ocupacion); (Ocupacion_ord)
```

```{r}
table(Ocupacion) # Frecuencias
prop.table(table(Ocupacion)) # Proporciones
```

```{r}

freq <- table(Ocupacion)
porc <- round(prop.table(freq)*100, 1)
mi_min <- names(freq)[which.min(freq)]
(mi_min)
mi_max <- names(freq)[which.max(freq)]
(mi_max)
```

```{r}
bar_colors <- c("skyblue","blue","pink",
            "purple")

# Gráfico
bp <- barplot(freq,
              col = bar_colors,
              main = "Ocupacion",
              xlab = "",
              ylab = "Frecuencia",
              ylim = c(0, 50), # << ahora llega hasta 50
              cex.names = 0.9,
              space = 0.5
)

# Agregar valores encima de cada barra
text(bp, freq, labels = freq, pos = 3, cex = 1, font = 2)
text(bp, freq, labels = freq, pos = 3, cex = 0.9, font = 2)
     
```

```{r}
pie(freq,
    labels = paste(names(freq), porc, "%"),
    col = rainbow(length(freq), 0.9),
    main = "Ocupacion")
```
