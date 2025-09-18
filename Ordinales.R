---
title: "Rangos"
author: "Evelin Zavala"
date: "2025-09-17"
output: html_document
---

```{r setup, include=FALSE}
knitr::opts_chunk$set(echo = TRUE)
```

---
title: "ordinales 5"
author: "BUENDIA CAMACHO DANA PAOLA"
date: "2025-09-15"
output: html_document
---

```{r setup, include=FALSE}
knitr::opts_chunk$set(echo = TRUE)
```

```{r}
Rangos <- c("Principiante", "Principiante", "Aprendiz", "Intermedio", "Experto",
            "Intermedio", "Experto", "Aprendiz", "Principiante", "Avanzado",
            "Intermedio", "Experto", "Experto", "Experto", "Avanzado", "Intermedio",
            "Intermedio", "Principiante", "Principiante", "Principiante",
            "Principiante", "Principiante", "Intermedio", "Aprendiz", "Aprendiz",
            "Aprendiz", "Intermedio", "Aprendiz", "Principiante", "Intermedio",
            "Avanzado", "Avanzado", "Intermedio", "Avanzado", "Principiante",
            "Intermedio", "Avanzado", "Intermedio", "Avanzado")
```

```{r}
niveles_rangos <- c("Principiante", "Aprendiz", "Intermedio", "Avanzado", "Experto")

Rangos <- factor(Rangos,
                 levels = niveles_rangos,
                 ordered = TRUE)
```

```{r}
tabla_Rangos <- table(Rangos)
tabla_Rangos
```

```{r}
porcentajes <- prop.table(tabla_Rangos) * 100
porcentajes
```

```{r}
numeros <- as.numeric(Rangos)

mediana <- median(numeros)
cuartiles <- quantile(numeros, probs = c(0.25, 0.5, 0.75))

cat("Mediana: ", niveles_rangos[mediana], "\n")
cat("Cuartiles:\n")
cat("Q1: ", niveles_rangos[cuartiles[1]], "\n")
cat("Q2: ", niveles_rangos[cuartiles[2]], "\n")
cat("Q3: ", niveles_rangos[cuartiles[3]], "\n")
```

```{r}
barplot(tabla_Rangos,
        col = c("pink", "green", "blue", "orange", "yellow"),
        main = "Distribución de Rangos",
        ylab = "Frecuencia",
        las = 2)
```

```{r}
pie(tabla_Rangos,
    col = rainbow(length(tabla_Rangos)),
    main = "Distribución de Rangos",
    labels = paste(names(tabla_Rangos), round(porcentajes, 1), "%"))
```


··##Ejericicio 2_____________________________________________________________···


---
title: "Reseñas de hoteles"
author: "Evelin Zavala"
date: "2025-09-15"
output: html_document
---

```{r setup, include=FALSE}
knitr::opts_chunk$set(echo = TRUE)
```

```{r}
Reseñas <- c("4 estrellas", "5 estrellas", "1.5 estrellas", "4.5 estrellas", "5 estrellas",
             "4.5 estrellas", "2.5 estrellas", "4 estrellas", "4.5 estrellas", "1.5 estrellas",
             "3.5 estrellas", "1 estrella", "3.5 estrellas", "3 estrellas", "5 estrellas",
             "1.5 estrellas", "2 estrellas", "2 estrellas", "1.5 estrellas", "4.5 estrellas",
             "4.5 estrellas", "1.5 estrellas", "3.5 estrellas", "4 estrellas", "1 estrella",
             "4 estrellas", "4.5 estrellas", "3.5 estrellas", "1 estrella", "4 estrellas",
             "4 estrellas", "2 estrellas", "1 estrella", "4.5 estrellas", "1 estrella")
```

```{r}
# Para ordenar, se definen los niveles; del más bajo al más alto
niveles <- c("1 estrella", "1.5 estrellas", "2 estrellas", "2.5 estrellas", "3 estrellas",
             "3.5 estrellas", "4 estrellas", "4.5 estrellas", "5 estrellas")

Reseñas <- factor(Reseñas,
                  levels = niveles,
                  ordered = TRUE)
```

```{r}
tabla_Reseñas <- table(Reseñas)
tabla_Reseñas
```

```{r}
porcentajes <- prop.table(tabla_Reseñas) * 100
porcentajes
```

```{r}
numeros <- as.numeric(Reseñas)

mediana <- median(numeros)
cuartiles <- quantile(numeros, probs = c(0.25, 0.5, 0.75))

cat("Mediana: ", niveles[mediana], "\n")
cat("Cuartiles:\n")
cat("Q1: ", niveles[cuartiles[1]], "\n")
cat("Q2: ", niveles[cuartiles[2]], "\n")
cat("Q3: ", niveles[cuartiles[3]], "\n")
```

```{r}
barplot(tabla_Reseñas,
        col = rainbow(length(tabla_Reseñas)),
        main = "Reseñas de Hoteles (Estrellas)",
        ylab = "Frecuencia",
        las = 2)
```

```{r}
pie(tabla_Reseñas,
    col = rainbow(length(tabla_Reseñas)),
    main = "Reseñas de Hoteles (Estrellas)",
    labels = paste(names(tabla_Reseñas), round(porcentajes, 1), "%"))
```



··##Ejercicio 3 ___________________________________________________________···

---
title: "Grado de dolor"
author: "Evelin Zavala"
date: "2025-09-15"
output: html_document
---

```{r setup, include=FALSE}
knitr::opts_chunk$set(echo = TRUE)
```

```{r}
GradoDolor <- c("severo","leve","leve","severo","leve","leve","leve","moderado",
                "moderado","moderado","moderado","severo","severo","moderado",
                "leve","severo","leve","severo","leve","moderado","severo",
                "severo","leve","leve","leve","severo","severo","leve")
```

```{r}
GradoDolor <- factor(GradoDolor,
                     levels = c("leve", "moderado", "severo"),
                     ordered = TRUE)
```

```{r}
tabla_Dolor <- table(GradoDolor)
tabla_Dolor
```

```{r}
porcentajes <- prop.table(tabla_Dolor) * 100
porcentajes
```

```{r}
# Convertir factor a números
numeros <- as.numeric(GradoDolor)

mediana <- median(numeros)
cuartiles <- quantile(numeros, probs = c(0.25, 0.5, 0.75))

niveles <- levels(GradoDolor)
cat("Mediana: ", niveles[mediana], "\n")
cat("Cuartiles:\n")
cat("Q1: ", niveles[cuartiles[1]], "\n")
cat("Q2: ", niveles[cuartiles[2]], "\n")
cat("Q3: ", niveles[cuartiles[3]], "\n")
```

```{r}
barplot(tabla_Dolor,
        col = c(rainbow(length(tabla_Dolor))),
        main = "Grado de Dolor",
        ylab = "Frecuencia",
        las = 2)
```

```{r}
pie(tabla_Dolor,
    col = c(rainbow(length(tabla_Dolor))),
    main = "Grado de Dolor",
    labels = paste(names(tabla_Dolor), round(porcentajes, 1), "%"))
```


··##Ejercicio 4________________________________________________________________···


---
title: "Nivel de satisfaccion"
author: "Evelin Zavala"
date: "2025-09-15"
output: html_document
---

```{r setup, include=FALSE}
knitr::opts_chunk$set(echo = TRUE)
```

```{r}
Nivel_Satisfaccion <- c("satisfecho","muy satisfecho","muy insatisfecho","neutral",
                      "muy satisfecho","muy insatisfecho","muy satisfecho","insatisfecho",
                      "muy insatisfecho","insatisfecho","muy satisfecho","insatisfecho",
                      "muy satisfecho","muy satisfecho","mas o menos insatisfecho",
                      "insatisfecho","satisfecho","muy satisfecho","neutral",
                      "mas o menos insatisfecho","muy insatisfecho","mas o menos satisfecho",
                      "insatisfecho","neutral","satisfecho","mas o menos satisfecho",
                      "satisfecho","neutral","muy insatisfecho","muy insatisfecho",
                      "neutral","satisfecho","insatisfecho","mas o menos satisfecho",
                      "mas o menos insatisfecho","muy insatisfecho","neutral",
                      "neutral","satisfecho","neutral","insatisfecho","mas o menos
                      insatisfecho","insatisfecho","mas o menos satisfecho","satisfecho",
                      "neutral","insatisfecho","insatisfecho")
length(Nivel_Satisfaccion)
```

```{r}
Nivel_Satisfaccion <- factor(Nivel_Satisfaccion,
                           levels = c("muy insatisfecho","insatisfecho","mas o menos
                                      insatisfecho","neutral", "mas o menos satisfecho",
                                      "satisfecho","muy satisfecho"),
                           ordered = TRUE)

```


```{r}
tabla_Satisfaccion <- table(Nivel_Satisfaccion)
tabla_Satisfaccion
```


```{r}
porcentajes <- prop.table(tabla_Satisfaccion) * 100
porcentajes
```


```{r}
# Convertir factor a números para calcular mediana y cuartiles
numeros <- as.numeric(Nivel_Satisfaccion)

mediana <- median(numeros)
cuartiles <- quantile(numeros, probs = c(0.25, 0.5, 0.75), na.rm = TRUE)

# Mostrar resultados como categorías
niveles <- levels(Nivel_Satisfaccion)
cat("Mediana: ", niveles[mediana], "\n")
cat("Cuartiles:\n")
cat("Q1: ", niveles[cuartiles[1]], "\n")
cat("Q2: ", niveles[cuartiles[2]], "\n")
cat("Q3: ", niveles[cuartiles[3]], "\n")
```

```{r}
barplot(tabla_Satisfaccion,
           col = 
          rainbow(length(tabla_Satisfaccion)))
        main ="Nivel de Satisfacción"
        ylab = "Frecuencia"
        
```

```{r}
pie(tabla_Satisfaccion,
    col = rainbow(length(tabla_Satisfaccion)),
    main = "Nivel de Satisfacción",
    labels = paste(names(tabla_Satisfaccion), round(porcentajes, 1), "%"))
```


··##Ejercicio 5__________________________________________________________···


---
title: "Nivel educativo"
author: "Evelin Zavala"
date: "2025-09-15"
output: html_document
---
```{r setup, include=FALSE}
knitr::opts_chunk$set(echo = TRUE)
```

```{r}
# Vector con los datos
NivelEducativo <- c("posdoctorado", "preparatoria", "preparatoria", "doctorado",
                    "primaria", "doctorado", "preparatoria", "secundaria",
                    "preparatoria", "preparatoria", "primaria", "doctorado",
                    "secundaria", "secundaria", "secundaria", "licenciatura",
                    "doctorado", "primaria", "maestría", "posdoctorado",
                    "maestría", "preparatoria", "posdoctorado", "doctorado",
                    "preparatoria", "doctorado", "maestría", "maestría",
                    "primaria", "licenciatura", "secundaria", "maestría",
                    "posdoctorado", "doctorado", "licenciatura", "doctorado",
                    "posdoctorado", "maestría", "primaria", "secundaria",
                    "preparatoria")

# Ver total de datos
length(NivelEducativo)
```

```{r}
# Definir el orden correcto
NivelEducativo <- factor(NivelEducativo,
                         levels = c("primaria", "secundaria", "preparatoria",
                                    "licenciatura", "maestría", "doctorado",
                                    "posdoctorado"),
                         ordered = TRUE)
```

```{r}
tabla_Educacion <- table(NivelEducativo)
tabla_Educacion
```

```{r}
porcentajes <- prop.table(tabla_Educacion) * 100
porcentajes
```

```{r}
proporciones <- prop.table(tabla_Educacion)
proporciones
```

```{r}
moda <- names(tabla_Educacion)[tabla_Educacion == max(tabla_Educacion)]
moda
```

```{r}
barplot(tabla_Educacion,
        col = "lightblue",
        main = "Frecuencia de Nivel Educativo",
        ylab = "Frecuencia",
        las = 2)
```

```{r}
pie(tabla_Educacion,
    col = rainbow(length(tabla_Educacion)),
    main = "Frecuencia de Nivel Educativo",
    labels = paste(names(tabla_Educacion), round(porcentajes, 1), "%"))
```



