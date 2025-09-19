---
title: "Tareita PyE"
author: "Evelin Zavala"
date: "`r format(Sys.time(), '%d %B, %Y')`"
output:
  html_document:
    toc: true
    toc_float: true
    toc_depth: 3
    code_folding: show
    theme: bootstrap
    css: styles.css
  word_document: default
---

```{r}
u<-runif(10,0,1);head(u,20)
plot(u,col="darkblue")
```
```{r}
u<-runif(100,0,1);head(u,20)
plot(u,col="orange")
```
```{r}
u<-runif(1000,0,1);head(u,20)
plot(u,col="lightgreen")
```
```{r}
u<-runif(10000,0,1);head(u,20)
plot(u,col="darkgray")
```
```{r}
u<-runif(100000,0,1);head(u,20)
plot(u,col="darkred")
```
```{r}
u<-runif(1000000,0,1);head(u,20)
plot(u,col="pink")
```

## Generacion de 25,000 VA uniformes

```{r}
u<-runif(25000,0,1);head(u,20)
plot(u,col="cyan")
```

## Generacion de 50,000 VA uniformes

```{r}
u<-runif(50000,0,1);head(u,20)
plot(u,col="purple")
```

## Generacion de 25,000 VA uniformes

```{r}
u<-runif(100000,0,1);head(u,20)
plot(u,col="lightblue")
```
         
## Extraccion de muestras
 
```{r}
muestra1<-sample(u,100, replace = FALSE)
plot(muestra1,
      main="Muestra de tamaño 100",
      xlab="Elementos de la muestra",
      ylab="Uniformes(0,1)",
      col="blue")
     
```

##Histograma

```{r}
hist(u,col=rainbow(15,0.23))
```

```{r}
hist(muestra1, col="green")
```

## Muestra 2

```{r}

muestra2<-sample(u,250, replace = FALSE)
plot(muestra2,
      main="Muestra de tamaño 100",
      xlab="Elementos de la muestra",
      ylab="Uniformes(0,1)",
      col="yellow")
     
```

##Histograma

```{r}
hist(u,col=rainbow(15,0.23))
```

```{r}
hist(muestra2, col="gray")
```

#Generacion de numeros aleatorios normales
## Generacion de 10 VA normales(0,1)
```{r}
u<- rnorm(10,0,1);head(u,20)
plot(u,col="red")
```

```{r}
u<- rnorm(100,0,1);head(u,20)
plot(u,col="brown")
```

