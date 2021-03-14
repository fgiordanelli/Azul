# Azul

---
title: "Bom modo para analisar Subscrição"
output:
  html_document:
    toc: true
    toc_float:
      collapsed: true
---



```{r setup, include=FALSE}
knitr::opts_chunk$set(echo = TRUE)
library(dplyr)
library(scales)
library(DT)
library(grid)
library(gridExtra)
library(kableExtra)
library(ggplot2)
library(ggthemes)
library(haven)
library(readr)
```


```{r echo=FALSE, message=FALSE, warning=FALSE}
base_cubo <- read_csv2("C:/Users/Fabrício Giordanelli/Desktop/ALFAPROMOTORA/base_cubo_BA_resumida.csv",  locale(encoding = "ISO-8859-1", decimal_mark = ",") , col_names = TRUE,col_types = NULL) %>% filter(CATEGORIA_TARIFARIA != "10 - Passeio Nacional")

base_cubo_BA <- read_csv2("C:/Users/Fabrício Giordanelli/Desktop/ALFAPROMOTORA/base_cubo_BA_resumida.csv",  locale(encoding = "ISO-8859-1", decimal_mark = ",") , col_names = TRUE,col_types = NULL) %>% filter(CATEGORIA_TARIFARIA == "10 - Passeio Nacional")

base_cubo$vehanos <- as.numeric(base_cubo$vehanos)
base_cubo$priganrcf <- as.numeric(base_cubo$priganrcf)
base_cubo$expocasco <- as.numeric(base_cubo$expocasco)

base_cubo_BA$vehanos <- as.numeric(base_cubo_BA$vehanos)
base_cubo_BA$priganrcf <- as.numeric(base_cubo_BA$priganrcf)
base_cubo_BA$expocasco <- as.numeric(base_cubo_BA$expocasco)

```



```{r, echo=FALSE, message=FALSE, warning=FALSE}
funcao <- function(base_cubo, ...) {
  base_cubo   %>%  
    group_by(.dots = lazyeval::lazy_dots(...)) %>%
    summarize(
      vig = sum(nitem, na.rm = TRUE),
      exp = round(sum(vehanos, na.rm = TRUE),2),
      pgmed = round(sum(prigan, na.rm = TRUE)/sum(vehanos, na.rm = TRUE),2),
      TxR = round(sum(indpp,indpt,indrb, na.rm = TRUE)/sum(iscascotot, na.rm = TRUE),2),
      TxC = round(sum(premit, na.rm = TRUE)/sum(iscascotot, na.rm = TRUE),2),
      SP = round(sum(carga, na.rm = TRUE)/sum(prigan, na.rm = TRUE),2),
      SPRcf = round(sum(indrcf, na.rm = TRUE)/sum(priganrcf, na.rm = TRUE),2), 
      Fq = round(sum(nbsini, na.rm=TRUE)/sum(vehanos, na.rm = TRUE),2),
      FqPP = round(sum(PP, na.rm=TRUE)/sum(expocasco, na.rm = TRUE),2),
      FqPT = round(sum(PT, na.rm=TRUE)/sum(expocasco,na.rm = TRUE),2),
      FqRB = round(sum(RBPURO, na.rm=TRUE)/sum(expocasco, na.rm = TRUE),2),
      FqFT = round(sum(FTPURO, na.rm=TRUE)/sum(expocasco, na.rm = TRUE),2),
      FqRCF = round(sum(RCF, na.rm=TRUE)/sum(expocasco, na.rm = TRUE),2)
      ) %>%
    mutate(mix = 100*(round(vig / sum(vig),4))) %>%
    select_(.dots = lazyeval::lazy_dots(...), "vig", "mix", "everything()")
}

base_cubo$TIPO_SEGURO <- factor(base_cubo$TIPO_SEGURO, levels = c("Seguro Novo", "Renovação Congênere", "Renovação Azul"))

base_cubo_BA$TIPO_SEGURO <- factor(base_cubo_BA$TIPO_SEGURO, levels = c("Seguro Novo", "Renovação Congênere", "Renovação Azul"))

```


## Visão Geral {.tabset .tabset-fade}

### Geral

```{r, echo=FALSE, message=FALSE, warning=FALSE}
kable(funcao(base_cubo)) %>%
  kable_styling()
```

### Analisar subscrição

```{r, echo=FALSE, message=FALSE, warning=FALSE}
kable(funcao(base_cubo_BA)) %>%
  kable_styling()
```


## Tipo Seguro {.tabset .tabset-fade}

### Geral

```{r, echo=FALSE, message=FALSE, warning=FALSE}
kable(funcao(base_cubo,TIPO_SEGURO)) %>%
  kable_styling()
```

### Analisar subscrição

```{r, echo=FALSE, message=FALSE, warning=FALSE}
kable(funcao(base_cubo_BA,TIPO_SEGURO)) %>%
  kable_styling()
```


## Tipo seguro e RNS geral {.tabset .tabset-fade}

### Geral

```{r, echo=FALSE, message=FALSE, warning=FALSE}
kable(funcao(base_cubo,TIPO_SEGURO, ind_rns)) %>%
  kable_styling()
```

### Analisar subscrição

```{r, echo=FALSE, message=FALSE, warning=FALSE}
kable(funcao(base_cubo_BA,TIPO_SEGURO, ind_rns)) %>%
  kable_styling()
```


## Score Serasa {.tabset .tabset-fade}

### Geral

```{r, echo=FALSE, message=FALSE, warning=FALSE}
kable(funcao(base_cubo,TIPO_SEGURO, pontuacao3)) %>%
  kable_styling()
```

### Analisar subscrição

```{r, echo=FALSE, message=FALSE, warning=FALSE}
kable(funcao(base_cubo_BA,TIPO_SEGURO, pontuacao3)) %>%
  kable_styling()
```


## Tempo de casa {.tabset .tabset-fade}

### Geral

```{r, echo=FALSE, message=FALSE, warning=FALSE}
kable(funcao(base_cubo,TIPO_SEGURO, tempo_casa)) %>%
  kable_styling()
```

### Analisar subscrição

```{r, echo=FALSE, message=FALSE, warning=FALSE}
kable(funcao(base_cubo_BA,TIPO_SEGURO, tempo_casa)) %>%
  kable_styling()
```
