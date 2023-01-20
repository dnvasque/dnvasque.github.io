---
layout: post
title: "Correlación de parámetros del suelo"
date: "20 - 01 - 2023"
---

\renewcommand{\figurename}{Figura}
\renewcommand{\tablename}{Tabla}
\vspace{-1cm}

```{=html}
<style>
body {
text-align: justify}
</style>
```

```{r message=FALSE, warning=FALSE, include=FALSE}
library(readxl)
library(rstatix)
library(dplyr)
library(tidyverse)
library(ggpubr)
library(gridExtra)
library(fmsb)
library(broom)
library(flextable)
library(xtable)
library(pander)
library(agricolae)
library(tidyverse)
library(ggthemes)
library(multcompView)
library(pander)
```

```{r include=FALSE}
data <- read.delim(
  "https://gist.githubusercontent.com/dnvasque/1d99bda3138cb33d60fa9ffb9597e34d/raw/c1e74e19ddb2edb4eff4cb625e6a0ce5b40fca57/cor_car_0-30")

data.cor <- data.frame(data)
df <- data.cor

df$cuartel <- as.factor(df$cuartel)
df$code <- as.factor(df$code)

df$AI3 <- df$AI3*-1

df2 <- df
df2[,1:2] <- NULL

names(df2)
df2$stockC <- NULL
df2$N <- NULL
df2$P <- NULL
df2$K <- NULL
df2$sb <- NULL
df2$psb <- NULL
df2$ce <- NULL
df2$magr <- NULL
df2$qCO2 <- NULL
df2$N <- NULL
df2$AA <- NULL
```

```{r echo= F, fig.cap = "Correlograma entre propiedades del suelo e indicadores de salud de suelo, 0-30 cm"}
library(corrplot)

datamatrix.car <- cor(df2, method = c("spearman"))
res <- cor.mtest(df2, conf.level = 0.95)
corrplot(datamatrix.car, p.mat = res$p, insig = "label_sig",cl.cex = 0.5, tl.cex = 0.5,
         sig.level = c(.001, .01, .05), pch.cex = .5, pch.col = "white", type = 'lower')
```
