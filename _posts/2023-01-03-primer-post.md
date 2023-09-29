---
layout: post
title: "CAN"
---

```{=html}
<style>
body {
text-align: justify}
</style>
```
\renewcommand{\figurename}{Figura}
\renewcommand{\tablename}{Tabla}

\vspace{-1cm}

```{r eval=T, warning=FALSE, include=F}
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
```

```{r eval=T, warning=FALSE, include=F}
data <- read.delim(
  "https://gist.githubusercontent.com/dnvasque/6038c07b9f964610474e815887da5755/raw/8dad3d7c9504e18ede81994807782ee93b9ec9c1/CAN_pto_montt")
data <- data.frame(data)
data$Year <- as.factor(data$Year)
data$Month <- as.factor(data$Month)
data$mm <- as.numeric(data$mm)
```

```{r warning=FALSE, include=FALSE}
CAN.aov <- aov(mm ~ Month, data = data)
CAN.tukey <- TukeyHSD(CAN.aov)
cld <- multcompLetters4(CAN.aov, CAN.tukey, reversed = TRUE)
dt <- group_by(data, Month) %>%
  summarise(w=mean(mm), sd = sd(mm)) %>%
  arrange(desc(w))
cld <- as.data.frame.list(cld$Month)
dt$cld <- cld$Letters
```

```{r echo=FALSE}
dt$Month = factor(dt$Month, levels = c('Enero', 'Febrero', 'Marzo', 
                                       'Abril', 'Mayo', 'Junio',
                                       'Julio', 'Agosto', 'Septiembre',
                                       'Octubre', 'Noviembre', 'Diciembre'))
plot <- ggplot(dt, aes(Month, w)) + 
  geom_bar(stat = "identity", aes(fill = w), show.legend = F) +
  geom_errorbar(aes(ymin = w-sd, ymax=w+sd), width = 0.2) +
  labs(x = "", y = "Precipitaciones medias (mm)") +
  geom_text(aes(label = cld, y = w + sd), vjust = -0.5) +
  scale_y_continuous(expand = c(0, 0), limits = c(0,400)) +
  theme_classic() +
  theme(axis.text = element_text(angle = 45)) +
  theme(axis.text = element_text(hjust = 1)) +
  theme(panel.grid.major.y = element_line(color = "grey",
                                          linewidth = 0.25,
                                          linetype = 1)) +
  ggtitle("Identificación de CAN - Pluviometría") +
  theme(plot.title = element_text(hjust = 0.5))

plot
```
