
library(tidyverse)
library(dplyr)
library(ggplot2)

extinct <- read_csv("data/extinct_natives.csv")

boxplot(extinct$max_max_height, horizontal = FALSE)

plot1 <- ggplot(data = extinct,
                aes(y = max_max_height)) +
  geom_boxplot() +
  ylim(0,15)

plot1
