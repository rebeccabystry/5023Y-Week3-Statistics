library(tidyverse)
library(skimr)
library(rstatix)
library(equatiomatic)
library(patchwork)

wood_data <- read_csv("Data/wood_density.csv")

str(wood_data)
view(wood_data)

wood_data %>% 
  ggplot(aes(x=Density, y=Hardness)) +
  geom_smooth(method="lm") +
  geom_point()

help(geom_smooth)