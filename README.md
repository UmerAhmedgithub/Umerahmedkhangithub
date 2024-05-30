# Umerahmedkhangithub
My Script 6 with Sir Haseeb
# Activating repositories
setRepositories()
# installing packages
install.packages("readxl", dependencies = TRUE)
install.packages("tidyverse", dependencies = TRUE)
install.packages("agricolae", dependencies = TRUE)
install.packages("devtools", dependencies = TRUE)
devtools::install_github("JLSteenwyk/ggpubfigs", force = TRUE)
install.packages("ggpubr", dependencies = TRUE)
#Loading Packages
library(readxl)
library(tidyverse)
library(agricolae)
library(devtools)
library(ggpubfigs)
library(ggplot2)
library(ggpubfigs)
library(ggpubr)
# Additional libraries and packages 
install.packages("dplyr", dependencies = TRUE)
library(dplyr)
install.packages("MASS")
library(MASS)
install.packages("multcomp", dependencies = TRUE)
library(multcomp)
# Load data and necessary packages
data("PlantGrowth")
view(PlantGrowth)
# Applying statistics
df <- PlantGrowth
# Calculating the maximum value to draw letters
value_x <- df %>% group_by(group) %>% 
  summarize(max_value = max(weight)) 



# Performing HSD test
hsd <- HSD.test(aov(weight ~ group, data = df), trt = "group", group = TRUE)

sig.letters <- hsd$groups[order(row.names(hsd$groups)),]



# Visualization using ggplot
ggplot(data = df, mapping = aes(x = group, y = weight, fill = group)) +
  geom_boxplot() +
  stat_boxplot(geom = 'errorbar', width = 0.1) +
  geom_text(data = value_max, aes(x = group, y = 0.15 + max_value, 
                                  label = sig.letters$groups),
            vjust = 0) +
  labs(title = "Publication Ready Figures", x = "Treatments", y = "Weight") +
  theme_classic() +
  theme(legend.position = "none")
