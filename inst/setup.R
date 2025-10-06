library(ggplot2)
library(dplyr)
potions <- readr::read_delim("http://perso.ens-lyon.fr/lise.vaudor/grimoireStat/datasets/potions.csv",
                             delim=";")