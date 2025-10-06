library(ggplot2)
library(dplyr)
potions <- readr::read_delim(paste0("http://perso.ens-lyon.fr/lise.vaudor/",
                                    "grimoireStat/datasets/potions.csv"),
                             delim=";")

potions_long=potions %>% 
  tidyr::pivot_longer(cols=starts_with("i_"),
                      names_to="i_type",
                      names_prefix="i_",
                      values_to="i_valeur")

ggplot(potions_long,
       aes(x=m_preparation, y=i_valeur, fill=m_formule))+
  geom_boxplot()+
  scale_y_log10()+
  facet_wrap(vars(i_type),
             scales="free_y")