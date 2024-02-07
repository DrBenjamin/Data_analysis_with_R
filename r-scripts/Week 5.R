### Course "Data analysis with R"
### Week 5
## Libraries
pacman::p_load(tidyverse, NHANES, here, ggplot2, cowplot)

browseURL(here("meta_data", "Codebook for Apo E data.pdf"))

browseURL(here("meta_data", "Codebook for LowBirthWt13 data.pdf"))

read_csv(here("raw_data", "ApoE Study.csv"))

read_csv(here("raw_data", "LowBirthWt13.csv"))
