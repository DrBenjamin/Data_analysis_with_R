### Course "Data analysis with R"
### Week 1
## Libraries
pacman::p_load(tidyverse)

## Loading data
# Piped functions
esoph %>%
  filter(agegp == "65-74") %>%     # only keep rows where age group is 65 to 74
  filter(alcgp == "0-39g/day") %>% # only keep rows where alcohol consumption is 0 to 39 g per day
  select(-agegp) %>%               # remove the age group variable
  select(-alcgp)                   # remove the alcohol consumption variable
