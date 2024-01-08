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

# Create a new dataset called iris_tibble containing the iris dataset
iris_tibble <- iris %>%
  # select the sepal and species variables
  select(Sepal.Length,         
         Sepal.Width,         
         Species) %>%   
  # keep only the observations for setosa and virginica  
  filter(Species == "setosa"| Species == "virginica")  

# Look at the resulting object
glimpse(iris_tibble)

# Make a scatterplot for the sepal width against the sepal length
iris_tibble %>%
  # set up the ggplot object axes
  ggplot(aes(x = Sepal.Length, y = Sepal.Width))+
  # add data points coloured and shaped by species
  geom_point(aes(color=Species, shape=Species)) +
  # add better x and y axes titles 
  labs(x = "Sepal Length", y = "Sepal Width") +
  # change to a different theme  
  theme_classic()
