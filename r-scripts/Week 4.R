### Course "Data analysis with R"
### Week 4
## Libraries
library(forcats)
library(tidyverse)

# Let's create a vector named colors made of colour names. What class is the colors vector?
colors <- c("red","blue","green","green","red","blue","red","green","blue")
# 1. Transform the colors vector into a factor and store it into a vector called colors_fct.
# 2. What class is colors_fct?
# 3. What are the levels of the colors_fct vector?
# 4. Using base R, change the order of the factor levels to red, blue and green.
# 5. What are the levels of colors_fct now?
# 6. Onto the Tidyverse (forcats) methods! Let's create a tibble named colTib with a variable named colours made of the colors_fct vector
# 7. Transform (recode…) the levels of the colours_fct variable so the colours start with an uppercase letter.

# 1.
colors_fct <- factor(colors)

# 2. 
View(colors_fct)
str(colors_fct)
class(colors_fct)

# 3. 
levels(colors_fct)

# 4. Using base R, change the order of the factor levels to red, blue and green.
colors_fct <- factor(colors_fct, levels = c("red", "blue", "green"))

# 5. What are the levels of colors_fct now?
levels(colors_fct)

# 6. Onto the Tidyverse (forcats) methods! Let's create a tibble named colTib with a variable named colours made of the colors_fct vector
colTib <- tibble(colours = colors_fct)
str(colTib)
  
# 7. Transform (recode…) the levels of the colours_fct variable so the colours start with an uppercase letter.
colTib <- colTib %>%
  mutate(colours = fct_recode(colours, "Red" = "red", "Blue" = "blue", "Green" = "green"))

