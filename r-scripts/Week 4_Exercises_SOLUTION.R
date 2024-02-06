# Factors exercises
library(tidyverse)

# Exercise 1
# Let's create a vector named colors made of colour names
colors <- c("red","blue","green","green","red","blue","red","green","blue")
# What class is the colors vector?
class(colors)
# colors belongs to the character class

# Transform the colors vector into a factor and store it into a vector called colors_fct
colors_fct <- factor(colors)
colors_fct
# What class is colors_fct?
class(colors_fct)
# colors_fct belongs to the factor class
str(colors)
str(colors_fct)
# What are the levels of the colors_fct vector?
levels(colors_fct)
# Using base R, change the order of the factor levels to red, blue and green
colors_fct <- factor(colors_fct, 
       levels = c("red","blue", "green"))
# What are the levels of colors_fct now?
levels(colors_fct)

# Onto the Tidyverse (forcats) methods...
# Let's create a tibble named colTib with a variable named colours made of the original colors vector
colTib <- tibble(colours_fct = colors_fct)
# Transform the levels of the colours_fct variable so the colours start with an uppercase letter
colTib <- colTib %>%
  mutate(colours_fct = fct_recode(colours_fct, "Red" = "red","Blue" = "blue","Green"= "green"))
levels(colTib$colours_fct)
glimpse(colTib)

# Exercise 2
# We will use the airquality dataset that comes with R
data(airquality)
glimpse(airquality)
# The month column is data type integer, let’s change it to a factor
airquality <- airquality %>%
  mutate( Month_fct = factor(Month) )
levels(airquality$Month_fct)
# Let’s rename the months from a number to the month's name using the fct_recode function.
airquality <-
  airquality %>% 
  mutate(
    Month_fct = fct_recode(Month_fct, 
                           May = '5', June = '6',
                           July = '7', Aug = '8',
                           Sept = '9')
  )

glimpse(airquality$Month_fct)
# Let's create a boxplot of the temperature for each month
ggplot(airquality, aes(x = Month, y = Temp)) +
  geom_boxplot(aes(fill = Month)) +
  labs(title = "Daily Temperatures Aggregated by Month", x = "Month", fill = "Month")
# This doesn't work as expected because the Month variable is a character
ggplot(airquality, aes(x = Month_fct, y = Temp)) +
  geom_boxplot(aes(fill = Month_fct)) +
  labs(title = "Daily Temperatures Aggregated by Month", x = "Month", fill = "Month")

# Another useful function is fct_relevel. This function allows us to change 
# any number of levels to any position.
# Let's change the airquality dataset with months in the following order:
# Sept, July, May, Aug, June
airquality <- airquality %>%
  mutate(Month_fct = 
      fct_relevel(Month_fct, 
                  'Sept', 'July', 'May', 'Aug', 'June'))

levels(airquality$Month_fct)
# Given that our month variable was already in the correct order, this may not 
# seem useful at first. However, when you need to visualize or model your data 
# in a particular way, the fct_relevel function is extremely useful!
airquality %>%
  ggplot(aes(x=Month_fct, y=Temp)) +
  geom_boxplot(aes(fill = Month_fct)) +
  ggtitle(label = "Notice how the order of the level 'Month' has changed")

# Exercise 3
# Write the code and results for these two exercises into an RMarkdown file.
