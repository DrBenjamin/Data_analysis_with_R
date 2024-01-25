# Install NHANES if you don't already have it
install.packages("NHANES")

# Load the tidyverse and NHANES packages
library(tidyverse)
library(NHANES)

# Create dataframe called 'data' containing the NHANES dataset
data <- NHANES



###############
## Histogram ##
###############
# Create a histogram for the DirectChol variable in female individuals
data %>%
  filter(Gender == "female") %>%
  drop_na(DirectChol) %>%
  ggplot(aes(x = DirectChol)) + 
    geom_histogram()

# Change the binwidth to 0.1 and the boundary to 1
data %>%
  filter(Gender == "female") %>%
  drop_na(DirectChol) %>%
  ggplot(aes(x = DirectChol)) + 
    geom_histogram(aes(y = after_stat(density)), binwidth = 0.1, boundary = 1)

# Change the bar colours, labels for the x and y axes, and theme of the plot.
data %>%
  filter(Gender == "female") %>%
  drop_na(DirectChol) %>%
  ggplot(aes(x=DirectChol)) + 
    geom_histogram(aes(y = after_stat(density)), binwidth = 0.1, boundary = 1,
                   col = "darkmagenta", fill = "thistle") +
    ylab("Density") +
    xlab("Direct HDL cholesterol (mmol/L)") +
    theme_classic(base_size = 18)

# Split the histogram to have a separate histogram for each BMI_WHO value
data %>%
  filter(Gender == "female") %>%
  drop_na(DirectChol, BMI_WHO) %>%
  ggplot(aes(x=DirectChol)) + 
    geom_histogram(aes(y = after_stat(density)),
                   binwidth = 0.1,
                   boundary = 1,
                   col = "darkmagenta",
                   fill = "thistle") +
    facet_grid(rows = vars(BMI_WHO)) +
    ylab("Density") +
    xlab("Direct HDL cholesterol (mmol/L)") +
    theme_classic(base_size = 18)



#############
## Boxplot ##
#############
# Create a boxplot for the DirectChol variable for each value of the BMI_WHO 
# variable in female individuals aged 20 to 25
data %>% 
  filter(Gender == "female" & Age >= 20 & Age <= 25) %>%
  drop_na(DirectChol) %>%
  ggplot(aes(y = DirectChol)) +
    geom_boxplot()

# Split the boxplot to have a separate boxplot for each category of BMI_WHO,
# remove outliers, add the data points onto the plots, change the colours,
# themes and labels.
data %>% 
  filter(Gender== "female" & Age >= 20 & Age <= 25) %>%
  drop_na(DirectChol, BMI_WHO) %>%
  ggplot(aes(x = BMI_WHO, y = DirectChol, color = BMI_WHO)) +
    geom_boxplot(outlier.shape = NA) +
    scale_color_brewer(palette = "Dark2") + 
    geom_jitter(shape = 16, position = position_jitter(0.2)) +
    labs(title = "Direct HDL cholesterol per BMI category",
         y = "Direct HDL cholesterol (mmol/L)",
         x = "BMI category (WHO)",
        col = "BMI category") +
    theme_classic(base_size = 14)



#############
## Barplot ##
#############
# Create a barchart for AgeDecade in female individuals aged over 20
data %>% 
  filter(Gender == "female" & Age >= 20) %>%
  drop_na(AgeDecade) %>%
  ggplot(aes(x = AgeDecade)) +
  geom_bar()

# Add the Education variable to create a side-by-side barchart, change the colours,
# labels and theme.
data %>% 
  filter(Gender =="female" & Age >= 20) %>%
  drop_na(AgeDecade, Education) %>%
  ggplot(aes(x = AgeDecade, fill = Education)) +
    geom_bar(position = 'dodge') +
    scale_fill_brewer(palette = "YlGnBu") +
    labs(x = "Age decade",
         y = "Number of individuals") +
    theme_classic(base_size = 16)

# Transform the barplot into a percentage barplot (group function).
data %>%
  filter(Gender == "female" & Age >= 20) %>%
  drop_na(AgeDecade,Education) %>%
  ggplot(aes(x = AgeDecade, group = Education, fill = Education)) +
    geom_bar(aes(y = ..prop.. ), position = 'dodge') +
    scale_fill_brewer(palette = "YlGnBu") +
    labs(x = "Age decade",
         y = "Percentage of individuals") +
    theme_classic(base_size = 16) +
    scale_y_continuous(labels = scales::percent)

# Display a separate barplot for each BMI_WHO category
data %>% 
  filter(Gender=="female" & Age >= 20) %>%
  drop_na(AgeDecade,Education) %>%
  ggplot(aes(x = AgeDecade, group = Education, fill = Education)) +
    geom_bar(aes(y = ..prop.. )) +
    scale_fill_brewer(palette="YlGnBu") +
    labs(x = "Age decade",
         y = "Percentage of individuals") +
    theme_classic(base_size = 16) +
    scale_y_continuous(labels = scales::percent) +
    facet_grid(cols = vars(Education))



###############
## Tile plot ##
###############
# Create a tile plot to visualise counts of individuals in each combination of
# the AgeDecade and Education variables
data %>% 
  filter(Gender =="female") %>% 
  drop_na(AgeDecade, Education) %>%
  count(AgeDecade, Education) %>%
  # Flip the counts so that the plot color is the right way up
  mutate(n = -n) %>%
  ggplot(aes(x = AgeDecade, y = Education)) +
    geom_tile(aes(fill = n)) +
    labs(x = "Age decade", y = "Education", fill = "Count") +
    scale_fill_distiller(palette = "RdPu") +
    theme_classic(base_size = 16)



#################
## Scatterplot ##
#################
# Initialise a plot object called init for the DirectChol variable against the
# BMI variable in female individuals aged over 18
init <- data %>%
  drop_na(BMI, DirectChol) %>%
  filter(Gender == "female" & Age >= 18) %>% 
  ggplot(aes(x = BMI, y = DirectChol))

# Create a scatter plot for the DirectChol variable against the
# BMI variable in female individuals aged over 18
init +
  geom_point()

# Add a trend line, and change the colours, labels and theme.
init +
  geom_point(col = "darkmagenta") + 
    geom_smooth(method = lm, col = "black", lwd = 1.2) +
    xlab("BMI") + 
    ylab("Direct HDL cholesterol (mmol/L)") +
    theme_classic(base_size = 16)
