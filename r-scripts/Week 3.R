### Course "Data analysis with R"
### Week 3
## Libraries
pacman::p_load(tidyverse, finalfit, gtsummary, here, NHANES)


## Histogram
NHANES %>%
  ggplot(aes(x = Age)) +
  geom_histogram(aes(y = ..count.., fill = ..density..), bins = 20)

NHANES %>% t.test(x = .$Age, mu = 35, data = ., conf.int = TRUE)


## Non-parametric analog = Wilson signed rank test
NHANES %>% wilcox.test(x = .$Age, mu = 35, data = ., conf.int = TRUE)


## 2 sample t-test version for comparison
NHANES %>%
  drop_na(Weight) %>%
  ggplot(aes(x = Weight)) +
  geom_histogram(aes(y = ..count.., fill = ..density..)) +
  facet_grid(cols = vars(Gender))
# Possibly normally distributed, although interesting "bumps" on the left hand side (children?)

NHANES %>%
  t.test(.$Weight ~ .$Gender, data = .)

NHANES %>%
  wilcox.test(.$Weight ~ .$Gender, data = ., conf.int = TRUE)


## A new test, possibly cheaper than existing options, to detect the presence 
## of Helicobacter pylori is being developed. In the sample of individuals 
## tested (drawn from the attenders of adyspepsia clinic in Glasgow), 
## diagnosis was confirmed by endoscopic biopsy. The data are in the file: 
## Helicobacter.csv. Test is coded as 0 for negative and 1 for positive. Biopsy 
## is coded as 0 if H. pylori is absent and 1 if H. pylori is present
## 1.Calculate appropriate summary measures for test performance and describe 
## the test’s performance in this population.
install.packages("reportROC")
library(reportROC)
#Binary
Helicobacter <- read_csv(here("raw_data", "Helicobacter.csv"))
with(Helicobacter, table(Test, Biopsy))

reportROC(gold = Helicobacter$Biopsy, predictor.binary = Helicobacter$Test,plot = FALSE, positive = 'l')
#with prevalence 60% (1269 *0.6 = 761)



Helicobacter60 <- tibble(testpositive = c(rep("1",794), rep("0",475)), biopsy = c(rep("1",708), rep("0",86), rep("1",53), rep("0",422)))
with(Helicobacter60, table(testpositive, biopsy))
reportROC(gold = Helicobacter60$biopsy, predictor.binary = Helicobacter60$testpositive, plot = FALSE, positive = 'l')



#continuous
AnginaMI <- read_csv("AnginaMI.csv")
AnginaMI %>%
  ggplot(aes(y = CreatineKinase,x = Diagnosis))+
  geom_boxplot()

reportROC(gold = AnginaMI$DiagnosisCat, predictor = AnginaMI$CreatineKinase,plot = TRUE, positive = 'l')

# b) boxplot
AnginaMI %>%
  ggplot(aes(y = CreatineKinase, x = Diagnosis))+
  geom_boxplot(outlier.shape = NA)


## 2.b) What would you estimate the PPV and NPV of the test to be if it was 
## applied in London, if the prevalence of H. pylori was about 60% there?


## Extra Stuff
getOption("defaultPackages")

# Update packages
old.packages()
install.packages("installr")
library(installr)
updateR()
.libPaths()

# Show packages
installed.packages()
available.packages()

# Show Libraries
library()
lapply(.libPaths(), list.dirs, recursive = FALSE, full.names = FALSE)
search()