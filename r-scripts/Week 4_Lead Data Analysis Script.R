#####  Example - An analysis of the Edinburgh Lead Study data set
#
# See the codebook provided separately online.
#
# Section headings below inserted to correspond to the Analysis flowchart
#
## Libraries
library(tidyverse)
library(finalfit)
library(here)


## Data Input
# Assumes working directory correctly set prior to this step
Lead <- read_csv("LeadAb.csv")

# Read Course script
browseURL(here("meta_data", "LEAD_example.pdf"))

# Read Codebook
browseURL(here("meta_data", "Codebook for LEADAB data.pdf"))


## Data Checking
# Can be useful to inspect data by eye - can often spot rogue values, data entry issues etc
view(Lead) 
# Useful quick summary of data set, will point out NA frequency etc. Do we get the expected range for
# categorical variables (i.e. codebook says 1,2 but seeing 3, 4, 99 as well etc?)
summary(Lead)       

# Now useful to convert categorical variables to factors (so can check level frequencies etc)
# Create a working data frame (adding a suffix "w") to leave the original data untouched...
# Levels are listed in numerical order: 0, 1, 2, etc
Leadw <- Lead %>%
  mutate(
    school = factor(school),
    sex = factor(sex, labels = c("Male", "Female")),
    moveschl = factor(moveschl, labels = c("No", "Yes")),
    classyr = factor(classyr, labels = c("Primary 3", "Primary 4")),
    timeday = factor(timeday, labels = c("AM", "PM")),
    handed = factor(handed, labels = c("Right", "Left")),
    msoc = factor(msoc),
    fsoc = factor(fsoc),
    mqualif = factor(
      mqualif,
      labels = c("None", "Apprentice", "School", "Higher School", "FE", "Degree")
    ),
    fqualif = factor(
      fqualif,
      labels = c("None", "Apprentice", "School", "Higher School", "FE", "Degree")
    ),
    unempl = factor(unempl, labels = c("Yes", "No")),
    workmum = factor(workmum, labels = c("No", "PT", "FT")),
    carphone = factor(carphone, labels = c("Neither", "Only 1", "Both")),
    gestat = factor(gestat, labels = c("38 Weeks+", "34-37 Weeks", "<34 Weeks")),
    brthwt = factor(brthwt, labels = c("No", "Yes")),
    medhist = factor(medhist, labels = c("No", "Yes"))
  )

summary(Leadw)       
# Useful check of recoding plus looking for categories that have very low frequencies - e.g. moveschl and unempl
# might not be very helpful as 1 category very small.

# In general, likely more to do here, but this is quite a clean data set, so can move on...


##  Exploratory Data Analysis
# Interested in possible relationships between sex, classyr, msoc (categorical) and
# bloodpb, ageint, famhist (continuous) on abscore, the educational attainment score for each child.
# Using quite basic plots here for now - will want to improve some of these for reporting purposes, but
# perhaps not necessary for basic checks at this stage.

Leadw %>%   # Look at abscore overall
  ggplot(aes(x = abscore)) +
  geom_histogram(bins = 20, colour = "blue")

# Look at abscore by each of the categorical variables - graph and numerically
Leadw %>%
  ggplot(aes(y = abscore, x = sex)) +
  geom_boxplot()
Leadw %>%
  group_by(sex) %>%
  summarize(
    min = min(abscore),
    mean = mean(abscore),
    median = median(abscore),
    max = max(abscore),
    SD = sd(abscore)
  )

Leadw %>%
  ggplot(aes(y = abscore, x = classyr)) +
  geom_boxplot()
Leadw %>%
  group_by(classyr) %>%
  summarize(
    min = min(abscore),
    mean = mean(abscore),
    median = median(abscore),
    max = max(abscore),
    SD = sd(abscore)
  )

Leadw %>%
  ggplot(aes(y = abscore, x = msoc)) +
  geom_boxplot()
Leadw %>%
  group_by(msoc) %>%
  summarize(
    min = min(abscore),
    mean = mean(abscore),
    median = median(abscore),
    max = max(abscore),
    SD = sd(abscore)
  )

# Look at individual continuous variables of interest
Leadw %>%
  ggplot(aes(x = bloodpb)) +
  geom_histogram(bins = 20, colour = "blue")
# Some positive skewness here (not unusually for this type of lab measurement). May be easier to look at this on transformed scale? scale?
Leadw %>%
  ggplot(aes(x = log(bloodpb))) +
  geom_histogram(bins = 20, colour = "blue")
Leadw %>%
  ggplot(aes(x = sqrt(bloodpb))) +
  geom_histogram(bins = 20, colour = "blue")
# Sqare root scale may be a reasonable choise to make relatinships easier to see.

Leadw %>%
  ggplot(aes(x = ageint)) +
  geom_histogram(bins = 20, colour = "blue")

Leadw %>%
  ggplot(aes(x = famhist)) +
  geom_histogram(bins = 20, colour = "blue")
# Note the irregular distribution - lots of absence of family disruption, small number of cases of some instances. Likely better replaced
# by a classification of None versus Some. Move this variable into the categorical group for analysis...

Leadw <- Leadw %>%
  mutate(famhistbin = famhist != 0)    # Create Boolean variable - T = score of 1 or more, F = score of 0

# Look at scatterplots of continuous variables against abscore
Leadw %>%
  ggplot(aes(y = abscore, x = sqrt(bloodpb))) +
  geom_point()
# Lower abscore associated with higher blood Pb?

Leadw %>%
  ggplot(aes(y = abscore, x = ageint)) +
  geom_point()
# Slightly difficult plot to interpret - needs some quantification to really see if any relationship.


## Develop Analysis Plan
# Thinking time needed... Happens off-line, but will need to think about data types, structure of problem etc. Here,
# can use t-tests to asses abscore vs categorical variables; msoc would need to use a 1-way ANOVA (discussed later, so leave for now).
# Abscore versus continuous can be assessed by correlation coefficient, and later would start using regression models etc 
# to do more complex multifactorial investigation.


## Investigate Assumptions
# Can see SDs for abscore in different groups from above, so perhaps just need to add Q_Q plots here
Leadw %>%
  ggplot(aes(sample = abscore)) +
  stat_qq() +
  stat_qq_line(color = 2) +
  facet_grid(cols = vars(sex))

Leadw %>%
  ggplot(aes(sample = abscore)) +
  stat_qq() +
  stat_qq_line(color = 2) +
  facet_grid(cols = vars(classyr))

Leadw %>%
  ggplot(aes(sample = abscore)) +
  stat_qq() +
  stat_qq_line(color = 2) +
  facet_grid(cols = vars(famhistbin))
# Sets of plots look reasonably Normal, so no particular concerns re t-test assumptions. Poorest is the smaller group of 
# children with family history of disruption - smaller = more noise, so less concerned about this.


## Carry Out Analysis
# t-tests first...
Leadw %>% t.test(.$abscore ~ .$sex, data = .)
Leadw %>% t.test(.$abscore ~ .$classyr, data = .)
Leadw %>% t.test(.$abscore ~ .$famhistbin, data = .)
# Omitting ANOVA for msoc for now...

with(Leadw, cor(abscore, sqrt(bloodpb)))  # Simple base R function - easier to use directly here, rather than piping
with(Leadw, cor(abscore, ageint))  
# Both bloodpb and age seem to be associated with decreasing abscore, which is interesting...

# More analysis will likely continue from here.


## Interpretation, Conclusions and Reporting now follow...
# May have code here for "presentation-quality" plots or additional information needed for reporting, not
# previously generated.

# May produce a report via Markdown, incorporating the key code from this script

