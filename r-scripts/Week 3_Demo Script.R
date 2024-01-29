# Diagnostic Testing methods - Demonstration

# New package needed - install on first run of this script (then ignore)
install.packages(reportROC)

# Activate packages needed for the analyses
library(reportROC)
library(tidyverse)

######## Demo - CAD data

CAD<-read_csv(file ="CAD Example.csv",col_names = T)
CAD; # Just to check the structure of the data set

with(CAD, table(ETT,Disease)); # Simple tabulation to check numbers are same as lecture. Note that rows and columns are reversed, though!

# The reportROC package is very simple - just a single routine to calculate statistics, confidence intervals
# and draw ROC curves. Some alteration needed depending on whether binary or continuous test results are obtained.
# Variables used all need to be numeric.
# Parameters: gold = specifies the actual "disease" status
# predictor.binary = specifies the (binary) test result
# plot = turns off production of the ROC plot
# positive = identifies whether the test result that correlates with Disease is the larger (l) or smaller (s) value - 
# (in this example, the tabulation suggests that the larger value (ETT = 1) tracks with CAD)

reportROC(gold = CAD$Disease,predictor.binary = CAD$ETT,plot=F,positive='l')
# The output produces estimates and confidence intervals for the common measures like sensitivty, predictive values and likelihood ratios.


####### Demo - Pneumoconiosis example

pneumo<-read_csv(file = "Pneumo.csv",col_names = T)

# Quick look at how FEV1 associates with affected or unaffected status (use Group factor here);
# box and whisker plot of FEV1 by Group (using the character version of the diagnosis information makes a neater plot here)
pneumo %>%
  ggplot(aes(y=FEV1,x=Group))+
  geom_boxplot()

# For the next command, note that we are using a continuous "predictor = ", not a binary "predictor.binary ="
# Need to use the numeric Status variable, not Group character variable
# Also note that "positive" is set to "s" for small, as small values of FEV1 are associated with pneumonia, according to the boxplot.

reportROC(gold = pneumo$Status,predictor = pneumo$FEV1,plot=T,positive='s')

# Note that the output now identifies a cutoff value - FEV1 < 78.5 predicts Affected, larger values Unaffected.
# The AUC of the curve is 0.79 (95% CI 0.65, 0.93), which seems substantially larger than 0.5,
# so the test is having some diagnostic success. Room for improvement, though!

