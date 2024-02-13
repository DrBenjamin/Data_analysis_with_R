### Course "Data analysis with R"
### Week 5
## Libraries
pacman::p_load(tidyverse, finalfit, gtsummary, here)

# Using the ApoE Study dataset:
# • Fit a simple logistic regression model for the outcome (gosfav) in terms of ApoE status.
# • What happens when we add the grouped GCS score as a second explanatory variable to the above model?
# • Is there any evidence that grouped GCS may be an effect modifier of the ApoE/ outcome relationship?
# Loading data
apoe <- read_csv(here("raw_data", "ApoE Study.csv"))
apoe <- apoe %>%
  mutate(gosfav = factor(gosfav, labels = c("Unfavourable (1-3)", "Favourable (4-7)"))) %>%
  mutate(gosfav_outcome = fct_relevel(gosfav, "Favourable (4-7)")) %>%
  mutate(apoe_e4 = factor(apoe_e4, labels = c("No", "Yes"))) %>%
  mutate(gcs_grp = factor(gcs_grp))

apoe.lr <- glm(gosfav ~ apoe_e4 + gcs_grp, data = apoe, familiy = binomial("logit"))
summary(apoe.lr)
exp(cbind(OR = apoe.lr$coeff, confint(apoe.lr)))
anova(apoe.lr, test = "Chisq")










gcs_grp
ApoE_W %>%
  select(alle_present, gos_outcome, gcs_grp) %>%
  drop_na() %>%
  group_by(gcs_grp) %>%
  table() %>%
  prop.table(margin = 1)
