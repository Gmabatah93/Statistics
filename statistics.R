library(readr)
library(dplyr)
library(ggplot2)
library(infer)

# NUMERICAL: Single Parameter ----

# On a given day, 20 1 bedroom apartments were randomly selected on Craiglist Manhattan from apartments listed "by owner". Is the mean of the median a better measure of typical rent in Manhattan ?
# Data
manhat <- read_csv("Data/manhattan.csv")
# EDA
# - median
median_hat <- manhat %>% 
  specify(response = rent) %>% 
  calculate(stat = "median") %>% 
  pull
# - visual
manhat %>% 
  ggplot(aes(rent)) +
  geom_histogram(bins = 20) +
  geom_vline(xintercept = median_hat, color = "red")

# Confidence Interval
# - 15,000 bootsrap samples 
median_boot <- manhat %>% 
  specify(response = rent) %>% 
  generate(reps = 15000, type = "bootstrap") %>% 
  calculate(stat = "median")
# - Percentile Method
median_boot %>% get_confidence_interval(type = "percentile")
# - Standard Error Method
median_boot %>% get_confidence_interval(type = 'se', point_estimate = median_hat)

# Hypothesis Test: Rent < $2,500
# - 15,000 bootstrap distibution centered at null
median_null_boot <- manhat %>% 
  specify(response = rent) %>% 
  hypothesize(null = "point", med = 2500) %>% 
  generate(reps = 15000, type = "bootstrap") %>% 
  calculate(stat = "median")
# - pvalue
median_null_boot %>% get_p_value(obs_stat = median_hat, direction = "left")
median_null_boot %>% visualise() + shade_p_value(obs_stat = median_hat, direction = "left")

# Conclusion
# - With a 95% we reject the H0
# - We are 95% Confidence that the true Median Rent in Man is btw [2010, 2690]




# The state of NC released to the public a large dataset containing information on births recorded in this state. 
# The dataset has been of interest to medical researchers studying the relation between habits and practices of expectant mothers and the birth of their children. 

# Data
ncbirths <- openintro::ncbirths
# - clean: NAs
ncbirths_complete <- ncbirths %>% filter(!is.na(visits))

# Confidence Interval
# - 15,000 bootstrap distibution
visits_boot <- ncbirths_complete %>% 
  specify(response = visits) %>% 
  generate(reps = 15000, type = "bootstrap") %>% 
  calculate(stat = "mean")

# Hypothesis Test: Birth Weigh > 7lbs
# - 15,000 bootstrap centered around the null
weight_null_boot <- ncbirths_complete %>% 
  specify(response = visits) %>% 
  hypothesize(null = "point", mu = 7) %>% 
  generate(reps = 1500, type = "bootstrap") %>% 
  calculate(stat = "mean")
# - observed mean
weight_mean_hat <- ncbirths_complete %>%
  specify(response = weight) %>% 
  calculate(stat = "mean")
# - p value
weight_null_boot %>% visualise(obs_stat = weight_mean_hat, direction = "right")
weight_null_boot %>% get_p_value(obs_stat = weight_mean_hat, direction = "right")
# Confidenct Interval
x_boot <- ncbirths_complete %>% 
  specify(response = weight) %>%
  generate(reps = 1500, type = "bootstrap") %>% 
  calculate(stat = "mean")
x_boot %>% get_confidence_interval(type = "se", point_estimate = x_hat, level = 0.95)
x_boot %>% visualise() + shade_ci(endpoints = c(7.02, 7.20))
# Conclusion
# - With a 95% Confidence we REJECT the null Hypothesis
# - We are 95% Confidence the True mean lies between [,]





# NUMERICAL: Two Population ----

# A random sample of taken of nearly 10% of UCLA courses. We want to test whether there is a difference between the average prices of textbooks sold in the bookstore vs. on Amazon.

# Data
text <- openintro::textbooks

# Confidence Interval: Mean
t.test(text$diff, conf.level = 0.90)
t.test(text$diff, conf.level = 0.95)
t.test(text$diff, conf.level = 0.99)
# Confidence Interval: Median
text_diff_median <- text %>% 
  specify(response = diff) %>% 
  generate(reps = 15000, type = "bootstrap") %>% 
  calculate(stat = "median")
text_diff_median %>% get_confidence_interval(type = 'percentile')



# The highschool and beyond survey is conducted on high school seniors by the National Center of Education Statistics.
# We randomly sampled 200 observations from this survey

# Data
hsb2 <- openintro::hsb2
hsb2_diff <- hsb2 %>% select(math, science) %>% 
  mutate(diff = math - science)
# - median
hsb_median <- hsb2_diff %>% 
  summarise(median_diff = median(diff)) %>% 
  pull()
# 15,000 Bootstrap medians centered at Null
hsb_diff_boot <- hsb2_diff %>% 
  specify(response = diff) %>% 
  hypothesise(null = "point", med = 0) %>% 
  generate(reps = 15000, type = "bootstrap") %>% 
  calculate(stat = "median")
# pvalue
hsb_diff_boot %>% visualise(obs_stat = hsb_median, direction = "both")
hsb_diff_boot %>% get_p_value(obs_stat = hsb_median, direction = "both")




# Does a treatment using embryonic stem cells help improve heart function following a heart attack more so than tradional therapy ?
# Is there a difference between the mean heart pumping capacities of sheep's hearts in the control and treatment groups ?

# Data
stem <- openintro::stem_cell
stem_diff <- stem %>% mutate(change = after - before)
# - obs mean
diff_mean <- stem_diff %>% group_by(trmt) %>% 
  summarize(change = mean(change)) %>% 
  summarize(diff = diff(change)) %>%
  pull
# 1,000 different means via randomization (Sampling Distribution)
stem_diff_perm <- stem_diff %>% 
  specify(change ~ trmt) %>% 
  hypothesise(null = "independence") %>% 
  generate(reps = 1000, type = "permute") %>% 
  calculate(stat = "diff in means", order = c("esc", "ctrl"))
# p value
stem_diff_perm %>% visualise(obs_stat = diff_mean, direction = "both")
stem_diff_perm %>% get_p_value(obs_stat = diff_mean, direction = "both")



# The state of NC released to the public a large dataset containing information on births recorded in this state. 
# The dataset has been of interest to medical researchers studying the relation between habits and practices of expectant mothers and the birth of their children. 

# Data
ncbirths <- openintro::ncbirths
# - clean: NAs
ncbirths_complete <- ncbirths %>% filter(!is.na(visits))
# - observed difference in means
births_diff_means <- ncbirths_complete %>% 
  group_by(habit) %>% 
  summarize(mean_weight = mean(weight)) %>% 
  pull() %>% diff()

# 1,000 different means via randomization (Sampling Distribution)
births_diff_perm <- ncbirths_complete %>% 
  specify(weight ~ habit) %>% 
  hypothesise(null = "independence") %>% 
  generate(reps = 1000, type = "permute") %>% 
  calculate(stat = "diff in means", order = c("nonsmoker","smoker"))
# - p value
births_diff_perm %>% visualise(obs_stat = births_diff_means, direction = "both")
births_diff_perm %>% get_p_value(obs_stat = births_diff_means, direction = "both")

# 1,500 bootstrap difference in means
births_diff_boot <- ncbirths_complete %>% 
  specify(weight ~ habit) %>% 
  generate(reps = 1500, type = "bootstrap") %>% 
  calculate(stat = "diff in means", order = c("nonsmoker","smoker"))
# - confidence interval
births_diff_boot %>% get_confidence_interval(level = 0.95, type = "percentile")



# HOURS WORK vs. GENDER
acs12 <- openintro::acs12
acs_hrs <- acs12 %>% 
  filter(!is.na(hrs_work), !is.na(gender)) %>% 
  select(hrs_work, gender)
# EDA
ggplot(acs_hrs, aes(gender, hrs_work)) + geom_boxplot()
obs_diff <- acs_hrs %>% 
  group_by(gender) %>% 
  summarize(mean_hrs = mean(hrs_work)) %>% 
  summarize(obs_diff = diff(mean_hrs)) %>% pull
# HYPOTHESIS-Unpaired(t)
t.test(hrs_work ~ gender, data = acs_hrs)
acs_hrs %>% 
  t_test(formula = hrs_work ~ gender,
         alternative = "two-sided",
         mu = 0)
