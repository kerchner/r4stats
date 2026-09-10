# Data set is from this paper:
#  Bernard, G.R., Wheeler, A.P., Russell, J.A., Schein, R., Summer, W.R.,
#  Steinberg, K.P., et al. The effects of ibuprofen on the physiology and
#  survival of patients with sepsis.
#  The Ibuprofen in Sepsis Study Group. N. Engl. J. Med. 1997, 336: 912-8.
#  https://doi.org/10.1056/NEJM199703273361303

# Load packages
# If you've never used these packages before, you'll need to:
#   install.packages('dplyr', 'ggplot2)
library(dplyr)
library(ggplot2)

# Create a 'data' folder
dir.create('data')

# Download the data and save it locally
download.file("https://go.gwu.edu/sepsisdata", "data/sepsis.csv")

# Read the data into an R data frame
df <- read.csv('data/sepsis.csv')

# explore the data a bit
summary(df)
summary(df$temp0)

# explore a categorical variable that we will be using for grouping
table(df$treat)
table(df$treat, useNA='always')

# another way to check for missing data
table(!is.na(df$temp2))

# H0: Mean baseline temp. of sepsis patients is 101F
# Ha: not equal to 101F

# 1-sample t-test ----

## Histogram 

df %>% ggplot(aes(x = temp0)) +
  geom_histogram(aes(y = ..density..),
                 color = "red", binwidth = 1) +
  labs(title = "Histogram of Baseline Temperature",
       x = "Baseline temperature (F)",
       y = "density") +
  theme(plot.title = element_text(hjust = 0.5)) +
  # superimpose a normal distribution with the same mean and standard deviation
  stat_function(fun = dnorm,
                args = list(mean = mean(df$temp0),
                            sd = sd(df$temp0)))

# Mean looks like it would be reasonably representative of the
# central tendency of the data

# test whether temp0 and "rnorm" come from the same distribution (in this case, normal)
ks.test(df$temp0, "rnorm")

# Look at normality
df %>%
  ggplot(aes(sample = temp0)) +
  geom_qq() +
  geom_qq_line()

# Central Limit Theorem tells us that when sample size is "large",
# violation of normality is not a major issue


t.test(df$temp0, mu = 101, conf.level = 0.90)

# Note the test statistic, df (degrees of freedom), p value

# Extract some key values from the t test result

ttest1 <- t.test(df$temp0, mu = 100.7, conf.level = 0.90)

pv <- ttest1$p.value
ci_lb <- ttest1$conf.int[1]
ci_ub <- ttest1$conf.int[2]

# Nonparametric alternative (more conservative):
# Wilcoxon Signed Rank Test

wilcox.test(df$temp0, mu = 100.7, conf.level = 0.95)

# H0:  Mean baseline temp. => 100.5F
# Ha:  Mean baseline temp < 100.5F
# alpha = 0.1 / conf. level = 90%

t.test(df$temp0, mu = 100.5, conf.level = 0.90,
       alternative = "less")


####
# Paired T Test ----
# For treated patients, is there a reduction in temperature after 2 hours?
####

treated_df <- df %>%
  filter(treat == 1) %>%
  mutate(temp0_2 = temp0 - temp2)
  
table(!is.na(treated_df$temp0_2))

treated_df %>% ggplot(aes(x = temp0_2)) +
  geom_histogram(aes(y = ..density..),
                 color = "red", binwidth = 1) +
  labs(title = "Histogram of Temperature Reduction",
       x = "Temperature Reduction (F)",
       y = "density") +
  theme(plot.title = element_text(hjust = 0.5)) +
  stat_function(fun = dnorm,
                args = list(mean = mean(treated_df$temp0_2, na.rm = TRUE),
                            sd = sd(treated_df$temp0_2, na.rm = TRUE)))  

treated_df %>%
  ggplot(aes(sample = temp0_2)) +
  geom_qq() +
  geom_qq_line() 

# Paired t-test
t.test(treated_df$temp0, treated_df$temp2, mu = 0,
       paired = TRUE,
       conf.level = 0.95)

# Performing this as a one-sample t-test looking at the difference in temperatures
# should yield the same result
t.test(treated_df$temp0_2, mu = 0,
       conf.level = 0.95)



####
# 2-sample T-Test ----
# Two groups:  Treated, and Control
####

# Look at the number of patients in each group
df %>%
  ggplot() +
  aes(x = factor(treat), fill = factor(treat)) + # try this first without factor!
  geom_bar()

# Alternatively:
df %>% count(treat) %>%
  ggplot() +
  aes(x = factor(treat), y = n) +
  geom_col()

# Make "treat" into a factor variable so it can be handled
# as a categorical variable
df <- df %>%
  mutate(treat = factor(treat,
                        levels = c(0, 1),
                        labels = c("Control",
                                   "Treatment")),
         temp0_2 = temp0 - temp2)

# new bar plot but with treat as a factor variable
df %>%
  ggplot() +
  aes(x = treat, fill = treat) +
  geom_bar()
  
# Ways to get the count of patients in each group
table(df$treat)
df %>% count(treat)

# We can't really overlap histograms
# So we'll use density plots
df %>%
  ggplot() +
  geom_density(aes(x = temp0_2, fill = treat, color = treat),
               alpha = 0.4) +
  labs(title = "Distribution of Temperature Reduction",
       x = "Temperature Reduction (F)",
       y = "density") +
  theme(plot.title = element_text(hjust = 0.5))

df %>%
  ggplot(aes(sample = temp0_2)) +
  geom_qq() +
  geom_qq_line() +
  facet_wrap(~ treat)

df3 %>%
  ggplot(aes(x = temp0_2, y = treat,
             fill = treat)) +
  geom_violin(alpha = 0.7, outliers = FALSE, fill = NA) +
  geom_jitter(aes(color = treat),
              width = 0, height = 0.1, alpha = 0.7) + 
  labs(title = "Temperature Reduction",
       subtitle = "Baseline to 2 Hours",
       x = "Temperature Reduction (F)",
       y = "Treatment Group") +
  theme(plot.title = element_text(hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5)) +
  theme(legend.position = "none")

# F Test to compare variances between the two sample groups.
# Is the ratio between 0.5 - 2 ?
var.test(data = df, temp0_2 ~ treat)

t.test(data = df, temp0_2 ~ treat)  # Default var.equal = FALSE

t.test(data = df, temp0_2 ~ treat,
       var.equal = TRUE)  

df %>%
  group_by(treat) %>%
  summarize(n_group = n(),
            mean_temp_reduction = mean(temp0_2, na.rm = TRUE),
            var_temp_reduction = var(temp0_2, na.rm = TRUE))

# Non-parametric alternative 

wilcox.test(data = df, temp0_2 ~ treat, var.equal = FALSE)

####
# ANOVA ----
####

df %>% ggplot(aes(x = apache)) +
  geom_histogram(color = "red", binwidth = 1) +
  labs(title = "Histogram of APACHE II Score",
       x = "APACHE II Score",
       y = "n") +
  theme(plot.title = element_text(hjust = 0.5)) 

# Create 3 categories
df4 <- df %>%
  mutate(apache_cat = cut(apache,
                          breaks = c(0, 12, 20, 50),
                          labels = c("Poor", "Moderate", "Good")),
         temp0_2 = temp0 - temp2)

# Confirm that the categorizing worked, using table()
table(df4$apache_cat, df4$apache, useNA = "always")

df4 %>% ggplot() + aes(x = apache_cat, fill = apache_cat) +
  geom_bar() +
  theme(plot.title = element_text(hjust = 0.5)) 

# Filter down to just treated patients, remove missing data
df4_treat <- df4 %>%
  filter(treat == "Treatment") %>%
  filter(!is.na(temp0_2)) %>%
  filter(!is.na(apache_cat))

table(df4_treat$apache_cat)

df4_treat %>% ggplot(aes(x = temp0_2)) +
  geom_histogram(aes(y = ..density..),
                 color = "red", binwidth = 1) +
  labs(title = "Histogram of Temperature Reduction",
       x = "Temperature Reduction (F)",
       y = "density") +
  theme(plot.title = element_text(hjust = 0.5)) +
  stat_function(fun = dnorm,
                args = list(mean = mean(df4$temp0_2, na.rm = TRUE),
                            sd = sd(df4$temp0_2, na.rm = TRUE))) +
  facet_wrap(~ apache_cat)

df4_treat %>%
  ggplot(aes(sample = temp0_2)) +
  geom_qq() +
  geom_qq_line() +
  facet_wrap(~ apache_cat)

df4_treat %>%
  ggplot(aes(x = temp0_2, y = apache_cat,
             fill = apache_cat)) +
  geom_boxplot(alpha = 0.7) +
  labs(title = "Temperature Reduction",
       subtitle = "Baseline to 2 Hours",
       x = "Temperature Reduction (F)",
       y = "APACHE II Score Group") +
  theme(plot.title = element_text(hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5)) +
  theme(legend.position = "none") +
  coord_flip()

# If normally distributed, use Bartlett's test to hypothesis test whether
# variances are all homogeneous / equal
bartlett.test(data = df4_treat, temp0_2 ~ apache_cat)

# If not normally distributed, use car::leveneTest()
# install.packages("car")
# leveneTest...

anova_results <- anova(lm(data = df4_treat, temp0_2 ~ apache_cat))

anova_results

pairwise.t.test(df4_treat$temp0_2, df4_treat$apache_cat,
                p.adj = "none")

# Use Bonferonni correction for small number of comparisons
# (multiplies p-value by # of pairwise comparisons)
pairwise.t.test(df4_treat$temp0_2, df4_treat$apache_cat,
                p.adj = "bonf")

# Use Tukey's HSD for larger number of comparisons
# Also provides confidence interval for each difference
TukeyHSD(aov(temp0_2 ~ apache_cat, data = df4_treat))
