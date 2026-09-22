# Data sets are from these papers:
#
# MacMahon, B., Cole, P., Lin, T. M., Lowe, C. R., Mirra, A. P., Ravnihar, B., Salber, E. J.,
# Valaoras, V. G., & Yuasa, S.  (1970). Age at first birth and breast cancer risk.
# Bulletin of the World Health Organization, 43, 209-221.
#
# Mandel, E., Bluestone, C. D., Rockette, H. E., Blatter, M. M., Reisinger, K. S., 
# Wucher, E. P., & Harper, J. (1982). Duration of effusion after antibiotic treatment
# for acute otitis media: Comparison of cefaclor and amoxicillin. Pediatric Infectious Diseases, 1, 310–316.

library(dplyr)
library(ggplot2)
library(readxl)
library(forcats)

# Download links without the GW link shortener:
#  https://ndownloader.figshare.com/files/25320434?private_link=91f0bbf7458cd866f43c
#  https://ndownloader.figshare.com/files/25320440?private_link=c35de8173b8f2c3fd19b

download.file(url = 'https://go.gwu.edu/bcdata',
              destfile = 'data/BC.xlsx', mode = 'wb')

download.file(url = 'https://go.gwu.edu/eardata',
              destfile = 'data/EAR.xlsx', mode = 'wb')

########################################
# 1. Inference for sample proportion
########################################

# Let's look at the proportion of smokers vs. non-smokers
# in this data set as compared to a hypothetical percentage

bc_df <- read_xlsx('data/BC.xlsx')

table(bc_df$psmk, useNA = 'always')

psmk_table <- table(bc_df$psmk)

# prop.table() and proportions() are equivalent
prop.table(psmk_table)
proportions(psmk_table)

# Currently psmk is numerical - 0 or 1,
# so when we plot it, we get this:
bc_df %>% ggplot() +
  geom_bar(aes(x = psmk))

# We can also plot as _relative_ frequency (proportion)
bc_df %>% ggplot() +
  geom_bar(aes(x = psmk,
               y = ..prop.., # <- height is proportional within each group
               group = 1)) # <-- This groups all data into one 'hard coded' group called 1.  It could be called anything.


# Fix it to a factor variable, true categorical variable
bc_df <- bc_df %>%
  mutate(psmk = factor(psmk)) %>% # first make it a factor
  mutate(psmk = fct_recode(psmk,  # then recode it
                           "Never Smoked" = "0",
                           "Smoked" = "1"))
# note the labels now
table(bc_df$psmk, useNA = "always")

# note the better graph
bc_df %>% ggplot(aes(x = psmk, fill = psmk)) + # <-- bad style!
  geom_bar() +
  labs(title = "Smoking History",
       y = "n",
       x = "") +
  theme(plot.title = element_text(hjust = 0.5),
        legend.position = "none") # Remove the legend, it adds no information in this case

###
# Test of proportions
###

# Test to determine whether our data do or don't support
# the hypothesis of an observed proportion of 1/3 who smoked
prop.test(table(bc_df$psmk), p = 0.333, correct = FALSE)
# correct = TRUE adds continuity correction
# Continuity correction comes into play when you're using a chi-squared test or z-test for proportions
# and your data are discrete counts, but the test statistic assumes a continuous distribution.
#
# You should consider using continuity correction in the following cases:
#  Two-proportion z-test or chi-squared test for 2×2 tables
#  Small sample sizes, especially when any expected cell count is less than 10
#  When using normal approximation to the binomial or hypergeometric distribution
#
# The correction adjusts for the fact that you're approximating a discrete distribution (like binomial)
# with a continuous one (like normal), by subtracting or adding 0.5 to the observed difference in proportions.

# But which is the "event" outcome?  Conf interval is 56.6-62.1% so that looks more like Never Smoked
# Apparently it's the first (left) column in the table
# So we need to flip it
psmk_table <- table(bc_df$psmk)[c(2, 1)]

prop.test(psmk_table, p = 0.35, conf.level = 0.90)

# Null hypothesis:  p ≤ 0.35
prop.test(psmk_table, p = 0.35, conf.level = 0.90,
          alternative = "greater")

# Assign the test results to an object we'll call "ptest"
# so that we can extract the values we're interested in
ptest <- prop.test(psmk_table, p = 0.35, conf.level = 0.90)

lower_bound_ci <- ptest$conf.int[1]
upper_bound_ci <- ptest$conf.int[2]
pvalue <- ptest$p.value

# Exact binomial test - hypothesis test if rules of thumb are not satisfied
binom.test(psmk_table, p = 0.35)

############################################
# 2. Inference for a multinomial variable
############################################

# Let's look at whether the ages that the women in this
# data set are consistent with a hypothetical assertion
# about the distribution of ages when American women first give birth

# afb = age at first birth

# First, get a look at the distribution of afb

bc_df %>%
  ggplot(aes(x = afb)) +
  geom_histogram(binwidth = 1,
                 color = "red")

table(bc_df$afb)

# Remove rows with afb==98 (or >= 60).  Clearly these don't represent actual observations.
bc_df <- bc_df %>%
  filter(afb < 60)

# or, a more elegant solution:
bc_df <- bc_df %>%
  mutate(afb = na_if(afb, 98))

table(bc_df$afb)

bc_df %>%
  ggplot(aes(x = afb, y = ..density..)) +
  geom_histogram(binwidth = 1,
                 color = "red")

# Assertion:
# 50% first give birth before age 25
# 40% first give birth between 25-29
# 10% first give birth at 30 or above.

bc_df <- bc_df %>%
  mutate(afb_cat = cut(afb,
                       breaks = c(10, 25, 30, 60),
                       right = FALSE,
                       labels = c("Under 25",
                                  "25 to Under 30",
                                  "30 and Above")))

# This is a handy way to see whether we
# created the categorical variable correctly
table(bc_df$afb_cat, bc_df$afb)

# bar graph
bc_df %>%
  tidyr::drop_na(afb_cat) %>%
  ggplot() +
  geom_bar(aes(x = afb_cat), fill = 'skyblue') +
  labs(title = "Age at first birth",
       y = "n",
       x = "") +
  theme(plot.title = element_text(hjust = 0.5),
        legend.position = "none") +
  theme_minimal()

# check that we've satisfied assumptions
# of n ≥ 5 for expected (for CI) and observed (for hypothesis test)

afb_cat_table <- table(bc_df$afb_cat)

# observed # in each category is ≥ 5
afb_cat_table

proportions(afb_cat_table)

x2test <- chisq.test(afb_cat_table, p = c(0.5, 0.4, 0.1))

# expected # in each category is ≥ 5
x2test$expected

x2test
# p value < 0.01, so if our alpha == 0.10, then we can reject

#####################################################################
# 3. Measures of association between binomial variables (1 predictor and response)
#####################################################################

# Is there an association between clearing ear infections
# and using amoxicilin vs. ceflacor?

ear_df <- read_xlsx('data/EAR.xlsx')

summary(ear_df)

table(ear_df$Antibo, ear_df$Clear, useNA="always")

ear_df <- ear_df %>%
  mutate(Clear = factor(Clear,
                        levels = c(0, 1),
                        labels = c("Not Cleared", "Cleared")),
         Antibo = factor(Antibo,
                         levels = c(1, 2),
                         labels = c("CEF", "AMO")))

table(ear_df$Antibo, ear_df$Clear)

treat_table <- table(ear_df$Antibo, ear_df$Clear)

prop.table(treat_table)

rowSums(treat_table)

# Risk ratio
risk_table <- treat_table/rowSums(treat_table)
risk_ratio <- risk_table[1, 1]/risk_table[2, 1]


# Chi-squared test
x2test <- chisq.test(treat_table)
x2test

# Alternative for when expected cell counts are NOT all ≥5
fisher.test(treat_table)

# The epiR package gives us a convenient
# way to pass a 2x2 table and get back
# odds ratio, risk ratio, risk difference

# install.packages("epiR")
library(epiR)

epirr <- epi.2by2(treat_table, conf.level = 0.95)
epirr

# Notice that the columns in the treatment table
# are in the wrong order!!!
# We need to associate "Cleared" with "Outcome+"
# and "Not Cleared" with "Outcome-"

# Swap columns
treat_table <- treat_table[ , c(2, 1)]

# Note that if needed,
# we can use t() function to transpose rows/columns

# Re-run
epirr <- epi.2by2(treat_table, conf.level = 0.95)
epirr

odds_ratio_est <- epirr$massoc.detail$OR.strata.wald$est
odds_ratio_est

