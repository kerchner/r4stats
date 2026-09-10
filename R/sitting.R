library(dplyr)
library(ggplot2)
library(readxl) # Need this in order to read Excel files
library(corrplot)

# download.file("https://figshare.com/ndownloader/files/31333153?private_link=63a21ef8ccc25066b8dd",
#               "data/sitting.xlsx",
#               mode='wb')

# Siddarth P, Burggren AC, Eyre HA, Small GW, Merrill DA (2018) Sedentary behavior associated with reduced medial temporal lobe thickness in middle-aged and older adults. PLoS ONE 13(4): e0195549.
# https://doi.org/10.1371/journal.pone.0195549
#
# - Examined associations between sedentary behavior
#   and medial temporal lobe (MTL) subregion integrity
# - 35 non-demented middle-aged and older adults
# - Measured physical activity levels w/questionnaire
# - Measured MTL thickness w/MRI scan
# - Adjusted for age

# Load data
download.file("https://go.gwu.edu/sittingdata",
              "data/sitting.xlsx",
              mode='wb')

sitting <- read_xlsx('data/sitting.xlsx', na = '.')

sitting <- sitting %>%
  select(Sex, Age, Sitting, METminwk, e4grp,
         IPAQgrp, MTL = TOTAL)

# Histogram
sitting %>% ggplot(aes(x = Sitting)) +
  geom_histogram(binwidth = 1, fill = 'white',
                 color = 'black') +
  labs(x = "Sitting (hours/day)",
       y = "Frequency",
       title = "Hours/day spent sitting")

sitting %>% ggplot(aes(x = MTL)) +
  geom_histogram(breaks = seq(2.2, 3.1, by=0.1), fill = 'white',
                 color = 'black') +
  labs(x = "MTL thickness (mm)",
       y = "Frequency",
       title = "MTL thickness (mm)")

# Scatterplot
sitting %>%
  ggplot(aes(x = Sitting,
             y = MTL,
             shape = Sex,
             color = METminwk)) + 
  geom_point()

# Pearson correlation
# Null hypothesis = no linear association
cor.test(x = sitting$Sitting,
         y = sitting$MTL,
         method = "pearson")

# Correlation

# First extract just the numeric variables
sitting_numeric_vars <- sitting %>% select(Age, Sitting, METminwk, MTL)

# Correlation matrix
cor_matrix <- round(cor(sitting_numeric_vars), 3)
cor_matrix

# Correlation plot
# for more info and lots of examples:
#   https://cran.r-project.org/web/packages/corrplot/vignettes/corrplot-intro.html
corrplot(cor_matrix, type = "upper", diag = FALSE)


# simple linear regression
sitting.lm <- lm(data = sitting,
                 formula = MTL ~ Sitting)
summary(sitting.lm)

sitting_residuals <- sitting.lm$model
sitting_residuals$residuals <- sitting.lm$residuals

# QQ plot
sitting_residuals %>% ggplot(aes(sample = residuals)) +
  geom_qq() + 
  geom_qq_line()

sitting_residuals %>%
  ggplot(aes(x = residuals)) +
  geom_histogram(bins = 20)

sitting %>%
  ggplot() + 
  geom_point(aes(x = Sitting,
                 y = MTL,
                 shape = Sex,
                 color = METminwk)) +
  geom_abline(slope = beta_1,
              intercept = y_intercept)

# Show 95% confidence interval band
sitting %>%
  ggplot() + 
  geom_point(aes(x = Sitting,
                 y = MTL,
                 shape = Sex,
                 color = METminwk)) +
  stat_smooth(method = lm,
              mapping = aes(x = Sitting,
                            y = MTL),
              model = y ~ x,
              level = 0.95)

# Choose and set the reference level for e4grp
sitting$e4grp <- factor(sitting$e4grp)
sitting$e4grp <- relevel(sitting$e4grp, ref = "Non-E4")

sitting.lm2 <- lm(data = sitting,
                  formula = MTL ~ Sitting + e4grp)
summary(sitting.lm2)

y_intercept <- sitting.lm2$coefficients['(Intercept)']
beta_1 <- sitting.lm2$coefficients['Sitting']

# use model 1, but use shape and color to show other variables
sitting %>%
  ggplot(aes(x = Sitting,
             y = MTL,
             shape = Sex,
             color = log(METminwk))) + 
  geom_point() +
  geom_abline(slope = beta_1,
              intercept = y_intercept) +
  geom_abline(slope = beta_1,
              intercept = y_intercept + sitting.lm2$coefficients['e4grpE4'],
              color = 'green')

sitting_predict <- data.frame(Sitting = c(3.5, 9, 11),
                              e4grp = c("Non-E4", "E4", "E4"))
predict(sitting.lm2,
        newdata = sitting_predict,
        type = "response")

# Some more models

sitting.lm3 <- lm(data = sitting,
                  formula = MTL ~ Sitting + Age)
summary(sitting.lm3)

# Interaction term
# X1*X2 adds three terms in the model:  X1 + X2 + X1X2
# X1:X2 only adds X1X2
sitting.lm4 <- lm(data = sitting,
                  formula = MTL ~ Sitting*e4grp)
summary(sitting.lm4)

