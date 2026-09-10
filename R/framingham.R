library(dplyr)
library(forcats)
library(ggplot2)

# https://framinghamheartstudy.org
# - Long-term prospective study of the etiology of cardiovascular disease
#   among a population of subjects in Framingham, MA
# - Began in 1948 with 5,209 subjects
# - Is the source of the term "risk factor"
# - Over 3,000 peer-reviewed papers published based on this study
# - Participants were each followed for a total of 24 years for
#   cardiovascular events (heart attack, stroke, death, etc.)

# Load data
# download.file('https://ndownloader.figshare.com/files/26537294', 'data/framingham.csv')
download.file('https://go.gwu.edu/framingham', 'data/framingham.csv')
df_raw <- read.csv('data/framingham.csv')

df_raw <- df_raw %>%
  select(STROKE, HYPERTEN, AGE, SEX)

df <- df_raw %>%
  mutate(STROKE = factor(STROKE,
                         levels = c(0, 1),
                         labels = c("No stroke", "Stroke")),
         HYPERTEN = factor(HYPERTEN,
                           levels = c(0,1),
                           labels = c("No HTN", "HTN")),
         SEX = factor(SEX,
                      levels = c(1, 2),
                      labels = c("Male", "Female")))

table(df$STROKE, df$HYPERTEN, useNA = "always")      

stroke.glm <- glm(data = df,
                  formula = STROKE ~ HYPERTEN,
                  family = binomial("logit"))
summary(stroke.glm)

stroke_logOR <- coef(stroke.glm)
stroke_OR <- exp(stroke_logOR)

stroke_log_ci <- confint(stroke.glm)
stroke_ci <- exp(stroke_log_ci)
stroke_ci

# Add more model terms
stroke.glm2 <- glm(data = df,
                  formula = STROKE ~ HYPERTEN + AGE + SEX,
                  family = binomial("logit"))
summary(stroke.glm2)

stroke_logOR <- coef(stroke.glm2)
stroke_OR <- exp(stroke_logOR)
stroke_OR
# read and interpret

stroke_log_ci <- confint(stroke.glm2)
stroke_ci <- exp(stroke_log_ci)
stroke_ci
# read and interpret

stroke_predict <- data.frame(HYPERTEN = c("No HTN", "HTN"),
                             AGE = c(50, 60),
                             SEX = c("Female", "Male"))
predict(stroke.glm2,
        newdata = stroke_predict,
        type = "response")