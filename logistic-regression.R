# Required
library(tidyverse)
library(here)
# Import data
here::here()
df.raw <- read_delim('ao.csv', delim = ';',
                     col_types = cols(
                       outcome = col_factor(levels = c('0','1')),
                       age = col_integer(),
                       gender = col_factor(levels = c('0','1')),
                       maxilar = col_factor(levels = c('0','1')),
                       ohi = col_factor(levels = c('0','1')),
                       infection = col_factor(levels = c('0','1')),
                       trauma = col_factor(levels = c('0','1')),
                       disease = col_factor(levels = c('0','1')),
                       alcohol = col_factor(levels = c('0','1')),
                       tobacco = col_factor(levels = c('0','1'))
                     ))
glimpse(df.raw)
# Label; since the data came from SPSS (D'OH!), we have only 0 and 1
df.raw %>%
  mutate(outcome = recode(outcome, '0' ='health', '1' ='alveolar osteitis')) %>%
  mutate(outcome = factor(outcome, levels = c('health', 'alveolar Osteitis'))) %>%
  mutate(gender = recode(gender, '0' ='male', '1' ='female')) %>%
  mutate(gender = factor(gender, levels = c('male', 'female'))) %>%
  mutate(maxilar = recode(maxilar, '0' ='mandibular', '1' ='maxilar')) %>%
  mutate(maxilar = factor(maxilar, levels = c('mandibular', 'maxilar'))) %>%
  mutate(ohi = recode(ohi, '0' ='bad', '1' ='good')) %>%
  mutate(ohi = factor(ohi, levels = c('bad', 'good'))) %>%
  mutate(infection = recode(infection, '0' ='yes', '1' ='no')) %>%
  mutate(infection = factor(infection, levels = c('yes', 'no'))) %>%
  mutate(trauma = recode(trauma, '0' ='yes', '1' ='no')) %>%
  mutate(trauma = factor(trauma, levels = c('yes', 'no'))) %>%
  mutate(disease = recode(disease, '0' ='yes', '1' ='no')) %>%
  mutate(disease = factor(disease, levels = c('yes', 'no'))) %>%
  mutate(alcohol = recode(alcohol, '0' ='yes', '1' ='no')) %>%
  mutate(alcohol = factor(alcohol, levels = c('yes', 'no'))) %>%
  mutate(tobacco = recode(tobacco, '0' ='smoker', '1' ='non-smoker')) %>%
  mutate(tobacco = factor(tobacco, levels = c('smoker', 'non-smoker')))
# Visualize
df.raw %>%
  group_by(outcome,gender) %>%
  summarize(n=n())
# Since age is not important to model, we will drop
df <- subset(df.raw,drop=c(2))
# Model
fit1 <- glm(outcome ~.,family=binomial(link='logit'),data=df)
summary(fit1)
fit2 <- glm(outcome ~infection+trauma+tobacco,family=binomial(link='logit'),data=df)
summary(fit2)
# Train

# Test