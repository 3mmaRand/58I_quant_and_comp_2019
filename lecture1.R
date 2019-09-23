library(tidyverse)
library(multcomp)
library(Rmisc)

stag <- read.table("../data/stag.txt", header = TRUE)
mod <- lm(data = stag, mand ~ jh)

ggplot(data = stag, aes(x = jh, y = mand)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE) +
  ylim(0, 1.8) +
  theme_classic()

chaff <- read.table("../data/chaff.txt", header = T)
chaff <- chaff %>% gather(key = sex, value = mass)
chaffsum <- summarySE(data = chaff, measurevar = "mass", groupvars = "sex")
ggplot(data = chaffsum, aes(x = sex, y = mass)) +
  geom_point() +
  geom_errorbar(aes(ymin = mass - se, ymax = mass + se), width = .1) +
  theme_classic()


# lm() example, one-way anova equivalent, colony diameter
# grown on three different media
culture <- read.table("data/culture.txt", header = TRUE, sep = ",")

ggplot(data = culture, aes(x = medium, y = diameter)) +
  geom_jitter(width = 0.2)

mod1 <- lm(data = culture, diameter ~ medium)
summary(mod1)

anova(mod1)
# mod2 <- lm(data = culture, diameter ~ 1)
# summary(mod2)
# mean(culture$diameter)
# 
# anova(mod1, mod2)

# posthoc
post <- glht(mod1, linfct = mcp(medium = "Tukey"))
print(summary(post))

plot(mod1)
shapiro.test(mod1$residuals)
