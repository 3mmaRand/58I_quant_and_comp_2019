library(tidyverse)
library(multcomp)

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
