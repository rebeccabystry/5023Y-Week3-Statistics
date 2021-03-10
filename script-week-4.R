library(tidyverse)

darwin_data <- read_csv("Data/darwin.csv")
str(darwin_data)

darwin_data <- darwin_data %>% 
  pivot_longer(cols=c("Self":"Cross"), # we will be taking columns Self AND Cross
               names_to="type", # we will put the "names" into one new column, labelled "type"
               values_to="height") # we will put the values from both columns into one new column, "height"

view(darwin_data)
str(darwin_data)

darwin_data <- darwin_data %>% 
  mutate(type = factor(type, levels = c("Self","Cross"))) 
#Changing column "type" from a character type into a factor type for use in statistical modelling 

view(darwin_data)
str(darwin_data)

darwin_data <- darwin_data %>% # Turning column "pair" into factor as well since the numbers are not actually continuous
  # The second (pair) in brackets seems to specify the data we actually want to be in that column? Cos I changed it to "type" to see what happens and it ended up containing the type data. 
  mutate(pair = factor(pair))

view(darwin_data)
str(darwin_data)

darwin_lm <- lm(height ~ type + pair, data = darwin_data)
darwin_lm
summary(darwin_lm) # Pair 1 defines the intercept - it is the mean height of the first pair.

# Now "collapse" these results into an ANOVA:
summary(aov(darwin_lm))

# Calculating 95% confidence intervals for our two means...

library(emmeans)

estimates <- emmeans(darwin_lm, specs="type")
estimates
help("as_tibble")

estimates %>% 
  as_tibble %>% 
  ggplot(aes(x=type, y=emmean, colour=type)) +
  geom_pointrange(aes(ymin=lower.CL, ymax=upper.CL)) + # 95% confidence intervals
  geom_pointrange(aes(ymin=emmean-SE, ymax=emmean+SE), size = 1.4) # (66%) Standard Error

# "Forcing" the model into a dataframe so upper and lower confidence intervals can be mutated
library(broom)
tidymodel1 <- broom::tidy(darwin_lm) %>% 
  mutate(lwr=((estimate-(std.error*2))),
         upr=(estimate+(std.error*2)))

view(tidymodel1)


# PLotting the confidence intervals against the intercept (from pair 1)

tidymodel1 %>% 
ggplot(aes(x = estimate, y=term)) + # Makes the "frame"
  geom_pointrange(aes(xmin=lwr, xmax=upr)) + # Adds the pair data
  geom_vline(xintercept=0, linetype="dashed")

tidymodel2 <- broom::tidy(darwin_lm, conf.int=T) 
tidymodel2[2,] ## only pull out row 2 

tidymodel2

tidymodel2 %>% 
  ggplot(aes(x=estimate, y=term)) +
  geom_pointrange(aes(xmin=conf.low, xmax=conf.high )) +
  geom_vline(xintercept=0,linetype="dashed")

