library(tidyverse)
ggplot(data = diamonds)+geom_bar(mapping = aes(x = cut))
diamonds %>% count(cut)

ggplot(data = diamonds) + geom_histogram(mapping = aes(x = carat),binwidth = 0.5)
diamonds %>% count(cut_width(carat,0.5))

smaller <- diamonds %>%
  filter(carat<3)
count(smaller)
ggplot(data = smaller, mapping = aes(x = carat))+geom_histogram(binwidth = 0.1)

ggplot(data = smaller, mapping = aes(x = carat, colour = cut))+geom_freqpoly()

ggplot(data = smaller, mapping = aes(x = carat))+geom_histogram(binwidth = 0.01)

ggplot(data = faithful, mapping = aes(x = eruptions))+geom_histogram(binwidth = 0.25)

# UNUSUAL VALUES
ggplot(diamonds)+geom_histogram(mapping = aes(x = y), binwidth = 0.5)
ggplot(diamonds)+geom_histogram(mapping = aes(x = y), bidwidth = 0.5)+coord_cartesian(ylim = c(0,50))


unusual <-diamonds %>%
  filter(y<3 | y>20) %>%
  select(price, x,y,z) %>%
  arrange(y)
unusual

# MISSING VALUES
diamnods2 <- diamonds %>% filter(between(y,3,20))
ggplot(data = diamnods2, mapping = aes(x = x, y = y))+geom_point(na.rm = TRUE)
ggplot(data = diamonds, mapping = aes(x = x, y = y))+geom_point(na.rm = TRUE)


nycflights13::flights %>%
  mutate(cancelled = is.na(dep_time),
  sched_hour = sched_dep_time %/%100,
  sched_min = sched_dep_time %%100,
  sched_dep_time = sched_hour+sched_min/60) %>%
  ggplot(mapping = aes(sched_dep_time))+geom_freqpoly(mapping = aes(colour = cancelled), binwidth = 1/4)

# COVARATON
# A categorical and continous variable
ggplot(data = diamonds, mapping = aes(x = price))+ geom_freqpoly(mapping = aes(colour = cut), binwidth = 500)
ggplot(diamonds)+geom_bar(mapping = aes(cut))

ggplot(data = diamonds, mapping = aes(x = price, y = ..density..))+geom_freqpoly(mapping = aes(colour = cut), binwidth = 500)

# display continous variable broken down bu a categorical variable -> boxplot
ggplot(data = diamonds, mapping = aes(x = cut, y = price))+geom_boxplot()

ggplot(data = mpg)+geom_boxplot(mapping = aes(x = reorder(class, hwy, FUN = median), y = hwy))

ggplot(data = mpg)+geom_boxplot(mapping = aes(x = reorer(class, hwy, FUN = median), y = hwy))+coord_flip(
  
)

# TWO CATEGORICAL VARIABLES
ggplot(data = diamonds)+geom_count(mapping = aes(x = cut, y = color))
diamonds %>% count(color, cut)

# TWO CONTINUOUS VARIABLES
ggplot(data = diamonds)+geom_point(mapping = aes(x = carat, y = price))

ggplot(data = diamonds)+geom_point(mapping = aes(x = carat, y = price),alpha = 1/100)

ggplot(data = smaller, mapping = aes(x = carat, y = price))+geom_boxplot(mapping = aes(group = cut_width(carat, 0.1)))
    
# FINE TUNING
library(modelr)
mod <-lm(log(price)~log(carat),data = diamonds)
diamnods2 <-diamonds %>% add_residuals(mod)%>%mutate(resid = exp(resid))
ggplot(data = diamnods2)+ geom_point(mapping = aes(x = carat, y = resid))
ggplot(data = diamnods2)+geom_point(mapping = aes(x = carat, y = price))
ggplot(data = diamnods2)+geom_boxplot(mapping = aes(x = cut, y=resid))
