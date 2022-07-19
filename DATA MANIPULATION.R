# DATA MANIPULATION
library(nycflights13)
library(tidyverse)
view(flights)

# FILTER ROWS WITH FILTER
jan1 = filter(flights, month == 1, day == 1)
print(jan1)
(dec<-filter(flights, month == 12, day == 25))
(nov_dec <- filter(flights, month %in%c(11,12)))
(filter(flights, !(arr_delay>120 | dep_delay >120)))
is.na(flights)
df<-tibble(x = c(1,NA,3))
filter(df, is.na(x)|x>1)

# ARRANGE ROWS WITH ARRANGE()
arrange(flights, year, month, day)
arrange(flights, desc(dep_delay))

# SELECT COLUMNS WITH SELECT()
select(flights, year, month, day)
select(flights, year:day)
select(flights, -(year:day))
select(flights, time_hour, air_time, everything())
select(flights, tail_num = tailnum)

# ADDING NEW VARIABLES WITH MUTATE()
flights_sml = select(flights, year:day, ends_with("delay"), distance, air_time)
mutate(flights_sml, gain = dep_delay-air_time, speed = distance/air_time*60)

flights_sml = mutate(flights_sml, gain = dep_delay - air_time, hours = air_time/60, gain_per_hour = gain/hours)
view(flights_sml)

# TRANSMUTATE -> if you only want to keep new variables
transmute(flights, gain = dep_delay - air_time, hours = air_time/60, gain_per_hour = gain/hours)
transmute(flights, dep_time, hour = dep_time %/%100, minute = dep_time %% 100) # integer_division -> %/%, remainer %%

# GROUPED SUMMARISE WITH SUMMARISE()
summarise(flights, delay = mean(dep_delay, na.rm = TRUE))
byday <- group_by(flights, year, month, day)
summarise(byday, delay = mean(dep_delay, na.rm = TRUE))

# COMBING MULTIPLE OPERATION WITH THE PIPE
by_dest <-group_by(flights, dest)
delay<-summarise(by_dest, count = n(), mean(distance, na.rm = TRUE), delay = mean(arr_delay, na.rm = TRUE))
(delay <- filter(delay, count>20, dest != "HNL"))

ggplot(data = delay, mapping = aes(x = dest, y = delay)) + geom_point(mapping = aes(size = count), alpha = 1/3)+geom_smooth()

# PIPE
delays <- flights %>%
  group_by(dest)%>%
  summarise(
    count = n(),
    dist = mean(distance, na.rm = TRUE),
    delay = mean(arr_delay, na.rm = TRUE)) %>%
      filter(count>20, dest != "ABQ")
  
# MISSING VALUES
flights%>%
  group_by(year, month, day) %>%
  summarise(mean = mean(dep_delay))

flights %>%
  group_by(year, month, day) %>%
  summarise(mean = mean(dep_delay, na.rm = TRUE))

not_cancelled = flights %>%
  filter(!is.na(dep_delay), !is.na(arr_delay))

not_cancelled %>%
  group_by(year, month, day) %>%
  summarise(mean = mean(dep_delay))

# COUNT
delay <- not_cancelled %>%
  group_by(tailnum) %>%
  summarise(daley = mean(arr_delay))

ggplot(data = delays, mapping = aes(x = delay))+geom_freqpoly(binwidth = 10)

delays <- not_cancelled %>%
  group_by(tailnum) %>%
  summarise(delay = mean(arr_delay, na.rm = TRUE), n = n())
ggplot(data = delays, mapping = aes(x = n, y = delay)) + geom_point(alpha = 1/10)
delays %>%
  filter(n>25) %>%
  ggplot(mapping = aes(x = n, y = delay))+geom_point(alpha = 1/10)

# SUMMARY FUNCTIONS -> mean, median, sd, IQR, mad
not_cancelled %>%
  group_by(year, month, day) %>%
  summarise(
    avg_delay1 = mean(arr_delay),
    avg_delay2 = mean(arr_delay[arr_delay>0])
  )

not_cancelled %>% group_by(dest) %>% summarise(distance_sd = sd(distance)) %>% arrange(desc(distance_sd))

not_cancelled %>%
  group_by(year, month, day) %>%
  summarise(
    first = min(dep_time),
    last = max(dep_time)
  )

not_cancelled %>%
  group_by(year, month, day) %>%
  summarise(first_dep = first(dep_time),
            last_dep = last(dep_time))

not_cancelled %>%
  group_by(year, month, day) %>%
  mutate(r = min_rank(desc(dep_time))) %>%
  filter(r %in% range(r))

# NUMBER OF NON-MISSING VALUES -> SUM(!is.na(x), n_distinc(x))
not_cancelled %>%
  group_by(dest) %>%
  summarise(carriers = n_distinct(carrier)) %>%
  arrange(desc(carriers))

not_cancelled %>%
  count(tailnum, wt = distance)

not_cancelled %>%
  group_by(year, month, day) %>%
  summarise(n_early = sum(dep_time < 500))

not_cancelled %>%
  group_by(year, month, day) %>%
  summarise(hour_perc = mean(arr_delay > 60))

# GROUPING BY MULTIPLE VARIABLES
delay <- group_by(flights, year, month, day)
per_day <- summarise(daily, flights = n())

# GROUPED MUTATES
flights %>%
  group_by(year, month, day) %>%
  filter(rank(desc(arr_delay)) < 10)

popular_dests <- flights %>% group_by(dest) %>% filter(n()>356)
