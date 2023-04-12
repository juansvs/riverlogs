library(tidyverse)
library(lubridate)

# Import data
dat_raw <- read_csv("../Data/timelapse_database.csv")
dat_raw

### Pre processing
# Filter to keep only unique records. Make sure to arrange it so that if there
# is a crossing do not delete it.
dat <- dat_raw %>% filter(!species %in% c("nothing", "setup", "corrupted","uid", 
                                      "lizard_uid","iguana_green", "bat", "bird", "horse")) %>% # remove non-animal records
  group_by(station, species, camera) %>% arrange(DateTime) %>% 
  mutate(tdif = difftime(DateTime,lag(DateTime),units = "mins")) %>% # time difference between detections
  mutate(newevent = is.na(tdif) | tdif>duration(5, "minutes")) %>% ungroup() %>% # is the detection a new event?
  mutate(eventnum = cumsum(newevent)) %>% # label independent events
  group_by(eventnum) %>% mutate(eventcross = any(crossing)) %>% ungroup() %>% 
  filter(newevent) %>% select(station, species, DateTime, eventcross, behavior)

# Count occurrences of each species
dat %>% count(species) %>% arrange(desc(n)) %>% view()

# There were 31 different species. The most common species were tamanduas (213),
# followed by coatis (190), opossums (146), skunks (88), tayras (88), ocelots
# (86), birds (78), people (78), curassows (70), pumas (44), agoutis (37), pacas
# (19), four-eyed opossums (18), collared peccaries (8), margays (6). These
# species, however did not use the logs equally
dat %>% count(species, eventcross) %>% 
  pivot_wider(names_from = eventcross, values_from = n, values_fill = 0) %>% 
  mutate(prop = (.$2-.$))
  mutate(prop = (true-false)/false) %>% arrange(desc(false+true)) %>% view()

# Tamanduas always were seen crossing the logs relatively often (5:1), similar to coatis
# (7:1), opossums (13:1), skunks (11:1), tayras (6:1), ocelots (10:1), and pumas
# (4:1). Some species like pacas and peccaries were observed but were never seen
# on the logs

# How about the difference between logs?
dat %>% count(station, species, eventcross) %>% 
  pivot_wider(names_from = eventcross, values_from = n, values_fill = 0) %>% 
  mutate(prop = `TRUE`/(`TRUE`+`FALSE`)) %>% 
  ggplot(aes(species,prop))+geom_boxplot()+
  theme_classic(base_size = 16)+
  theme(axis.text.x = element_text(hjust = 0, angle = -45))

# Are animals using logs above rivers to cross them?
# Yes, but not all, and not at the same frequency.

# Is the frequency of detection more than expected? (compared to similar cameras nearby)
log_data <- readxl::read_excel("../data/covariates_data_base.xlsx")
names(log_data)[1] <- "station"
log_data

# Is there difference among log types?
dat %>% count(station, eventcross) %>% pivot_wider(names_from = eventcross, values_from = n, values_fill = 0)

# Does log use (abs/rel frequency) depend on factors about the logs? First I
# need to change the station codes so that they match those in the covariates
# database
dat <- dat %>% mutate(station = if_else(str_length(station)==5, str_replace(station, "G", "G0"), station))
dat_wcovs <- dat %>% left_join(log_data)
#### Create a binomial GLM to see if the prob of crossing depends on the species or
# factors about the logs
dat_wcovs %>% select(eventcross, Log_lenght, Diameter, Stream_width, Log_to_water) %>% plot()
# There doesn't seem to be much correlation between the covariates and the proportion of crossing

# GLM
m1 <- glm(eventcross~Diameter+Log_lenght+Stream_width+Log_to_water, data = dat_wcovs, family = "binomial")
plot(m1)
summary(m1)
# There are some logs where most of the events are
# crossings we could do a hierarchical model where the probability of crossing
# (binomial) is dependent on the species as well as the station
