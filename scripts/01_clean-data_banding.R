# prepare banding data for analysis

df = read_csv("data/banding_DE.csv")

# clean and filter data ----

all.sp <- df %>% 
  filter(Species %in% c("REKN", "RUTU")) %>% 
  select(`BirdID`, `Capture Date`, Location, Species, `Flag Color In`, `Flag Code In`,
         `Metal In`, `Metal Out`, `Weight`) %>% 
  rename(date = `Capture Date`, loc = `Location`, species = `Species`,
         col = `Flag Color In`, code = `Flag Code In`, weight = Weight) %>% 
  mutate(code = substr(code, 3, nchar(code)-1),
         date = parse_date_time(date, orders = "%m/%d/%Y %I:%M:%S",
                                truncated =3),
         year = year(date),
         day = ifelse(month(date) == 5, day(date), day(date)+31))
all.sp


# check weights and find averages ----
all.sp %>% 
  ggplot(aes(x = Weight)) +
  geom_histogram() +
  facet_wrap(~Species)

all.sp %>% 
  filter(Species == "RUTU" & Weight < 75) 
  ggplot(aes(x = Weight)) +
  geom_histogram()

# summarize catches by mean, sd, N
all.sp.means <- all.sp %>% 
  group_by(species, day, year) %>%
  summarize(avg = mean(weight, na.rm = T), 
            sd = sd(weight, na.rm = T),
            n = length(!is.na(weight))) %>%
  ungroup() %>%
  filter(n > 24) %>% 
  mutate(catchID = c(1:length(mean))) 
all.sp.means

ggplot(all.sp.means, aes(x = day, y = avg, group = year)) +
  geom_linerange(aes(ymin = avg-sd, ymax = avg+sd), lwd = 1, alpha= 0.6,
                 position = position_dodge(width = 1)) +
  geom_point(size = 2, alpha = 0.6, 
             position = position_dodge(width = 1)) +
  facet_wrap(~species)

# within-catch variance ----

# change in within-catch variance over season?
all.sp.means %>%
  ggplot(aes(x = day, y = sd, col = species)) +
  geom_point(size = 2, alpha = 0.7) +
  facet_wrap(~year, scales = "free") +
  theme(axis.text = element_text(size = 16),
        axis.title = element_text(size = 18),
        legend.text = element_text(size= 16),
        legend.title = element_text(size = 18)) +
  ylab("SD of Weight") + 
  xlab("Julian Day")

all.sp.means %>%
  ggplot(aes(x = day, y = sd, col = species)) +
  geom_point(size = 2, alpha = 0.7) +
  theme(axis.text = element_text(size = 16),
        axis.title = element_text(size = 18),
        legend.text = element_text(size= 16),
        legend.title = element_text(size = 18)) +
  ylab("SD of Weight") + 
  xlab("Julian Day")

# distribution of SD
all.sp.means %>%
  ggplot(aes(x = sd)) +
  geom_histogram(fill = "gray80", col = "black") +
  facet_wrap(~species, scales = "free") 


# summary statistics ----

# number of individuals
nrow(all.sp)

all.sp %>% 
  count(species)

# number of catches
nrow(all.sp.means)

all.sp.means %>% 
  count(species)

# number of individuals per catch
all.sp.means %>% 
  group_by(species) %>% 
  summarize(min = min(n),
            max = max(n),
            median = median(n))

# number of catches per year
all.sp.means %>% 
  count(species, year) %>% 
  group_by(species) %>% 
  summarize(min = min(nn), max = max(nn), med = median(nn))


# join averages to individual data and save ----
all.sp.dat <- all.sp %>% 
  left_join(all.sp.means, by = c("species", "day", "year")) %>% 
  select(year, day, species, weight, avg, sd, n, catchID)
all.sp.dat

write.csv(all.sp.dat, "output/all_weights.csv", row.names = F)
