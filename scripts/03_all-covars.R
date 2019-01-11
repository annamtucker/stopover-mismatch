# prepare all covariates for mass gain model

# horseshoe crab abundance and timing ----
hsc = read_csv("output/all_hsc.csv")
hsc.timing = read_csv("output/hsc_smooth.csv")

# change over time?
ggplot(hsc, aes(x = year, y = q95)) +
  geom_point() +
  geom_smooth(method = "lm") +
  ylim(0,1)

hsc %>% 
  filter(!year %in% c(2003, 2005)) %>% 
  ggplot(aes(x = year, y = q95)) +
  geom_point() +
  geom_smooth(method = "lm") +
  ylim(0,1)


summary(trend <- lm(data = hsc, q95 ~ year))

# water temperature ----
temp = read_csv("data/water_temp.csv")

day.avg <- temp %>%
  group_by(fulldate) %>%
  summarize(avg = mean(water_temp, na.rm = TRUE),
            sd = sd(water_temp, na.rm = TRUE)) %>%
  ungroup() %>% 
  full_join(temp[,c(1:5)]) %>% 
  distinct()
day.avg



temp %>%
  distinct(fulldate, day, month, year, mayday) %>%
  left_join(day.avg) %>%
  filter(avg < 28) %>% 
  ggplot(aes(x = mayday, y = avg)) +
  geom_line(aes(col = as.character(year)), lwd = 1, alpha = 0.8) +
  geom_line(aes(y = avg+sd, col = as.character(year)), lty = 2) +
  geom_line(aes(y = avg-sd, col = as.character(year)), lty = 2) +
  geom_hline(yintercept = 15, lty = 2) +
  xlab("Days since May 1") +
  ylab(expression("Average daily water temperature ( " ~degree*C~")")) +
  scale_color_discrete(name = "Year") +
  guides(col = guide_legend(override.aes = list(alpha = 1, lty = 1)))

# change in May water temp
may_avg <- temp %>% 
  filter(month == 5) %>% 
  group_by(year) %>% 
  summarize(avg = mean(water_temp, na.rm = T))

summary(mod <- lm(avg ~ year, data = may_avg))

# exlude 2003 and 2005
excl <- may_avg %>% 
  filter(!year %in% c(2003, 2005))

summary(mod2 <- lm(avg ~ year, data = excl))


# cumulative sum of hours above 15C
hours <- temp %>% 
    mutate(above15 = ifelse(water_temp > 15, 1, 0),
           above15 = ifelse(is.na(above15), 0, above15)) %>% 
    group_by(year, mayday) %>%
    summarize(hoursum = sum(above15)) %>% 
    ungroup() %>% 
    group_by(year) %>% 
    arrange(mayday) %>% 
    mutate(daysum = cumsum(hoursum)) %>% 
    ungroup()

pal = colorRampPalette(brewer.pal(9, "Blues"))

ggplot(hours, aes(x = mayday, y = daysum)) +
  geom_line(aes(col = as.character(year)), lwd = 1) +
  scale_color_manual(values = pal(length(unique(hours$year))))
    

# ifsa ~ water temp ----

# daily 
day.avg %>% 
  rename(water.avg = avg,
         water.sd = sd) %>% 
  full_join(hsc.timing, by = c("mayday" = "day", "year" = "year")) %>% 
  mutate(sd = sqrt(var)) %>% 
  ggplot(aes(x = water.avg, y = avg))+
  geom_segment(aes(x = water.avg-water.sd, xend = water.avg+water.sd, 
                   yend = avg), lwd = 1, alpha = 0.5) +
  geom_linerange(aes(ymin = avg-sd, ymax = avg+sd), 
                 lwd = 1, alpha = 0.5) +
  geom_point(size = 3, alpha = 0.5) +
  xlim(10, 25) +
  xlab("Water  temperature ("~degree*C~")") +
  ylab("Index of horseshoe crab spawning (IFSA)")  
  

day.avg %>% 
  rename(water.avg = avg,
         water.sd = sd) %>% 
  full_join(hsc.timing, by = c("mayday" = "day", "year" = "year")) %>% 
  ggplot(aes(x = water.avg, y = cum.prop, col = day))+
  geom_segment(aes(x = water.avg-water.sd, xend = water.avg+water.sd, 
                   yend = cum.prop), lwd = 1, alpha = 0.5) +
  geom_point(size = 3, alpha = 0.8) +
  xlim(10, 25) +
  xlab("Daily average water  temperature ("~degree*C~")") +
  ylab("Cumulative proportion of \n horseshoe crab spawning occurred") +
  scale_color_viridis_c(name = "Day in May",
                      guide = guide_colorbar(barwidth = 5, direction = "horizontal",
                                             barheight = 0.75, title.position = "top")) +
  theme(legend.position = c(0.05, 0.9))


# yearly
temp %>% 
  filter(month == 5) %>% 
  group_by(year) %>% 
  summarize(avg = mean(water_temp, na.rm = T),
            sd = sd(water_temp, na.rm = T)) %>% 
  full_join(hsc) %>% 
  rename(ifsa = tot.sqm,
         lci = lcl90,
         uci = ucl90) %>% 
  ggplot(aes(x = avg, y = ifsa)) +
  #geom_segment(aes(x = avg-sd, xend = avg+sd, yend = ifsa), lwd = 1, alpha = 0.5) +
  geom_linerange(aes(ymin = ifsa-lci, ymax = ifsa+uci), 
                 lwd = 1, alpha = 0.8) +
  geom_point(size = 3, alpha = 0.5) +
  xlab("Average May water temperature ("~degree*C~")") +
  ylab("Average statewide IFSA")


# cumulative hours
hours %>% 
  full_join(hsc.timing, by = c("year", "mayday" = "day")) %>% 
  ggplot(aes(x = daysum, y = cum.prop, col = mayday)) +
  geom_point(size = 2, alpha = 0.8) +
  scale_color_viridis_c(name = "Days since May 1") +
  xlab("Cumulative sum of hours with water temperature > 15C") +
  ylab("Cumulative proportion of horseshoe crab\nspawning occurred") +
  theme(legend.position = "top")


# number of days above 15C ----
arr.all = read_csv("output/arrival_days.csv")

n15 <- day.avg %>% 
  full_join(arr.all, by = "year") %>% 
  group_by(species, year) %>% 
  mutate(is15_q50 = case_when(
              mayday < (6+q50*3) & avg > 15 ~ 1,
              TRUE ~ 0),
         is15_q95 = case_when(
              mayday < (6+q95*3) & avg > 15 ~ 1,
              TRUE ~ 0)) %>% 
  summarize(n15_q50 = sum(is15_q50),
            n15_q95 = sum(is15_q95)) %>% 
  mutate(n15_q50 = ifelse(year %in% c(1997, 2017), NA, n15_q50),
         n15_q95 = ifelse(year %in% c(1997, 2017), NA, n15_q95))
n15    

hours_arr <- arr.all %>% 
  mutate(day95 = 6+q95*3) %>% 
  select(year, species, day95) %>% 
  inner_join(hours, by = c("year", "day95" = "mayday"))
hours_arr

# combine all covariates ----
covars <- as.tibble(expand.grid(year = c(1997:2018),
                                species = c("REKN", "RUTU"))) %>% 
  left_join(hours_arr[,c(1,2,3,5)]) %>% 
  left_join(n15) %>% 
  left_join(hsc[,c(1:4, 7:8)]) %>% 
  rename(hsc_q50 = q50,
         hsc_q95 = q95,
         ifsa = tot.sqm,
         ifsa.var = vartot) %>% 
  left_join(arr.all) %>% 
  select(-X1) %>% 
  mutate(q50 = 6+q50*3,
         q95 = 6+q95*3) %>% 
  rename(bird_q50 = q50,
         bird_q95 = q95)
covars


write.csv(covars, "output/all_covars.csv", row.names= F)


# how similar are number of days above 15 and cumulative sum of hours above 15
ggplot(covars, aes(x = n15_q95, y = daysum)) + 
  geom_point(size = 3, aes = 0.9) +
  xlab("Number of days with average water temp >15C") +
  ylab("Cumulative sum of hours with water temp >15C") 





# GLM prop spawn available ~ water temp + total IFSA----
covars %>% 
  select(year, species, ifsa, hsc_q50, n15_q50) %>% 
  rename(spawn = hsc_q50,
         n15 = n15_q50) %>% 
  filter(year > 2003 & year < 2018) %>% 
  mutate(ifsa_sc = (ifsa-mean(ifsa,na.rm = T))/sd(ifsa, na.rm = T),
         spawn_sc = (spawn-mean(spawn, na.rm = T))/sd(spawn, na.rm = T),
         n15_sc = (n15-mean(n15,na.rm = T))/sd(n15, na.rm = T)) -> covars.mod

summary(mod <- lm(data = covars.mod, spawn_sc ~ n15_sc + ifsa_sc))

covars %>% 
  select(year, species, ifsa, hsc_q95, n15_q95) %>% 
  rename(spawn = hsc_q95,
         n15 = n15_q95) %>% 
  filter(year > 2003 & year < 2018) %>% 
  mutate(ifsa_sc = (ifsa-mean(ifsa,na.rm = T))/sd(ifsa, na.rm = T),
         spawn_sc = (spawn-mean(spawn, na.rm = T))/sd(spawn, na.rm = T),
         n15_sc = (n15-mean(n15,na.rm = T))/sd(n15, na.rm = T)) -> covars.mod

summary(mod <- lm(data = covars.mod, spawn_sc ~ n15_sc + ifsa_sc))

