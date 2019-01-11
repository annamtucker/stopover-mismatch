# determine availability of horseshoe crab eggs

hsc = read.csv("data/hsc_survey_raw_dat.csv", stringsAsFactors = FALSE)

hsc %>%
  mutate(MayDay = ifelse(Year %in% c(2004, 2008, 2012, 2016),
                         JulianDay - 121, JulianDay - 120)) -> hsc
head(hsc)

hsc %>% 
  group_by(BeachId, Date) %>% 
  summarize(n_quad = max(QuadNum)) %>% 
  ungroup() %>% 
  count(n_quad) %>% 
  arrange(-n_quad)

hsc %>% 
  group_by(BeachId, Date) %>% 
  count(QuadBatchID) %>% 
  ungroup() %>% 
  count(n) %>% 
  rename(quads_per_batch = n) %>% 
  arrange(-nn)


hsc %>% 
  group_by(BeachId, Date) %>% 
  count(QuadBatchID) %>% 
  ungroup() %>% 
  rename(quads_per_batch = n) %>% 
  filter(quads_per_batch < 100) %>% 
  left_join(hsc) %>% 
  ggplot(aes(x = quads_per_batch, y = Females)) +
  geom_point()
  
  

hsc %>% 
  group_by(BeachId, Date) %>% 
  count(QuadBatchID) %>% 
  ungroup() %>% 
  rename(quads_per_batch = n) %>% 
  ggplot(aes(x = quads_per_batch)) +
  geom_histogram() +
  facet_wrap(~BeachId)

# ridiculous numbers of females only on surveys with only 1 quadrat reported
# assume that full 100 quadrats were surveyed (?)


# beach info   

beaches = read.csv("data/hsc_beach_info.csv", stringsAsFactors = F)


# find avg number of females per sq meter for each survey, then average for each night

#from Smith et al 2002
#S_hi = number of possible samples = 100
#L_hi = length of beach 
#s = number of random starts = 2
# srata = state
# t= number of days sampled per lunar period

# find beach length and number of samples for each survey
hsc %>% 
  distinct(BeachId, Date, .keep_all = T) %>% 
  mutate(SurveyId = c(1:nrow(.))) %>% 
  left_join(beaches[,c(1,2,4,5)], by = c("BeachId" = "BeachID")) %>% 
  select(SurveyId, BeachId, Date, Length) -> surveys

hsc %>% 
  full_join(surveys) %>% 
  group_by(SurveyId) %>% 
  mutate(n_samps = length(unique(QuadBatchID)),
         n_quad = length(unique(QuadNum, QuadBatchID))) %>% 
  ungroup() -> hsc_full

hsc_full %>% 
  group_by(SurveyId, Length, Date) %>% 
  summarize(avgF = mean(Females, na.rm = T)) %>% 
  mutate(avgF.adj = avgF/Length) %>% 
  ungroup() %>% 
  group_by(Date) %>% 
  summarize(ifsa = mean(avgF.adj)) %>% 
  ungroup() %>% 
  mutate(date = parse_date(Date, format = "%d-%b-%y"),
         year = year(date),
         day = ifelse(month(date) == 5, day(date),
                      ifelse(month(date) == 6, day(date)+31, day(date)+61))) %>% 
  group_by(year) %>% 
  arrange(day) %>% 
  mutate(cum.ifsa = cumsum(ifsa),
         cum.prop = cum.ifsa/sum(ifsa)) %>% 
  ungroup() -> hsc_ifsa
  
hsc_ifsa %>% 
  ggplot(aes(x = day,y = ifsa)) +
  geom_point()+
  facet_wrap(~year)
  

# recalculate assuming 100 quads per survey with only 1 reported

hsc_full %>% 
  group_by(SurveyId, n_quad, Length, Date) %>% 
  summarize(sumF = sum(Females, na.rm = T)) %>% 
  mutate(n_quad_new = ifelse(n_quad == 1, 100, n_quad),
         avgF = sumF/n_quad_new,
         avgF.adj = avgF/Length) %>% 
  ungroup() %>% 
  group_by(Date) %>% 
  summarize(ifsa = mean(avgF.adj)) %>% 
  ungroup() %>% 
  mutate(date = parse_date(Date, format = "%d-%b-%y"),
         year = year(date),
         day = ifelse(month(date) == 5, day(date),
                      ifelse(month(date) == 6, day(date)+31, day(date)+61))) %>% 
  group_by(year) %>% 
  arrange(day) %>% 
  mutate(cum.ifsa = cumsum(ifsa),
         cum.prop = cum.ifsa/sum(ifsa)) %>% 
  ungroup() -> hsc_ifsa


# smooth cumulative proportion ####
predict_loess = function(x){
  mod = loess(cum.prop ~ day, data = x)
  p = predict(mod, c(min(x$day):max(x$day)))
  tibble(p.day = c(min(x$day):max(x$day)),
         predicted = p)
}

alldays = as.tibble(expand.grid(day = c(min(hsc_ifsa$day):max(hsc_ifsa$day)),
                                year = unique(hsc_ifsa$year)))

hsc_ifsa %>% 
  full_join(alldays) %>% 
  nest(-year) %>% 
  mutate(smoothed = map(data, predict_loess)) %>% 
  unnest(data, smoothed) %>% 
  arrange(year, day) -> hsc_smooth


smooth_plot <- ggplot(hsc_smooth) +
  geom_line(aes(x = p.day, y = predicted), lwd = 1, col = "gray30") +
  geom_point(aes(x = day, y = cum.prop)) +
  facet_wrap(~year) +
  xlab("Day in May") +
  ylab("Cumulative proportion\nof spawning occurred")
smooth_plot

write.csv(hsc_smooth, "output/hsc_smooth.csv", row.names = F)



