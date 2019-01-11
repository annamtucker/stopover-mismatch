# generate night specific ifsa estimates
# 30 jul 18

# code adapted from "AdjustedISAscript2017.R" 
# originally by Jonathan Daily and Dave Smith 2011

source("scripts/00_load-packages.R")

# exported from 2017 hsc survey database on 30 jul 18
beaches = read_csv("data/hsc-database/Beaches.csv")
quadobs = read_csv("data/hsc-database/QuadObs.csv")
quadrats = read_csv("data/hsc-database/Quadrats.csv")

# these were hard-coded into the script I received
# ALen = average beach length by state
ALen <- c(.881, .589)
# TLen = total beach length available for surveying by state
TLen <- c(12.2, 7.1)
# TNum = total number of survey beaches by state
TNum <- c(13, 12)

state.vals = tibble(State = c("DE", "NJ"),
                    ALen = ALen,
                    TLen = TLen,
                    TNum = TNum)



# merge files and some data cleaning ----
quadobs %>% 
  rename(BeachID = BeachId) %>% 
  full_join(beaches) %>% 
  full_join(quadrats)  %>% 
  mutate(Males = ifelse(BeachName == "Sea Breeze", Males/100, Males),
         Females = ifelse(BeachName == "Sea Breeze", Females/100, Females),
         Date = parse_date(Date, format = "%d-%b-%y"),
         Year = year(Date),
         Sys_S = ifelse(QuadNum %% 2 == 0, 1, 2),
         Tot = Males + Females) %>% 
  filter(Year > 1999 & Females < 20) %>% 
  arrange(Date) -> dat


# I don't have lunar period data but the following function assigns
# surveys to lunar periods within year

find_lp = function(y){
  yrs = unique(year(y))
  all_lp = c()
  for(t in 1:length(yrs)){
    x = sort(y[year(y) == yrs[t]])
    lp = rep(NA, length(x))
    lp[1] = 1
    for(i in 2:(length(x)-1)){
      lp[i] = ifelse(x[i]-x[i-1] < 3, lp[i-1], lp[i-1]+1)
    }
    lp[length(x)] = lp[length(x)-1]
    all_lp = c(all_lp, lp)
  }
  all_lp
}

dat$Lunar = find_lp(dat$Date)

# visual check that lunar periods were assigned correctly
dat %>% 
  mutate(year = year(Date),
         day = yday(Date)) %>% 
  group_by(BeachID, year, Lunar) %>% 
  count(day) %>% 
  ggplot(aes(x = day, y = n, fill = as.character(Lunar))) +
  geom_bar(stat = "identity") +
  facet_wrap(~year)

# functions ----
wt.var <- function(x, maxv = 7){ 
  d <- length(x)
  ifelse(d > 1, (maxv - d)/(maxv * d)*var(x), NA)
}

cv <- function(x, ...) sd(x, ...)/mean(x, ...)



# calculate density and variance at each level ----

# survey (beach + date) ----
bch.date <- dat %>% 
  group_by(BeachName, Date, Sys_S, Lunar, Year) %>% 
  summarize(Males = mean(Males, na.rm = T),
            Females = mean(Females, na.rm = T),
            Tot = mean(Tot, na.rm = T)) %>% 
  ungroup()  %>% 
  gather(type, val, 6:8) %>% 
  group_by(BeachName, Date, type, Lunar, Year) %>% 
  summarize(avg = mean(val, na.rm = T),
            var = var(val, na.rm = T)) %>% 
  ungroup() %>% 
  full_join(beaches)
bch.date

# beach within lunar period ----
bch.lunar <- bch.date %>% 
  group_by(BeachName, Lunar, type, Year) %>% 
  summarize(var = wt.var(avg),
            avg = mean(avg, na.rm = T)) %>% 
  ungroup() %>% 
  full_join(beaches)
bch.lunar

var3 <- bch.date %>% 
  group_by(BeachName, type, Year) %>% 
  summarize(varsum = sum(var, na.rm = T),
            length = mean(Length, na.rm = T)) %>% 
  mutate(var = varsum*(length/0.05-2)/(length/0.05*2),
         var = ifelse(is.na(var), 0, var)) %>% 
  full_join(beaches) %>% 
  select(BeachName, Year, type, var, State)
var3

# Impute between night variance within lunar period for beaches 
# with <2 and >0 nights sampled within a lunar period
# Average within state and lunar period CVs and beach level within 
# lunar period density estimates are used for imputation

state.cvs <- bch.date %>% 
  filter(!(is.na(State) | is.na(Lunar))) %>% 
  group_by(BeachName, Lunar, State, type, Year) %>% 
  summarize(cv = cv(avg, na.rm = T)) %>% 
  ungroup() %>% 
  group_by(Lunar, State, type, Year) %>% 
  summarize(cv = mean(cv, na.rm= T))
state.cvs

var2 <- bch.lunar %>% 
  left_join(state.cvs) %>% 
  group_by(State, Lunar, BeachName, type, Year) %>% 
  mutate(imp.var = ifelse(length(cv), avg^2 * cv^2, NA)) %>% 
  ungroup() %>% 
  group_by(BeachName, Year, type, State) %>% 
  summarize(var = sum(imp.var))
var2

# state within lunar period ----

state.lunar <- bch.lunar %>% 
  group_by(Lunar, State, type, Year) %>% 
  summarize(tot = sum(avg),
            totlength = sum(Length)) %>% 
  left_join(state.vals, by = "State") %>% 
  mutate(tot.sqm = tot/totlength*ALen)
state.lunar


# state within year ----

state <- bch.lunar %>% 
  group_by(BeachName, Year, State, Length, type) %>% 
  summarize(avg = mean(avg, na.rm = T)) %>% 
  ungroup() %>% 
  group_by(State, Year, type) %>% 
  summarize(tot = sum(avg),
            totlength = sum(Length)) %>% 
  left_join(state.vals) %>% 
  mutate(tot.sqm = tot/totlength*ALen) %>% 
  ungroup()
state

var1 <- bch.lunar %>% 
  group_by(BeachName, Year, State, Length, type) %>% 
  summarize(avg = mean(avg, na.rm = T)) %>% 
  ungroup() %>% 
  group_by(State, Year, type) %>% 
  summarize(n.beaches = length(unique(BeachName)),
            var = var(avg)) %>% 
  left_join(state.vals) %>% 
  mutate(fpc = (TNum-n.beaches)/(TNum*n.beaches),
         var = var*fpc) %>% 
  ungroup() %>% 
  select(State, Year, type, var, n.beaches)
var1

var.state <- var2 %>% 
  rename(var2 = var) %>% 
  full_join(var3) %>% 
  rename(var3 = var) %>% 
  mutate(varsum = var2+var3) %>% 
  group_by(State, Year, type) %>% 
  summarize(var2_3 = sum(varsum)) %>% 
  full_join(var1) %>% 
  mutate(vartot = var + var2_3/(20*n.beaches*16)) %>% 
  rename(var1 = var)
var.state


state.ests <- state %>% 
  full_join(var.state) %>% 
  select(State, Year, type, tot.sqm, vartot) %>% 
  mutate(lcl90 = exp(log(tot.sqm) - 1.645*sqrt(vartot)/tot.sqm),
         ucl90 = exp(log(tot.sqm) + 1.645*sqrt(vartot)/tot.sqm))
state.ests


# baywide ----
year.ests <- state.ests %>% 
  full_join(state.vals) %>% 
  mutate(state.ind = TLen*tot.sqm,
         state.var = (TNum^2)*vartot) %>% 
  group_by(Year, type) %>% 
  summarize(year.ind = sum(state.ind),
            year.var = sum(state.var),
            totL = sum(TLen),
            totN = sum(TNum)^2,
            index = year.ind/totL,
            var = year.var/totN) %>% 
  ungroup() %>% 
  mutate(lcl90 = exp(log(index) - 1.645*sqrt(var)/index),
         ucl90 = exp(log(index) + 1.645*sqrt(var)/index))
year.ests

year.ests %>% 
  filter(type == "Females")


# save DE IFSA
state.ests %>% 
  filter(State == "DE" & type == "Females") %>%
  ggplot(aes(x = Year, y = tot.sqm)) +
  geom_linerange(aes(ymin = lcl90, ymax = ucl90)) +
  geom_point(size = 2) +
  ylab("Statewide IFSA")

state.ests %>% 
  filter(State == "DE" & type == "Females") %>% 
  write.csv("output/ifsa_de.csv", row.names = F)


# find ifsa per night ----

bch.date %>% 
  filter(type == "Females") %>% 
  mutate(day = day(Date),
         lci = exp(log(avg) + 1.96*sqrt(var)/avg),
         uci = exp(log(avg) + 1.96*sqrt(var)/avg)) %>% 
  ggplot(aes(x = day, y = avg, col = BeachName)) +
  #geom_ribbon(aes(ymin = lci, ymax = uci), alpha = 0.01) +
  geom_point(alpha = 0.5) +
  facet_wrap(~Year)


bch.date %>% 
  filter(type == "Females" & State == "DE") %>% 
  group_by(Year, Date) %>% 
  summarize(var = var(avg, na.rm = T),
            avg = mean(avg, na.rm = T)) %>% 
  ungroup() %>% 
  mutate(day = day(Date)) %>% 
  ggplot(aes(x = day, y = avg, col = as.character(Year))) +
  geom_smooth(alpha = 0.5)


bch.date %>% 
  filter(type == "Females" & State == "DE") %>% 
  group_by(Year, Date) %>% 
  summarize(var = var(avg, na.rm = T),
            avg = mean(avg, na.rm = T)) %>% 
  ungroup() %>% 
  mutate(day = day(Date)) %>% 
  arrange(day) %>% 
  group_by(Year) %>% 
  mutate(cum_ifsa = cumsum(avg),
         cum_prop = cum_ifsa/sum(avg)) %>% 
  ungroup() %>% 
  ggplot(aes(x = day, y = cum_prop, group = Year, col = Year)) +
  geom_smooth(alpha = 0.2, se = F)

hsc_de <- bch.date %>% 
  filter(type == "Females" & State == "DE") %>% 
  group_by(Date) %>% 
  summarize(var = var(avg, na.rm = T),
            avg = mean(avg, na.rm = T)) %>% 
  ungroup() %>% 
  mutate(day = ifelse(month(Date) == 5, day(Date),
                      ifelse(month(Date) == 6, day(Date)+31, day(Date)+61)),
         year = year(Date)) %>% 
  arrange(day) %>% 
  group_by(year) %>% 
  mutate(cum.ifsa = cumsum(avg),
         cum.prop = cum.ifsa/sum(avg)) %>% 
  ungroup() 


# smooth cumulative proportion ####
predict_loess = function(x){
  mod = loess(cum.prop ~ day, data = x)
  p = predict(mod, c(min(x$day):max(x$day)))
  tibble(p.day = c(min(x$day):max(x$day)),
         predicted = p)
}

alldays = as.tibble(expand.grid(day = c(min(hsc_de$day):max(hsc_de$day)),
                                year = unique(hsc_de$year)))

hsc_smooth <- hsc_de %>% 
  full_join(alldays) %>% 
  nest(-year) %>% 
  mutate(smoothed = map(data, predict_loess)) %>% 
  unnest(data, smoothed) %>% 
  arrange(year, day) %>% 
  select(year, p.day, predicted) %>% 
  full_join(hsc_de, by = c("year", "p.day" = "day")) %>% 
  rename(day = p.day)
hsc_smooth

smooth_plot <- ggplot(hsc_smooth) +
  geom_line(aes(x = day, y = predicted), lwd = 1, col = "gray30") +
  geom_point(aes(x = day, y = cum.prop)) +
  facet_wrap(~year) +
  xlab("Day in May") +
  ylab("Cumulative proportion\nof spawning occurred")
smooth_plot

write.csv(hsc_smooth, "output/hsc_smooth.csv", row.names = F)

