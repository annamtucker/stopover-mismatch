# tables and figures

reknmod = readRDS("rekn_mass-gain-2.rds")
rutumod = readRDS("rutu_mass-gain-2.rds")

source("scripts/05_model-results.R")

# main text ----

# table 1: mass gain model hyperparameters ----
parm.names = c("m" = "Minimum average mass (g)",
               "d" = "Maximum average mass (g)",
               "p" = "Inflection point (day)",
               "b" = "Maximum slope (g/day)")

means_rekn <- reknparms %>% 
  ungroup() %>% 
  select(param, est, estimate, .lower, .upper) %>% 
  filter(est == "avg") %>% 
  select(-est) %>% 
  mutate(param = fct_relevel(parm.names[param], parm.names)) %>% 
  arrange(param) %>% 
  mutate(species = "Red knot")

cv_rekn <- reknparms %>% 
  ungroup() %>% 
  select(param, est, estimate, .lower, .upper) %>% 
  filter(est == "cv") %>% 
  select(-est) %>% 
  mutate(param = fct_relevel(parm.names[param], parm.names)) %>% 
  arrange(param) %>% 
  mutate(species = "Red knot")


means_rutu <- rutuparms %>% 
  ungroup() %>% 
  select(param, est, estimate, .lower, .upper) %>% 
  filter(est == "avg") %>% 
  select(-est) %>% 
  mutate(param = fct_relevel(parm.names[param], parm.names)) %>% 
  arrange(param) %>% 
  mutate(species = "Ruddy turnstone")

cv_rutu <- rutuparms %>% 
  ungroup() %>% 
  select(param, est, estimate, .lower, .upper) %>% 
  filter(est == "cv") %>% 
  select(-est) %>% 
  mutate(param = fct_relevel(parm.names[param], parm.names)) %>% 
  arrange(param)%>% 
  mutate(species = "Ruddy turnstone")

write.csv(bind_rows(means_rekn, means_rutu), 
          file = "output/tab1_means.csv", row.names = F)
write.csv(bind_rows(cv_rekn, cv_rutu), 
          file = "output/tab1_cv.csv", row.names = F)


# table 2: covariate betas and inclusion probabilities ----
# alpha 1 = intercept
# alpha 2 = effect of n15 on proportion of spawn available

# betas
# 1 = rate ~ ifsa
# 2 = rate ~ spawn available
# 3 = rate ~ ifsa*spawn available
# 4 = inflection pt ~ ifsa
# 5 = inflection pt  ~ spawn available
# 6 = inflection pt  ~ ifsa*spawn available

reknmod$samples %>% 
  spread_samples(alpha[cov]) %>% 
  mean_qi(estimate = alpha)

rutumod$samples %>% 
  spread_samples(alpha[cov]) %>% 
  mean_qi(estimate = alpha)

# inclusion probabilities
inc_rekn <- reknmod$samples %>% 
  spread_samples(ind[cov]) %>% 
  group_by(cov) %>% 
  summarize(pr = mean(ind)) %>% 
  mutate(species = "Red knot")
  
inc_rutu <- rutumod$samples %>% 
  spread_samples(ind[cov]) %>% 
  group_by(cov) %>% 
  summarize(pr = mean(ind)) %>% 
  mutate(species = "Ruddy turnstone")                
                 


probs_rekn <- reknmod$samples %>% 
  spread_draws(ind[cov], beta[cov]) %>%
  filter(ind == 1) %>% 
  group_by(cov) %>% 
  summarize(pr_less = mean(beta < 0),
            pr_greater = mean(beta > 0)) %>% 
  mutate(species = "Red knot")

probs_rutu <- rutumod$samples %>% 
  spread_draws(ind[cov], beta[cov]) %>%
  filter(ind == 1) %>% 
  group_by(cov) %>% 
  summarize(pr_less = mean(beta < 0),
            pr_greater = mean(beta > 0)) %>% 
  mutate(species = "Ruddy turnstone")


beta_rekn <- reknmod$samples %>% 
  spread_draws(ind[cov], beta[cov]) %>%
  filter(ind == 1) %>% 
  group_by(cov) %>% 
  mean_qi(estimate = beta) %>% 
  mutate(species = "Red knot") %>% 
  select(cov, estimate, .lower, .upper, species) %>% 
  full_join(inc_rekn) %>% 
  full_join(probs_rekn)

beta_rutu <- rutumod$samples %>% 
  spread_draws(ind[cov], beta[cov]) %>%
  filter(ind == 1) %>% 
  group_by(cov) %>% 
  mean_qi(estimate = beta) %>% 
  mutate(species = "Ruddy turnstone") %>% 
  select(cov, estimate, .lower, .upper, species) %>% 
  full_join(inc_rutu) %>% 
  full_join(probs_rutu)


write.csv(bind_rows(beta_rekn, beta_rutu), 
          file = "output/tab2_betas.csv", row.names = F)


# fig 1: cumulative arrivals/spawning ----
hsc = read_csv("output/hsc_smooth.csv")
ifsa_de = read_csv("output/ifsa_de.csv")
entries = read_csv("output/entry_probs.csv")

hsc %>% 
  mutate(species = "hsc") %>% 
  full_join(ifsa_de, by = c("year" = "Year")) -> hsc


pal = brewer.pal(9, "Greens")


cum_entries <- entries %>% 
  mutate(year = c(2005:2018)[year],
         day = 6+occ*3) %>% 
  ggplot() +
  geom_ribbon(data = hsc, ymin = 0, aes(x = day, ymax = predicted, fill = tot.sqm),
              alpha = 0.8) +
  geom_line(data = hsc, aes(x = day, y = predicted), col = pal[9]) +
  geom_point(aes(x = day, y = cum_prob, col = species), size = 2) +
  geom_line(aes(x = day, y = cum_prob, col = species),
            lwd = 1.5, alpha = 0.6) +
  facet_wrap(~year, scales = "free") +
  xlim(0, 40) +
  ylim(0,1) +
  xlab("Days since May 1") +
  ylab("Cumulative proportion/probability") +
  scale_color_manual(labels = c("Red knot", "Ruddy turnstone"),
                     name = "", 
                     values = c("black",
                                "gray40")) +
  theme(legend.position = "top",
        strip.background = element_rect(fill = "white"),
        legend.title = element_text(size = 12),
        legend.key.width = unit(1,"line")) +
  scale_fill_gradient(name = expression(paste("Statewide spawning abundance (females per"~m^2~")")),
                      breaks = c(0.5, 0.75), 
                      low = pal[2],
                      high = pal[9]) 
cum_entries

save_plot(cum_entries, file = "figs/01_entries-spawning.tiff",
          base_width = 10, base_height = 8)


# fig 2: effect of water temp on spawning ----
covars = read_csv("output/all_covars.csv")

spawn_temp <- covars %>% 
  ggplot(aes(x = n15_q95, y = hsc_q95)) +
  geom_point(size = 3, position = "jitter", alpha = 0.75) + 
  geom_smooth(method = "lm", se = F, lty = 2, col = "black") +
  xlab("Number of days with mean water temperature > 15"~degree~"C") +
  ylab("Proportion of total horseshoe crab\nspawning activity occurred")
spawn_temp

save_plot(spawn_temp, file = "figs/02_spawn-temp.tiff",
          base_width = 8, base_height = 6)


# fig 3: predicted mass across season ----

all.sp.dat = read_csv("output/all_weights.csv")

rekn <- filter(all.sp.dat, species == "REKN")
rutu <- filter(all.sp.dat, species == "RUTU")

predict_mass = function(wmax, wmin, p, b){
  x = wmax + ((wmin-wmax)/(1+(c(1:34)/p)^b))
  data.frame(day = c(1:34),
             pred = x)
}



# red knot

# number of iterations to sample for plot
n = 10000

# predicted value for n randomly selection iterations for each year
rkploty <- rkmodparms %>% 
  group_by(year) %>%
  sample_n(n) %>% 
  mutate(pred = pmap(list(wmax, wmin, p, b), predict_mass)) %>% 
  unnest() %>% 
  rename(id = .iteration) %>% 
  ungroup() %>% 
  select(id, year, day, pred)

# mean predicted values for each year
rkyrmeans <- rkmodparms %>% 
  gather(parm, val, 5:8) %>% 
  group_by(year, parm) %>% 
  summarize(mean = mean(val)) %>% 
  spread(parm, mean) %>% 
  ungroup() %>% 
  mutate(pred = pmap(list(wmax, wmin, p, b), predict_mass)) %>% 
  unnest() %>% 
  ungroup() %>% 
  rename(meanpred = pred) %>% 
  select(year, day, meanpred)

# global average predicted values, join to year averages
rkplotmeans <- rkgparm_sum %>%
  select(parm, estimate) %>% 
  spread(parm, estimate) %>% 
  mutate(pred = pmap(list(wmax, wmin, p, b), predict_mass)) %>% 
  unnest() %>% 
  rename(globpred = pred) %>% 
  select(day, globpred) %>% 
  full_join(rkyrmeans)
  
# add yearly uncertainty, join catch means and sd, and format year and day
rkfullplot <- rkploty %>% 
  full_join(rkplotmeans) %>% 
  mutate(year = year + 1996) %>% 
  left_join(distinct(rekn[,c("year", "day", "avg", "sd")])) %>% 
  mutate(day = ifelse(year %in% c(2004, 2008, 2012, 2016), day+4, day+5))

reknpred <- ggplot(rkfullplot, aes(x = day)) +
  geom_line(alpha = 0.1, col = "gray70", aes(y = pred, group = id)) +
  geom_line(aes(y = globpred), lty = 2, lwd = 1) +
  geom_line(aes(y = meanpred), lwd = 1) +
  geom_point(aes(y = avg), size= 2) +
  geom_linerange(aes(ymin = avg - sd, ymax = avg + sd), lwd = 1) +
  facet_wrap(~year, scales = "free") +
  xlim(3, 40) +
  scale_y_continuous(breaks = c(75, 100, 125, 150, 175, 200), limits = c(75, 200)) +
  xlab("Day in May") +
  ylab("Average mass (g)") +
  theme(strip.background = element_rect(fill = "gray80"))



# ruddy turnstone

# number of iterations to sample for plot
n = 10000

# predicted value for n randomly selection iterations for each year
rtploty <- rtmodparms %>% 
  group_by(year) %>%
  sample_n(n) %>% 
  mutate(pred = pmap(list(wmax, wmin, p, b), predict_mass)) %>% 
  unnest() %>% 
  rename(id = .iteration) %>% 
  ungroup() %>% 
  select(id, year, day, pred)

# mean predicted values for each year
rtyrmeans <- rtmodparms %>% 
  gather(parm, val, 5:8) %>% 
  group_by(year, parm) %>% 
  summarize(mean = mean(val)) %>% 
  spread(parm, mean) %>% 
  ungroup() %>% 
  mutate(pred = pmap(list(wmax, wmin, p, b), predict_mass)) %>% 
  unnest() %>% 
  ungroup() %>% 
  rename(meanpred = pred) %>% 
  select(year, day, meanpred)

# global average predicted values, join to year averages
rtplotmeans <- rtgparm_sum %>%
  select(parm, estimate) %>% 
  spread(parm, estimate) %>% 
  mutate(pred = pmap(list(wmax, wmin, p, b), predict_mass)) %>% 
  unnest() %>% 
  rename(globpred = pred) %>% 
  select(day, globpred) %>% 
  full_join(rtyrmeans)

# add yearly uncertainty, join catch means and sd, and format year and day
rtfullplot <- rtploty %>% 
  full_join(rtplotmeans) %>% 
  mutate(year = year + 1996) %>% 
  left_join(distinct(rutu[,c("year", "day", "avg", "sd")])) %>% 
  mutate(day = ifelse(year %in% c(2004, 2008, 2012, 2016), day+4, day+5))

rutupred <- ggplot(rtfullplot, aes(x = day)) +
  geom_line(alpha = 0.1, col = "gray70", aes(y = pred, group = id)) +
  geom_line(aes(y = globpred), lty = 2, lwd = 1) +
  geom_line(aes(y = meanpred), lwd = 1) +
  geom_point(aes(y = avg), size= 2) +
  geom_linerange(aes(ymin = avg - sd, ymax = avg + sd), lwd = 1) +
  facet_wrap(~year, scales = "free") +
  xlim(3, 40) +
  scale_y_continuous(breaks = c(75, 100, 125, 150, 175, 200), limits = c(75, 200)) +
  xlab("Day in May") +
  ylab("Average mass (g)") +
  theme(strip.background = element_rect(fill = "gray80"))


both_pred = plot_grid(reknpred, rutupred, ncol = 1, align = "hv", scale = 0.9,
                      labels = c("A. Red knot", "B. Ruddy turnstone"),
                      label_x = 0.1, hjust = 0)

save_plot(both_pred, file = "figs/03_predicted-mass.tiff", 
          base_height = 18, base_width = 12)



# fig 4: year-specific model parameters ----
striplabs = c("b" = "Maximum\nrate", 
              "p" = "Inflection\npoint",
              "wmax" = "Maximum\naverage mass",
              "wmin" = "Minimum\naverage mass")


rkmodparms %>% 
  gather(parm, val, 5:8) %>% 
  group_by(parm, year) %>% 
  mean_qi(estimate = val) %>% 
  ungroup() %>% 
  full_join(rkgparm_sum, by = "parm") %>% 
  mutate(parm = fct_relevel(striplabs[parm], "Maximum\nrate"),
         species ="rekn") -> rkparms_toplot

both_toplot <- rtmodparms %>% 
  gather(parm, val, 5:8) %>% 
  group_by(parm, year) %>% 
  mean_qi(estimate = val) %>% 
  ungroup() %>% 
  full_join(rtgparm_sum, by = "parm") %>% 
  mutate(parm = fct_relevel(striplabs[parm], "Maximum\nrate"),
         species = "rutu") %>% 
  full_join(rkparms_toplot) %>% 
  mutate(species = c("rutu" = "Ruddy turnstone", "rekn" = "Red knot")[species])


rate <- both_toplot %>% 
  filter(parm == "Maximum\nrate") %>% 
  ggplot(aes(x = year+1996, group = year)) +
  geom_rect(aes(xmin = 1997, xmax = 2018, ymin = .lower.y, ymax = .upper.y),
            fill = "gray80") +
  geom_hline(aes(yintercept = estimate.y), lty = 2) +
  geom_linerange(aes(ymin = .lower.x, ymax = .upper.x), lwd = 1)+
  geom_point(aes(y = estimate.x), size = 3) +
  facet_wrap( ~ species, scales = "free") +
  scale_y_continuous(limits = c(0, 45), breaks = seq(0, 45, 10)) +
  scale_x_continuous(breaks = seq(1997, 2018, 3), name = "Year") +
  theme(strip.background = element_rect(fill = "white"),
        strip.text = element_text(size = 12, margin = margin(b = 5))) +
  ylab("Rate of mass gain\n(g/day)") 

rate_dist <- both_toplot %>% 
  filter(parm == "Maximum\nrate") %>% 
  ggplot(aes(x = estimate.x, fill = species, col = species, lty = species)) +
  geom_density(alpha = 0.5, lwd = 1, col = "black") +
  geom_rug(size = 1, lty = 1) +
  scale_color_manual(labels = c("Red knot", "Ruddy turnstone"),
                     name = "",
                     values = c("black", "gray40")) +
  scale_fill_manual(labels = c("Red knot", "Ruddy turnstone"),
                     name = "",
                     values = c("black", "gray40")) +
  coord_flip() +
  scale_x_continuous(limits = c(0, 45), breaks = seq(0, 45, 10)) +
  xlab("") +
  theme(axis.title.x = element_blank(),
        axis.text.x = element_blank(),
        axis.ticks.x = element_blank(),
        legend.position = "none") 


infpt <- both_toplot %>% 
  filter(parm == "Inflection\npoint") %>% 
  ggplot(aes(x = year+1996, group = year)) +
  geom_rect(aes(xmin = 1997, xmax = 2018, ymin = .lower.y, ymax = .upper.y),
            fill = "gray80") +
  geom_hline(aes(yintercept = estimate.y), lty = 2) +
  geom_linerange(aes(ymin = .lower.x, ymax = .upper.x), lwd = 1)+
  geom_point(aes(y = estimate.x), size = 3) +
  facet_wrap( ~ species, scales = "free") +
  scale_x_continuous(breaks = seq(1997, 2018, 3), name = "Year") +
  scale_y_continuous(limits = c(14, 32), breaks = seq(10, 40, 5)) +
  theme(strip.background = element_rect(fill = "white"),
        strip.text = element_text(size = 12, margin = margin(b = 5))) +
  ylab("Day of maximum\nrate (day in May)") 

infpt_dist <-  both_toplot %>% 
  filter(parm == "Inflection\npoint") %>% 
  ggplot(aes(x = estimate.x, fill = species, col = species, lty = species)) +
  geom_density(alpha = 0.5, lwd = 1, col = "black") +
  geom_rug(size = 1, lty = 1) +
  scale_color_manual(labels = c("Red knot", "Ruddy turnstone"),
                     name = "",
                     values = c("black", "gray40")) +
  scale_fill_manual(labels = c("Red knot", "Ruddy turnstone"),
                    name = "",
                    values = c("black", "gray40")) +
  coord_flip() +
  scale_x_continuous(limits = c(14, 32), breaks = seq(10, 40, 5)) +
  xlab("") +
  theme(axis.title.x = element_blank(),
        axis.text.x = element_blank(),
        axis.ticks.x = element_blank(),
        legend.position = "none") 

min <- both_toplot %>% 
  filter(parm == "Minimum\naverage mass") %>% 
  ggplot(aes(x = year+1996, group = year)) +
  geom_rect(aes(xmin = 1997, xmax = 2018, ymin = .lower.y, ymax = .upper.y),
            fill = "gray80") +
  geom_hline(aes(yintercept = estimate.y), lty = 2) +
  geom_linerange(aes(ymin = .lower.x, ymax = .upper.x), lwd = 1)+
  geom_point(aes(y = estimate.x), size = 3) +
  facet_wrap( ~ species, scales = "free") +
  scale_x_continuous(breaks = seq(1997, 2018, 3), name = "Year") +
  scale_y_continuous(limits = c(80,150), breaks = seq(80, 140, 10)) +
  theme(strip.background = element_rect(fill = "white"),
        strip.text = element_text(size = 12, margin = margin(b = 5))) +
  ylab("Average mass (g)") 

min_dist <- both_toplot %>% 
  filter(parm == "Minimum\naverage mass") %>% 
  ggplot(aes(x = estimate.x, fill = species, col = species, lty = species)) +
  geom_density(alpha = 0.5, lwd = 1, col = "black") +
  geom_rug(size = 1, lty = 1) +
  scale_color_manual(labels = c("Red knot", "Ruddy turnstone"),
                     name = "",
                     values = c("black", "gray40")) +
  scale_fill_manual(labels = c("Red knot", "Ruddy turnstone"),
                    name = "",
                    values = c("black", "gray40")) +
  coord_flip() +
  scale_x_continuous(limits = c(80,150), breaks = seq(80, 140, 10)) +
  xlab("") +
  theme(axis.title.x = element_blank(),
        axis.text.x = element_blank(),
        axis.ticks.x = element_blank(),
        legend.position = "none") 


max <- both_toplot %>% 
  filter(parm == "Maximum\naverage mass") %>% 
  ggplot(aes(x = year+1996, group = year)) +
  geom_rect(aes(xmin = 1997, xmax = 2018, ymin = .lower.y, ymax = .upper.y),
            fill = "gray80") +
  geom_hline(aes(yintercept = estimate.y), lty = 2) +
  geom_linerange(aes(ymin = .lower.x, ymax = .upper.x), lwd = 1)+
  geom_point(aes(y = estimate.x), size = 3) +
  facet_wrap( ~ species, scales = "free") +
  scale_x_continuous(breaks = seq(1997, 2018, 3), name = "Year") +
  scale_y_continuous(limits = c(120, 220), breaks = seq(120, 240, 20)) +
  theme(strip.background = element_rect(fill = "white"),
        strip.text = element_text(size = 12, margin = margin(b = 5))) +
  ylab("Average mass (g)") 

max_dist <- both_toplot %>% 
  filter(parm == "Maximum\naverage mass") %>% 
  ggplot(aes(x = estimate.x, fill = species, col = species, lty = species)) +
  geom_density(alpha = 0.5, lwd = 1, col = "black") +
  geom_rug(size = 1, lty = 1) +
  scale_color_manual(labels = c("Red knot", "Ruddy turnstone"),
                     name = "",
                     values = c("black", "gray40")) +
  scale_fill_manual(labels = c("Red knot", "Ruddy turnstone"),
                    name = "",
                    values = c("black", "gray40")) +
  coord_flip() +
  scale_x_continuous(limits = c(120, 220), breaks = seq(120, 240, 20)) +
  xlab("") +
  theme(axis.title.x = element_blank(),
        axis.text.x = element_blank(),
        axis.ticks.x = element_blank(),
        legend.position = "none") 


yrparms <- plot_grid(rate, rate_dist,
                     infpt, infpt_dist,
                     min, min_dist, 
                     max, max_dist, 
                     ncol = 2, 
                     align = "hv", 
                     axis = "tblr",
                     scale = 0.9,
                     rel_widths = c(4,1),
                     labels = c("A. Maximum rate", "", 
                                "B. Inflection point", "",
                                "C. Minimum average mass", "",
                                "D. Maximum average mass", ""),
                     label_x = 0.1, hjust = 0, label_y = c(1, 1.1, 1.1, 1.1)) 

yrparms

save_plot(yrparms, file = "figs/04_year-parms.tiff", 
          base_width = 10, base_height = 12)


# fig 5: effect of covariates on timing and rate ----
covlabs = c("1" = "rate ~ ifsa",
            "2" = "rate ~ prop", 
            "3" = "rate ~ ifsa x prop",
            "4" = "inf pt ~ ifsa",
            "5" = "inf pt ~ prop",
            "6" = "inf pt ~ ifsa x prop")


reknbetas <- reknmod$samples %>% 
  spread_draws(ind[cov], beta[cov]) %>%
  filter(ind == 1) %>% 
  mutate(species = "Red knot") %>% 
  full_join(inc_rekn) %>% 
  ungroup()

rutubetas <- rutumod$samples %>% 
  spread_draws(ind[cov], beta[cov]) %>%
  filter(ind == 1) %>% 
  mutate(species = "Ruddy turnstone") %>% 
  full_join(inc_rutu) %>% 
  ungroup()

# continuous inclusion probability
reknbetas %>% 
  bind_rows(rutubetas) %>% 
  mutate(cov = fct_reorder(covlabs[cov], pr)) %>% 
  ggplot(aes(y = cov, x = beta, fill = pr)) +
  geom_halfeyeh() +
  facet_wrap(~species) +
  geom_vline(xintercept = 0, lty = 2, lwd = 1) + 
  scale_x_continuous(breaks = seq(-20, 20, 5)) +
  scale_fill_viridis_c(option = "A", limits = c(0,1)) +
  theme(strip.background = element_rect(fill = "white"),
        strip.text = element_text(size = 12, margin = margin(b = 5))) 


# plot betas


predict_beta = function(beta, species, p, b, type){
  
  x = seq(-2, 2, 0.05)
  
  int = ifelse(type == "b", b, p)
  
  y = int + beta*x
  
  return(data.frame(x = x, pred = y))
  
}

rkgparms %>% 
  mutate(species = "Red knot") -> rkg

rtgparms %>% 
  mutate(species = "Ruddy turnstone") %>% 
  bind_rows(rkg) %>% 
  select(species, .chain, .iteration, .draw, p, b) -> ints

pred_betas <- reknbetas %>% 
  bind_rows(rutubetas) %>% 
  full_join(ints) %>% 
  mutate(type = ifelse(cov %in% c(1, 2, 3), "b", "p")) %>% 
  group_by(species, cov) %>% 
  mutate(pred = pmap(list(beta, species, p, b, type), predict_beta)) %>% 
  ungroup() %>% 
  unnest(pred) %>% 
  group_by(species, cov, x) %>% 
  mean_qi(est = pred)



pred_inf <- reknbetas %>% 
  bind_rows(rutubetas) %>% 
  filter(cov == 5) %>% 
  group_by(species) %>% 
  mutate(pred = map2(beta, species, predict_inf)) %>% 
  unnest(pred) %>% 
  group_by(species, x) %>% 
  mean_qi(est = pred)


ggplot(pred_inf, aes(x = x)) +
  geom_line(aes(y = .lower), lty = 2, lwd = 1) +
  geom_line(aes(y = .upper), lty = 2, lwd = 1) +
  geom_line(aes(y = est), lwd = 1) + 
  facet_wrap(~species, scales = "free") +
  ylim(15, 30)+
  theme(strip.background = element_rect(fill = "white"),
        strip.text = element_text(size = 14, margin = margin(2, 2, 2, 2)),
        axis.text = element_text(size = 14),
        axis.title = element_text(size = 14),
        legend.position = "none") +
  xlab("Relative proportion of spawn available (scaled)") +
  ylab("Inflection point of mass gain curve\n(day in May)") 





# relationship between covs and model parms
rkmodparms %>% 
  mutate(species = "REKN") -> rk

toplot <- rtmodparms %>% 
  mutate(species = "RUTU") %>% 
  full_join(rk) %>% 
  gather(parm, val, 5:8) %>% 
  group_by(species, year, parm) %>% 
  mean_qi(estimate = val) %>% 
  ungroup() %>% 
  mutate(year = year + 1996) %>% 
  full_join(covars, by = c("species", "year")) %>% 
  mutate(ifsa_sc = (ifsa - mean(ifsa, na.rm = T))/sd(ifsa, na.rm = T),
         spawn_sc = (hsc_q95 - mean(hsc_q95, na.rm = T))/sd(hsc_q95, na.rm = T)) %>% 
  filter(parm %in% c("b", "p"))  %>% 
  mutate(species = c("REKN" = "Red knot", "RUTU" = "Ruddy turnstone")[species]) 

maxrate_hsc <- toplot %>% 
  filter(parm == "b") %>% 
  ggplot(aes(x = ifsa_sc*spawn_sc, col = species)) +
  geom_smooth(aes(y = estimate), method = "lm", lty = 2, se = F, col = "gray60") +
  geom_linerange(aes(ymin = .lower, ymax = .upper), lwd = 1,
                 alpha = 0.8, position = position_dodge(width = 0.25)) +
  geom_point(aes(y = estimate), size = 4, alpha = 0.8,
             position = position_dodge(width = 0.25)) +
  xlab("Relative horseshoe crab egg availability\n(scaled abundance x proportion available)") +
  ylab("Maximum rate of mass gain\n(g/day)") +
  facet_wrap(~species, scales = "free") +
  scale_color_manual(values = c("palegreen4", "dodgerblue3")) +
  scale_y_continuous(breaks = seq(0, 40, 10), limits = c(0, 40))+
  theme(strip.background = element_rect(fill = "white"),
        strip.text = element_text(size = 14, margin = margin(2, 2, 2, 2)),
        axis.text = element_text(size = 14),
        axis.title = element_text(size = 14),
        legend.position = "none")
maxrate_hsc


rate_hsc <- pred_betas %>% 
  filter(cov == 3) %>% 
  ggplot(aes(x = x)) +
  geom_line(aes(y = .lower), lty = 2, lwd = 1) +
  geom_line(aes(y = .upper), lty = 2, lwd = 1) +
  geom_line(aes(y = est), lwd = 1) + 
  facet_wrap(~species, scales = "free") +
  ylim(0, 30) +
  xlim(-0.5, 2) +
  theme(strip.background = element_rect(fill = "white"),
        strip.text = element_text(size = 14, margin = margin(2, 2, 2, 2)),
        axis.text = element_text(size = 14),
        axis.title = element_text(size = 14)) + 
  xlab("Relative horseshoe crab egg availability\n(scaled abundance x proportion available)") +
  ylab("Maximum rate of mass gain\n(g/day)") 
rate_hsc


toplot %>% 
  filter(parm == "b") %>% 
  mutate(rel_avail = ifsa_sc*spawn_sc) %>% 
  full_join(pred_rate, by = c("rel_avail" = "x", "species")) %>% 
  select(species, year, rel_avail, estimate, .lower.x, .upper.x, est, .lower.y, .upper.y) %>% 
  ggplot(aes(x = rel_avail, col = species)) +
  geom_line(aes(y = .lower.y), lty = 2, lwd = 1, col = "black") +
  geom_line(aes(y = .upper.y), lty = 2, lwd = 1, col = "black") +
  geom_line(aes(y = est), lwd = 1, col = "black") + 
    geom_linerange(aes(ymin = .lower.x, ymax = .upper.x), lwd = 1,
                 alpha = 0.8, position = position_dodge(width = 0.25)) +
  geom_point(aes(y = estimate), size = 4, alpha = 0.8,
             position = position_dodge(width = 0.25)) +
  xlab("Relative horseshoe crab egg availability\n(scaled abundance x proportion available)") +
  ylab("Maximum rate of mass gain\n(g/day)") +
  facet_wrap(~species, scales = "free") +
  scale_color_manual(values = c("palegreen4", "dodgerblue3")) +
  scale_y_continuous(breaks = seq(0, 40, 10), limits = c(0, 40))+
  theme(strip.background = element_rect(fill = "white"),
        strip.text = element_text(size = 14, margin = margin(2, 2, 2, 2)),
        axis.text = element_text(size = 14),
        axis.title = element_text(size = 14),
        legend.position = "none")



infpt_hsc <- toplot %>% 
  filter(parm == "p") %>% 
  mutate(species = c("REKN" = "Red knot", "RUTU" = "Ruddy turnstone")[species]) %>% 
  ggplot(aes(x = spawn_sc, col = species)) +
  geom_smooth(aes(y = estimate), lty = 2, method = "lm", se = F, col = "gray60") +
  geom_linerange(aes(ymin = .lower, ymax = .upper), lwd = 1,
                 alpha = 0.8, position = position_dodge(width = 0.25)) +
  geom_point(aes(y = estimate), size = 4, alpha = 0.8, 
             position = position_dodge(width = 0.25)) +
  xlab("Relative proportion of spawn available (scaled)") +
  ylab("Inflection point of mass gain curve\n(day in May)") +
  ylim(15, 32) +
  facet_wrap(~species, scales = "free") +
  scale_color_manual(values = c("palegreen4", "dodgerblue3")) +
  theme(strip.background = element_rect(fill = "white"),
        strip.text = element_text(size = 14, margin = margin(2, 2, 2, 2)),
        axis.text = element_text(size = 14),
        axis.title = element_text(size = 14),
        legend.position = "none")
infpt_hsc



inf_hsc <- pred_betas %>% 
  filter(cov == 5) %>% 
  ggplot(aes(x = x)) +
  geom_line(aes(y = .lower), lty = 2, lwd = 1) +
  geom_line(aes(y = .upper), lty = 2, lwd = 1) +
  geom_line(aes(y = est), lwd = 1) + 
  facet_wrap(~species, scales = "free") +
  ylim(15, 30) +
  xlim(-2, 1.5) +
  theme(strip.background = element_rect(fill = "white"),
        strip.text = element_text(size = 14, margin = margin(2, 2, 2, 2)),
        axis.text = element_text(size = 14),
        axis.title = element_text(size = 14)) + 
  xlab("Relative proportion of spawn available (scaled)") +
  ylab("Inflection point of mass gain curve\n(day in May)") 
inf_hsc



fig5 <- plot_grid(rate_hsc, inf_hsc, ncol = 1, labels = c("A", "B"))

save_plot(fig5, file = "figs/05_rate-pt-hsc.tiff", base_width = 8, 
          base_height = 8.5)






# fig 6: association between start day and other model parameters ----
convert_parms = function(b, p, wmax, wmin){
  
  mod = expression(wmax + ((wmin - wmax)/(1 + (x/p)^b)))
  
  # predicted mass
  x = c(1:34)
  pred = eval(mod)
  
  # first derivative
  x = seq(1,34,0.01)
  d1 = D(mod, "x")
  
  # max rate of mass gain
  max.rate = max(eval(d1))
  
  # second derivative - change in rate of mass gain
  d2 = D(d1, "x")
  
  # fxn to find inflection point
  ip = function(y){
    for(i in 1:length(y)){
      if(sign(y[i]) == sign(y[i+1])) next
      return(i)
    }
  }
  D2 = try(ip(eval(d2)), silent = T)
  inf.pt = ifelse(class(D2) == "try-error", NA, x[ip(eval(d2))])
  
  # start point is day before inflection point with maximum absolute 2nd deriv
  # stop point is day after inflection point with maximum absolute 2nd deriv
  x_all = seq(1,34,0.01)
  
  x = x_all[which(x_all < inf.pt)]
  st = x[which.max(abs(eval(d2)))]
  
  x = x_all[which(x_all > inf.pt)]
  stop = x[which.max(abs(eval(d2)))]
  
  # mean rate of mass gain between start and stop points
  if(length(st) > 0 & length(stop) > 0){
    x = c(st:stop)
    mean.rate = mean(eval(d1))
  } else mean.rate = st = stop = NA
  
  return(c("max.rate" = max.rate, "mean.rate" = mean.rate,
           "inf.pt" = inf.pt, "start" = st, "stop" = stop))
}


# hyperparameters

rkderivs_g <- rkmod %>% 
  spread_draws(wmax.mu, wmin.mu, p.mu, b.mu, 
               sigma1, sigma2, sigma3, sigma4) %>% 
  mutate(year = "global") %>% 
  rename(d.avg = wmax.mu, m.avg = wmin.mu, p.avg = p.mu, b.avg = b.mu,
         d.sig = sigma1, m.sig = sigma2, b.sig = sigma3, p.sig = sigma4) %>% 
  gather(parm, val, 4:11)  %>% 
  mutate(param = substr(parm, 1, 1),
         est = substr(parm, 3, 5))  %>% 
  select(-parm) %>% 
  spread(est, val) %>% 
  select(-sig) %>% 
  spread(param, avg) %>% 
  rename(wmax = d, wmin = m) %>% 
  mutate(derivs = pmap(list(b, p, wmax, wmin), convert_parms)) %>% 
  unnest() %>% 
  mutate(val = rep(c("max.rate", "mean.rate", "inf.pt", "start", "stop"), 
                   length.out = nrow(rkderivs_g)))


rkderivs_g_sum <- rkderivs_g %>% 
  spread(val, derivs) %>% 
  filter(!is.na(start)) %>% 
  gather(val, est, 9:13) %>% 
  group_by(val) %>% 
  mean_qi(est) 


rtderivs_g <- rtmod %>% 
  spread_draws(wmax.mu, wmin.mu, p.mu, b.mu, 
               sigma1, sigma2, sigma3, sigma4) %>% 
  mutate(year = "global") %>% 
  rename(d.avg = wmax.mu, m.avg = wmin.mu, p.avg = p.mu, b.avg = b.mu,
         d.sig = sigma1, m.sig = sigma2, b.sig = sigma3, p.sig = sigma4) %>% 
  gather(parm, val, 4:11)  %>% 
  mutate(param = substr(parm, 1, 1),
         est = substr(parm, 3, 5))  %>% 
  select(-parm) %>% 
  spread(est, val) %>% 
  select(-sig) %>% 
  spread(param, avg) %>% 
  rename(wmax = d, wmin = m) %>% 
  mutate(derivs = pmap(list(b, p, wmax, wmin), convert_parms)) %>% 
  unnest() %>% 
  mutate(val = rep(c("max.rate", "mean.rate", "inf.pt", "start", "stop"), 
                   length.out = nrow(rkderivs_g)))
         
         
rtderivs_g_sum <- rtderivs_g %>% 
  spread(val, derivs) %>% 
  filter(!is.na(start)) %>% 
  gather(val, est, 9:13) %>% 
  group_by(val) %>% 
  mean_qi(est) 


pushover("done")

# yearly parameters
rkderivs <- rkmodparms %>% 
  sample_frac(0.5) %>% 
  mutate(derivs = pmap(list(b, p, wmax, wmin), convert_parms)) %>% 
  mutate(species = "RUTU") %>% 
  unnest() %>% 
  ungroup() %>% 
  mutate(val = rep(c("max.rate", "mean.rate", "inf.pt", "start", "stop"), 
                   nrow(rkmodparms)/2)) %>% 
  spread(val, derivs) 

rtderivs <- rtmodparms %>% 
  sample_frac(0.5) %>% 
  mutate(derivs = pmap(list(b, p, wmax, wmin), convert_parms)) %>% 
  mutate(species = "RUTU") %>% 
  unnest() %>% 
  ungroup() %>% 
  mutate(val = rep(c("max.rate", "mean.rate", "inf.pt", "start", "stop"), 
                   nrow(rtmodparms)/2)) %>% 
  spread(val, derivs) 

# write.csv(rtderivs, "output/rutu_derivs.csv", row.names = F)
# write.csv(rkderivs, "output/rekn_derivs.csv", row.names = F)

rtderivs = read.csv("output/rutu_derivs.csv")
rkderivs = read.csv("output/rekn_derivs.csv")

rkderivs %>% 
  bind_rows(rtderivs) -> derivs



derivs %>% 
  filter(!is.na(start)) %>% 
  group_by(year, species) %>% 
  mean_qi(inf.pt, max.rate, mean.rate, start, stop) %>% 
  ungroup() %>% 
  mutate(year = year+1996) -> deriv_sum


derivs %>% 
  filter(!is.na(start)) %>% 
  gather(est, val, 10:14) %>%  
  group_by(year, species, est) %>% 
  summarize(med = median(val)) %>% 
  ungroup() %>% 
  group_by(species, est) %>% 
  mean_qi(estimate= med)

derivs %>% 
  select(species, year, start, stop) %>% 
  mutate(window = stop-start) %>% 
  group_by(species, year) %>% 
  summarize(medwin = median(window, na.rm = T)) %>% 
  ungroup() %>% 
  group_by(species) %>% 
  mean_qi(estimate = medwin)


avgrate_day <- deriv_sum %>% 
  full_join(covars) %>% 
  filter(!is.na(species)) %>% 
  mutate(species = c("REKN" = "Red knot", "RUTU" = "Ruddy turnstone")[species]) %>% 
  ggplot(aes(x = start, y = mean.rate)) +
  geom_linerange(aes(ymin = mean.rate.lower, ymax = mean.rate.upper)) +
  geom_segment(aes(x = start.lower, xend = start.upper, 
                   yend = mean.rate)) +
  geom_point(size = 2) +
  xlab("Start of mass gain (day in May)") +
  ylab("Average rate of\nmass gain (g/day)") +
  facet_wrap(~species, scales = "free") +
  ylim(0, 30) +
  theme(strip.background = element_rect(fill = "white"))+
  scale_x_continuous(breaks = seq(5,25,5), limits = c(0, 30))
avgrate_day

stopday_day <- deriv_sum %>% 
  mutate(species = c("REKN" = "Red knot", "RUTU" = "Ruddy turnstone")[species]) %>% 
  mutate(start = start, stop = stop,
         start.low = start.lower, start.high = start.upper,
         stop.low = stop.lower, stop.high = stop.upper) %>% 
  ggplot(aes(x = start,y = stop)) +
  facet_wrap(~species, scales = "free") +
  geom_linerange(aes(ymin = stop.low, ymax= stop.high)) +
  geom_segment(aes(x = start.low, xend = start.high, y = stop, yend = stop)) +
  geom_point(size = 2) +
  xlab("Start of mass gain (day in May)") +
  ylab("Stop of mass gain\n(day in May)") +
  theme(strip.background = element_rect(fill = "white"))+
  scale_x_continuous(breaks = seq(5,40,5), limits = c(5, 40)) +
  scale_y_continuous(breaks = seq(15, 40, 5), limits = c(12, 40))
stopday_day


maxmass_day <- derivs %>% 
  filter(!is.na(start)) %>% 
  mutate(species = c("REKN" = "Red knot", "RUTU" = "Ruddy turnstone")[species]) %>% 
  group_by(year, species) %>% 
  mean_qi(wmax, start) %>% 
  ungroup() %>% 
  mutate(year = year+1996)  %>% 
  mutate(start = start, start.low = start.lower, start.high = start.upper) %>% 
  ggplot(aes(x = start, y = wmax)) +
  facet_wrap(~species, scales = "free") +
  xlim(0, 30) +
  geom_linerange(aes(ymin = wmax.lower, ymax = wmax.upper)) +
  geom_segment(aes(x = start.low, xend = start.high, y = wmax, yend = wmax)) +
  geom_point(size = 2) +
  xlab("Start of mass gain (day in May)") +
  ylab("Predicted maximum\naverage mass (g)") +
  theme(strip.background = element_rect(fill = "white")) +
  scale_y_continuous(breaks = seq(120, 240, 20), limits = c(130, 220))
maxmass_day

start_corr = plot_grid(avgrate_day, stopday_day, maxmass_day, ncol = 2, 
                       labels = c("A", "B", "C"), scale = 0.9)
save_plot(start_corr, file = "figs/06_startday-corrs.tiff", base_width = 10, base_height= 7)

# supporting info ----

# table s1: numbers of catches, birds weighed, & resighted each year ----
banding = read_csv("output/all_weights.csv")

n_catch <- banding %>% 
  group_by(year, species) %>% 
  summarize(n_catches = length(unique(day))) %>% 
  spread(species, n_catches) %>% 
  rename(rekn.catch = REKN, rutu.catch = RUTU)

n_weighed <- banding %>% 
  group_by(year, species) %>% 
  summarize(n_weighed = length(weight)) %>% 
  spread(species, n_weighed) %>% 
  rename(rekn.weighed = REKN, rutu.weighed = RUTU)

resight = read_csv("output/resighting-clean.csv")

n_resight <- resight %>% 
  filter(species %in% c("REKN", "RUTU")) %>% 
  group_by(year, species) %>% 
  summarize(n_resight = length(unique(flag))) %>% 
  spread(species, n_resight) %>% 
  rename(rekn.resight = REKN, rutu.resight = RUTU)

n_catch %>% 
  full_join(n_weighed) %>% 
  full_join(n_resight) %>% 
  select(year, rekn.catch, rekn.weighed, rekn.resight,
         rutu.catch, rutu.weighed, rutu.resight) %>% 
  write.csv("output/tabs01_sample-sizes.csv", row.names = F)


resight %>% 
  filter(species %in% c("REKN", "RUTU")) %>% 
  group_by(species) %>% 
  summarize(ind = length(unique(flag)))

# table s2: year-specific mass gain model parameter estimates, rekn ----
rk_modests <- rkmodparms %>% 
  gather(parm, val, 5:8) %>% 
  group_by(year, parm) %>% 
  mean_qi(val) %>% 
  select(year, parm, val, .lower, .upper) %>% 
  mutate(val = round(val, 1),
         .lower = round(.lower, 1),
         .upper = round(.upper, 1)) %>% 
  mutate(sumval = paste(val, "(", .lower, ",", .upper, ")")) %>% 
  select(year, parm, sumval) %>% 
  spread(parm, sumval) %>% 
  select(year, wmin, wmax, p, b)

rk_dervests <- rkderivs %>% 
  select(year, start, stop, mean.rate) %>% 
  filter(!is.na(start)) %>% 
  gather(parm, val, 2:4) %>% 
  group_by(year, parm) %>% 
  mean_qi(val) %>% 
  select(year, parm, val, .lower, .upper) %>% 
  mutate(val = round(val, 1),
         .lower = round(.lower, 1),
         .upper = round(.upper, 1)) %>% 
  mutate(sumval = paste(val, "(", .lower, ",", .upper, ")")) %>% 
  select(year, parm, sumval) %>% 
  spread(parm, sumval) %>% 
  select(year, start, stop, mean.rate)

rk_modests %>% 
  full_join(rk_dervests) %>% 
  write.csv("output/tabs02_rekn-parms.csv", row.names = F)


# table s3: year-specific mass gain model parameter estimates, rutu ----
rt_modests <- rtmodparms %>% 
  gather(parm, val, 5:8) %>% 
  group_by(year, parm) %>% 
  mean_qi(val) %>% 
  select(year, parm, val, .lower, .upper) %>% 
  mutate(val = round(val, 1),
         .lower = round(.lower, 1),
         .upper = round(.upper, 1)) %>% 
  mutate(sumval = paste(val, "(", .lower, ",", .upper, ")")) %>% 
  select(year, parm, sumval) %>% 
  spread(parm, sumval) %>% 
  select(year, wmin, wmax, p, b)

rt_dervests <- rtderivs %>% 
  select(year, start, stop, mean.rate) %>% 
  filter(!is.na(start)) %>% 
  gather(parm, val, 2:4) %>% 
  group_by(year, parm) %>% 
  mean_qi(val) %>% 
  select(year, parm, val, .lower, .upper) %>% 
  mutate(val = round(val, 1),
         .lower = round(.lower, 1),
         .upper = round(.upper, 1)) %>% 
  mutate(sumval = paste(val, "(", .lower, ",", .upper, ")")) %>% 
  select(year, parm, sumval) %>% 
  spread(parm, sumval) %>% 
  select(year, start, stop, mean.rate)

rt_modests %>% 
  full_join(rt_dervests) %>% 
  write.csv("output/tabs03_rutu-parms.csv", row.names = F)


# fig s1: derivates of mass gain curves ----

convert_parms_plot = function(wmax, wmin, b, p){
  # wmax = parms[1]
  # wmin = parms[2]
  # b = parms[3]
  # p = parms[4]
  
  mod = expression(wmax + ((wmin - wmax)/(1 + (x/p)^b)))
  
  # predicted mass
  x = seq(1,34,0.01)
  pred = eval(mod)
  
  # first derivative
  x = seq(1,34,0.01)
  d1 = D(mod, "x")
  
  # max rate of mass gain
  max.rate = max(eval(d1))
  
  # second derivative - change in rate of mass gain
  d2 = D(d1, "x")
  
  ip = function(y){
    for(i in 1:length(y)){
      if(sign(y[i]) == sign(y[i+1])) next
      return(i)
    }
  }
  D2 = try(ip(eval(d2)), silent = T)
  inf.pt = ifelse(class(D2) == "try-error", NA, x[ip(eval(d2))])
  
  # start point is day before inflection point with maximum absolute 2nd deriv
  # stop point is day after inflection point with maximum absolute 2nd deriv
  x_all = x
  
  x = x_all[which(x_all < inf.pt)]
  st = x[which.max(abs(eval(d2)))]
  
  x = x_all[which(x_all > inf.pt)]
  stop = x[which.max(abs(eval(d2)))]
  
  # start point is max day before inflection point with 3nd deriv == 0
  # stop point is min day after inflection point with 3nd deriv == 0
  x = seq(1,34,0.01)
  d3 = D(d2, "x")
  
  x_all = seq(1,34,0.01)
  
  x = x_all[which(x_all < inf.pt)]
  st2 = x[which.max(eval(d3))]
  
  x = x_all[which(x_all > inf.pt)]
  stop2 = x[which.max(eval(d3))]
  
  # start point is maximum absolute 4th deriv
  
  
  
  # mean rate of mass gain between start and stop points
  x = c(st:stop)
  mean.rate = mean(eval(d1))
  
  x = c(st2:stop2)
  mean.rate2 = mean(eval(d1))
  
  x = seq(1, 34, 0.1)
  return(data.frame(x = x,
                    f = eval(mod),
                    d1 = eval(d1),
                    d2 = eval(d2),
                    d3 = eval(d3),
                    start = st, 
                    stop = stop,
                    start2 = st2,
                    stop2 = stop2,
                    mean.rate = mean.rate,
                    mean.rate2 = mean.rate2))
  
  
  # PLOTS
  
  # x = c(1:34)
  # par(mfrow = c(2,2))
  # plot(x, eval(mod), type = "l", lwd = 2, xlab = "Day", ylab = "Predicted mass", 
  #      xlim = c(1, 34), main = "Red knot, overall average")
  # abline(v = st, lty = 2)
  # abline(v = stop, lty = 2)
  # 
  # plot(x, eval(d1), type = "l", lwd = 2, xlab = "Day", ylab = "Rate of mass gain\n(g/day)",
  #      main = "First derivative")
  # abline(v = st, lty = 2)
  # abline(v = stop, lty = 2)
  # 
  # plot(x, eval(d2), type = "l", lwd = 2, xlab = "Day", 
  #      ylab = "Change in rate of mass gain\n(g/day/day)",
  #      main = "Second derivative")
  # abline(v = st, lty = 2)
  # abline(v = stop, lty = 2)
  
  # return(c("max.rate" = max.rate, "mean.rate" = mean.rate,
  #             "inf.pt" = inf.pt, "start" = st, "stop" = stop))
}

reknparms %>% 
  ungroup() %>% 
  filter(est == "avg") %>% 
  select(param, estimate) %>% 
  spread(param, estimate) %>% 
  rename(wmin = m, wmax = d) %>% 
  mutate(deriv = pmap(list(wmax, wmin, b, p), convert_parms_plot)) %>% 
  select(deriv) %>% 
  unnest() %>% 
  select(x, f, d1, d2, start, stop, mean.rate) %>% 
  gather(type, val, 2:4) %>% 
  mutate(type = c("f" = "Predicted mass", "d1" = "Rate of mass gain", 
                  "d2" = "Change in rate of mass gain")[type],
         type = fct_relevel(as.factor(type), "Predicted mass", "Rate of mass gain",
                            "Change in rate of mass gain")) %>% 
  ggplot(aes(x = x, y = val)) +
  geom_line(lwd = 1) +
  geom_vline(aes(xintercept = start), lty = 2) +
  geom_vline(aes(xintercept = stop), lty = 2) +
  xlab("Day") +
  ylab("") +
  facet_wrap(~type, scales = "free", ncol = 2) +
  theme(strip.background = element_rect(fill= "white"),
        strip.text = element_text(margin = margin(0.1, 0.1, 0.1, 0.1, "cm"))) -> rkdervplot
rkdervplot

rutuparms %>% 
  ungroup() %>% 
  filter(est == "avg") %>% 
  select(param, estimate) %>% 
  spread(param, estimate) %>% 
  rename(wmin = m, wmax = d) %>% 
  mutate(deriv = pmap(list(wmax, wmin, b, p), convert_parms_plot)) %>% 
  select(deriv) %>% 
  unnest() %>% 
  select(x, f, d1, d2, start, stop, mean.rate) %>% 
  gather(type, val, 2:4) %>% 
  mutate(type = c("f" = "Predicted mass", "d1" = "Rate of mass gain", 
                  "d2" = "Change in rate of mass gain")[type],
         type = fct_relevel(as.factor(type), "Predicted mass", "Rate of mass gain",
                            "Change in rate of mass gain")) %>% 
  ggplot(aes(x = x, y = val)) +
  geom_line(lwd = 1) +
  geom_vline(aes(xintercept = start), lty = 2) +
  geom_vline(aes(xintercept = stop), lty = 2) +
  xlab("Day") +
  ylab("") +
  facet_wrap(~type, scales = "free", ncol = 2) +
  theme(strip.background = element_rect(fill= "white"),
        strip.text = element_text(margin = margin(0.1, 0.1, 0.1, 0.1, "cm"))) -> rtdervplot
rtdervplot


dervplot = plot_grid(rkdervplot, rtdervplot, ncol = 1, labels = c("A. Red knot",
                                                                  "B. Ruddy turnstone"),
                     align = "hv", scale = 0.9,
                     label_x = 0.1, hjust = 0, label_y = c(1, 1))
save_plot(dervplot, file = "figs/s01_derivatives.tiff", base_width = 8, base_height = 10)
