# model results
reknmod = readRDS("rekn_mass-gain-2.rds")
rutumod = readRDS("rutu_mass-gain-2.rds")


# covariates ----
# 1 = rate ~ ifsa
# 2 = rate ~ spawn available
# 3 = rate ~ ifsa*spawn available
# 4 = inflection pt ~ ifsa
# 5 = inflection pt  ~ spawn available
# 6 = inflection pt  ~ ifsa*spawn available

par(mfrow = c(2,3))
for(i in 1:6){
  hist(reknmod$sims.list$beta[,i], col = "gray", breaks = 100, 
       main = round(mean(reknmod$sims.list$ind[,i]), 3))
}


par(mfrow = c(2,3))
for(i in 1:6){
  hist(rutumod$sims.list$beta[,i], col = "gray", breaks = 100, 
       main = round(mean(rutumod$sims.list$ind[,i]), 3))
}


# red knot ----

rkmod = reknmod$samples
rkmod %>%
  spread_draws(wmax[year], wmin[year], p[year], b[year]) -> rkmodparms

rkmod %>% 
  spread_draws(wmax.mu, wmin.mu, p.mu, b.mu) %>% 
  mutate(year = "global") %>% 
  rename(wmax = wmax.mu, wmin = wmin.mu,p = p.mu, b = b.mu) -> rkgparms

rkgparms  %>% 
  gather(parm, val, 4:7) %>% 
  group_by(parm) %>% 
  mean_qi(estimate = val) -> rkgparm_sum

rkmod %>% 
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
  mutate(cv = sig/avg) %>%
  gather(est, val, 6:8) %>% 
  group_by(param, est) %>% 
  mean_qi(estimate = val) -> reknparms



# ruddy turnstone ---- 

rtmod = rutumod$samples
rtmod %>%
  spread_draws(wmax[year], wmin[year], p[year], b[year]) -> rtmodparms

rtmod %>% 
  spread_draws(wmax.mu, wmin.mu, p.mu, b.mu) %>% 
  mutate(year = "global") %>% 
  rename(wmax = wmax.mu, wmin = wmin.mu,p = p.mu, b = b.mu) -> rtgparms

rtgparms %>% 
  gather(parm, val, 4:7) %>% 
  group_by(parm) %>% 
  mean_qi(estimate = val) -> rtgparm_sum

rtmod %>% 
  spread_draws(wmax.mu, wmin.mu, p.mu, b.mu, 
                 sigma1, sigma2, sigma3, sigma4) %>% 
  mutate(year = "global") %>% 
  rename(d.avg = wmax.mu, m.avg = wmin.mu, p.avg = p.mu, b.avg = b.mu,
         d.sig = sigma1, m.sig = sigma2, b.sig = sigma3, p.sig = sigma4) %>%
  gather(parm, val, 4:11) %>% 
  mutate(param = substr(parm, 1, 1),
         est = substr(parm, 3, 5)) %>% 
  select(-parm) %>% 
  spread(est, val) %>% 
  mutate(cv = sig/avg) %>%
  gather(est, val, 6:8) %>% 
  group_by(param, est) %>% 
  mean_qi(estimate = val) -> rutuparms


# prob cv for rate and inf pt is greater for rekn than rutu

rtcv <- rtmod %>% 
  spread_draws(p.mu, b.mu, sigma3, sigma4) %>% 
  rename(p.avg = p.mu, b.avg = b.mu, b.sig = sigma3, p.sig = sigma4) %>%
  gather(parm, val, 4:7) %>% 
  mutate(param = substr(parm, 1, 1),
         est = substr(parm, 3, 5)) %>% 
  select(-parm) %>% 
  spread(est, val) %>% 
  mutate(cv = sig/avg) %>% 
  select(-c(avg, sig)) %>% 
  rename(rutu_cv = cv)

rkcv <- rkmod %>% 
  spread_draws(p.mu, b.mu, sigma3, sigma4) %>% 
  rename(p.avg = p.mu, b.avg = b.mu, b.sig = sigma3, p.sig = sigma4) %>%
  gather(parm, val, 4:7) %>% 
  mutate(param = substr(parm, 1, 1),
         est = substr(parm, 3, 5)) %>% 
  select(-parm) %>% 
  spread(est, val) %>% 
  mutate(cv = sig/avg) %>% 
  select(-c(avg, sig)) %>% 
  rename(rekn_cv = cv)


rtcv %>% 
  full_join(rkcv) %>% 
  mutate(rekn_greater_cv = ifelse(rekn_cv > rutu_cv, 1, 0)) %>% 
  group_by(param) %>% 
  summarize(avg = mean(rekn_greater_cv))
