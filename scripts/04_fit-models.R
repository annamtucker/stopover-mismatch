# fit mass gain models

# prepare data ----
weights = read_csv("output/all_weights.csv")
covars = read_csv("output/all_covars.csv")

# scale and center covariates
scaled.covs <- covars %>% 
  select(year, species, n15_q95, hsc_q95, ifsa) %>% 
  rename(spawn = hsc_q95,
         n15 = n15_q95) %>% 
  mutate(ifsa_sc = (ifsa-mean(ifsa,na.rm = T))/sd(ifsa, na.rm = T),
         spawn_sc = (spawn-mean(spawn, na.rm = T))/sd(spawn, na.rm = T),
         n15_sc = (n15-mean(n15,na.rm = T))/sd(n15, na.rm = T))
scaled.covs


# red knot ----
rekn <- weights %>% 
  filter(species == "REKN" & day < 38) %>% 
  select(-catchID) %>% 
  mutate(catch = NA)

rekn %>% 
  group_by(year, day) %>%
  count() %>%
  as.data.frame() -> catch.df

catch.df$id = c(1:nrow(catch.df))

for(i in 1:nrow(rekn)){
  rekn$catch[i] = catch.df$id[catch.df$year == rekn$year[i] &
                                catch.df$day == rekn$day[i]]
}

rekn.covs <- scaled.covs %>% 
  filter(species == "REKN") %>% 
  mutate(n15_sc = ifelse(year %in% c(1997, 2017), 0, n15_sc))


rekn.dat = list(w = rekn$weight,
                d = rekn$day,
                c = rekn$catch,
                n.catches = length(unique(rekn$catch)),
                n.ind = nrow(rekn),
                n.days = 37,
                year = rekn$year - min(rekn$year) + 1,
                n.years = length(unique(rekn$year)),
                ifsa_sc = rekn.covs$ifsa_sc,
                spawn_sc = rekn.covs$spawn_sc,
                n15_sc = rekn.covs$n15_sc)

# ruddy turnstone ----
rutu <- weights %>% 
  filter(species == "RUTU" & day < 38) %>% 
  select(-catchID) %>% 
  mutate(catch = NA)

# drop first catch of 2014-- average 65g- very low, seem not possible
rutu %>% 
  filter(!(year == 2014 & day == 10)) -> rutu

rutu %>% 
  group_by(year, day) %>%
  count() %>%
  as.data.frame() -> catch.df

catch.df$id = c(1:nrow(catch.df))

for(i in 1:nrow(rutu)){
  rutu$catch[i] = catch.df$id[catch.df$year == rutu$year[i] &
                                catch.df$day == rutu$day[i]]
}

rutu.covs <- scaled.covs %>% 
  filter(species == "RUTU") %>% 
  mutate(n15_sc = ifelse(year %in% c(1997, 2017), 0, n15_sc))


rutu.dat = list(w = rutu$weight,
                d = rutu$day,
                c = rutu$catch,
                n.catches = length(unique(rutu$catch)),
                n.ind = nrow(rutu),
                n.days = 37,
                year = rutu$year - min(rutu$year) + 1,
                n.years = length(unique(rutu$year)),
                ifsa_sc = rutu.covs$ifsa_sc,
                spawn_sc = rutu.covs$spawn_sc,
                n15_sc = rutu.covs$n15_sc)


# mcmc setup ----

nc = 3
nt = 1
ni = 100000
nb = 50000
na = 10000

params = c("wmax", "p", "b", "wdiff", "wmin", "mu.sig", "sig.sig", "sig.catch",
           "wdiff.mu", "wmin.mu", "p.mu", "b.mu", "wmax.mu", 
           "sigma1", "sigma2", "sigma3", "sigma4", "bayesp", "ind", "beta", "alpha",
           "ifsa_sc", "spawn_sc", "ifsa.sig", "ifsa.mean", "spawn.sig",
           "fit.new", "fit")

inits = function(){list(wmax.mu = runif(1, 130, 250),
                        wmin.mu = runif(1, 80, 130),
                        p.mu = runif(1, 5, 24),
                        b.mu = runif(1, 1, 15),
                        eps1 = rnorm(22),
                        eps2 = rnorm(22),
                        eps3 = rnorm(22),
                        eps4 = rnorm(22))
}
  

# fit models ----
success = FALSE
while(!success){
  s <- try(reknmod <- jags(rekn.dat, inits, params, "scripts/jags/mass-gain_rekn.jags",
                           n.chains = nc, n.iter = ni, n.adapt = na,
                           n.burnin = nb, n.thin = nt, parallel = T))
  success <- class(s)!='try-error'
  pushover("REKN weight gain model error, rerunning")
}

while(length(which(reknmod$summary[,8] > 1.1)) > 0){
  pushover("REKN weight gain model updating")
  reknmod = update(reknmod, n.iter = 15000, parameters.to.save = params)
}

saveRDS(reknmod, file ="rekn_mass-gain-2.rds")
pushover("REKN weight gain model finished")



success = FALSE
while(!success){
  s <- try(rutumod <- jags(rutu.dat, inits, params, "scripts/jags/mass-gain_rutu.jags",
                           n.chains = nc, n.iter = ni, n.adapt = na,
                           n.burnin = nb, n.thin = nt, parallel = T))
  success <- class(s)!='try-error'
  pushover("RUTU weight gain model error, rerunning")
}

while(length(which(rutumod$summary[,8] > 1.1)) > 0){
  pushover("RUTU weight gain model updating")
  rutumod = update(rutumod, n.iter = 15000, parameters.to.save = params)
}

saveRDS(rutumod, file = "rutu_mass-gain-2.rds")
pushover("RUTU weight gain model finished")


# convergence and diagnostics ----
reknmod = readRDS("rekn_mass-gain-2.rds")
rutumod = readRDS("rutu_mass-gain-2.rds")


mean(reknmod$sims.list$bayesp)

hist(reknmod$summary[,8])

par(mfrow = c(5,5), mar = c(1,1,1,1))
for(i in 1:22){
  hist(reknmod$sims.list$wmax[,i], col = "gray", breaks = 100, main = "")
}

par(mfrow = c(5,5), mar = c(1,1,1,1))
for(i in 1:22){
  hist(reknmod$sims.list$wmin[,i], col = "gray", breaks = 100, main = "")
}

par(mfrow = c(5,5), mar = c(1,1,1,1))
for(i in 1:22){
  hist(reknmod$sims.list$p[,i], col = "gray", breaks = 100, main = "")
}

par(mfrow = c(5,5), mar = c(1,1,1,1))
for(i in 1:22){
  hist(reknmod$sims.list$b[,i], col = "gray", breaks = 100, main = "")
}

mean(rutumod$sims.list$bayesp)

hist(rutumod$summary[,8])

par(mfrow = c(5,5), mar = c(1,1,1,1))
for(i in 1:22){
  hist(rutumod$sims.list$wmax[,i], col = "gray", breaks = 100, main = "")
}

par(mfrow = c(5,5), mar = c(1,1,1,1))
for(i in 1:22){
  hist(rutumod$sims.list$wmin[,i], col = "gray", breaks = 100, main = "")
}

par(mfrow = c(5,5), mar = c(1,1,1,1))
for(i in 1:22){
  hist(rutumod$sims.list$p[,i], col = "gray", breaks = 100, main = "")
}

par(mfrow = c(5,5), mar = c(1,1,1,1))
for(i in 1:22){
  hist(rutumod$sims.list$b[,i], col = "gray", breaks = 100, main = "")
}




