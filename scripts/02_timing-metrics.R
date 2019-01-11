# arrival timing and proportion of spawn available

# arrival timing ----
entries = read_csv("output/entry_probs.csv")

entries %>% 
  ggplot(aes(x = occ, y = cum_prob, col = as.character(year))) +
  geom_smooth(se = F, alpha = 0.5) + 
  facet_wrap(~species) +
  theme(legend.position = "null")

# find occasions with 50% and 95% cumulative arrival probability
arr.sum <- entries %>%
  rename(cum.prop = cum_prob) %>% 
  group_by(species, year) %>% 
  summarize(q50 = occ[which.min(abs(cum.prop - 0.5))],
            q95 = occ[which.min(abs(cum.prop - 0.95))]) %>% 
  ungroup()
arr.sum

# assign years prior to 2005 the median arrival times
arr.all <- arr.sum %>% 
  mutate(year = c(2005:2018)[year]) %>% 
  full_join(tibble(year = rep(c(1997:2004), 2),
                   species = rep(c("REKN", "RUTU"), each = length(c(1997:2004))),
                   q50 = rep(c(median(arr.sum$q50[arr.sum$species == "REKN"]),
                               median(arr.sum$q50[arr.sum$species == "RUTU"])),
                             each =  length(c(1997:2004))),
                   q95 = rep(c(median(arr.sum$q95[arr.sum$species == "REKN"]),
                               median(arr.sum$q95[arr.sum$species == "RUTU"])),
                             each =  length(c(1997:2004))))) %>% 
  arrange(year)
arr.all  

write.csv(arr.all, "output/arrival_days.csv", row.names = F)

# horseshoe crab timing ----
hsc = read_csv("output/hsc_smooth.csv")
ifsa_de = read_csv("output/ifsa_de.csv")

hsc %>% 
  mutate(species = "hsc") -> hsc


find_prop_avail = function(year, species, quant = c(50, 95)){

  if(quant == 50){
    occ = arr.all$q50[arr.all$year == year & arr.all$species == species]
  }
  
  if(quant == 95){
    occ = arr.all$q95[arr.all$year == year & arr.all$species == species]
  }
  
  day = 6+occ*3
  hsc$predicted[hsc$year == year & hsc$day == day]
}

hsc_avail <- as.tibble(expand.grid(year = c(2003:2017),
                                   species = c("REKN", "RUTU"))) %>% 
                         mutate(q50 = map2_dbl(year, species, find_prop_avail, quant = 50),
                                q95 = map2_dbl(year, species, find_prop_avail, quant = 95))
hsc_avail

hsc_avail %>% 
  gather(quant, prop, 3:4) %>% 
  ggplot(aes(x = prop)) +
  geom_histogram() +
  facet_grid(species~quant)

# join with statewide ifsa ests for each year
all_hsc <- hsc_avail %>% 
  full_join(ifsa_de, by = c("year" = "Year"))
all_hsc

write.csv(all_hsc, "output/all_hsc.csv", row.names = F)

# plot all
hsc %>% 
  full_join(ifsa_de, by = c("year" = "Year")) -> hsc

entries %>% 
  select(year, occ, species, cum_prob) %>% 
  mutate(year = c(2005:2018)[year],
         day = 6+occ*3) %>% 
  ggplot() +
  geom_ribbon(data = hsc, ymin = 0, aes(x = day, ymax = predicted, fill = tot.sqm), 
              alpha = 0.9) +
  geom_line(data = hsc, aes(x = day, y = predicted), col = "black") +
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
  scale_fill_viridis(name = expression(paste("Statewide spawning abundance (females per"~m^2~")")),
                     option = "B",
                     begin = 0, end = 0.8,
                     breaks = c(0.5, 0.75))

