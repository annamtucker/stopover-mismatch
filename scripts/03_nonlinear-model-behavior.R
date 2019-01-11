# explore mass gain model behavior


wmax = 170
wmin = 100
bs = c(1,2,5,10,20)
ps = c(5,10,15,20,25)
meanb = mean(bs)
meanp = mean(ps)
day = c(1:34)

# define model expressions
mod.mu = expression(wmax + ((wmin - wmax)/
                              (1 + (day/meanp)^meanb)))
mod.p = expression(wmax + ((wmin - wmax)/
                             (1 + (day/p)^meanb)))
mod.b = expression(wmax + ((wmin - wmax)/
                             (1 + (day/meanp)^b)))
plot(eval(mod.mu), type = "l")


# How do the values of p and b influence the shape of the curve?  

plot.p = data.frame()
for(i in 1:length(ps)){
  p = ps[i]
  df = data.frame(Day = c(1:34),
                  pval = rep(p, 34),
                  results = eval(mod.p))
  plot.p = rbind(plot.p, df)
}

plot.p %>%
  mutate(plab = paste("p =", pval)) %>%
  arrange(plab) %>%
  ggplot(aes(x = Day, y = results, col = plab)) +
  geom_line(lwd = 1.5) +
  ylab("Predicted average weight (g)") +
  viridis::scale_color_viridis(discrete = T, name = "") +
  theme(axis.text = element_text(size = 12),
        axis.title = element_text(size = 14),
        legend.text = element_text(size = 14))


# The parameter p influences the inflection point
# with smaller values resulting in an earlier inflection point.  

# How does the value of b influence the shape of the curve?  
  
plot.b = data.frame()
for(i in 1:length(bs)){
  b = bs[i]
  df = data.frame(Day = c(1:34),
                  bval = rep(b, 34), 
                  results = eval(mod.b))
  plot.b = rbind(plot.b, df)
}

plot.b %>%
  arrange(bval) %>%
  mutate(blab = paste("b =", bval),
         blab = factor(blab, 
                       levels = unique(blab[order(bval)]))) %>%
  ggplot(aes(x = Day, y = results, col = blab)) +
  geom_line(lwd = 1.5) +
  ylab("Predicted average weight (g)") +
  viridis::scale_color_viridis(discrete = T, name = "") +
  theme(axis.text = element_text(size = 12),
        axis.title = element_text(size = 14),
        legend.text = element_text(size = 14),
        legend.position = c(0.9,0.2))


The parameter b influences the slope and inflection point of the curve. Smaller values for b are associated with shallower slopes and more convex shape, while larger values of b are associated with a sharp dramatic increase.  
