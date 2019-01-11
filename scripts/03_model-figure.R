
require(tidyverse)
require(cowplot)

wmax = 160
wmin = 105
bs = c(1,2,5,10,20)
ps = c(5,10,15,20,25)
meanb = 7
meanp = 18
day = c(1:34)

# define model expressions
mod.mu = expression(wmax + ((wmin - wmax)/
                              (1 + (day/meanp)^meanb)))
mod.p = expression(wmax + ((wmin - wmax)/
                             (1 + (day/p)^meanb)))
mod.b = expression(wmax + ((wmin - wmax)/
                             (1 + (day/meanp)^b)))
plot(eval(mod.mu), type = "l")



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
  viridis::scale_color_viridis(discrete = T, name = "") 


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
  viridis::scale_color_viridis(discrete = T, name = "") 


df = tibble(day = day,
            results = eval(mod.mu))

ggplot(df, aes(x = day, y = results)) +
  geom_hline(yintercept = wmin, lwd = 2, col = "gray10", alpha = 0.6) +
  geom_hline(yintercept = wmax, lwd = 2, col = "gray10", alpha = 0.6) +
  geom_vline(xintercept = meanp, lwd = 2, col = "gray10", alpha = 0.6) +
  geom_abline(intercept = 6.5, slope = meanb, lwd = 2, col = "gray10", alpha = 0.6) +
  geom_vline(xintercept = 12, lwd = 2, lty = 2, col = "gray30", alpha = 0.6) +
  geom_vline(xintercept = 25, lwd = 2, lty = 2, col = "gray30", alpha = 0.6) +
  geom_abline(intercept = 60, slope = 4, lwd = 2, lty = 2, col = "gray30", alpha = 0.6) +
  geom_line(lwd = 3) +
  xlab("Day in May") +
  ylab("Average mass") +
  ylim(100, 170) +
  annotate("text", label = "Minimum average mass", x = 4, y = wmin+3, size = 4.5) +
  annotate("text", label = "Maximum average mass", x = 4, y = wmax+2, size = 4.5) +
  annotate("text", label = "Inflection\npoint", x = meanp+2, y = 115, size = 4.5) +
  annotate("text", label = "Maximum\nrate", x = 21, y = 168, size = 4.5) +
  annotate("text", label = "Start day", x = 10, y = 130, size = 4.5) +
  annotate("text", label = "Stop day", x = 27, y = 130, size = 4.5) +
  annotate("text", label = "Average\nrate", x = 30, y = 168, size = 4.5) +
  scale_linetype_manual(values = c(A = 1, B = 2))



