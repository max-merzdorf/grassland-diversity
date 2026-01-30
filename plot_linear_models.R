plot(x = c(planet_metrics$date[1], planet_metrics$date[3]),
     y = c(planet_metrics$B1_cv[1], planet_metrics$B1_cv[3]))

# more months
df <- data.frame(x = c(planet_metrics$date[1], planet_metrics$date[2],
                       planet_metrics$date[3], planet_metrics$date[4]),
                 y = c(planet_metrics$B1_cv[1], planet_metrics$B1_cv[2],
                       planet_metrics$B1_cv[3], planet_metrics$B1_cv[4]))
plot(x = df$x, y = df$y)
reg <- lm(y ~ x, data = df)
abline(reg = reg, col = "red")

# env params
de <- 