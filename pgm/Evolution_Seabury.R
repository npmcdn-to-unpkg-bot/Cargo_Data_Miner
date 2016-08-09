######### EVOLUTION SEABURY #########
SEA <- read.csv(file.path(DATA, 'evo_seabury.csv'), header = FALSE)
colnames(SEA) <- c('Year','Air.weight','Air.value','Surface.weight','Surface.value')


ggplot(SEA, aes(Year), ylab = 'Weight') + 
  geom_line(aes(y = Air.weight, colour = "Air.weight"),cex = 1) + 
  # geom_line(aes(y = Air.value, colour = "Air.value"),cex = 1) +
  # geom_line(aes(y = Surface.weight, colour = "Surface.weight"), cex = 1) +
  # geom_line(aes(y = Surface.value, colour = "Surface.value"), cex = 1) +

  
  ylab('Weight') + 
  xlab('Date') +
  labs(title = 'Weight evolution')+
  theme_bw()
