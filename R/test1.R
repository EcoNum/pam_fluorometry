
source("R/rlc_function.R")

# Bouture 7 acropora ####
file <- "data/data_csv/2018-07-20-bt7-rlc.csv" # file localisation
y <- import_rlc(file) # importation
nb <- 14 # nb of aio
y$bouture <- 7
y$genre <- "Acropora"
y$aqua <- "A2"



y <- rename_rlc(data = y, old_var_all = "F[:digit:]", new_var = "f", nb_aio = nb)
y <- rename_rlc(data = y, old_var_all = "Fm'[:digit:]", new_var = "fm", nb_aio = nb)
y <- rename_rlc(data = y, old_var_all = "Y[:punct:]II[:punct:][:digit:]",
                new_var = "y", nb_aio = nb)


y$y_mean <- calc_rlc(data = y, var_name = "y[:digit:]", fun = mean)
y$y_sd <- calc_rlc(data = y, var_name = "y[:digit:]", fun = sd)
y$aio <- nb


y <- compute_retr_rlc(data = y, yield = "y[:digit:]", par = "par", nb_aio = 14)

test <- RLC(PAR = y$par, rETR = y$retr1, model = "Webb")

t <- c("date", "num", "par", "y_mean", "y_sd", "aio", "bouture", "genre", "aqua")
y1 <- select(y, t)

# bouture 8 ####
#
file <- "data/test_ip_ge_02/2018-07-19-bt8-y.csv" # file localisation
nb <- 14
y$bouture <- 8
y$genre <- "Acropora"
y$aqua <- "A2"
#
y <- import_rlc(file)
y <- rename_rlc(data = y, old_var_all = "F[:digit:]", new_var = "f", nb_aio = nb)
y <- rename_rlc(data = y, old_var_all = "Fm'[:digit:]", new_var = "fm", nb_aio = nb)
y <- rename_rlc(data = y, old_var_all = "Y[:punct:]II[:punct:][:digit:]", new_var = "y", nb_aio = nb)

y$y_mean <- calc_rlc(data = y, var_name = "y[:digit:]", fun = mean)
y$y_sd <- calc_rlc(data = y, var_name = "y[:digit:]", fun = sd)
y$aio <- nb
y$bouture <- 8
y$genre <- "Acropora"
t <- c("date", "num", "par", "y_mean", "y_sd", "aio", "bouture", "genre", "aqua")
y2 <- select(y, t)

# bouture 9
file <- "data/test_ip_ge_02/2018-07-19-bt9-y.csv" # file localisation
nb <- 16
y$bouture <- 9
y$genre <- "Acropora"
y$aqua <- "A2"

y <- import_rlc(file)
y <- rename_rlc(data = y, old_var_all = "F[:digit:]", new_var = "f", nb_aio = nb)
y <- rename_rlc(data = y, old_var_all = "Fm'[:digit:]", new_var = "fm", nb_aio = nb)
y <- rename_rlc(data = y, old_var_all = "Y[:punct:]II[:punct:][:digit:]", new_var = "y", nb_aio = nb)

y$y_mean <- calc_rlc(data = y, var_name = "y[:digit:]", fun = mean)
y$y_sd <- calc_rlc(data = y, var_name = "y[:digit:]", fun = sd)
y$aio <- nb
y$bouture <- 9
y$genre <- "Acropora"

t <- c("date", "num", "par", "y_mean", "y_sd", "aio", "bouture", "genre", "aqua")
y3 <- select(y, t)

yield <- bind_rows(y1, y2, y3)
rm(y, y1, y2, y3, file, nb, t)

library(chart)
chart(data = yield, formula = y_mean ~ as.factor(bouture)) +
  geom_point() +
  geom_errorbar(aes(ymin = y_mean - y_sd, ymax = y_mean + y_sd), width = 0.5 ) +
  labs( y = "Fluorescence maximale") +
  scale_y_continuous(limits = c(0,1))
