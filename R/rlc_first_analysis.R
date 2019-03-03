# Analyse for RLC

SciViews::R
library(stringr)
library(flow)
library(chart)

## First example bt9 is a RLC

file <- "data/test_ip_ge/2018-07-17-bt9.csv" # position
nb <- 10 # number of AIO

# Importation
rlc <- read_delim(file,
  ";", escape_double = FALSE, trim_ws = TRUE)
#remove last variable that is a unwanted variable
rlc <- select(rlc, - starts_with("X"))


# combine date and time in date
# this time is GMT time
rlc$Time <- as.character(rlc$Time)
rlc$Date <- paste(rlc$Date, rlc$Time, sep = " ")
rlc <- select(rlc, -Time)
rlc$Date <- as.POSIXct(rlc$Date, format = "%d.%m.%y %H:%M:%S")



#names(rlc)
t <- names(rlc)
#rename variables
f <- paste(rep(x = "f", times = nb), c(1:nb),sep = "")
fm <- paste(rep(x = "fm", times = nb), c(1:nb),sep = "")
y <- paste(rep(x = "y", times = nb), c(1:nb),sep = "")
ynpq <- paste(rep(x = "y_npq", times = nb), c(1:nb),sep = "")
yno <- paste(rep(x = "y_no", times = nb), c(1:nb),sep = "")

names(rlc) <- c("date", "no", "par",f, fm, y, ynpq, yno )
rm(t, f,fm,y,yno,ynpq)

# regular expression for extract interesting column
t <- names(rlc)

# compute retr
name <- "y[:digit:]"
rlc %>.%
  select(., c(par, str_subset(t, name))) -> etr
etr <- etr[2:11]*etr$par
names(etr) <- paste(rep(x = "retr", times = nb), c(1:nb),sep = "")

rlc <- bind_cols(rlc, etr)
rm(etr, name)

# regular expression for extract interesting column
t <- names(rlc)

# for f ####
name <- "f[:digit:]"
rlc %>.%
  select(., str_subset(t, name)) %>.%
  apply(., MARGIN = 1, FUN = mean) -> m

rlc %>.%
  select(., str_subset(t, name)) %>.%
  apply(., MARGIN = 1, FUN = sd) -> s
rlc$f_sd <- s


# for fm ####
name <- "fm[:digit:]"
rlc %>.%
  select(., str_subset(t, name)) %>.%
  apply(., MARGIN = 1, FUN = mean) -> m
rlc$fm_mean <- m

rlc %>.%
  select(., str_subset(t, name)) %>.%
  apply(., MARGIN = 1, FUN = sd) -> s
rlc$fm_sd <- s

# for y ######
name <- "y[:digit:]"
rlc %>.%
  select(., str_subset(t, name)) %>.%
  apply(., MARGIN = 1, FUN = mean) -> m
rlc$y_mean <- m

rlc %>.%
  select(., str_subset(t, name)) %>.%
  apply(., MARGIN = 1, FUN = sd) -> s
rlc$y_sd <- s

# for y_npq1 ####
name <- "y_npq[:digit:]"
rlc %>.%
  select(., str_subset(t, name)) %>.%
  apply(., MARGIN = 1, FUN = mean) -> m
rlc$y_npq_mean <- m

rlc %>.%
  select(., str_subset(t, name)) %>.%
  apply(., MARGIN = 1, FUN = sd) -> s
rlc$y_npq_sd <- s

# for y_no ####
name <- "y_no[:digit:]"
rlc %>.%
  select(., str_subset(t, name)) %>.%
  apply(., MARGIN = 1, FUN = mean) -> m
rlc$y_no_mean <- m

rlc %>.%
  select(., str_subset(t, name)) %>.%
  apply(., MARGIN = 1, FUN = sd) -> s
rlc$y_no_sd <- s

# for etr ####
name <- "retr[:digit:]"
rlc %>.%
  select(., str_subset(t, name)) %>.%
  apply(., MARGIN = 1, FUN = mean) -> m
rlc$retr_mean <- m

rlc %>.%
  select(., str_subset(t, name)) %>.%
  apply(., MARGIN = 1, FUN = sd) -> s
rlc$retr_sd <- s

attr(rlc, "spec") <- NULL


### Rendement de photosynthese
chart(rlc,formula = y_mean ~ par)+
  geom_point() +
  geom_line() +
  geom_errorbar(aes(ymin= y_mean-y_sd, ymax=y_mean+y_sd),
    width=.2, position=position_dodge(0.05))

### Visualiser  les rendements rendement #################################################
chart(rlc,formula = y_mean ~ par) +
  geom_point() +
  geom_line() +
  geom_errorbar(aes(ymin= y_mean-y_sd, ymax=y_mean+y_sd),
    width=.2, position=position_dodge(0.05))+
  geom_point(f_aes(y_no_mean ~ par), color = "red") +
  geom_line(f_aes(y_no_mean ~ par), color = "red") +
  geom_errorbar(aes(ymin= y_no_mean-y_no_sd, ymax=y_no_mean+y_no_sd),
    width=.2, position=position_dodge(0.05), color = "red")+
  geom_point(f_aes(y_npq_mean ~ par), color = "blue") +
  geom_line(f_aes(y_npq_mean ~ par), color = "blue") +
  geom_errorbar(aes(ymin= y_npq_mean-y_npq_sd, ymax=y_npq_mean+y_npq_sd),
    width=.2, position=position_dodge(0.05), color = "blue")


### Visualiser  les retr #################################################
chart(rlc,formula = retr_mean ~ par)+
  geom_point() +
  geom_line() +
  geom_errorbar(aes(ymin= retr_mean- retr_sd, ymax=retr_mean+ retr_sd),
    width=.2, position=position_dodge(0.05))
