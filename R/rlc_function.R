## RLC optimisation de l'analysis
# création d'une fonction

# Importation
import_rlc <- function(file){
  dat <- readr::read_delim(file,
    ";", escape_double = FALSE, trim_ws = TRUE)
  dat <- select(dat, - starts_with("X"))
  dat$Time <- as.character(dat$Time)
  dat$Date <- paste(dat$Date, dat$Time, sep = " ")
  dat <- select(dat, -Time)
  dat$Date <- as.POSIXct(dat$Date, format = "%d.%m.%y %H:%M:%S")
  dat <- rename(dat, date = Date, num = No., par = PAR)
  attr(dat, "spec") <- NULL
  dat
}

#file <- "data/test_ip_ge/2018-07-17-bt9.csv"
#rlc <- import_rlc(file = "data/test_ip_ge/2018-07-17-bt9.csv")

# compute retr

compute_retr_rlc <- function(data, yield, par, nb_aio){
  t <- names(data)
  dat <- select(data, c(par, str_subset(t, yield)))
  dat <- as.data.frame(dat)
  dat <- dat[, c(1:nb_aio+1) ] *  dat[,1]
  names(dat) <- paste(rep(x = "retr", times = nb_aio), c(1:nb_aio),sep = "")
  data <- bind_cols(data, dat)
  data
}

#rlc <- compute_retr_rlc(data = rlc, yield = "Y[:punct:]II[:punct:][:digit:]", par = "par", nb_aio = 10)


rename_rlc <- function(data, old_var_all, new_var, nb_aio){
  t <- names(data)
  dat <- select(data, c(str_subset(t, old_var_all)))
  data <- select(data, -c(str_subset(t, old_var_all)))
  t <- paste(rep(x = new_var, times = nb_aio), c(1:nb_aio),sep = "")
  names(dat) <- t
  data <- bind_cols(data, dat)
}


#rlc <- rename_rlc(data = rlc, old_var_all = "F[:digit:]", new_var = "f", nb_aio = 10)
#rlc <- rename_rlc(data = rlc, old_var_all = "Fm'[:digit:]", new_var = "fm", nb_aio = 10)
#rlc <- rename_rlc(data = rlc, old_var_all = "Y[:punct:]II[:punct:][:digit:]", new_var = "y", nb_aio = 10)
#rlc <- rename_rlc(data = rlc, old_var_all = "Y[:punct:]NPQ[:punct:][:digit:]", new_var = "y_npq", nb_aio = 10)
#rlc <- rename_rlc(data = rlc, old_var_all = "Y[:punct:]NO[:punct:][:digit:]", new_var = "y_no", nb_aio = 10)


# mean and sd by row

calc_rlc <- function(data, var_name, fun = mean){
  names(data) -> t
  dat <- select(data,  c(str_subset(t, var_name)))
  m <- apply(X = dat, MARGIN = 1, FUN = fun)
}
#rlc$f_mean <- calc_rlc(data = rlc, var_name = "f[:digit:]", fun = mean)
#rlc$f_sd <- calc_rlc(data = rlc, var_name = "f[:digit:]", fun = sd)


# I try to use the function writing by J.R or Ph.G., the function is writing as a function for a package.
#

#' Calculation of Rapid Light Curves (RLC)
#'
#' @param PAR Photosynthetically Active Radiations in µmol photos/m^2/s
#' @param ETR Electron Transfert rate in µmol electrons/m^2/s
#' @param model model used to fit the RLC: "Webb", "JP", or "EP" for asymptotic models, or "PGH" for a model with photoinhibition
#' @param start starting values to use for the model's parameters alpha, beta and max (beta only used by the "PGH" model)
#' @param metadata a single-row data frame with metadata to be added to the results
#'
#' @return a 'RLC' S3 object with print(), summary(), plot() & lines() methods available
#' @export
#'
#' @examples
RLC <- function(PAR, rETR, model = "Webb",
                start = list(alpha = NA, beta = 0, max = NA), metadata = NULL) {

  model = match.arg(model, c("Webb", "JP", "EP", "PGH"))

  # If there is a zero PAR, eliminate it now
  if (PAR[1] == 0 && PAR[2] == 0) {
    PAR <- PAR[-(1:2)]
    rETR <- rETR[-(1:2)]
  }
  # Again for another one
  if (PAR[1] == 0) {
    PAR <- PAR[-1]
    rETR <- rETR[-1]
  }

  dat <- data.frame(PAR = PAR, rETR = rETR)

  fun <- switch(model,
                Webb = function(x, alpha, max)
                  max * (1 - exp(-x * alpha/max)),
                JP   = function(x, alpha, max)
                  max * tanh(x/alpha),
                EP   = function(x, alpha, max)
                  max * ((alpha * x) / (max^2 + (alpha * x)^2)^(1/2)),
                PGH  = function(x, alpha, beta, max)
                  max * (1 - exp(-x * alpha/max)) * (exp(-beta * x/max))
  )
  # For last model:
  # rETRmax = max * alpha/(alpha + beta) * (beta/(alpha + beta))^(beta/alpha)

  if (model != "PGH") {
    start <- start[names(start) != "beta"]
    reg <- nls(rETR ~ fun(PAR, alpha, max), data = dat, start = start)
  } else {
    reg <- nls(rETR ~ fun(PAR, alpha, beta, max), data = dat, start = start)
  }

  res <- coef(reg)

  if (model == "JP")
    res[["alpha"]] <- 1 / res[["alpha"]] * res[["max"]]

  if (model == "PGH") {
    alpha <- res[["alpha"]]
    beta <- res[["beta"]]
    max <- res[["max"]]
    res[["rETRmax"]] <- max * (alpha/(alpha + beta)) * (beta/(alpha + beta))^(beta/alpha)
  } else res[["rETRmax"]] <- res[["max"]]

  res[["ik"]] <- res[["rETRmax"]] / res[["alpha"]]

  res <- as.data.frame(as.list(res))
  if (!is.null(metadata)) {
    res <- cbind(metadata, res)
  }
  attr(res, "model") <- model
  attr(res, "data") <- dat
  attr(res, "regression") <- reg
  attr(res, "function") <- fun
  class(res) <- c("RLC", "data.frame")
  res
}

print.RLC <- function(x, ...) {
  cat("A Rapid Light Curve (RLC) from PAM measurements\n")
  cat("model:", attr(x, "model"), "\n")
  print(as.data.frame(unclass(x)))
  invisible(x)
}

summary.RLC <- function(object, ...) {
  summary(attr(object, "regression"))
}

plot.RLC <- function(x, y, col = "red", pcol = "black", main = "Rapid Light Curve",
                     ylim = c(0, max(x$rETRmax, attr(x, "data")$rETR)), ...) {
  dat <- attr(x, "data")
  plot(dat$PAR, dat$rETR, xlab = expression(paste("PAR (", µmol*phantom(.)*photons %.% m^{-2} %.% s^{-1}, ")")), ylab = expression(paste("rETR (", µmol*phantom(.)*electrons %.% m^{-2} %.% s^{-1}, ")")),
       col = pcol, main = main, ylim = ylim, ...)
  newX <- list(PAR = seq(0, max(dat$PAR), length.out = 100))
  lines(newX$PAR, predict(attr(x, "regression"), newdata = newX), col = col)
  # Annotate the plot
  grid()
  abline(h = x[["rETRmax"]], col = col, lty = 2)
  #abline(a = 0, b = x[["alpha"]], col = col, lty = 2)
  #abline(v = x[["ik"]], col = col, lty = 2)
  #invisible(dat)
}

lines.RLC <- function(x, col = "blue", ...) {
  dat <- attr(x, "data")
  newX <- list(PAR = seq(0, max(dat$PAR), length.out = 100))
  lines(newX$PAR, predict(attr(x, "regression"), newdata = newX), col = col)
  abline(h = x[["rETRmax"]], col = col, lty = 2)
  abline(a = 0, b = x[["alpha"]], col = col, lty = 2)
  abline(v = x[["ik"]], col = col, lty = 2)
  invisible(dat)
}
