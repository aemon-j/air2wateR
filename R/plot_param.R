#'@title Plot the parameters from the calibration of the air2water model
#'
#'@description
#'This plots the results of the calibration for the model.
#'
#'@param sim_folder filepath; the directory where simulation files are contained
#'
#'@keywords methods
#'@author
#'Sebastiano Piccoloroaz, Tadhg Moore
#'@examples
#'\dontrun{
#'sim_folder <- system.file('extdata', package = 'air2wateR')
#'gen_param(sim_folder = sim_folder, mean_depth = 147)
#'run_air2water(sim_folder = sim_folder)
#'plot_param(sim_folder = sim_folder)
#'}
#'@importFrom reshape2 melt
#'@import ggplot2
#'@export
plot_param <- function(sim_folder) {
  wd <- getwd()
  setwd(sim_folder)
  on.exit({
    setwd(wd)
  })

  input_data <- read.table('input.txt', stringsAsFactors = FALSE)
  folder <- input_data[1,1]
  IDair <- input_data[2,1]
  IDwat <- input_data[3,1]
  dt <- input_data[5,1]
  version <- input_data[6,1]
  index <- input_data[8,1]
  runmode  <- input_data[10,1]
  toll <- 2;  # minimum (maximum if index = RMS) efficiency index used to make the dotty plots
  path_output <- paste0('output_',version)

  file_name <- file.path(folder, path_output,
                         paste0('1_', runmode, '_', index, '_',
                                IDair, '_', IDwat, '_c_', dt, '.out'))
  tmp <- read.delim(file_name,sep = "\t",header=T);       # efficiency in calibration and validation
  eff_cal <- tmp[1,1]
  eff_val <- tmp[2,1]

  ##################
  # 1. Dotty plots #
  ##################
  file_name <- file.path(folder, path_output, paste0('00_',runmode,'_',
                                                     index,'_',IDair,'_',IDwat,
                                                     '_c_',dt,'.out'))
  nr <- read.table(file_name);    # Number of rows in the binary file containing the parameter sets
  nr <- as.numeric(nr)
  file_name <- file.path(folder, paste0('output_', version, '/0_', runmode, '_',
                                        index,'_',IDair,'_',IDwat,'_c_',dt,'.out'))
  file_con <- file(file_name, "rb")
  ntot <- (nr*9)          # nr*9 = total number of elements stored in the binary file
  parset <- readBin(file_con, double(), n = ntot)
  close(file_con)
  parset <- matrix(parset, byrow = TRUE, ncol = 9)
  parset <- as.data.frame(parset)
  colnames(parset) <- c("p1", "p2", "p3", "p4", "p5", "p6", "p7", "p8", index)
  if (index == 'RMS') {
    parset[,9] <- -parset[,9] # when RMS is used as efficiency index, air2water works with -RMS
    I <- which(parset[,9] <= toll)
    parset <- parset[I, ]
    I_best <- which.min(parset[, 9]); eff_best <- parset[I_best,9]
    par_best <- parset[I_best, 1:9]
    plot_limits = c(eff_best * 0.9, toll);
    unit_meas_eff <- '\u00B0C'
  } else {
    I <- which(parset[,9] >= toll)
    parset = parset[I, ];
    I_best = which.max(parset[, 9]); eff_best <- parset[I_best, 9]
    par_best <- parset[I_best, 1:9]
    plot_limits = c(toll, eff_best * 1.1);
    unit_meas_eff <- ''
  }
  Isub <- sample(I, min(1000, length(I))) # subsample the cloud to avoid computational burden
  parset <- parset[Isub, ]
  ref <- colnames(parset)[ncol(parset)]
  mlt <- reshape2::melt(parset, id.vars = ncol(parset))
  bst <- reshape2::melt(par_best, id.vars = ncol(par_best))

  red <- '#d53900'

  p1 <- ggplot(mlt) +
    geom_point(aes_string('value', ref)) +
    geom_point(data = bst, aes_string('value', ref), colour = red) +
    facet_wrap(~variable, scales = 'free_x')
  return(p1)

}
