#'@title Extract lake surface temperature
#'
#'@description
#'This extracts the modelled temperature and returns a dataframe
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
#'out <- get_outputs(sim_folder = sim_folder)
#'out$wml <- out$C * 147 # estimate well-mixed layer as a function of mean depth
#'}
#'
#'@export
get_outputs <- function(sim_folder) {
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

  #########################
  # 2. Calibration period #
  #########################
  # Temperature
  file_name <- file.path(folder, path_output, paste0('2_', runmode, '_', index,
                                             '_', IDair, '_', IDwat, '_cc_', dt,'.out'))
  C <- read.table(file_name)
  colnames(C) = c("year","month","day","AT","LSWT_obs","LSWT_sim")
  I <- which(C[,1]==-999)
  C <- C[-I,]                 # Remove the 1st year (warm up)
  C[C==-999] <- 'NA'          # Replace -999 with NA
  C$datetime <- ISOdate(C$year,C$month,C$day )
  C <- C[, c("datetime", "AT", "LSWT_obs", "LSWT_sim")]

  # Well mixed layer
  file_name <- file.path(folder, path_output, paste0('4_',runmode,'_',index,
                                                     '_',IDair,'_',IDwat,'_cc_',dt,'.out'))
  C_wml <- read.table(file_name)
  colnames(C_wml) = c("delta")
  C_wml <- C_wml[-I,]
  C$C <- C_wml

  C$status <- "calibration"


  #########################
  # 3. Validation period #
  #########################
  file_name <- file.path(folder, path_output, paste0('3_',runmode,'_',index,'_',
                                                     IDair,'_',IDwat,'_cv_',dt,'.out'))
  V <- read.table(file_name)
  colnames(V) = c("year","month","day","AT","LSWT_obs","LSWT_sim")
  I <- which(V[,1]==-999)
  V <- V[-I,]                 # Remove the 1st year (warm up)
  V[V==-999] <- 'NA'          # Replace -999 with NA
  V$datetime <- ISOdate(V$year,V$month,V$day )
  V <- V[, c("datetime", "AT", "LSWT_obs", "LSWT_sim")]

  # Well mixed layer
  file_name <- file.path(folder, path_output, paste0('5_',runmode,'_',index,'_',
                                                     IDair,'_',IDwat,'_cv_',dt,'.out'))
  V_wml <- read.table(file_name)
  colnames(V_wml) = c("delta")
  V_wml <- V_wml[-I,]                 # Remove the 1st year (warm up)
  V$C <- V_wml

  V$status <- "validation"

  all <- rbind(C, V)
  all$LSWT_obs <- as.numeric(all$LSWT_obs) # convert character to numeric

  return(all)

}
