#'@title Run the air2water model
#'
#'@description
#'This runs the air2water model on the specific simulation stored in \code{sim_folder}.
#'The specified \code{sim_folder} must contain a valid air2water file setup. It also
#'allows the model to be run in calibration or forward mode.
#'
#'@param sim_folder filepath; the directory where simulation files are contained
#'@param mode character; mode in which the model will be run. There are three options:
#'\enumerate{
#'  \item "pso" - Particle swarm operator which runs a calibration routine and then a model simulation with the best parameters (default)
#'  \item "forward" - runs the model once with a set of provided parameters.
#'  \item "lhc" - Uses Latin hypercube sampling to calibrate the model and then a model simulation with the best parameters.
#'}
#'@param use_pars boolean; use the parameters from the recent calibration. Only in
#'"forward" mode. This will use the results from calibration routine in file
#' "1_XXX_xxx.out" and write to "parameters_forward.txt" before running the model. Defaults to TRUE.
#'@param verbose Save output as character vector. Defaults to FALSE
#'
#'@keywords methods
#'@author
#'Tadhg Moore
#'@examples
#'\dontrun{
#'sim_folder <- system.file('extdata', package = 'air2wateR')
#'gen_param(sim_folder = sim_folder, mean_depth = 147)
#'run_air2water(sim_folder = sim_folder, method = "pso")
#'run_air2water(sim_folder = sim_folder, method = "forward", use_pars = TRUE)
#'}
#'@export
#'@importFrom utils packageName
run_air2water <- function (sim_folder = ".", mode = "pso", use_pars = TRUE,
                           verbose = FALSE) {

  if(!file.exists(file.path(sim_folder, "input.txt"))) {
    stop("No 'input.txt' in ", sim_folder,'\nThis file is required!')
  }
  input_data <- read.table(file.path(sim_folder, "input.txt"), stringsAsFactors = FALSE)
  folder <- input_data[1,1]
  IDair <- input_data[2,1]
  IDwat <- input_data[3,1]
  dt <- input_data[5,1]
  version <- input_data[6,1]
  index <- input_data[8,1]
  runmode  <- input_data[10,1]

  path_output <- paste0('output_',version)

  if(mode != "forward") {
    if(!file.exists(file.path(sim_folder, folder, "parameters.txt"))) {
      stop("No 'parameters.txt' in ", sim_folder,'\nThis file is required!')
    }
  }
  input_data <- readLines(file.path(sim_folder, 'input.txt'))


  if(mode == "forward") {
    input_data[11] <- "FORWARD"
    if(use_pars) {
      file_name <- file.path(sim_folder, folder, path_output,
                             paste0('1_', runmode, '_', index, '_',
                                    IDair, '_', IDwat, '_c_', dt, '.out'))
      if(!file.exists(file.path(file_name))) {
        stop("No '", file_name, "' present. ",
        "\nModel needs to be calibrated first\nRe-run with mode = 'pso' or 'lhc'")
      }
      tmp <- readLines(file_name, n = 1)
      writeLines(tmp, file.path(sim_folder, folder, 'parameters_forward.txt'))
    } else {
      if(!file.exists(file.path(sim_folder, folder, "parameters_forward.txt"))) {
        stop("No 'parameters_forward.txt' in ", sim_folder,'\nThis file is required!')
      }
    }
  } else if(mode == "pso") {
    input_data[11] <- "PSO"
  } else if(mode == "lhc") {
    input_data[11] <- "LATHYP"
  }

  # Write out the input file
  write.table(input_data, file = file.path(sim_folder, "input.txt"), quote = FALSE,
              row.names = FALSE, col.names = FALSE)


  if (.Platform$pkgType == "win.binary") {
    return(run_air2waterWin(sim_folder, verbose = verbose))
  }

  ### macOS ###
  if (grepl('mac.binary',.Platform$pkgType)) {
    # stop('No air2water executable available for your machine yet...')
    maj_v_number <- as.numeric(strsplit(
      Sys.info()["release"][[1]],'.', fixed = TRUE)[[1]][1])

    if (maj_v_number < 13.0) {
      stop('pre-mavericks mac OSX is not supported. Consider upgrading')
    }

    return(run_air2waterOSx(sim_folder, verbose = verbose))

  }

  if (.Platform$pkgType == "source") {
    #stop('No air2water executable available for your machine yet...')
    return(run_air2waterNIX(sim_folder, verbose = verbose))
  }
}

run_air2waterWin <- function(sim_folder, verbose = FALSE){

  if(.Platform$r_arch == 'x64'){
    air2water_path <- system.file('extbin/air2water.exe', package = 'air2wateR') #packageName()
  }else{
    stop('No air2water executable available for your machine yet...')
  }

  origin <- getwd()
  setwd(sim_folder)

  tryCatch({
    if (verbose){
      out <- system2(air2water_path, wait = TRUE, stdout = TRUE,
                     stderr = "")
    } else {
      out <- system2(air2water_path)
    }
    setwd(origin)
    return(out)
  }, error = function(err) {
    print(paste("air2water_ERROR:  ",err))
    setwd(origin)
  })
}



# run_air2waterOSx <- function(sim_folder, nml = TRUE, nml_file = 'air2water.nml', verbose = TRUE, args){
#   #lib_path <- system.file('extbin/macair2water/bin', package=packageName()) #Not sure if libraries needed for air2water
#
#   air2water_path <- system.file('exec/macair2water', package=packageName())
#
#   # ship air2water and libs to sim_folder
#   #Sys.setenv(DYLD_FALLBACK_LIBRARY_PATH=lib_path) #Libraries?
#
#   if(nml){
#     args <- c(args, nml_file)
#   }else{
#     args <- c(args,'--read_nml')
#   }
#
#   origin <- getwd()
#   setwd(sim_folder)
#
#   tryCatch({
#     if (verbose){
#       out <- system2(air2water_path, wait = TRUE, stdout = "",
#                      stderr = "", args = args)
#
#     } else {
#       out <- system2(air2water_path, wait = TRUE, stdout = NULL,
#                      stderr = NULL, args=args)
#     }
#
#     setwd(origin)
# 	return(out)
#   }, error = function(err) {
#     print(paste("air2water_ERROR:  ",err))
#
#     setwd(origin)
#   })
# }

run_air2waterOSx <- function(sim_folder, verbose=FALSE){
  air2water_path <- system.file('extbin/macair2water', package='air2wateR')


  origin <- getwd()
  setwd(sim_folder)
  Sys.setenv(LD_LIBRARY_PATH=system.file('extbin/nixair2water',
                                         'air2wateR'))
  tryCatch({
    if (verbose){
      out <- system2(air2water_path, wait = TRUE, stdout = TRUE,
                     stderr = "")
    } else {
      out <- system2(air2water_path)
    }
    setwd(origin)
    return(out)
  }, error = function(err) {
    print(paste("air2water_ERROR:  ",err))
    setwd(origin)
  })

}

run_air2waterNIX <- function(sim_folder, verbose=FALSE){
  air2water_path <- system.file('extbin/nixair2water', package='air2wateR')


  origin <- getwd()
  setwd(sim_folder)
  Sys.setenv(LD_LIBRARY_PATH=system.file('extbin/nixair2water',
                                         'air2wateR'))
  tryCatch({
    if (verbose){
      out <- system2(air2water_path, wait = TRUE, stdout = TRUE,
                     stderr = "")
    } else {
      out <- system2(air2water_path)
    }
    setwd(origin)
    return(out)
  }, error = function(err) {
    print(paste("air2water_ERROR:  ",err))
    setwd(origin)
  })

}
