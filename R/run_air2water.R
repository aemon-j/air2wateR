#'@title Run the air2water model
#'
#'@description
#'This runs the air2water model on the specific simulation stored in \code{sim_folder}.
#'The specified \code{sim_folder} must contain valid NML files.
#'
#'@param sim_folder filepath; the directory where simulation files are contained
#'@param par_file filepath; to file with air2water setup. Defaults to 'air2water.par'
#'@param verbose Save output as character vector. Defaults to FALSE
#'
#'@keywords methods
#'@author
#'Tadhg Moore
#'@examples
#'\dontrun{
#'sim_folder <- system.file('extdata', package = 'air2wateR')
#'gen_param(sim_folder = sim_folder, mean_depth = 147)
#'run_air2water(sim_folder = sim_folder)
#'}
#'@export
#'@importFrom utils packageName
run_air2water <- function (sim_folder = ".", verbose = FALSE) {

  if(!file.exists(file.path(sim_folder, "parameters.txt"))) {
    stop("No 'parameters.txt' in ", sim_folder,'\nThis file is required!')
  }

  if(!file.exists(file.path(sim_folder, "input.txt"))) {
    stop("No 'input.txt' in ", sim_folder,'\nThis file is required!')
  }

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
