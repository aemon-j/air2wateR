#'@title Generate the parameters for the air2water model
#'
#'@description
#'This calculates the 8 parameters needed to run air2water using the mean depth of
#' the lake.
#'
#'@param sim_folder filepath; folder which contains the meteorological forcing data
#'@param mean_depth numeric; mean depth of the lake (m)
#'
#'@keywords methods
#'@author
#'Sebastiano Piccoloroaz, Tadhg Moore
#'@examples
#'\dontrun{
#'sim_folder <- system.file('extdata', package = 'air2wateR')
#'gen_param(sim_folder = sim_folder, mean_depth = 147)
#'}
#'@export
gen_param <- function(sim_folder, mean_depth) {

  if(!file.exists(file.path(sim_folder, "input.txt"))) {
    stop("No 'input.txt' in ", sim_folder,'\nThis file is required!')
  }

  if(!is.numeric(mean_depth)) {
    stop("mean_depth must be a numeric value.")
  }

  # Initialization
  n <- 2
  empty_matrix <- matrix(NA, nrow = n+1, ncol = n+1)
  Rad_a <- empty_matrix; Rad_b <- empty_matrix; ew_par <- empty_matrix;

  # Constants
  rho <- 1000;            # (kg/m3)
  cp <- 4186;             # (J/kg K)
  s_Boltzman <- 5.67E-8;  # (W/m2 K4)

  # Plausible range for solar radiation (W/m2)
  min_maxrad <- 200; max_maxrad <- 450;  delta_maxrad <- max_maxrad-min_maxrad   # range of maximum values
  min_minrad <- 0;   max_minrad <- 250;  delta_minrad <- max_minrad-min_minrad   # range of minimum values
  maxrad <- seq(from = min_maxrad, to = max_maxrad, by = delta_maxrad/n)
  minrad <- seq(from = min_minrad, to = max_minrad, by = delta_minrad/n)
  for(i in 1:(n+1)){
    for(j in 1:(n+1)){
      Rad_a[i,j] <- (maxrad[i]-minrad[j])*0.5    # amplitude
      Rad_b[i,j] <- minrad[j]+Rad_a[i]           # average
    }
  }
  minRad_a <- min(Rad_a);    maxRad_a <- max(Rad_a)   # range of annual amplitude
  minRad_b <- min(Rad_b);    maxRad_b <- max(Rad_b)   # range of annual average

  # Plausible range for shortwave reflectivity (albedo) rs
  min_rs <- 0.04; max_rs <- 0.20; delta <- max_rs-min_rs
  rs <- seq(from = min_rs, to = max_rs, by = delta/n)

  # Plausible range for water temperature
  min_Temperatura_rif <- 0; max_Temperatura_rif <- 30; delta <- max_Temperatura_rif-min_Temperatura_rif
  Temperatura_rif <- seq(from = min_Temperatura_rif, to = max_Temperatura_rif, by = delta/n)
  Kelvin0 <- 273.15 + Temperatura_rif
  min_Delta_T <- 0; max_Delta_T <- 30; delta <- max_Delta_T-min_Delta_T
  Delta_T <- seq(from = min_Delta_T, to = max_Delta_T, by = delta/n)

  # Plausible values for the emissivities of atmosphere (epsilon_a) and water (epsilon_w)
  min_epsilon_a <- 0.6; max_epsilon_a <- 0.9; delta <- max_epsilon_a-min_epsilon_a
  epsilon_a <- 0.97*seq(from = min_epsilon_a, to = max_epsilon_a, by = delta/n)    # (1-ra)*epsilon_a, where ra=0.03 (reflection of infrared radiation from water surface)
  epsilon_w <- 0.97

  # Plausible values for the sensible/latent heat transfer functions (alpha_s/alpha_l, W/m2K)
  min_alpha_s <- 3; max_alpha_s <- 15; delta <- max_alpha_s-min_alpha_s
  alpha_s <- seq(from = min_alpha_s, to = max_alpha_s, by = delta/n)
  alpha_l <- alpha_s/0.61          # 0.61=Bowen coefficient
  min_Delta_alpha_s <- 0.1; max_Delta_alpha_s <- 15; delta <- max_Delta_alpha_s-min_Delta_alpha_s
  Delta_alpha_s <- seq(from = min_Delta_alpha_s, to = max_Delta_alpha_s, by = delta/n)
  Delta_alpha_l <- Delta_alpha_s

  # Plausible values for the atmospheric water pressure (ea, mbar)
  min_ea <- 5; max_ea <- 15; delta <- max_ea-min_ea
  ea <- seq(from = min_ea, to = max_ea, by = delta/n)
  min_Delta_ea <- 0.1; max_Delta_ea <- 10; delta <- max_Delta_ea-min_Delta_ea
  Delta_ea <- seq(from = min_Delta_ea, to = max_Delta_ea, by = delta/n)

  # Plausible values for the water vapor saturation pressure at the temperature of water (ew, mbar)
  for (i in 1:(n+1)){
    for (j in 1:(n+1)){
      ew_par[i,j] <- 6.112*exp(Temperatura_rif[i]*17.67/(Temperatura_rif[i]+243.5))
    }
  }
  ew <- cbind( max(0,min(ew_par)), max(ew_par) );

  # Reaction volume participating to the heat exchange with the atmosphere
  min_D <- 1+mean_depth/20; max_D <- max(10,mean_depth); delta <- max_D-min_D
  D <- seq(from = min_D, to = max_D, by = delta/n)

  # Denominator
  den <- rho*cp*D/86400;    # Conversion from seconds to days (air2water works at daily time step)

  # Parameter 1 (all possible combinations)
  p1_par <- array(NA,dim=c(n+1,n+1,n+1,n+1,n+1,n+1,n+1,n+1))
  for (i in 1:(n+1)) {
    for (j in 1:(n+1)) {
      for (k in 1:(n+1)) {
        for (l in 1:(n+1)){
          for (m in 1:(n+1)){
            for (o in 1:(n+1)){
              for (p in 1:(n+1)){
                for (q in 1:(n+1)){
                  temp <- 6.112*exp(Temperatura_rif[m]*17.67/
                                      (Temperatura_rif[m]+243.5))*( 1 - 17.67*243.5/
                                                                      (Temperatura_rif[m]+243.5)^2*Temperatura_rif[m] )

                  p1_par[i,j,k,l,m,o,p,q] <-  ( (1-rs[p])*Rad_b[i,i] +
                                                  s_Boltzman*Kelvin0[m]^3*
                                                  (epsilon_a[j]-epsilon_w)*(273.15-3*Temperatura_rif[m])+
                                                  alpha_l[k]*( ea[l] - temp ))/   #note that the sign of this term is wrong in Piccolroaz et al., 2013
                    den[o];

                }
              }
            }
          }
        }
      }
    }
  }

  p1 <- cbind(min(p1_par), max(p1_par))
  p1[2] <- min(2,p1[2])


  # Parameter 2
  p2 <- cbind( (4*s_Boltzman*epsilon_a[1]*Kelvin0[1]^3 + alpha_s[1])/den[n+1],
               (4*s_Boltzman*epsilon_a[n+1]*Kelvin0[n+1]^3 + alpha_s[n+1])/den[1] )

  # Parameter 3 (all possible combinations)
  p3_par <- array(NA,dim=c(n+1,n+1,n+1,n+1,n+1,n+1))
  for (i in 1:(n+1)) {
    for (j in 1:(n+1)) {
      for (k in 1:(n+1)){
        for (l in 1:(n+1)){
          for (o in 1:(n+1)){
            for (p in 1:(n+1)){
              p3_par[i,j,k,l,o,p] <- (4*s_Boltzman*epsilon_a[i]*Kelvin0[o]^3*
                                        ( 1 - (epsilon_a[i]-epsilon_w)/epsilon_a[i])+
                                        alpha_s[p]+alpha_l[j]*6.112*exp(Temperatura_rif[k]*17.67/
                                                                          (Temperatura_rif[k]+243.5))*17.67*243.5/
                                        (Temperatura_rif[k]+243.5)^2)/den[l]
            }
          }
        }
      }
    }
  }
  p3 <- cbind( min(p3_par),  max(p3_par));

  # Parameter 4
  p4 <- cbind(1, 100*mean_depth^(-0.35));

  # Parameter 5
  p5 <- cbind( ( (1-rs[n+1])*minRad_a + alpha_l[1]*Delta_ea[1]+Delta_alpha_l[1]*(ea[1] - ew[2] + Delta_ea[1]) + Delta_alpha_s[1]*Delta_T[1] )/den[n+1],
               ( (1-rs[1])*maxRad_a + alpha_l[n+1]*Delta_ea[n+1]+Delta_alpha_l[n+1]*(ea[n+1] - ew[1] + Delta_ea[n+1]) + Delta_alpha_s[n+1]*Delta_T[n+1] )/den[1])
  p5[1] <- max(0,p5[1]);

  # Parameter 6
  p6 <- cbind(0, 1)

  # Parameter 7
  p7 <- cbind(0, 150);

  # Parameter 8
  p8 <- cbind(0, 0.5);

  parameters <- rbind(p1,p2,p3,p4,p5,p6,p7,p8)
  parameters <- t(parameters)

  write.table(parameters, file = file.path(sim_folder, 'parameters.txt'),
              row.names = FALSE, col.names = FALSE)

  message('Written parameters:\n')
  print((round(parameters, 2)))
  message('to file: ', file.path(sim_folder, 'parameters.txt'))


}
