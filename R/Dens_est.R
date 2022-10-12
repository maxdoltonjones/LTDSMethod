#' Density estimate
#'
#' Calculates the number of tortoises per acre at recipient site
#' @param ltds_data Truncated data from a full LTDS survey
#' @param effort Distance in meters walked during the full survey
#' @param area Total area of the recipient site (permitted, habitat area)
#' @return A numeric value showing number of tortoises per acre
#' @examples
#' #Read in csv file showing transect lines
#' start.end <- read_csv("./Start_end_locations.csv")
#'
#' #Input coordinate file
#' effort <- samp_effort(start.end, zone = 17, plot = T)
#'
#' #Read in csv file with raw ltds data
#' ltds_data <- read_csv("./LTDS_example_data.csv")
#'
#' #Input raw data into function
#' new_ltds_data <- ltds_crop(ltds_data = ltds_data, trunc = 5, save = T)
#'
#' #now use ltds data, the effort of survey and total area of site (acres)
#' tort.abundance <- dens_est(ltds_data = new_ltds_data, effort = effort, area = 400)
#'
#' @export
dens_est <-function(ltds_data, effort, area) {
  nburr <- nrow(ltds_data)
  ifelse(nburr <= 200, naug <- 300, naug <- 450)
  ntot <- nburr+naug
  dist.a <- c(ltds_data$Distance, rep(NA,times=naug))
  y <- c(rep(1,times=nburr), rep(0,times=naug))
  #dist.mn <- mean(df$distance)
  #dist.sd <- sd(df$distance)
  Bx <- max(ltds_data$Distance)
  ltds_data <- ltds_data %>%                               # Replacing values
    mutate(Occupied = replace(Occupied, Occupied == "Yes", 1)) %>%
    mutate(Occupied = replace(Occupied, Occupied == "No", 0)) %>%
    mutate(Occupied = replace(Occupied, Occupied == "Unknown", 0))
  ltds_data$Occupied <- as.numeric(ltds_data$Occupied)
  occ.a <- c(ltds_data$Occupied, rep(NA, times=naug))
  L <- effort
  size <- c(ltds_data$Burrow_width / 10, rep(NA,times=naug))


  ###############
  # NIMBLE MODEL
  ###############

  #Data augmentation version (non-aug version does not perform well)
  ltds.aug.code <- nimbleCode({

    #Loop through number of burrows (+ aug) to estimate probability of detection, prob of being real, and prob of occupancy
    for(b in 1:ntot){
      w[b] ~ dbern(psi)  #augmentation - prob of being real
      size[b] ~ dnorm(size.mn, sd=size.sd)  #Size distribution


      #Loop through both observers to estimate detection probability
      #dist.a[b] ~ T(dnorm(dist.mn, sd=dist.sd), 0, )  #distance from line for the missed ones; half-normal distribution  (T() is truncation)
      dist.a[b] ~ dunif(0, Bx)
      #y is the data on whether a burrow is real or augmented
      y[b] ~ dbern(mu[b])
      mu[b] <- w[b]*p[b]   #mu is rescaling p to only be for "real" burrows

      #Probability of burrow detection - half normal function
      logp[b] <- exp(-1*pow(dist.a[b],2) / (2*sigma2))  #p0 is baseline prob, sigma is half-width
      p[b] <- logp[b]*xi[b]

      #Modified Heather's code for breakpoint size effect on p
      #Creating a value based on size of tortoise (J[b])
      #0 == < 20; 1 == > 20 (b.point)
      J[b] <- step(size[b] - b.point)
      #Using this to determine if the probability should be 1 or higher
      #At this stage, all smaller tortoise will be 1
      q[b] <- 1+((((m*size[b]) + intercept)-1)*J[b])
      #Assigning new probability based on tortoise size (xi[b])
      #Divides by the probability above, which will now be 0-1 for smaller
      #tortoises, and stay at 1 for the larger ones
      xi[b] <- ((m*size[b]) + intercept)/q[b]

      #occupancy part
      o[b] ~ dbern(phi)  #Probability of burrow occupancy
      ow[b] <- o[b]*w[b]  #burrow is real and occupied
    }


    ###Priors
    psi ~ dunif(0,1)
    #p0 ~ dunif(0.0001,1)
    sigma ~ dunif(0.0001,10)
    sigma2 <- sigma*sigma
    phi ~ dunif(0,1)

    size.mn ~ T(dnorm(20, sd=5), 0, )
    size.sd ~ T(dnorm(1, sd=5), 0, )

    #Heather's breakpoint model for effect of size on detection
    p.online ~ dunif(.2, 0.8)		#estimated detection on line for 5 cm burrows
    #b.point <- 20 #could alternatively set break-point for perfect detection
    b.point ~ dunif(15, 25) #prior for size of burrow where perfect detection is reached
    m <- (1-p.online)/(b.point-5) 	#slope for detection on the line for smaller burrows
    intercept <- p.online-(5*m)	## finding intercept via the detection of the 5cm burrow


    #Derived variables
    #Density from Buckland et al. 2005 - area under detection curve is the same as 1/p0 (or the p function at dist=0)
    N <- sum(w[1:ntot])
    D <- N / (2*L*Bx)
    Nt <- sum(ow[1:ntot])
    Dt <- Nt/(2*L*Bx)

  })   ## end Nimble model


  ######################
  # PREPARE DATA FOR NIMBLE
  ######################

  data.for.nimble <- list(
    dist.a = dist.a,
    y = y,
    size = size,
    o = occ.a
  )

  constants <- list(
    ntot = ntot,
    Bx = Bx,
    L = L
    #obs = obs
    #dist.mn = dist.mn,
    #dist.sd = dist.sd
  )

  w.i <- c(rep(1,times=nburr), rep(0,times=naug))

  initz <- function(){
    list(
      #p0=runif(1, 0.2, 0.8),
      psi=runif(1,0.2,0.8),
      phi=runif(1,0.2,0.8),
      p.online=runif(1, 0.2, 0.8),
      b.point=runif(1,15,25),
      size.mn=runif(1,16,20),
      size.sd=runif(1,5,10),
      w=w.i
      #sigma=runif(1, 1, 2)
    )
  }


  #system.time(
  mcmc.out <- nimbleMCMC(code=ltds.aug.code, constants=constants, data=data.for.nimble, inits = initz, nchains=3, nburnin=5000, niter=10000, thin=5,
                         summary=TRUE, monitors=c("sigma2", "psi", "phi", "N", "D", "Nt", "Dt", "size.mn", "size.sd", "p.online", "b.point"))  #"p0",
  #)

  #mcmc.out$summary$all.chains

  #Extrapolate from surveyed area to entire site: (400 acres = 162 hectares = 1620000 m2)
  #prop.ef <- 2*L*Bx/1620000
  #148/(prop.ef)

  #Save output to list object

  area.m <- area*4046.86
  nt <-mcmc.out$summary$all.chains[4]
  prop.ef <- 2*L*Bx/area.m
  estim <- nt/prop.ef
  density <- estim/area

  return(density)
}
