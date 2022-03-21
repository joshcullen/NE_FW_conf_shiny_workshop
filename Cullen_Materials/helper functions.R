
# function to convert list object from rerddapXtracto::rxtracto() into a data.frame for ggplot
array2df <- function(lon, lat, var, var.name, time) {
  dims <- dim(var)
  rast.df <- expand.grid(x = lon, y = lat, time = time)
  rast.df$var <- array(apply(var, 3, rbind), dims[1] * dims[2] * dims[3])
  names(rast.df)[4] <- var.name
  
  return(rast.df)
}

#----------------------------------------------

### HMM assuming CRW using common distributions; modified from Leos-Barajas & Michelot (2018) arXiv pre-print (https://arxiv.org/pdf/1806.10639.pdf)

HMM.sim = function(nsim, nobs, SL.params, TA.params, Z0) {  
  #nsim=number of simulations
  #nobs=number of observations per simulation (vector)
  #SL.params=df of shape and rate params
  #TA.params=df of mean TA and concen. param
  #Z0=coords of initial location
  
  #uses gamma and wrapped cauchy distribs
  #behaviors params must be in order
  #for simulating w/ 3 behavioral states
  
  if (nsim != length(nobs)) stop("'nobs' must be a vector of length 'nsim'")
  
  track.list<- list()  #store all tracks of all durations
  max.cnt<- table(nobs) %>% max()
  n.type<- nsim/max.cnt
  sim.ID<- rep(1:max.cnt, n.type)  #running 5 different simulations per track length
  sim.ID2<- rep(1:n.type, each = max.cnt)
  
  for (j in 1:nsim) {
    
    # Number of states
    N <- 3
    # transition probabilities
    Gamma <- matrix(c(0.9, 0.05, 0.05,
                      0.05, 0.9, 0.05,
                      0.05, 0.05, 0.9),
                    nrow = 3, ncol = 3)
    # initial distribution set to the stationary distribution 
    delta <- solve(t(diag(N) - Gamma + 1), rep(1, N))
    
    # state-dependent distribution params
    #step length (gamma)
    gamma.shape <- SL.params[,1]
    gamma.rate <- SL.params[,2]
    
    #turning angle (wrapped Cauchy)
    WC.mean <- TA.params[,1]
    WC.concen <- TA.params[,2]
    
    nobs1 <- nobs[j]
    S <- rep(NA, nobs1) 
    y <- matrix(NA, nobs1, 2)  #2 cols; for step length and turning angle
    
    # initialize state and observation
    S[1] <- sample(1:N, size=1, prob=delta)  #latent state
    y[1,1] <- rgamma(n=1, shape=gamma.shape[S[1]], rate=gamma.rate[S[1]])  #SL
    y[1,2] <- rwrappedcauchy(n=1, mu=circular(WC.mean[S[1]]), rho=WC.concen[S[1]]) %>%  #TA
      ifelse(. > pi, .-(2*pi), .)  #deals with values between pi and 2pi
    
    # simulate state and observation processes forward
    for(t in 2:nobs1) {
      S[t] <- sample(1:N, size=1, prob=Gamma[S[t-1],])  #latent state
      y[t,1] <- rgamma(n=1, shape=gamma.shape[S[t]], rate=gamma.rate[S[t]])  #SL
      y[t,2] <- rwrappedcauchy(n=1, mu=circular(WC.mean[S[t]]), rho=WC.concen[S[t]]) %>%  #TA
        ifelse(. > pi, .-(2*pi), .)  #deals with values between pi and 2pi
    }
    
    # cumulative angle
    Phi <- cumsum(y[,2])
    
    # step length components
    dX <- y[,1]*cos(Phi)
    dY <- y[,1]*sin(Phi)
    
    # actual X-Y values
    X <- c(Z0[1], Z0[1] + cumsum(dX))
    Y <- c(Z0[2], Z0[2] + cumsum(dY))
    
    track<- data.frame(id = as.character(paste0(sim.ID[j],"_",sim.ID2[j])),
                       time1 = 1:(nobs1 + 1),
                       x = X,
                       y = Y,
                       SL = c(NA, y[,1]),
                       TA = c(NA, y[,2]),
                       state = c(NA, S),
                       track_length = nobs1)
    
    track.list[[j]]<- track
  }
  
  names(track.list)<- track.list %>% modify_depth(1, ~unique(.$id)) %>% unlist()
  
  
  return(track.list)
}