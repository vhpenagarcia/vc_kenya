#### Functions and data source

### Data
clima_data <- read.csv("R01RenewalClimateDat_DATA_2022-08-16_0928.csv")

prop_imm_time <- read.csv("prop_imm_time.csv")

occupants <- data.frame(Site = c("Kisumu","Ukunda"), prop = c(4.6129032,7.2990654))

n_containers <- read.csv("n_containers.csv")

environments <- c("Household","Non-Household")
cities <- c("Kisumu","Ukunda")

dists_ef <- data.frame(
  Stage = rep(c("eggs","adult"), each = 4),
  Site = rep(rep(c("Kisumu","Ukunda"), each = 2),2),
  Environment = rep(c("Household","Non-Household"),4),
  fun = c(rep("lnorm",4),"weibull",rep("lnorm",3)),
  par1 = c(3.30,3.40,3.53,3.61,3.062928,2.045432,2.452693,2.588756),
  par2 = c(0.672,0.645,0.797,0.760,4.764800,0.4821921,0.6680835,0.9028559)
)

proportion_premises <- data.frame(Site = rep(c("Kisumu","Ukunda"), each = 2),
                                  Environment = rep(c("Household","Non-Household"),2),
                                  prop = c(0.623,0.527,0.538,0.326))


temps <- data.frame(Site = c("Kisumu","Ukunda"),
                    temp = c(24.7,28.3))


### Functions
prob_minfect_mordecai <- function(t){
  b <- ifelse((t > 35.83),0,8.49e-4*t*(t - 17.05)*(35.83 - t)^0.5)
  c <- 4.91e-4*t*(t - 12.22)*(37.46 - t)^0.5
  return(b*c)
}


a_mordecai <- function(t){
  a <-2.02e-4*t*(t-13.35)*(40.08-t)^0.5
  return(a)
}

mortality_mordecai <- function(t){
  lf <- -1.48e-01*(t - 9.16)*(t - 37.73)
  return(1/(lf))
}

pdr_mordecai <- function(t){
  pdr <- 1.09e-04*t*(t-10.39)*(43.05-t)^0.5
  return(pdr)
}

VC <- function(m,a,bc,u,pdr){
  vc <- (m*(a^2)*bc*(exp(-u/pdr)))/u
  return(vc)
}


n_mosquitoes <- function(prop_mean,cont_prop,param_eggs,param_adult,n){
  prem_pos <- rbinom(1,n,prop_mean)
  if (prem_pos == 0){
    return(0)
  } else{
    cont_pos <- sample(cont_prop$n_container,prem_pos,replace = TRUE, prob = cont_prop$prop) ## Number of positive containers per house
    females_sim <- c()
    for(k in 1:length(cont_pos)){
      female_q <- c()
      n_con_pos <- cont_pos[k]
      eggs_n <- round(rlnorm(n_con_pos, meanlog = param_eggs$par1, sdlog = param_eggs$par2)) #number of eggs per container
      for(f in 1:length(eggs_n)){
        n_egg_p <- plnorm(eggs_n[f], meanlog = param_eggs$par1, sdlog = param_eggs$par2)
        female_q[f] <- ifelse(param_adult$Site == "Kisumu" & param_adult$Environment == "Household",
                              qweibull(n_egg_p, shape = 3.062928, scale = 4.764800),
                              qlnorm(n_egg_p, meanlog = param_adult$par1, sdlog = param_adult$par2))
        females_sim[k] <- round(sum(female_q))
      }
    }
    return(sum(females_sim))
  }
}

internal_vc <- function(prop_mean,n,prob,cont_prob,param_eggs,par_fem,t,prop_h=1,prop_v=1){
  Nh <- sum(rpois(n,prob))
  Nm <- n_mosquitoes(prop_mean = prop_mean, cont_prop = cont_prob, param_eggs = param_eggs,
                     param_adult = par_fem, n = n)
  m <- (Nm*prop_v)/(Nh*prop_h)
  a <- a_mordecai(t)
  bc <- prob_minfect_mordecai(t)
  um <- mortality_mordecai(t)
  pdr <- pdr_mordecai(t)
  vc <- VC(m = m, a = a, bc = bc, u = um, pdr = pdr)
  return(vc)
}

m_values <- function(prop_mean,n,prob,cont_prob,param_eggs,par_fem){
  Nh <- sum(rpois(n,prob))
  Nm <- n_mosquitoes(prop_mean = prop_mean, cont_prop = cont_prob, 
                     param_eggs = param_eggs, param_adult = par_fem, n = n)
  m <- Nm/Nh
  return(m)
}


n_mosq_house <- function(prop_mean,cont_prop,param_eggs,param_adult){
  prem_pos <- rbinom(1,1,prop_mean)
  n <- if(prem_pos == 0){
    0
  } else {
    cont_pos <- sample(cont_prop$n_container,prem_pos,replace = TRUE, prob = cont_prop$prop) ## Number of positive containers per house
    females_sim <- c()
    for(k in 1:length(cont_pos)){
      female_q <- c()
      n_con_pos <- cont_pos[k]
      eggs_n <- round(rlnorm(n_con_pos, meanlog = param_eggs$par1, sdlog = param_eggs$par2)) #number of eggs per container
      for(f in 1:length(eggs_n)){
        n_egg_p <- plnorm(eggs_n[f], meanlog = param_eggs$par1, sdlog = param_eggs$par2)
        female_q[f] <- ifelse(param_adult$Site == "Kisumu" & param_adult$Environment == "Household",
                              qweibull(n_egg_p, shape = 3.062928, scale = 4.764800),
                              qlnorm(n_egg_p, meanlog = param_adult$par1, sdlog = param_adult$par2))
        females_sim[k] <- round(sum(female_q))
      }
    }
    return(sum(females_sim))
  }
  return(n)
}



