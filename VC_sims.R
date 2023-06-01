library(tidyverse)
library(lubridate)


## Formatting temperature data
temp_data <- clima_data %>% select(date_collected,redcap_event_name,temp_mean_hobo,temp_min_hobo,
                                   temp_max_hobo) %>%
  filter(redcap_event_name == "ukunda_arm_1" | redcap_event_name == "kisumu_arm_1") %>%
  mutate(Site = factor(case_when(redcap_event_name == "ukunda_arm_1" ~ "Ukunda",
                                 redcap_event_name == "kisumu_arm_1" ~ "Kisumu"))) %>%
  rename(Date = date_collected, t_mean = temp_mean_hobo, 
         t_min = temp_min_hobo, t_max = temp_max_hobo) %>%
  select(Date,Site,t_mean,t_min,t_max)
temp_data$Date <- as.Date(temp_data$Date)

head(temp_data)
temp_prop_data <- inner_join(temp_data,prop_imm_time,by=c("Site","Date"))
head(temp_prop_data)


### single-structure simulations
ss_vc <- data.frame(matrix(ncol = 6, nrow = 0))
colnames(ss_vc) <- c("Site","Environment","house","Nh","Nm","m")
n <- 1000

for(e in 1:length(environments)){
  cont_prob1 <- n_containers %>% filter(Environment == environments[e])
  prop_pre1 <- proportion_premises %>% filter(Environment == environments[e])
  params1 <- dists_ef %>% filter(Environment == environments[e])
  for(s in 1:length(cities)){
    cont_prob <- cont_prob1 %>% filter(Site == cities[s])
    prop_pre <- prop_pre1 %>% filter(Site == cities[s])
    occupants_p <- occupants %>% filter(Site == cities[s])
    params <- params1 %>% filter(Site == cities[s])
    param_eggs <- params %>% filter(Stage == "eggs")
    param_adult <- params %>% filter(Stage == "adult")
    for(h in 1:n){
      Nh <- rpois(1,occupants_p$prop)
      Nm <- n_mosq_house(prop_mean = prop_pre$prop, cont_prop = cont_prob,
                         param_eggs = param_eggs, param_adult = param_adult)
      m <- ifelse((Nh==0),Nm/(Nh+1),Nm/Nh)
      transient_df <- data.frame(Site = cities[s], Environment = environments[e],
                                 house = h, Nh = Nh, Nm = Nm, m = m)
      ss_vc <- rbind(ss_vc,transient_df)
    }
  }
}


ss_vc %>% ggplot(aes(m,fill = Environment)) +
  geom_density(alpha = 0.5) +
  facet_wrap(.~Site) +
  xlim(0,15) +
  scale_fill_manual(values = c("indianred1","#0099FF")) +
  theme_bw() +
  theme(panel.grid.major = element_line(colour = "grey70", size = 0.2),
        panel.grid.minor = element_blank(),
        strip.background = element_rect(fill = "blue4"),
        strip.text.x = element_text(color = "white",size = rel(1.5)))


### Vectorial capacity accross temperature range
n <- 100
sims <- 1000
temp_range <- seq(from = 15, to = 36, by = 0.5)
VC_T_range <- data.frame(matrix(ncol = 5, nrow = 0))
colnames(VC_T_range) <- c("Site","Environment","Temp","VC","sd")

for(j in 1:length(environments)){
  cont_prob1 <- n_containers %>% filter(Environment == environments[j])
  prop_pre1 <- proportion_premises %>% filter(Environment == environments[j])
  params1 <- dists_ef %>% filter(Environment == environments[j])
  for(i in 1:length(cities)){
    cont_prob <- cont_prob1 %>% filter(Site == cities[i])
    prop_pre <- prop_pre1 %>% filter(Site == cities[i])
    occupants_p <- occupants %>% filter(Site == cities[i])
    params <- params1 %>% filter(Site == cities[i])
    param_eggs <- params %>% filter(Stage == "eggs")
    param_adult <- params %>% filter(Stage == "adult")
    for(t in 1:length(temp_range)){
      matrix_T <- replicate(sims,internal_vc(prop_mean = prop_pre$prop, n = n,
                                             prob = occupants_p$prop,
                                             cont_prob = cont_prob,
                                             param_eggs = param_eggs,par_fem = param_adult,
                                             t = temp_range[t]))
      sim_t_range <- data.frame(Site = cities[i], Environment = environments[j],
                                Temp = temp_range[t], VC = mean(matrix_T), sd = sd(matrix_T))
      VC_T_range <- rbind(VC_T_range,sim_t_range)
    }
  }
}

VC_T_range <- VC_T_range %>% mutate(upper_95CI = VC + 1.96*(sd/sqrt(sims)),
                                    lower_95CI = VC - 1.96*(sd/sqrt(sims)))


VC_T_range %>% ggplot(aes(Temp,VC)) +
  geom_ribbon(aes(ymin=lower_95CI, ymax=upper_95CI, fill=Environment)) +
  geom_line(aes(col=Environment)) +
  scale_color_manual(values = c("red","blue")) +
  scale_fill_manual(values = c("indianred1","#3366FF")) +
  facet_wrap(.~Site) +
  ylab("Vectorial Capacity") + xlab("Temp (°C)") +
  theme_bw() +
  theme(panel.grid.major = element_line(colour = "grey60", size = 0.2, linetype = "dotted"),
        panel.grid.minor = element_blank(),
        strip.background = element_rect(fill = "blue4"),
        strip.text.x = element_text(color = "white",size = rel(6)),
        text = element_text(size = rel(3.5)),
        legend.text = element_text(size = rel(3.5)))



### simulations for VC in time
### estimating VC
n = 1000

### Simulate VC
sim_RT_hist <- data.frame(matrix(ncol = 5, nrow = 0))
colnames(sim_RT_hist) <- c("Site","Environment","Date","VC","sd")

for(j in 1:length(environments)){
  cont_prob1 <- n_containers %>% filter(Environment == environments[j])
  prop_pre1 <- temp_prop_data %>% filter(Environment == environments[j])
  params1 <- dists_ef %>% filter(Environment == environments[j])
  for(i in 1:length(cities)){
    prop_pre <- prop_pre1 %>% filter(Site == cities[i])
    cont_prob <- cont_prob1 %>% filter(Site == cities[i])
    occupants_p <- occupants %>% filter(Site == cities[i])
    params <- params1 %>% filter(Site == cities[i])
    param_eggs <- params %>% filter(Stage == "eggs")
    param_adult <- params %>% filter(Stage == "adult")
    for(t in 1:nrow(prop_pre)){
      matrix_Rt <- replicate(sims,internal_vc(prop_mean = prop_pre$prop_mean[t],
                                              n = n,prob = occupants_p$prop,
                                              cont_prob = cont_prob,
                                              param_eggs = param_eggs,par_fem = param_adult,
                                              t = prop_pre$t_mean[t]))
      sim_rts <- data.frame(Site = cities[i], Environment = environments[j],
                            Date = prop_pre$Date[t], VC = mean(matrix_Rt), sd = sd(matrix_Rt))
      sim_RT_hist <- rbind(sim_RT_hist,sim_rts)
    }
  }
}


head(sim_RT_hist)
sim_RT_hist <- sim_RT_hist %>% mutate(upper_95CI = VC + 1.96*(sd/sqrt(sims)),
                                      lower_95CI = VC - 1.96*(sd/sqrt(sims)))

sim_RT_hist %>% 
  ggplot(aes(Date,VC, col = Environment, fill = Environment)) +
  geom_point(alpha = 0.7) +
  geom_smooth(method = "loess") +
  facet_wrap(.~Site, scales = "free_x") +
  scale_color_manual(values = c("indianred1","#3366FF")) +
  ylab("Vectorial Capacity") +
  theme_bw() +
  theme(panel.grid.major = element_line(colour = "grey60", size = 0.2, linetype = "dotted"),
        panel.grid.minor = element_blank(),
        strip.background = element_rect(fill = "blue4"),
        strip.text.x = element_text(color = "white",size = rel(6)),
        text = element_text(size = rel(3.5)),
        legend.text = element_text(size = rel(3.5)))



### Additional Information

### varying values of m
## Varying values of humans
temp_range <- seq(from = 15, to = 36, by = 0.5)
prop_h <- c(0.2,0.4,0.6,0.8,1)
VC_T_props <- data.frame(matrix(ncol = 6, nrow = 0))
colnames(VC_T_props) <- c("Site","Environment","Prop_H","Temp","VC","sd")


for(i in 1:length(cities)){
  cont_prob <- cont_prob1 %>% filter(Site == cities[i] & Environment == "Non-Household")
  prop_pre <- prop_pre1 %>% filter(Site == cities[i] & Environment == "Non-Household")
  occupants_p <- occupants %>% filter(Site == cities[i])
  params <- params1 %>% filter(Site == cities[i] & Environment == "Non-Household")
  param_eggs <- params %>% filter(Stage == "eggs")
  param_adult <- params %>% filter(Stage == "adult")
  for(t in 1:length(temp_range)){
    for(k in 1:length(prop_h)){
      matrix_T_props <- replicate(sims,internal_vc(n = n,prob = occupants_p$prop,
                                                   cont_prob = cont_prob,
                                                   param_eggs = param_eggs,par_fem = param_adult,
                                                   t = temp_range[t], prop_h = prop_h[k]))
      sim_t_range <- data.frame(Site = cities[i], Environment = "Non-Household", Prop_H = prop_h[k],
                                Temp = temp_range[t], VC = mean(matrix_T_props), sd = sd(matrix_T_props))
      VC_T_props <- rbind(VC_T_props,sim_t_range)
    }
  }
}

VC_T_props <- VC_T_props %>% mutate(upper_95CI = VC + 1.96*(sd/sqrt(sims)),
                                    lower_95CI = VC - 1.96*(sd/sqrt(sims)),
                                    Prop_H = factor(Prop_H, ordered = TRUE))


VC_T_HH <- VC_T_range %>% filter(Environment == "Household")

VC_T_props %>% ggplot(aes(Temp,VC)) +
  geom_ribbon(aes(ymin=lower_95CI,ymax=upper_95CI, fill=Prop_H), alpha = 0.5) +
  geom_line(aes(col=Prop_H)) +
  scale_fill_manual(values = c("#99CCFF","#0099FF","#3366FF","#0000FF","#0000CC")) +
  scale_colour_manual(values = c("#3366FF","#0000FF","#0000CC","#000099","#000066")) +
  geom_ribbon(data = VC_T_HH, aes(ymin=lower_95CI,ymax=upper_95CI), 
              alpha = 0.5, fill="indianred1") +
  geom_line(data = VC_T_HH, aes(Temp,VC), col = "red") +
  facet_wrap(.~Site) +
  theme_bw() +
  theme(panel.grid.major = element_line(colour = "grey60", size = 0.2, linetype = "dotted"),
        panel.grid.minor = element_blank(),
        strip.background = element_rect(fill = "blue4"),
        strip.text.x = element_text(color = "white"))


## Varying values of mosquitoes
VC_T_props2 <- data.frame(matrix(ncol = 6, nrow = 0))
colnames(VC_T_props2) <- c("Site","Environment","Prop_M","Temp","VC","sd")


for(i in 1:length(cities)){
  cont_prob <- cont_prob1 %>% filter(Site == cities[i] & Environment == "Non-Household")
  prop_pre <- prop_pre1 %>% filter(Site == cities[i] & Environment == "Non-Household")
  occupants_p <- occupants %>% filter(Site == cities[i])
  params <- params1 %>% filter(Site == cities[i] & Environment == "Non-Household")
  param_eggs <- params %>% filter(Stage == "eggs")
  param_adult <- params %>% filter(Stage == "adult")
  for(t in 1:length(temp_range)){
    for(k in 1:length(prop_h)){
      matrix_T_props <- replicate(sims,internal_vc(n = n,prob = occupants_p$prop,
                                                   cont_prob = cont_prob,
                                                   param_eggs = param_eggs,par_fem = param_adult,
                                                   t = temp_range[t], prop_v = prop_h[k]))
      sim_t_range <- data.frame(Site = cities[i], Environment = "Non-Household", Prop_M = prop_h[k],
                                Temp = temp_range[t], VC = mean(matrix_T_props), sd = sd(matrix_T_props))
      VC_T_props2 <- rbind(VC_T_props2,sim_t_range)
    }
  }
}

VC_T_props2 <- VC_T_props2 %>% mutate(upper_95CI = VC + 1.96*(sd/sqrt(sims)),
                                      lower_95CI = VC - 1.96*(sd/sqrt(sims)),
                                      Prop_M = factor(Prop_M, ordered = TRUE))


VC_T_props2 %>% ggplot(aes(Temp,VC)) +
  geom_ribbon(aes(ymin=lower_95CI,ymax=upper_95CI, fill=Prop_M), alpha = 0.5) +
  geom_line(aes(col=Prop_M)) +
  scale_fill_manual(values = c("#99CCFF","#0099FF","#3366FF","#0000FF","#0000CC")) +
  scale_colour_manual(values = c("#3366FF","#0000FF","#0000CC","#000099","#000066")) +
  geom_ribbon(data = VC_T_HH, aes(ymin=lower_95CI,ymax=upper_95CI), 
              alpha = 0.5, fill="indianred1") +
  geom_line(data = VC_T_HH, aes(Temp,VC), col = "red") +
  ylab("Vectorial Capacity") + xlab("Temp (°C)") + 
  labs(col = "Proportion", fill = "Proportion") +
  facet_wrap(.~Site) +
  theme_bw() +
  theme(panel.grid.major = element_line(colour = "grey60", size = 0.2, linetype = "dotted"),
        panel.grid.minor = element_blank(),
        strip.background = element_rect(fill = "blue4"),
        strip.text.x = element_text(color = "white",size = rel(1.5)),
        legend.position="bottom")




