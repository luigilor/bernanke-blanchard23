#Bernanke and Blanchard(2023): "What Caused U.S. Postpandemic Inflation?" replication with bootstrapping.
#Author: Luigi Lorenzoni
#This version: 17/09/24

#note: choose appropriately the n_bootstraps at line 337. for debug reasons its now set at 100, estimates were made w/ n=1000


################################################### PREPARATION ####
#preliminary ops
rm(list=ls())
gc()
# packages used
listofpackages <- c("zoo","xts", "readxl", "dplyr","ellipse","reshape2","ggplot2", "systemfit", "vars", "texreg", "here", "boot")

for (j in listofpackages){
  if(sum(installed.packages()[, 1] == j) == 0) {
    install.packages(j)
  }
  library(j, character.only = T)
}
setwd(here())

#data import and cleaning
data <- read.csv2(here("data", "data_us.csv"), header = TRUE) 
data<-data[,-1]

#relative prices
data$rpf<-data$food_cpi/data$lci
data$rpe<-data$energy_cpi/data$lci

#take logs for growth rates
data$cpi<-as.numeric(data$cpi)
data$logcpi<-log(data$cpi)
data$loglci<-log(data$lci)
data$logrpf<-log(data$rpf)
data$logrpe<-log(data$rpe)
data$logproductivity<-log(data$productivity)

#prep
dates <-seq(as.Date("1986-10-01"),length=146, by="quarters")
params <- c("logcpi", "loglci", "logrpe", "logrpf", "logproductivity" )

#first lags of raw data
for (param in params){
  lagged_col<-paste0(param,"_l",1)
  print(paste0("generating ", lagged_col))
  data[[lagged_col]]<-lag.xts(data[[param]], k=1)
}

#growth rates/catch-up creation according to BL-BE replication package
data$gp<-400*((data$logcpi)-data$logcpi_l1)
data$grpf<-400*((data$logrpf)-(data$logrpf_l1))
data$grpe<-400*((data$logrpe)-(data$logrpe_l1))
data$gw<-400*((data$loglci)-(data$loglci_l1))
data$gpty<-400*((data$logproductivity)-(data$logproductivity_l1))
data$gpty<-0.125*(data$gpty + lag.xts(data$gpty, k=1)+ lag.xts(data$gpty, k=2)+ lag.xts(data$gpty, k=3)+ lag.xts(data$gpty, k=4)+ lag.xts(data$gpty, k=5)+ lag.xts(data$gpty, k=6)+ lag.xts(data$gpty, k=7) )

params<-c("gp", "grpf", "grpe", "gpty", "gw", "vu", "exp1", "exp10", "shortage")

#lags of growth rates
for (param in params){
  for (i in 1:4){
    lagged_col<-paste0(param,"_l",i)
    print(paste0("generating ", lagged_col))
    data[[lagged_col]]<-lag.xts(data[[param]], k=i)
  }
}

#xts transf
params<-colnames(data)
tsdata <- xts(data[, c(params)], order.by=dates) # creates a time series object

#catchup
tsdata$catch_up<-0.25*(tsdata$gp+tsdata$gp_l1+tsdata$gp_l2+tsdata$gp_l3)-tsdata$exp1_l4
tsdata$catch_up_l1<-lag.xts(tsdata$catch_up, k=1)
tsdata$catch_up_l2<-lag.xts(tsdata$catch_up, k=2)
tsdata$catch_up_l3<-lag.xts(tsdata$catch_up, k=3)
tsdata$catch_up_l4<-lag.xts(tsdata$catch_up, k=4)

#data clean
dates<-seq(as.Date("1990-01-01"),length=133, by="quarters")
tsdata <- tsdata[which(index(tsdata)=="1990-01-01"):nrow(tsdata)]
#tsdata<-tsdata[-c(1:4)]
tsdata<-tsdata[,! colnames(tsdata) %in% c("cpi", "lci", "rpe", "rpf")]

saveRDS(tsdata, file=here("data", "tsdata_us.rds"))





################################################### ESTIMATION ####
#equations
eq_wage <- tsdata$gw ~  tsdata$gw_l1+tsdata$gw_l2+tsdata$gw_l3+tsdata$gw_l4+tsdata$vu_l1+tsdata$vu_l2+tsdata$vu_l3+tsdata$vu_l4+tsdata$exp1_l1+tsdata$exp1_l2+tsdata$exp1_l3+
  tsdata$exp1_l4+tsdata$catch_up_l1+tsdata$catch_up_l2+tsdata$catch_up_l3+tsdata$catch_up_l4+tsdata$gpty_l1

eq_price<- tsdata$gp ~ tsdata$gp_l1+tsdata$gp_l2+tsdata$gp_l3+tsdata$gp_l4+tsdata$gw+tsdata$gw_l1+tsdata$gw_l2+tsdata$gw_l3+tsdata$gw_l4+tsdata$grpe+tsdata$grpe_l1+tsdata$grpe_l2+
  tsdata$grpe_l3+tsdata$grpe_l4+tsdata$grpf+tsdata$grpf_l1+tsdata$grpf_l2+tsdata$grpf_l3+tsdata$grpf_l4+tsdata$gpty_l1+tsdata$shortage+tsdata$shortage_l1+tsdata$shortage_l2+
  tsdata$shortage_l3+tsdata$shortage_l4

eq_exp10<- tsdata$exp10~tsdata$gp+tsdata$gp_l1+tsdata$gp_l2+tsdata$gp_l3+tsdata$gp_l4+tsdata$exp10_l1+tsdata$exp10_l2+tsdata$exp10_l3+tsdata$exp10_l4-1

eq_exp1<- tsdata$exp1~ tsdata$exp1_l1+tsdata$exp1_l2+tsdata$exp1_l3+tsdata$exp1_l4+tsdata$exp10+tsdata$exp10_l1+tsdata$exp10_l2+tsdata$exp10_l3+tsdata$exp10_l4+tsdata$gp+tsdata$gp_l1+
  tsdata$gp_l2+tsdata$gp_l3+tsdata$gp_l4-1

#separate estimations for two reasons: (i) systemfit does not support data subsetting (ii) exporting of tables

#for bootstrapping one needs to estimate on full sample! thus this code draws from bl_be_us_1 (full sample specification)

restrict_gw<-c(0,1,1,1,1,0,0,0,0,1,1,1,1,0,0,0,0,0)

restrict_gp<-c(0,1,1,1,1,1,1,1,1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0)

restrict_exp10<-c(1,1,1,1,1,1,1,1,1)

restrict_exp1<-c(1,1,1,1,1,1,1,1,1,1,1,1,1,1)

gp_eq<-systemfit(eq_price, method="OLS", data=tsdata, restrict.matrix=restrict_gp, restrict.rhs=1)

gw_eq<-systemfit(eq_wage, method="OLS", data=tsdata, restrict.matrix=restrict_gw, restrict.rhs=1)

exp10_eq<-systemfit(eq_exp10, method="OLS", data=tsdata, restrict.matrix=restrict_exp10, restrict.rhs=1)

exp1_eq<-systemfit(eq_exp1, method="OLS", data=tsdata, restrict.matrix=restrict_exp1, restrict.rhs=1)

C_gp<-coef(gp_eq)
C_gw<-coef(gw_eq)
C_exp10<-coef(exp10_eq)
C_exp1<-coef(exp1_eq)

summary(gp_eq)
summary(gw_eq)
summary(exp10_eq)
summary(exp1_eq)




################################################### BOOSTRAP SIMULATIONS ###########

#function to bootstrap residuals (in practice, we boostrap rows so as to preserve horizontal correlation in the residuals matrix)
bootstrap_residuals<-function(original_residuals){
  row_index <- sample(1:nrow(tsdata), nrow(tsdata), replace = TRUE) #this is a vector of the same length of tsdata. each value has a number which I use to pick residuals from the residuals matrix
  # Create a df to store resampled residuals
  resampled_residuals<-data.frame(
    gw=rep(NA, nrow(tsdata)),
    gp=rep(NA, nrow(tsdata)),
    exp10=rep(NA, nrow(tsdata)),
    exp1=rep(NA, nrow(tsdata))
  )
  #here i randomly assign to each row j the k=row_index[j]th row of the of residuals matrix
  for (j in (1:nrow(tsdata))){
    resampled_residuals$gw[j]=original_residuals$gw[row_index[j]]
    resampled_residuals$gp[j]=original_residuals$gp[row_index[j]]
    resampled_residuals$exp10[j]=original_residuals$exp10[row_index[j]]
    resampled_residuals$exp1[j]=original_residuals$exp1[row_index[j]]
  }
  return(resampled_residuals)
  
}

#simulation, allowing for resampled residuals into the function 
simulation_bootstrap<-function(tsdata, tsdata_out, position, s, C_gp, C_gw, C_exp1, C_exp10, gw_eq, 
                               gp_eq, exp1_eq, exp10_eq, resampled_residuals){
  for (i in (position):(position+(length_sim-1))) {
    if (i==position){
      #(2)baseline
      if (s==2){
        tsdata$mu<-0
        energy<-0
        food<-0
        vu<-0
        short<-0
      }
      #(3_i)shocks
      else if(s==3){
        tsdata$mu[position]<-sd(tsdata[121:133]$grpe)
        energy<-1
        food<-0
        vu<-0
        short<-0
      }
      else if (s==4){
        tsdata$mu[position]<-sd(tsdata[121:133]$grpf)
        energy<-0
        food<-1
        vu<-0
        short<-0
      }
      else if (s==5){
        tsdata$mu[position]<-sd(tsdata[121:133]$shortage)
        energy<-0
        food<-0
        vu<-0
        short<-1
      }
      else if (s==6){
        tsdata$mu<-sd(tsdata[121:133]$vu)
        energy<-0
        food<-0
        vu<-1
        short<-0
      }
    }
    
    #exogenous variables
    tsdata[i, "grpe_f"][[1]]<-tsdata[i, "grpe"][[1]]+tsdata[i, "mu"][[1]]*energy
    tsdata[i, "grpf_f"][[1]]<-tsdata[i, "grpf"][[1]]+tsdata[i, "mu"][[1]]*food
    tsdata[i, "vu_f"][[1]]<-tsdata[i, "vu"][[1]]+tsdata[i, "mu"][[1]]*vu
    tsdata[i, "shortage_f"][[1]]<-tsdata[i, "shortage"][[1]]+tsdata[i, "mu"][[1]]*short
    #1:wage equation
    tsdata[i, "gw_f"][[1]]<-C_gw[[1]]+C_gw[[2]]*tsdata[i-1, "gw_f"][[1]]+C_gw[[3]]*tsdata[i-2, "gw_f"][[1]]+
      C_gw[[4]]*tsdata[i-3, "gw_f"][[1]]+C_gw[[5]]*tsdata[i-4, "gw_f"][[1]]+C_gw[[6]]*tsdata[i-1, "vu_f"][[1]]+
      C_gw[[7]]*tsdata[i-2, "vu_f"][[1]]+C_gw[[8]]*tsdata[i-3, "vu_f"][[1]]+C_gw[[9]]*tsdata[i-4, "vu_f"][[1]]+C_gw[[10]]*tsdata[i-1, "exp1_f"][[1]]+
      C_gw[[11]]*tsdata[i-2, "exp1_f"][[1]]+C_gw[[12]]*tsdata[i-3, "exp1_f"][[1]]+C_gw[[13]]*tsdata[i-4, "exp1_f"][[1]]+
      C_gw[[14]]*tsdata[i-1, "catch_up"][[1]]+C_gw[[15]]*tsdata[i-2, "catch_up"][[1]]+C_gw[[16]]*tsdata[i-3, "catch_up"][[1]]+
      C_gw[[17]]*tsdata[i-4, "catch_up"][[1]]+C_gw[[18]]*tsdata[i-1, "gpty"][[1]]+resampled_residuals$gw[i]*res
    #2:price equation
    tsdata[i, "gp_f"][[1]]<-C_gp[1]+C_gp[2]*tsdata[i-1, "gp_f"][[1]]+C_gp[3]*tsdata[i-2, "gp_f"][[1]]+
      C_gp[4]*tsdata[i-3, "gp_f"][[1]]+C_gp[5]*tsdata[i-4, "gp_f"][[1]]+C_gp[6]*tsdata[i, "gw_f"][[1]]+
      C_gp[7]*tsdata[i-1, "gw_f"][[1]]+C_gp[8]*tsdata[i-2, "gw_f"][[1]]+C_gp[9]*tsdata[i-3, "gw_f"][[1]]+
      C_gp[10]*tsdata[i-4, "gw_f"][[1]]+C_gp[11]*tsdata[i, "grpe_f"][[1]]+C_gp[12]*tsdata[i-1, "grpe_f"][[1]]+
      C_gp[13]*tsdata[i-2, "grpe_f"][[1]]+C_gp[14]*tsdata[i-3, "grpe_f"][[1]]+C_gp[15]*tsdata[i-4, "grpe_f"][[1]]+
      C_gp[16]*tsdata[i, "grpf_f"][[1]]+C_gp[17]*tsdata[i-1, "grpf_f"][[1]]+C_gp[18]*tsdata[i-2, "grpf_f"][[1]]+
      C_gp[19]*tsdata[i-3, "grpf_f"][[1]]+C_gp[20]*tsdata[i-4, "grpf_f"][[1]]+C_gp[21]*tsdata[i-1, "gpty"][[1]]+
      C_gp[22]*tsdata[i, "shortage_f"][[1]]+C_gp[23]*tsdata[i-1, "shortage_f"][[1]]+C_gp[24]*tsdata[i-2, "shortage_f"][[1]]+
      C_gp[25]*tsdata[i-3, "shortage_f"][[1]]+C_gp[26]*tsdata[i-4, "shortage_f"][[1]]+resampled_residuals$gp[i]*res
    #3:10y exp equation
    tsdata[i, "exp10_f"][[1]]<-C_exp10[1]*tsdata[i, "gp_f"][[1]]+C_exp10[2]*tsdata[i-1, "gp_f"][[1]]+C_exp10[3]*tsdata[i-2, "gp_f"][[1]]+
      C_exp10[4]*tsdata[i-3, "gp_f"][[1]]+C_exp10[5]*tsdata[i-4, "gp_f"][[1]]+C_exp10[6]*tsdata[i-1, "exp10_f"][[1]]+C_exp10[7]*tsdata[i-2, "exp10_f"][[1]]+
      C_exp10[8]*tsdata[i-3, "exp10_f"][[1]]+C_exp10[9]*tsdata[i-4, "exp10_f"][[1]]+resampled_residuals$exp10[i]*res
    #4:1y exp equation
    tsdata[i, "exp1_f"][[1]]<-C_exp1[1]*tsdata[i-1, "exp1_f"][[1]]+C_exp1[2]*tsdata[i-2, "exp1_f"][[1]]+
      C_exp1[3]*tsdata[i-3, "exp1_f"][[1]]+C_exp1[4]*tsdata[i-4, "exp1_f"][[1]]+C_exp1[5]*tsdata[i, "exp10_f"][[1]]+
      C_exp1[6]*tsdata[i-1, "exp10_f"][[1]]+C_exp1[7]*tsdata[i-2, "exp10_f"][[1]]+C_exp1[8]*tsdata[i-3, "exp10_f"][[1]]+
      C_exp1[9]*tsdata[i-4, "exp10_f"][[1]]+C_exp1[10]*tsdata[i, "gp_f"][[1]]+C_exp1[11]*tsdata[i-1, "gp_f"][[1]]+C_exp1[12]*tsdata[i-2, "gp_f"][[1]]+
      C_exp1[13]*tsdata[i-3, "gp_f"][[1]]+C_exp1[14]*tsdata[i-4, "gp_f"][[1]]+resampled_residuals$exp1[i]*res
    
  }
  if(s==2){
    tsdata_out$gp_f_baseline<-tsdata$gp_f[position:nrow(tsdata)]
    }
  else if(s==3){
    tsdata_out$response_1<-tsdata$gp_f[position:nrow(tsdata)]-tsoutput$median_2
  }
  else if(s==4){
    tsdata_out$response_2<-tsdata$gp_f[position:nrow(tsdata)]-tsoutput$median_2
  }
  else if (s==5){
    tsdata_out$response_3<-tsdata$gp_f[position:nrow(tsdata)]-tsoutput$median_2
  }
  else if(s==6){
    tsdata_out$response_4<-tsdata$gp_f[position:nrow(tsdata)]-tsoutput$median_2
  }
  return(tsdata_out)
}

#creation of objects for the iterations
results <- list()

original_residuals <- list(
  gw = residuals(gw_eq)[[1]],
  gp = residuals(gp_eq)[[1]],
  exp10 = residuals(exp10_eq)[[1]],
  exp1 = residuals(exp1_eq)[[1]]
)

length_sim=13
start_date<- as.Date("2020-01-01")
dates<-seq(start_date,length=length_sim, by="quarters")
position <- which(index(tsdata) == start_date)

tsoutput<-data.frame(
  col_1=c(rep(NA, length_sim))
)

#Residuals plot
plot_residuals<-data.frame(
  gw=NA,
  gp=NA,
  exp10=NA,
  exp1=NA
)
plot_residuals<-tail(plot_residuals, -1)

for (r in 1:1000){
  plot_residuals<-rbind(plot_residuals, bootstrap_residuals(original_residuals))
}

for (v in c("gw", "gp", "exp1", "exp10")){
  ggplot(plot_residuals, aes(x = plot_residuals[[v]])) +
    geom_density(fill="royalblue4", alpha=0.2) +  # Density plot with fill color and transparency
    labs(title = paste0("Density Plot of Residuals (",v,")"),
         x = "Residuals",
         y = "Density") +  # Adding titles and labels
    theme_minimal() +  # Using a minimal theme
    theme(
      plot.title = element_text(hjust = 0.5, size = 20, face = "bold"),  # Centering and styling the title
      axis.title = element_text(size = 15),  # Styling the axis titles
      axis.text = element_text(size = 12)  # Styling the axis text
    )
  ggsave(here("output", "output_us", "graphs_us","residuals", paste0("residuals",v, ".png")), width = 10, height = 8, dpi = 300)
  
}

#Iteration
rm=list("data")
v<-c(2,3,4,5,6)
gc()
for (s in v){
      gc()
      tsdata<-readRDS(file=here("data", "tsdata_us.rds"))
      #create simulated variables 
      tsdata$gp_f <- NA
      tsdata$gw_f <- NA
      tsdata$exp1_f <- NA
      tsdata$exp10_f<-NA
      tsdata$grpe_f<-NA
      tsdata$grpf_f<-NA
      tsdata$vu_f<-NA
      tsdata$shortage_f<-NA
      
      length_sim=13
      start_date<- as.Date("2020-01-01") #which date?
      dates<-seq(start_date,length=length_sim, by="quarters")
      position <- which(index(tsdata) == start_date)
      
      for(k in ((position-4):(position-1))){
          tsdata[k, "gp_f"][[1]]<-tsdata[k, "gp"][[1]]
          tsdata[k, "gw_f"][[1]]<-tsdata[k, "gw"][[1]]
          tsdata[k, "exp10_f"][[1]]<-tsdata[k, "exp10"][[1]]
          tsdata[k, "exp1_f"][[1]]<-tsdata[k, "exp1"][[1]]
          tsdata[k, "grpe_f"][[1]]<-tsdata[k, "grpe"][[1]]
          tsdata[k, "grpf_f"][[1]]<-tsdata[k, "grpf"][[1]]
          tsdata[k, "vu_f"][[1]]<-tsdata[k, "vu"][[1]]
          tsdata[k, "shortage_f"][[1]]<-tsdata[k, "shortage"][[1]]
      }
      
      tsdata$mu<-0
      energy<-0
      food<-0
      vu<-0
      short<-0
      res<-1
      n_bootstraps<-1000
      
      tsdata_out<-data.frame(
        col_1=c(rep(NA, length_sim))
      )
      tsdata_out <- xts(tsdata_out, order.by=dates) 
      
      for (b in 1:n_bootstraps) {
            # Resample residuals
            resampled_residuals <- bootstrap_residuals(original_residuals)
            # Run the simulation with the resampled residuals
            print(paste0("Iteration ",b, " scenario ", s))
            tsdata_out <- simulation_bootstrap(tsdata, tsdata_out, position, s, C_gp, C_gw, C_exp1, C_exp10, 
                                     gw_eq, gp_eq, exp1_eq, exp10_eq, 
                                     resampled_residuals)
            results[[b]]<- tsdata_out[,-1]
            matrix <- do.call(cbind, results)
            titles<-c("gp","exp1","exp10")
            names<-c(paste0("low_",s), paste0("median_",s), paste0("high_",s))
            tsoutput[[names[1]]] <- apply(matrix, 1, function(x) quantile(x, 0.025, na.rm = TRUE))
            tsoutput[[names[2]]] <- apply(matrix, 1, function(x) median(x, na.rm = TRUE))
            tsoutput[[names[3]]] <- apply(matrix, 1, function(x) quantile(x, 0.975, na.rm = TRUE))
      
      }
}
tsoutput <- xts(tsoutput, order.by=dates) 
tsoutput<- tsoutput[,-1]





################################################### GRAPHS#####
shock_time<-(1:length_sim)

ggplot(tsoutput, aes(x = shock_time)) +
  geom_ribbon(aes(ymin = low_3, ymax = high_3, fill = "95% CI"), alpha = 0.2, show.legend=FALSE) +
  geom_line(aes(y = median_3, color = "BA"), linewidth = 1, linetype = "solid") +
  geom_line(aes(y = low_3, color = "ci"), linewidth = 1, linetype = "dashed") +
  geom_line(aes(y = high_3, color = "ci"), linewidth = 1, linetype = "dashed") +
  labs(
    title = " ",
    x = "Quarters from shock",
    y = "Inflation (deviation from trend)",
    caption=paste0("Note: n. of iterations=", n_bootstraps)
  ) +
  ylim(-4, 4) +
  theme_minimal() +
  theme(
    legend.position = c(0.15, 0.95),  # Set the legend position (top-left)
    legend.title = element_blank(),
    legend.text = element_text(size = 8),
    axis.text = element_text(size = 12),
    axis.title = element_text(size = 10),
    plot.title = element_text(size = 12, hjust = 0.5)
  ) +
  scale_color_manual(
    values = c("forestgreen", "royalblue4"),
    labels = c("IRF", "95% CI")
  ) +
  scale_fill_manual(
    values = c("95% CI" = "royalblue4"),
  )


ggsave(here("output", "output_us", "graphs_us", "IRF_energy_us_boot.png"), width = 10, height = 8, dpi = 300)


ggplot(tsoutput, aes(x = shock_time, y=median_2)) +
  geom_ribbon(aes(ymin = low_4, ymax = high_4, fill = "95% CI"), alpha = 0.2, show.legend=FALSE) +
  geom_line(aes(y = median_4, color = "BA"), linewidth = 1, linetype = "solid") +
  geom_line(aes(y = low_4, color = "ci"), linewidth = 1, linetype = "dashed") +
  geom_line(aes(y = high_4, color = "ci"), linewidth = 1, linetype = "dashed") +
  labs(title=" ", 
       x = "Quarters from shock", 
       y = "Inflation (deviation from trend)",
       caption=paste0("Note: n. of iterations=", n_bootstraps)) +
  ylim(-3.5,4) +
  theme_minimal() +
  theme(
    legend.position = c(0.15, 0.95),  # Set the legend position (top-left)
    legend.title = element_blank(),
    legend.text = element_text(size = 8),
    axis.text = element_text(size = 12),
    axis.title = element_text(size = 10),
    plot.title = element_text(size = 12, hjust = 0.5)
  ) +
  scale_color_manual(
    values = c("chocolate2", "royalblue4"),
    labels = c("Food price shock", "95% CI")
  )+
  scale_fill_manual(
    values = c("95% CI" = "royalblue4"),
  )

ggsave(here("output", "output_us", "graphs_us", "IRF_food_us_boot.png"), width = 10, height = 8, dpi = 300)

ggplot(tsoutput, aes(x = shock_time, y=median_2)) +
  geom_ribbon(aes(ymin = low_5, ymax = high_5, fill = "95% CI"), alpha = 0.2, show.legend=FALSE) +
  geom_line(aes(y = median_5, color = "BA"), linewidth = 1, linetype = "solid") +
  geom_line(aes(y = low_5, color = "ci"), linewidth = 1, linetype = "dashed") +
  geom_line(aes(y = high_5, color = "ci"), linewidth = 1, linetype = "dashed") +
  labs(title=" ", x = "Quarters from shock", y = "Inflation(deviation from trend)",  
       caption=paste0("Note: n. of iterations=", n_bootstraps)) +
  ylim(-3.5,4) +
  theme_minimal() +
  theme(
    legend.position = c(0.15, 0.95),  # Set the legend position (top-left)
    legend.title = element_blank(),
    legend.text = element_text(size = 8),
    axis.text = element_text(size = 12),
    axis.title = element_text(size = 10),
    plot.title = element_text(size = 12, hjust = 0.5)
  ) +
  scale_color_manual(
    values = c("indianred3", "royalblue4"),
    labels = c("Shortage shock", "95% CI")
  )+
  scale_fill_manual(
    values = c("95% CI" = "royalblue4"),
  )

ggsave(here("output", "output_us", "graphs_us", "IRF_shortage_us_boot.png"), width = 10, height = 8, dpi = 300)


ggplot(tsoutput, aes(x = shock_time)) +
  geom_ribbon(aes(ymin = low_6, ymax = high_6, fill = "95% CI"), alpha = 0.2, show.legend=FALSE) +
  geom_line(aes(y = median_6, color = "BA"), linewidth = 1, linetype = "solid") +
  geom_line(aes(y = low_6, color = "ci"), linewidth = 1, linetype = "dashed") +
  geom_line(aes(y = high_6, color = "ci"), linewidth = 1, linetype = "dashed") +
  labs(
    title = " ",
    x = "Quarters from shock",
    y = "Inflation (deviation from trend)",
    caption=paste0("Note: n. of iterations=", n_bootstraps)
  ) +
  ylim(-4, 4) +
  theme_minimal() +
  theme(
    legend.position = c(0.15, 0.95),  # Set the legend position (top-left)
    legend.title = element_blank(),
    legend.text = element_text(size = 8),
    axis.text = element_text(size = 12),
    axis.title = element_text(size = 10),
    plot.title = element_text(size = 12, hjust = 0.5)
  ) +
  scale_color_manual(
    values = c("coral", "royalblue4"),
    labels = c("IRF", "95% CI")
  ) +
  scale_fill_manual(
    values = c("95% CI" = "royalblue4"),
  )

ggsave(here("output", "output_us", "graphs_us", "IRF_vu_us_boot.png"), width = 10, height = 8, dpi = 300)























  
  
  
  
  
  