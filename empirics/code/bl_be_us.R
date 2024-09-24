#What Caused U.S. Postpandemic Inflation? A Fiscal Extension of Bernanke and Blanchard (2023)
#Replication of Bernanke and Blanchard (2023)
#Author: Luigi Lorenzoni
#This version: 17/09/24


################################################### PREPARATION ####

#preliminary ops
rm(list=ls())
# packages used
listofpackages <- c("zoo","xts", "readxl", "dplyr","ellipse","reshape2","ggplot2", 
                    "systemfit", "vars", "texreg", "here", "tidyverse")

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
tsdata<-tsdata[,! colnames(tsdata) %in% c( "lci", "rpe", "rpf")]

saveRDS(tsdata, file=here("data", "tsdata_us.rds"))








################################################### GRAPHIC CHECKS ####

tsdata_a<-tsdata[80:133]
ggplot(tsdata_a, aes(x = dates[80:133], y=cpi)) +
  geom_line(aes(y = cpi, color = "CPI"), linewidth = 1, linetype = "solid") +
  geom_line(aes(y = energy_cpi, color = "Energy CPI"), linewidth = 1, linetype = "solid") +
  geom_line(aes(y = food_cpi, color = " Food CPI"), linewidth = 1, linetype = "solid") +
  labs(title="", x = "Date", y = "CPI", caption="Note: 1982-84=100") +
  theme_minimal() +
  theme(
    legend.position = c(0.15, 0.95),  # Set the legend position (top-left)
    legend.title = element_blank(),
    legend.text = element_text(size = 8),
    axis.text = element_text(size = 8),
    axis.title = element_text(size = 10),
    plot.title = element_text(size = 12, hjust = 0.5)
  ) +
  scale_color_manual(
    values = c("blue", "forestgreen", "coral"),
    labels = c("CPI", "Energy CPI", "Food CPI")
  )

ggsave(here("output","output_us","graphs_us", "cpi_us.png"), width = 10, height = 8, dpi = 300)

ggplot(tsdata, aes(x = dates, y=cpi)) +
  geom_line(aes(y = shortage, color = "CPI"), linewidth = 1, linetype = "solid") +
  labs(title="", x = "Date", y = "Index") +
  theme_minimal() +
  theme(
    legend.position = c(0.15, 0.95),  # Set the legend position (top-left)
    legend.title = element_blank(),
    legend.text = element_text(size = 8),
    axis.text = element_text(size = 8),
    axis.title = element_text(size = 10),
    plot.title = element_text(size = 12, hjust = 0.5)
  ) +
  scale_color_manual(
    values = c("blue"),
    labels = c("shortage")
  )

ggsave(here("output","output_us","graphs_us", "shortages_us.png"), width = 10, height = 8, dpi = 300)



#expectations
ggplot(tsdata, aes(x = dates, y=exp1)) +
  geom_line(aes(y = exp1, color = "BA"), linewidth = 1, linetype = "solid") +
  geom_line(aes(y = exp10, color = "VaR"), linewidth = 1, linetype = "solid") +
  labs(title="Inflation expectations: 1 year and 10 years", x = "Date", y = "Inflation Expectations") +
  ylim(-1, 5) +
  theme_minimal() +
  theme(
    legend.position = c(0.15, 0.95),  # Set the legend position (top-left)
    legend.title = element_blank(),
    legend.text = element_text(size = 8),
    axis.text = element_text(size = 8),
    axis.title = element_text(size = 10),
    plot.title = element_text(size = 12, hjust = 0.5)
  ) +
  scale_color_manual(
    values = c("blue", "deeppink4"),
    labels = c("exp1", "exp10")
  )

ggsave(here("output","output_us","graphs_us", "expectations_us.png"), width = 10, height = 8, dpi = 300)

#inflation
ggplot(tsdata, aes(x = dates, y=gp)) +
  geom_line(aes(y = gp, color = "BA"), linewidth = 1, linetype = "solid") +
  labs(title="Price Inflation", x = "Date", y = "Inflation") +
  ylim(-5, 12) +
  theme_minimal() +
  theme(
    legend.position = c(0.15, 0.95),  # Set the legend position (top-left)
    legend.title = element_blank(),
    legend.text = element_text(size = 8),
    axis.text = element_text(size = 8),
    axis.title = element_text(size = 10),
    plot.title = element_text(size = 12, hjust = 0.5)
  ) +
  scale_color_manual(
    values = c("dodgerblue4"),
    labels = c("gp")
  )
ggsave(here("output", "output_us", "graphs_us", "gp_us.png"), width = 10, height = 8, dpi = 300)

#wage inflation
ggplot(tsdata, aes(x = dates, y=gw)) +
  geom_line(aes(y = gw, color = "brown2"), linewidth = 1, linetype = "solid") +
  labs(title="Wage inflation", x = "Date", y = "% Inflation") +
  ylim(-7, 12) +
  theme_minimal() +
  theme(
    legend.position = c(0.15, 0.95),  # Set the legend position (top-left)
    legend.title = element_blank(),
    legend.text = element_text(size = 8),
    axis.text = element_text(size = 8),
    axis.title = element_text(size = 10),
    plot.title = element_text(size = 12, hjust = 0.5)
  ) +
  scale_color_manual(
    values = c("brown2"),
    labels = c("gw")
  )

ggsave(here("output", "output_us", "graphs_us", "gw_us.png"), width = 10, height = 8, dpi = 300)


#productivity growth

ggplot(tsdata, aes(x = dates, y=gpty)) +
  geom_line(aes(y = gpty, color = "BA"), linewidth = 1, linetype = "solid") +
  labs(title= "Productivity growth", x = "Date", y = "% growth") +
  ylim(-2, 5) +
  theme_minimal() +
  theme(
    legend.position = c(0.15, 0.95),  # Set the legend position (top-left)
    legend.title = element_blank(),
    legend.text = element_text(size = 8),
    axis.text = element_text(size = 8),
    axis.title = element_text(size = 10),
    plot.title = element_text(size = 12, hjust = 0.5)
  ) +
  scale_color_manual(
    values = c("darkorange4"),
    labels = c("gpty")
  )

ggsave(here("output", "output_us", "graphs_us", "gpty_us.png"), width = 10, height = 8, dpi = 300)

#v/u

ggplot(tsdata, aes(x = dates, y=vu)) +
  geom_line(aes(y = vu, color = "BA"), linewidth = 1, linetype = "solid") +
  labs(title= "Vacancy/Unemployment ratio", x = "Date", y = "V/U ") +
  ylim(0, 2) +
  theme_minimal() +
  theme(
    legend.position = c(0.15, 0.95),  # Set the legend position (top-left)
    legend.title = element_blank(),
    legend.text = element_text(size = 8),
    axis.text = element_text(size = 8),
    axis.title = element_text(size = 10),
    plot.title = element_text(size = 12, hjust = 0.5)
  ) +
  scale_color_manual(
    values = c("coral2"),
    labels = c("vu")
  )

ggsave(here("output", "output_us", "graphs_us", "vu_us.png"), width = 10, height = 8, dpi = 300)


ggplot(tsdata, aes(x = dates, y=grpe)) +
  geom_line(aes(y = grpe, color = "BA"), linewidth = 1, linetype = "solid") +
  labs(title="Relative price inflation: energy", x = "Date", y = "% inflation") +
  theme_minimal() +
  theme(
    legend.position = c(0.15, 0.95),  # Set the legend position (top-left)
    legend.title = element_blank(),
    legend.text = element_text(size = 8),
    axis.text = element_text(size = 8),
    axis.title = element_text(size = 10),
    plot.title = element_text(size = 12, hjust = 0.5)
  ) +
  scale_color_manual(
    values = c("forestgreen"),
    labels = c("grpe")
  )

ggsave(here("output", "output_us", "graphs_us", "grpe_us.png"), width = 10, height = 8, dpi = 300)

ggplot(tsdata, aes(x = dates, y=grpf)) +
  geom_line(aes(y = grpf, color = "VaR"), linewidth = 1, linetype = "solid") +
  labs(title="Relative price inflation: food", x = "Date", y = "% inflation") +
  ylim(-10, 10) +
  theme_minimal() +
  theme(
    legend.position = c(0.15, 0.95),  # Set the legend position (top-left)
    legend.title = element_blank(),
    legend.text = element_text(size = 8),
    axis.text = element_text(size = 8),
    axis.title = element_text(size = 10),
    plot.title = element_text(size = 12, hjust = 0.5)
  ) +
  scale_color_manual(
    values = c("chocolate2"),
    labels = c("grpf")
  )

ggsave(here("output", "output_us", "graphs_us", "grpf_us.png"), width = 10, height = 8, dpi = 300)




################################################### ESTIMATION#################################################

#equations
eq_wage <- tsdata$gw ~  tsdata$gw_l1+tsdata$gw_l2+tsdata$gw_l3+tsdata$gw_l4+tsdata$vu_l1+tsdata$vu_l2+tsdata$vu_l3+tsdata$vu_l4+tsdata$exp1_l1+tsdata$exp1_l2+tsdata$exp1_l3+
  tsdata$exp1_l4+tsdata$catch_up_l1+tsdata$catch_up_l2+tsdata$catch_up_l3+tsdata$catch_up_l4+tsdata$gpty_l1

eq_price<- tsdata$gp ~ tsdata$gp_l1+tsdata$gp_l2+tsdata$gp_l3+tsdata$gp_l4+tsdata$gw+tsdata$gw_l1+tsdata$gw_l2+tsdata$gw_l3+tsdata$gw_l4+tsdata$grpe+tsdata$grpe_l1+tsdata$grpe_l2+
  tsdata$grpe_l3+tsdata$grpe_l4+tsdata$grpf+tsdata$grpf_l1+tsdata$grpf_l2+tsdata$grpf_l3+tsdata$grpf_l4+tsdata$gpty_l1+tsdata$shortage+tsdata$shortage_l1+tsdata$shortage_l2+
  tsdata$shortage_l3+tsdata$shortage_l4

eq_exp10<- tsdata$exp10~tsdata$gp+tsdata$gp_l1+tsdata$gp_l2+tsdata$gp_l3+tsdata$gp_l4+tsdata$exp10_l1+tsdata$exp10_l2+tsdata$exp10_l3+tsdata$exp10_l4-1

eq_exp1<- tsdata$exp1~ tsdata$exp1_l1+tsdata$exp1_l2+tsdata$exp1_l3+tsdata$exp1_l4+tsdata$exp10+tsdata$exp10_l1+tsdata$exp10_l2+tsdata$exp10_l3+tsdata$exp10_l4+tsdata$gp+tsdata$gp_l1+
  tsdata$gp_l2+tsdata$gp_l3+tsdata$gp_l4-1

#names vector
names_gw=c("Constant","gw_l1","gw_l2","gw_l3","gw_l4","vu_l1", "vu_l2", "vu_l3","vu_l4","exp1_l1","exp1_l2",
           "exp1_l3","exp1_l4", "catch_up_l1","catch_up_l2", "catch_up_l3", "catch_up_l4", "gpty_l1")

names_gp=c("Constant","gp_l1","gp_l2","gp_l3","gp_l4","gw","gw_l1","gw_l2","gw_l3","gw_l4","grpe","grpe_l1",
              "grpe_l2","grpe_l3","grpe_l4","grpf","grpf_l1","grpf_l2","grpf_l3","grpf_l4","gpty_l1","shortage",
              "shortage_l1","shortage_l2","shortage_l3","shortage_l4")

names_exp10=c("gp","gp_l1","gp_l2","gp_l3","gp_l4", "exp10_l1","exp10_l2","exp10_l3","exp10_l4")

names_exp1=c("exp1_l1","exp1_l2","exp1_l3","exp1_l4","exp10","exp10_l1","exp10_l2","exp10_l3","exp10_l4","gp",
            "gp_l1","gp_l2","gp_l3","gp_l4" )


# Unrestricted estimation
system<-list(eq_wage, eq_exp10, eq_exp1)
sys_unr <- systemfit(system, "OLS", data = tsdata)
summary(sys_unr)


#restricted estimation by specified method, lag 4 
#separate estimations for two reasons: (i) systemfit does not support data subsetting (ii) exporting of tables

# restrictM_l4 <- matrix(c(0,1,1,1,1,0,0,0,0,1,1,1,1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0, 
#                          0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,1,1,1,1,1,1,1,1,1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
#                          0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,1,1,1,1,1,1,1,1,1,1,1,1,1,1),
#                        byrow=TRUE, nrow=3, ncol=41)
# r_rhs<-c(1,1,1)
#separate estimations for two reasons: (i) systemfit does not support data subsetting (ii) exporting of tables

restrict_gw<-c(0,1,1,1,1,0,0,0,0,1,1,1,1,0,0,0,0,0)

restrict_gp<-c(0,1,1,1,1,1,1,1,1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0)

restrict_exp10<-c(1,1,1,1,1,1,1,1,1)

restrict_exp1<-c(1,1,1,1,1,1,1,1,1,1,1,1,1,1)

gp_eq<-systemfit(eq_price, method="OLS", data=tsdata, restrict.matrix=restrict_gp, restrict.rhs=1)
summary(gp_eq)

gp_eq_1<-systemfit(eq_price, method="OLS", data=tsdata)

#here we restrict the sample to pre-covid, as in the paper
tsdata<-tsdata[1:120]

gw_eq<-systemfit(eq_wage, method="OLS", data=tsdata, restrict.matrix=restrict_gw, restrict.rhs=1)

gw_eq_1<-systemfit(eq_wage, method="OLS", data=tsdata)

exp10_eq<-systemfit(eq_exp10, method="OLS", data=tsdata, restrict.matrix=restrict_exp10, restrict.rhs=1)

exp10_eq_1<-systemfit(eq_exp10, method="OLS", data=tsdata)

exp1_eq<-systemfit(eq_exp1, method="OLS", data=tsdata, restrict.matrix=restrict_exp1, restrict.rhs=1)

exp1_eq_1<-systemfit(eq_exp1, method="OLS", data=tsdata)

C_gp<-coef(gp_eq)
C_gw<-coef(gw_eq)
C_exp10<-coef(exp10_eq)
C_exp1<-coef(exp1_eq)

summary(gp_eq)
summary(gw_eq)
summary(exp10_eq)
summary(exp1_eq)

linearHypothesis(gw_eq_1, restrict_gw, 1, test="F")
linearHypothesis(gp_eq_1, restrict_gp, 1, test="F")
linearHypothesis(exp1_eq_1, restrict_exp1, 1, test="F")
linearHypothesis(exp10_eq_1, restrict_exp10, 1, test="F")


# export tables
texreg(gp_eq, here("output", "output_us","tex_us", "gp_us.tex"), stars=c(0.01, 0.05, 0.1), single.row=T, table = FALSE, center=FALSE,
       custom.coef.names = names_gp, custom.model.names = "Price equation" )
texreg(gw_eq, file=here("output", "output_us", "tex_us","gw_us.tex"), stars=c(0.01, 0.05, 0.1), single.row=T,table = FALSE, center=FALSE,
       custom.coef.names = names_gw, custom.model.names = "Wage equation" )
texreg(exp10_eq, file=here("output", "output_us", "tex_us", "exp10_us.tex"), stars=c(0.01, 0.05, 0.1),single.row=T, table = FALSE, center=FALSE,
       custom.coef.names = names_exp10, custom.model.names = "10y exp equation" )
texreg(exp1_eq, file=here("output", "output_us","tex_us", "exp1_us.tex"), stars=c(0.01, 0.05, 0.1), single.row=T,table = FALSE, center=FALSE,
       custom.coef.names = names_exp1, custom.model.names = "1y exp equation" )



#coefplot
equations<-c("gp_eq", "gw_eq", "exp10_eq", "exp1_eq")
endogenous<-c("gp", "gw", "exp10", "exp1")
titles<-c("Price", "Wage", "10y expectations", "1y expectations")
for (v in endogenous){
  coef_df <- read_xlsx(here("auxil", paste0("coef_",v,".xlsx")), col_names=T)
  var_name<-paste0("names_",v)
  coef_df$coef<-get(var_name)
  var_name<-paste0("C_",v)
  coef_df$beta_r<-get(var_name)
  eq<-equations[which(endogenous == v)]
  coef_df$conf_low<-confint(get(eq))[, "2.5 %"]
  coef_df$conf_high <- confint(get(eq))[, "97.5 %"]
  
  plot<-ggplot(coef_df, aes(x = coef, y = beta_r)) +
    geom_point(aes(y=beta_r, shape = "Luigi", color="Luigi"), size = 3) +  
    geom_errorbar(aes(ymin = conf_low, ymax = conf_high), width = 0.2) +
    geom_point(aes(y = beta, shape = "Blanchard-Bernanke (2023)", color ="Blanchard-Bernanke (2023)"), size = 3) +  
    theme_minimal() +
    xlab("Variable") + ylab("Estimate") +
    theme(axis.text.x = element_text(angle = 45, hjust = 1),
          legend.position = "top", 
          legend.title = element_blank(),
          legend.text = element_text(size = 8),
          axis.text = element_text(size = 8),
          axis.title = element_text(size = 10),
          plot.title = element_text(size = 12, hjust = 0.5)) +
    scale_shape_manual(name = "Point Type", values = c("Luigi" = 0, "Blanchard-Bernanke (2023)" = 10)) +
    scale_color_manual(name = "Point Type", values = c("Luigi" = "blue", "Blanchard-Bernanke (2023)" = "red"))
  
  print(plot)
  ggsave(here("output","output_us","graphs_us", "coefplot_us", paste0(v,"_coefs.png")), width = 10, height = 8, dpi = 320)
  
  
}



################################################### SIMULATIONS ############################################

#simulation
simulation<-function(tsdata, tsdata_out, position, s, C_gp, C_gw, C_exp1, C_exp10, energy, food, vu, short, gw_eq, gp_eq, exp1_eq, exp10_eq){
for (i in (position):(position+(length_sim-1))) {
  if (i==position){
    #(2)baseline
    if (s==2){
      tsdata$mu<-0
      energy<-0
      food<-0
      vu<-0
      short<-0
      res<-0
    }
    #(3_i)shocks
    else if(s==3){
      tsdata$mu[position]<-sd(tsdata[121:133]$grpe)
      energy<-1
      food<-0
      vu<-0
      short<-0
      res<-0
    }
    else if (s==4){
      tsdata$mu[position]<-sd(tsdata[121:133]$grpf)
      energy<-0
      food<-1
      vu<-0
      short<-0
      res<-0
    }
    else if (s==5){
      tsdata$mu[position]<-sd(tsdata[121:133]$shortage)
      energy<-0
      food<-0
      vu<-0
      short<-1
      res<-0
    }
    else if (s==6){
      tsdata$mu<-sd(tsdata[121:133]$vu)
      energy<-0
      food<-0
      vu<-1
      short<-0
      res<-0
    }
  }
  print(paste0("Simulating period ", index(tsdata[i]), " ,scenario ", s))
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
    C_gw[[17]]*tsdata[i-4, "catch_up"][[1]]+C_gw[[18]]*tsdata[i-1, "gpty"][[1]]
  #2:price equation
  tsdata[i, "gp_f"][[1]]<-C_gp[1]+C_gp[2]*tsdata[i-1, "gp_f"][[1]]+C_gp[3]*tsdata[i-2, "gp_f"][[1]]+
    C_gp[4]*tsdata[i-3, "gp_f"][[1]]+C_gp[5]*tsdata[i-4, "gp_f"][[1]]+C_gp[6]*tsdata[i, "gw_f"][[1]]+
    C_gp[7]*tsdata[i-1, "gw_f"][[1]]+C_gp[8]*tsdata[i-2, "gw_f"][[1]]+C_gp[9]*tsdata[i-3, "gw_f"][[1]]+
    C_gp[10]*tsdata[i-4, "gw_f"][[1]]+C_gp[11]*tsdata[i, "grpe_f"][[1]]+C_gp[12]*tsdata[i-1, "grpe_f"][[1]]+
    C_gp[13]*tsdata[i-2, "grpe_f"][[1]]+C_gp[14]*tsdata[i-3, "grpe_f"][[1]]+C_gp[15]*tsdata[i-4, "grpe_f"][[1]]+
    C_gp[16]*tsdata[i, "grpf_f"][[1]]+C_gp[17]*tsdata[i-1, "grpf_f"][[1]]+C_gp[18]*tsdata[i-2, "grpf_f"][[1]]+
    C_gp[19]*tsdata[i-3, "grpf_f"][[1]]+C_gp[20]*tsdata[i-4, "grpf_f"][[1]]+C_gp[21]*tsdata[i-1, "gpty"][[1]]+
    C_gp[22]*tsdata[i, "shortage_f"][[1]]+C_gp[23]*tsdata[i-1, "shortage_f"][[1]]+C_gp[24]*tsdata[i-2, "shortage_f"][[1]]+
    C_gp[25]*tsdata[i-3, "shortage_f"][[1]]+C_gp[26]*tsdata[i-4, "shortage_f"][[1]]+residuals(gp_eq)[[1]][i]*res
  #3:10y exp equation
  tsdata[i, "exp10_f"][[1]]<-C_exp10[1]*tsdata[i, "gp_f"][[1]]+C_exp10[2]*tsdata[i-1, "gp_f"][[1]]+C_exp10[3]*tsdata[i-2, "gp_f"][[1]]+
    C_exp10[4]*tsdata[i-3, "gp_f"][[1]]+C_exp10[5]*tsdata[i-4, "gp_f"][[1]]+C_exp10[6]*tsdata[i-1, "exp10_f"][[1]]+C_exp10[7]*tsdata[i-2, "exp10_f"][[1]]+
    C_exp10[8]*tsdata[i-3, "exp10_f"][[1]]+C_exp10[9]*tsdata[i-4, "exp10_f"][[1]]
  #4:1y exp equation
  tsdata[i, "exp1_f"][[1]]<-C_exp1[1]*tsdata[i-1, "exp1_f"][[1]]+C_exp1[2]*tsdata[i-2, "exp1_f"][[1]]+
    C_exp1[3]*tsdata[i-3, "exp1_f"][[1]]+C_exp1[4]*tsdata[i-4, "exp1_f"][[1]]+C_exp1[5]*tsdata[i, "exp10_f"][[1]]+
    C_exp1[6]*tsdata[i-1, "exp10_f"][[1]]+C_exp1[7]*tsdata[i-2, "exp10_f"][[1]]+C_exp1[8]*tsdata[i-3, "exp10_f"][[1]]+
    C_exp1[9]*tsdata[i-4, "exp10_f"][[1]]+C_exp1[10]*tsdata[i, "gp_f"][[1]]+C_exp1[11]*tsdata[i-1, "gp_f"][[1]]+C_exp1[12]*tsdata[i-2, "gp_f"][[1]]+
    C_exp1[13]*tsdata[i-3, "gp_f"][[1]]+C_exp1[14]*tsdata[i-4, "gp_f"][[1]]
  
}
  if(s==2){
    tsdata_out$gw_f_baseline<-tsdata$gw_f[position:nrow(tsdata)]
    tsdata_out$gp_f_baseline<-tsdata$gp_f[position:nrow(tsdata)]
    tsdata_out$exp10_f_baseline<-tsdata$exp10_f[position:nrow(tsdata)]
    tsdata_out$exp1_f_baseline<-tsdata$exp1_f[position:nrow(tsdata)]
  }
  else if(s==3){
    tsdata_out$gp_f_energy<-tsdata$gp_f[position:nrow(tsdata)]
    tsdata_out$exp1_f_energy<-tsdata$exp1_f[position:nrow(tsdata)]
    tsdata_out$exp10_f_energy<-tsdata$exp10_f[position:nrow(tsdata)]
  }
  else if(s==4){
    tsdata_out$gp_f_food<-tsdata$gp_f[position:nrow(tsdata)]
  }
  else if (s==5){
    tsdata_out$gp_f_shortage<-tsdata$gp_f[position:nrow(tsdata)]
  }
  else if(s==6){
    tsdata_out$gp_f_vu<-tsdata$gp_f[position:nrow(tsdata)]
  }
  return(tsdata_out)
}

length_sim=13
start_date<- as.Date("2020-01-01") #which date?
dates<-seq(start_date,length=length_sim, by="quarters")
position <- which(index(tsdata) == start_date)

tsdata_out<-data.frame(
  col_1=c(rep(NA, length_sim))
)

tsdata_out <- xts(tsdata_out, order.by=dates) 

for (s in (2:6)){ #iterate function over shock types
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
      tsdata_out<-simulation(tsdata, tsdata_out, position, s, C_gp, C_gw, C_exp1, C_exp10, energy, food, vu, short, gw_eq, gp_eq, exp1_eq, exp10_eq)
      if (s==2){
        saveRDS(tsdata_out, file=here("data", "tsdata_out_us.rds"))
      }

}

tsdata_out$response_1<-tsdata_out$gp_f_energy-tsdata_out$gp_f_baseline
tsdata_out$response_2<-tsdata_out$gp_f_food-tsdata_out$gp_f_baseline
tsdata_out$response_3<-tsdata_out$gp_f_shortage-tsdata_out$gp_f_baseline
tsdata_out$response_4<-tsdata_out$gp_f_vu-tsdata_out$gp_f_baseline
tsdata_out$response_5<-tsdata_out$exp1_f_energy-tsdata_out$exp1_f_baseline
tsdata_out$response_6<-tsdata_out$exp10_f_energy-tsdata_out$exp10_f_baseline



###################################################  GRAPHS ###########################################################
#graphs limited to covid sample, as in the paper
dates <-seq(as.Date("2020-01-01"),length=13, by="quarters")

ggplot(tsdata[121:133], aes(x = dates, y=gp)) +
  geom_line(aes(y = gp, color = "BA"), linewidth = 1, linetype = "solid") +
  geom_line(aes(y = tsdata_out$gp_f_baseline, color = "VaR"), linewidth = 1, linetype = "solid") +
  labs(title="Price inflation: real v. simulation", x = "Date", y = "Inflation") +
  ylim(-2, 12) +
  theme_minimal() +
  theme(
    legend.position = c(0.15, 0.95),  # Set the legend position (top-left)
    legend.title = element_blank(),
    legend.text = element_text(size = 8),
    axis.text = element_text(size = 8),
    axis.title = element_text(size = 10),
    plot.title = element_text(size = 12, hjust = 0.5)
  ) +
  scale_color_manual(
    values = c("dodgerblue", "coral3"),
    labels = c("Inflation", "Inflation simulation")
  )

ggsave(here("output", "output_us", "graphs_us", "gp_forecast_us.png"), width = 10, height = 8, dpi = 300)

ggplot(tsdata[121:133], aes(x = dates, y=gp)) +
  geom_line(aes(y = gp, color = "BA"), linewidth = 1, linetype = "solid") +
  geom_line(aes(y = tsdata_out$gp_f_check, color = "VaR"), linewidth = 1, linetype = "solid") +
  labs(title="Price inflation: check on simulation including estimated residuals", x = "Date", y = "Inflation") +
  ylim(-2, 12) +
  theme_minimal() +
  theme(
    legend.position = c(0.15, 0.95),  # Set the legend position (top-left)
    legend.title = element_blank(),
    legend.text = element_text(size = 8),
    axis.text = element_text(size = 8),
    axis.title = element_text(size = 10),
    plot.title = element_text(size = 12, hjust = 0.5)
  ) +
  scale_color_manual(
    values = c("dodgerblue", "coral3"),
    labels = c("Inflation", "Inflation simulation")
  )

ggsave(here("output", "output_us", "graphs_us", "gp_forecast_us_check.png"), width = 10, height = 8, dpi = 300)


ggplot(tsdata[121:133], aes(x = dates, y=gw)) +
  geom_line(aes(y = gw, color = "BA"), linewidth = 1, linetype = "solid") +
  geom_line(aes(y = tsdata_out$gw_f_baseline, color = "VaR"), linewidth = 1, linetype = "solid") +
  labs(title="Wage Inflation: real v. simulated", x = "Date", y = "Inflation") +
  ylim(-1, 7) +
  theme_minimal() +
  theme(
    legend.position = c(0.15, 0.95),  # Set the legend position (top-left)
    legend.title = element_blank(),
    legend.text = element_text(size = 8),
    axis.text = element_text(size = 8),
    axis.title = element_text(size = 10),
    plot.title = element_text(size = 12, hjust = 0.5)
  ) +
  scale_color_manual(
    values = c("dodgerblue", "coral3"),
    labels = c("Wage Inflation", "Wage Inflation simulation")
  )
ggsave(here("output", "output_us", "graphs_us", "gw_forecast_us.png"), width = 10, height = 8, dpi = 300)

ggplot(tsdata[121:133], aes(x = dates, y=exp1)) +
  geom_line(aes(y = exp1, color = "BA"), linewidth = 1, linetype = "solid") +
  geom_line(aes(y = tsdata_out$exp1_f_baseline, color = "VaR"), linewidth = 1, linetype = "solid") +
  labs(title="1y Expectations: real v. simulated", x = "Date", y = "Inflation expectation") +
  ylim(0, 6) +
  theme_minimal() +
  theme(
    legend.position = c(0.15, 0.95),  # Set the legend position (top-left)
    legend.title = element_blank(),
    legend.text = element_text(size = 8),
    axis.text = element_text(size = 8),
    axis.title = element_text(size = 10),
    plot.title = element_text(size = 12, hjust = 0.5)
  ) +
  scale_color_manual(
    values = c("dodgerblue", "coral3"),
    labels = c("1y expectations", "1y expectations simulation")
  )

ggsave(here("output", "output_us", "graphs_us", "exp1_forecast_us.png"), width = 10, height = 8, dpi = 300)


ggplot(tsdata[121:133], aes(x = dates, y=exp10)) +
  geom_line(aes(y = exp10, color = "BA"), linewidth = 1, linetype = "solid") +
  geom_line(aes(y = tsdata_out$exp10_f_baseline, color = "VaR"), linewidth = 1, linetype = "solid") +
  labs(title= "10y Expectations: real v. simulated", x = "Date", y = "Inflation expectation") +
  ylim(1.1, 4) +
  theme_minimal() +
  theme(
    legend.position = c(0.15, 0.95),  # Set the legend position (top-left)
    legend.title = element_blank(),
    legend.text = element_text(size = 8),
    axis.text = element_text(size = 8),
    axis.title = element_text(size = 10),
    plot.title = element_text(size = 12, hjust = 0.5)
  ) +
  scale_color_manual(
    values = c("dodgerblue", "coral3"),
    labels = c("10y expectations", "10y expectations simulation")
  )

ggsave(here("output", "output_us", "graphs_us", "exp10_forecast_us.png"), width = 10, height = 8, dpi = 300)

shock_time<-(1:length_sim)
ggplot(tsdata_out, aes(x = shock_time, y=response)) +
  geom_line(aes(y = response_1, color = "BA"), linewidth = 1, linetype = "solid") +
  geom_line(aes(y = response_2, color = "Var"), linewidth = 1, linetype = "solid") +
  geom_line(aes(y = response_3, color = "VeZ"), linewidth = 1, linetype = "solid") +
  labs(title="Inflation IRFs: 1 sd exogenous shock", x = "Quarters from shock", y = "Inflation(deviation from trend)") +
  ylim(-0.75, 4) +
  theme_minimal() +
  theme(
    legend.position = c(0.15, 0.95),  # Set the legend position (top-left)
    legend.title = element_blank(),
    legend.text = element_text(size = 8),
    axis.text = element_text(size = 8),
    axis.title = element_text(size = 10),
    plot.title = element_text(size = 12, hjust = 0.5)
  ) +
  scale_color_manual(
    values = c("forestgreen", "brown3", "chocolate2"),
    labels = c("Energy shock","Food shock", "Shortage shock")
  )
ggsave(here("output", "output_us", "graphs_us", "IRFs_us.png"), width = 10, height = 8, dpi = 300)

ggplot(tsdata_out, aes(x = shock_time, y=response_2)) +
  geom_line(aes(y = response_4, color = "BA"), linewidth = 1, linetype = "solid") +
  labs(title="Inflation IRFs: 1 sd shock", x = "Quarters from shock", y = "Inflation(deviation from trend)") +
  ylim(0, 1) +
  theme_minimal() +
  theme(
    legend.position = c(0.15, 0.95),  # Set the legend position (top-left)
    legend.title = element_blank(),
    legend.text = element_text(size = 8),
    axis.text = element_text(size = 8),
    axis.title = element_text(size = 10),
    plot.title = element_text(size = 12, hjust = 0.5)
  ) +
  scale_color_manual(
    values = c("forestgreen"),
    labels = c("V/U shock")
  )

ggsave(here("output", "output_us", "graphs_us", "IRF_vu_us.png"), width = 10, height = 8, dpi = 300)

shock_time<-(1:length_sim)
ggplot(tsdata_out, aes(x = shock_time, y=response)) +
  geom_line(aes(y = response_5, color = "BA"), linewidth = 1, linetype = "solid") +
  geom_line(aes(y = response_6, color = "Var"), linewidth = 1, linetype = "solid") +
  labs(title="Expectations IRFs: 1 sd exogenous shock", x = "Quarters from shock", y = "Inflation exp(deviation from trend)") +
  ylim(-0.75, 4) +
  theme_minimal() +
  theme(
    legend.position = c(0.15, 0.95),  # Set the legend position (top-left)
    legend.title = element_blank(),
    legend.text = element_text(size = 8),
    axis.text = element_text(size = 8),
    axis.title = element_text(size = 10),
    plot.title = element_text(size = 12, hjust = 0.5)
  ) +
  scale_color_manual(
    values = c("forestgreen", "brown3"),
    labels = c("1y expectations","10y expectations")
  )

ggsave(here("output", "output_us", "graphs_us", "IRF_exp_us.png"), width = 10, height = 8, dpi = 300)






###################################################  HISTORICAL DECOMPOSITION   ######################################################################

rm(list=setdiff(ls(),c("C_gp", "C_gw", "C_exp1", "C_exp10", "tsdata")))

#simulation
simulation<-function(tsdata, tsdata_out, position, s, C_gp, C_gw, C_exp1, C_exp10){
  for (i in (position):(position+(length_sim-1))) {
    if (i==position){
      #baseline
      if (s==2){
        print("Baseline")
      }
      #energy
      else if(s==3){
        res<-0
        tsdata$grpe[position:nrow(tsdata)]<-0
      }
      #food
      else if (s==4){
        res<-0
        tsdata$grpf[position:nrow(tsdata)]<-0
      }
      #shortages
      else if (s==5){
        res<-0
        tsdata$shortage[position:nrow(tsdata)]<-mean(tsdata$shortage[1:(which(index(tsdata)=="2019-10-01"))])
      }
      #gpty
      else if (s==6){
        res<-0
        tsdata$gpty[position:nrow(tsdata)]<-mean(tsdata$gpty[1:(which(index(tsdata)=="2019-10-01"))])
      }
      #vu
      else if (s==7){
        res<-0
        tsdata$vu[position:nrow(tsdata)]<-tsdata$vu[which(index(tsdata)=="2019-10-01")]
      }
      #initial conditions
      else if(s==8){
        res<-0
        tsdata$grpe[position:nrow(tsdata)]<-0
        tsdata$grpf[position:nrow(tsdata)]<-0
        tsdata$shortage[position:nrow(tsdata)]<-mean(tsdata$shortage[1:(which(index(tsdata)=="2019-10-01"))])
        tsdata$gpty[position:nrow(tsdata)]<-mean(tsdata$gpty[1:which(index(tsdata)=="2019-10-01")])
        tsdata$vu[position:nrow(tsdata)]<-tsdata$vu[which(index(tsdata)=="2019-10-01")]
      }
    }
    print(paste0("Simulating period ", index(tsdata[i]), " ,scenario ", s))
    
    #exogenous
    tsdata[i, "grpe_f"][[1]]<-tsdata[i, "grpe"][[1]]
    tsdata[i, "grpf_f"][[1]]<-tsdata[i, "grpf"][[1]]
    tsdata[i, "vu_f"][[1]]<-tsdata[i, "vu"][[1]]
    tsdata[i, "shortage_f"][[1]]<-tsdata[i, "shortage"][[1]]

    #1:wage equation
    tsdata[i, "gw_f"][[1]]<-C_gw[[1]]+C_gw[[2]]*tsdata[i-1, "gw_f"][[1]]+C_gw[[3]]*tsdata[i-2, "gw_f"][[1]]+
      C_gw[[4]]*tsdata[i-3, "gw_f"][[1]]+C_gw[[5]]*tsdata[i-4, "gw_f"][[1]]+C_gw[[6]]*tsdata[i-1, "vu_f"][[1]]+
      C_gw[[7]]*tsdata[i-2, "vu_f"][[1]]+C_gw[[8]]*tsdata[i-3, "vu_f"][[1]]+C_gw[[9]]*tsdata[i-4, "vu_f"][[1]]+C_gw[[10]]*tsdata[i-1, "exp1_f"][[1]]+
      C_gw[[11]]*tsdata[i-2, "exp1_f"][[1]]+C_gw[[12]]*tsdata[i-3, "exp1_f"][[1]]+C_gw[[13]]*tsdata[i-4, "exp1_f"][[1]]+
      C_gw[[14]]*tsdata[i-1, "catch_up"][[1]]+C_gw[[15]]*tsdata[i-2, "catch_up"][[1]]+C_gw[[16]]*tsdata[i-3, "catch_up"][[1]]+
      C_gw[[17]]*tsdata[i-4, "catch_up"][[1]]+C_gw[[18]]*tsdata[i-1, "gpty"][[1]]
    #2:price equation
    tsdata[i, "gp_f"][[1]]<-C_gp[1]+C_gp[2]*tsdata[i-1, "gp_f"][[1]]+C_gp[3]*tsdata[i-2, "gp_f"][[1]]+
      C_gp[4]*tsdata[i-3, "gp_f"][[1]]+C_gp[5]*tsdata[i-4, "gp_f"][[1]]+C_gp[6]*tsdata[i, "gw_f"][[1]]+
      C_gp[7]*tsdata[i-1, "gw_f"][[1]]+C_gp[8]*tsdata[i-2, "gw_f"][[1]]+C_gp[9]*tsdata[i-3, "gw_f"][[1]]+
      C_gp[10]*tsdata[i-4, "gw_f"][[1]]+C_gp[11]*tsdata[i, "grpe_f"][[1]]+C_gp[12]*tsdata[i-1, "grpe_f"][[1]]+
      C_gp[13]*tsdata[i-2, "grpe_f"][[1]]+C_gp[14]*tsdata[i-3, "grpe_f"][[1]]+C_gp[15]*tsdata[i-4, "grpe_f"][[1]]+
      C_gp[16]*tsdata[i, "grpf_f"][[1]]+C_gp[17]*tsdata[i-1, "grpf_f"][[1]]+C_gp[18]*tsdata[i-2, "grpf_f"][[1]]+
      C_gp[19]*tsdata[i-3, "grpf_f"][[1]]+C_gp[20]*tsdata[i-4, "grpf_f"][[1]]+C_gp[21]*tsdata[i-1, "gpty"][[1]]+
      C_gp[22]*tsdata[i, "shortage_f"][[1]]+C_gp[23]*tsdata[i-1, "shortage_f"][[1]]+C_gp[24]*tsdata[i-2, "shortage_f"][[1]]+
      C_gp[25]*tsdata[i-3, "shortage_f"][[1]]+C_gp[26]*tsdata[i-4, "shortage_f"][[1]]
    #3:10y exp equation
    tsdata[i, "exp10_f"][[1]]<-C_exp10[1]*tsdata[i, "gp_f"][[1]]+C_exp10[2]*tsdata[i-1, "gp_f"][[1]]+C_exp10[3]*tsdata[i-2, "gp_f"][[1]]+
      C_exp10[4]*tsdata[i-3, "gp_f"][[1]]+C_exp10[5]*tsdata[i-4, "gp_f"][[1]]+C_exp10[6]*tsdata[i-1, "exp10_f"][[1]]+C_exp10[7]*tsdata[i-2, "exp10_f"][[1]]+
      C_exp10[8]*tsdata[i-3, "exp10_f"][[1]]+C_exp10[9]*tsdata[i-4, "exp10_f"][[1]]
    #4:1y exp equation
    tsdata[i, "exp1_f"][[1]]<-C_exp1[1]*tsdata[i-1, "exp1_f"][[1]]+C_exp1[2]*tsdata[i-2, "exp1_f"][[1]]+
      C_exp1[3]*tsdata[i-3, "exp1_f"][[1]]+C_exp1[4]*tsdata[i-4, "exp1_f"][[1]]+C_exp1[5]*tsdata[i, "exp10_f"][[1]]+
      C_exp1[6]*tsdata[i-1, "exp10_f"][[1]]+C_exp1[7]*tsdata[i-2, "exp10_f"][[1]]+C_exp1[8]*tsdata[i-3, "exp10_f"][[1]]+
      C_exp1[9]*tsdata[i-4, "exp10_f"][[1]]+C_exp1[10]*tsdata[i, "gp_f"][[1]]+C_exp1[11]*tsdata[i-1, "gp_f"][[1]]+C_exp1[12]*tsdata[i-2, "gp_f"][[1]]+
      C_exp1[13]*tsdata[i-3, "gp_f"][[1]]+C_exp1[14]*tsdata[i-4, "gp_f"][[1]]
    
  }
  if(s==2){
      tsdata_out$gp_real<-tsdata$gp_f[position:nrow(tsdata)]
      tsdata_out$gw_real<-tsdata$gw_f[position:nrow(tsdata)]
    }
    else if(s==3){
      tsdata_out$gp_energy<-tsdata$gp_f[position:nrow(tsdata)]
      tsdata_out$gw_energy<-tsdata$gw_f[position:nrow(tsdata)]
    }
    else if(s==4){
      tsdata_out$gp_food<-tsdata$gp_f[position:nrow(tsdata)]
      tsdata_out$gw_food<-tsdata$gw_f[position:nrow(tsdata)]
    }
    else if (s==5){
      tsdata_out$gp_short<-tsdata$gp_f[position:nrow(tsdata)]
      tsdata_out$gw_short<-tsdata$gw_f[position:nrow(tsdata)]
    }
    else if(s==6){
      tsdata_out$gp_prod<-tsdata$gp_f[position:nrow(tsdata)]
      tsdata_out$gw_prod<-tsdata$gw_f[position:nrow(tsdata)]
    }
    else if(s==7){
      tsdata_out$gp_vu<-tsdata$gp_f[position:nrow(tsdata)]
      tsdata_out$gw_vu<-tsdata$gw_f[position:nrow(tsdata)]
    }
    else if(s==8){
      tsdata_out$gp_init<-tsdata$gp_f[position:nrow(tsdata)]
      tsdata_out$gw_init<-tsdata$gw_f[position:nrow(tsdata)]
      }
    return(tsdata_out)
}

length_sim=13
start_date<- as.Date("2020-01-01") #which date?
dates<-seq(start_date,length=length_sim, by="quarters")
position <- which(index(tsdata) == start_date)

tsdata_out<-data.frame(
  col_1=c(rep(NA, length_sim))
)

tsdata_out <- xts(tsdata_out, order.by=dates) 


for (s in (2:8)){ 
  gc()
  #iterate function over shock types
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
  tsdata_out<-simulation(tsdata, tsdata_out, position, s, C_gp, C_gw, C_exp1, C_exp10)
  
}

tsdata_out$gp_energy<-tsdata_out$gp_real-tsdata_out$gp_energy
tsdata_out$gp_food<-tsdata_out$gp_real-tsdata_out$gp_food
tsdata_out$gp_short<-tsdata_out$gp_real-tsdata_out$gp_short
tsdata_out$gp_vu<-tsdata_out$gp_real-tsdata_out$gp_vu
tsdata_out$gp_prod<-tsdata_out$gp_real-tsdata_out$gp_prod
tsdata_out$gp_real<-tsdata$gp[121:133]


tsdata_out$gw_energy<-tsdata_out$gw_real-tsdata_out$gw_energy
tsdata_out$gw_food<-tsdata_out$gw_real-tsdata_out$gw_food
tsdata_out$gw_short<-tsdata_out$gw_real-tsdata_out$gw_short
tsdata_out$gw_vu<-tsdata_out$gw_real-tsdata_out$gw_vu
tsdata_out$gw_prod<-tsdata_out$gw_real-tsdata_out$gw_prod
tsdata_out$gw_real<-tsdata$gw[121:133]
tsdata_out$dates<-dates

tsdata_df <- data.frame(dates = index(tsdata_out), coredata(tsdata_out))
tsdata_df<-tsdata_df[,-2]


columns_to_remove <- grep("gw", names(tsdata_df), value = TRUE)
tsdata_df <- tsdata_df[ , !(names(tsdata_df) %in% columns_to_remove)]

data_long <- tsdata_df %>%
  pivot_longer(cols = -c(dates, gp_real), names_to = "variable", values_to = "value") %>%
  mutate(type = ifelse(value >= 0, "positive", "negative"),
         variable = factor(variable, levels = c("gp_energy", "gp_food", "gp_short", "gp_vu","gp_prod", "gp_init")))

# Plot
ggplot(data_long, aes(x = dates, y = value, fill = variable)) +
  geom_bar(data = subset(data_long, type == "positive"), stat = "identity") +
  geom_bar(data = subset(data_long, type == "negative"), stat = "identity") +
  geom_line(data = data_long, aes(x = dates, y = gp_real, group = 1), color = "black", linewidth = 1) +
  ylim(-8,13)+
  labs(x = "Date", y = "Inflation", title = " ") +
  theme_minimal() +
  theme(
    legend.title = element_blank(),
    legend.text = element_text(size = 8),
    axis.text = element_text(size = 14),
    axis.title = element_text(size = 10),
    plot.title = element_text(size = 12, hjust = 0.5)
  ) +
  scale_fill_manual(values = c("gp_prod"="forestgreen" ,"gp_init" = "gray36","gp_energy" = "royalblue4", "gp_food" = "orange2", 
                               "gp_short" = "darkslategray3",  "gp_vu" = "orangered3"),
                    labels = c("gp_prod"="Productivity","gp_energy" = "Energy", "gp_food" = "Food", 
                               "gp_short" = "Shortages", "gp_init" = "Initial Conditions", "gp_vu" = "V/U"))
  
ggsave(here("output","output_us", "graphs_us",paste0("gp_decomp_us.png")), width = 10, height = 8, dpi = 320)

tsdata_df <- data.frame(dates = index(tsdata_out), coredata(tsdata_out))
columns_to_remove <- grep("gp", names(tsdata_df), value = TRUE)
tsdata_df <- tsdata_df[ , !(names(tsdata_df) %in% columns_to_remove)]
tsdata_df<-tsdata_df[,-2]

data_long <- tsdata_df %>%
  pivot_longer(cols = -c(dates, gw_real), names_to = "variable", values_to = "value") %>%
  mutate(type = ifelse(value >= 0, "positive", "negative"),
         variable = factor(variable, levels = c("gw_energy", "gw_food", "gw_short", "gw_vu","gw_prod", "gw_init")))

# Plot
ggplot(data_long, aes(x = dates, y = value, fill = variable)) +
  geom_bar(data = subset(data_long, type == "positive"), stat = "identity") +
  geom_bar(data = subset(data_long, type == "negative"), stat = "identity") +
  geom_line(data = data_long, aes(x = dates, y = gw_real, group = 1), color = "black", linewidth = 1) +
  ylim(-8,13)+
  labs(x = "Date", y = "Wage Inflation", title = " ") +
  theme_minimal() +
  theme(
    legend.title = element_blank(),
    legend.text = element_text(size = 8),
    axis.text = element_text(size = 14),
    axis.title = element_text(size = 10),
    plot.title = element_text(size = 12, hjust = 0.5)
  ) +
  scale_fill_manual(values = c("gw_prod"="forestgreen" ,"gw_init" = "gray36","gw_energy" = "royalblue4", "gw_food" = "orange2", 
                               "gw_short" = "darkslategray3",  "gw_vu" = "orangered3"),
                    labels = c("gw_prod"="Productivity","gw_energy" = "Energy", "gw_food" = "Food", 
                               "gw_short" = "Shortages", "gw_init" = "Initial Conditions", "gw_vu" = "V/U"))

ggsave(here("output","output_us", "graphs_us",paste0("gw_decomp_us.png")), width = 10, height = 8, dpi = 320)
