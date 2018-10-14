rm(list = ls()) #delete workspace
setwd("~/Dropbox/___MA/social_RL_git/thesis_social_RL/simulated_agents")

library(readr)
library(tidyverse)
library(Rcpp)
library(ez)
# library(MASS)

#########TEST IF FITTED LEARNING RATES AND TEMPERATURES OF THE BEST FITTING MODEL TO THE SOCIAL DATA (RLW)
#########DIFFERS IN THE GROUPS AS CATEGORIZED BY MEANS OF COMPUTATIONAL PHENOTYPING
###########################################################################################################

#read in data
fit_RLW_6_social <-
  read_delim("~/Dropbox/___MA/social_RL_git/thesis_social_RL/simulated_agents/modelfit_agents_weight_24_30_new.txt",
             " ", col_names = F, trim_ws = TRUE)

names(fit_RLW_6_social)[1] <- "LL_RLW"
names(fit_RLW_6_social)[2] <- "alpha_fit_RLW"
names(fit_RLW_6_social)[3] <- "beta_fit_RLW"
names(fit_RLW_6_social)[4] <- "weight_fit_RLW"
names(fit_RLW_6_social)[5] <- "BIC_RLW"
names(fit_RLW_6_social)[6] <- "AIC_RLW"
names(fit_RLW_6_social)[7] <- "id_RLW"
names(fit_RLW_6_social)[8] <- "alpha_sim_RLW"
names(fit_RLW_6_social)[9] <- "beta_sim_RLW"
names(fit_RLW_6_social)[10] <- "weight_sim_RLW"


fit_RL_6_social <-
  read_delim("~/Dropbox/___MA/social_RL_git/thesis_social_RL/simulated_agents/modelfit_agents_standardtoweight_24_30_new.txt",
             " ", col_names = F, trim_ws = TRUE)

names(fit_RL_6_social)[1] <- "LL"
names(fit_RL_6_social)[2] <- "alpha_fit"
names(fit_RL_6_social)[3] <- "beta_fit"
names(fit_RL_6_social)[4] <- "BIC"
names(fit_RL_6_social)[5] <- "AIC"
names(fit_RL_6_social)[6] <- "id"
names(fit_RL_6_social)[7] <- "alpha_sim"
names(fit_RL_6_social)[8] <- "beta_sim"
#names(fit_RL_6_social)[9] <- "weight_sim"

df_fit_social <- cbind(fit_RL_6_social, fit_RLW_6_social)

df_fit_social$delta_BIC <- 
  df_fit_social$BIC - df_fit_social$BIC_RLW #calculate difference in BIC

df_fit_social$weight_sign <- 
  as.factor(ifelse(df_fit_social$weight_sim_RLW < 0, -1, 1)) #categorize by omega (positive/negative)

#plot shows that agents can be categorized by BIC difference
#BIC varies as a function of omega
#negative omega values --> RLW
#positive omega values --> RL
ggplot(aes(x = id, y = delta_BIC, fill = weight_sim_RLW), data = df_fit_social) +
  geom_bar(stat = "identity", alpha = 0.8) +
  scale_fill_gradient(low = "dodgerblue3", high = "sienna1", expression(paste("Fitted ", omega, " values")),
                       limits = c(-1, 1), breaks = c(-.75, -.25, .25, .75)) +
  guides(color = guide_colorbar(barwidth = 0.7, barheight = 10)) +
  ylab(expression(paste(Delta, "BIC (RL - RLW)"))) +
  xlab("Simulated agent") +
  scale_x_continuous(breaks = seq(5, 50, 5)) +
  theme_classic() +
  theme(axis.title.x = element_text(size = 15)) + 
  theme(axis.title.y = element_text(size = 15)) +
  theme(axis.text = element_text(size = 13, colour = "black")) +
  theme(legend.title = element_text(size = 15)) +
  theme(legend.text = element_text(size = 13))


#allows computational phenotyping
#1  = RLW
#-1 = RL
df_fit_social$group <- as.factor(ifelse(df_fit_social$delta_BIC > 0, 1, -1))

sum(df_fit_social$group == -1)

#compare fitted learning rates and temperatures of both groups
df_fit_compare <-
  df_fit_social %>% 
  select(alpha_fit_RLW, id, group, beta_fit_RLW, weight_fit_RLW)

#compare fitted learning rates of both groups
ggplot(aes(y = alpha_fit_RLW, color = group, x = group), data = df_fit_compare) +
  geom_boxplot() +
  scale_color_manual(values = c("sienna1", "dodgerblue3")) +
  scale_x_discrete(labels = c("RL", "RLW")) +
  scale_y_continuous(breaks = seq(0, 1, .2)) +
  theme_classic()

#compare fitted temperatures of both groups
ggplot(aes(y = beta_fit_RLW, x = group, color = group), data = df_fit_compare) +
  geom_boxplot() +
  scale_color_manual(values = c("sienna1", "dodgerblue3")) +
  scale_x_discrete(labels = c("RL", "RLW")) +
  scale_y_continuous(breaks = seq(0, 10, 2)) +
  theme_classic()

#compare fitted weights of both groups
ggplot(aes(y = weight_fit_RLW, color = group, x = group), data = df_fit_compare) +
  geom_boxplot() +
  scale_color_manual(values = c("sienna1", "dodgerblue3")) +
  scale_x_discrete(labels = c("RL", "RLW")) +
  scale_y_continuous(breaks = seq(-1, 1, .25)) +
  theme_classic()


df <- df_fit_compare

hist(df$beta_fit_RLW)
beta_test <- wilcox.test(beta_fit_RLW ~ group, data = df, paired = F, exact = F)
beta_test

#calculate Z statistic
Z <- qnorm(beta_test$p.value/2)
Z

#calculate effect size
effect <- abs(Z)/sqrt(50)
effect

median(df$beta_fit_RLW)
range(df$beta_fit_RLW)


hist(df$alpha_fit_RLW)

#like a paired t test for not normally distributed data
alpha_test <- wilcox.test(alpha_fit_RLW ~ group, data = df, paired = F, exact = F)
alpha_test

#calculate Z statistic
Z <- qnorm(alpha_test$p.value/2)
Z

#calculate effect size
effect <- abs(Z)/sqrt(50)
effect

hist(df$weight_fit_RLW)

#like a paired t test for not normally distributed data
weight_test <- wilcox.test(weight_fit_RLW ~ group, data = df, paired = F, exact = F)
weight_test

#calculate Z statistic
Z <- qnorm(weight_test$p.value/2)
Z

#calculate effect size
effect <- abs(Z)/sqrt(50)
effect



#run ANOVA for best fitting model for groups 
#phenotype group as between-group factor
#BIC and BIC_RLW as within-subject factors

df_fit_social <- 
  df_fit_social %>% 
  select(id, group, BIC, BIC_RLW)

dff <- 
  df_fit_social %>% 
  gather(key = type, value = BIC, BIC_RLW)

dff2 <- 
  df_fit_social %>% 
  gather(key = type, value = BIC_RLW, BIC)

names(dff2)[4] <- "value"

names(dff)[4] <- "value"

dat <- rbind(dff, dff2)

dat$id <- factor(dat$id)
dat$group <- factor(dat$group)
dat$alpha_fit_RLW <- as.numeric(dat$BIC)
dat$beta_fit_RLW <- as.numeric(dat$BIC_RLW)
dat$type <- factor(dat$type)

# change data frame to run mixed ANOVA --> interaction is effect of interest: group * BIC type 

model <- ezANOVA(data = dat, 
                    dv = .(value), 
                    wid = .(id), 
                    within = .(type), 
                    between = .(group),
                    detailed = T, type = 2)
#stats summary
ezStats(data = dat, 
        dv = .(value), 
        wid = .(id), 
        within = .(type), 
        between = .(group),
        type = 2)

#plot means
ezPlot(data = dat, 
       dv = .(value), 
       wid = .(id), 
       within = .(type), 
       between = .(group),
       x = .(group), 
       do_lines = F, 
       split = .(group), 
       x_lab ="group", 
       y_lab ="...", type = 2)





#plot BIC_RL as a function of BIC_RLW
#shows that RLW better fits the social condiion data if omega is < 0
#if omega > 0 RL and RLW fit the data almost equally well 
ggplot(aes(x = BIC_RLW, y = BIC, shape = group, color = weight_fit_RLW), data = df_fit_social) +
  geom_jitter(width = 5, height = 5, size = 4, alpha = 0.7) +
  scale_color_gradient(low = "dodgerblue3", high = "sienna1", expression(paste("Fitted ", omega, " values")),
                       limits = c(-1, 1), breaks = c(-.75, -.25, .25, .75)) +
  guides(color = guide_colorbar(barwidth = 0.7, barheight = 10)) +
  scale_shape_manual(values = c(16, 17), labels = c("RL \nn = 28 \n", "RLW \nn = 22"), "Computational \nphenotype") +
  ylab("BIC RL") +
  xlab("BIC RLW") +
  scale_x_continuous(breaks = seq(100, 1000, 200)) +
  scale_y_continuous(breaks = seq(100, 1000, 200)) +
  geom_abline(color = "gray31") +
  theme_classic() +
  theme(axis.title.x = element_text(size = 14)) + 
  theme(axis.title.y = element_text(size = 14))+
  theme(axis.text = element_text(size = 12, colour = "black")) +
  theme(legend.title = element_text(size = 13)) +
  theme(legend.text = element_text(size = 11.5)) 
  
hist(df_fit_social$BIC_RLW)
