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
  read_delim("~/Dropbox/___MA/social_RL_git/thesis_social_RL/simulated_agents/modelfit_agents_weight_18_30_new.txt",
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
  read_delim("~/Dropbox/___MA/social_RL_git/thesis_social_RL/simulated_agents/modelfit_agents_standardtoweight_18_30_new.txt",
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

#allows computational phenotyping
#1  = RLW
#-1 = RL
df_fit_social$group <- as.factor(ifelse(df_fit_social$delta_BIC > 0, 1, -1))

sum(df_fit_social$group == -1)

#plot shows that agents can be categorized by BIC difference
#BIC varies as a function of omega
#negative omega values --> RLW
#positive omega values --> RL
#plot BIC_RL as a function of BIC_RLW
#shows that RLW better fits the social condiion data if omega is < 0
#if omega > 0 RL and RLW fit the data almost equally well 
ggplot(aes(x = BIC_RLW, y = BIC, shape = group, color = weight_fit_RLW), data = df_fit_social) +
  geom_jitter(width = 5, height = 5, size = 4, alpha = 0.7) +
  scale_color_gradient(low = "dodgerblue3", high = "sienna1", expression(paste("Fitted ", omega, " values")),
                       limits = c(-1, 1), breaks = c(-.75, -.25, .25, .75)) +
  guides(color = guide_colorbar(barwidth = 0.7, barheight = 7)) +
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



################################################### RUN ANOVA TO DTERMINE BEST FITTING MODEL FOR EACH PHENOTYPE GROUP
#interaction is effect of interest: group * BIC type 
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
dat$type <- factor(dat$type)

# change data frame to run mixed ANOVA
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




############################################## POST HOC TESTS TO CHECK WHAT DRIVES INTERACTION BIC TYPE X GROUP

dat1 <- 
  dat %>% 
  filter(group == 1, type == "BIC")

dat2 <- 
  dat %>% 
  filter(group == -1, type == "BIC")

test1 <- wilcox.test(dat1$value, dat2$value)
test1

#calculate Z statistic
Z <- qnorm(test1$p.value/2)
Z

#calculate effect size
effect <- abs(Z)/sqrt(50)
effect

dat3 <- 
  dat %>% 
  filter(group == 1, type == "BIC_RLW")

dat4 <- 
  dat %>% 
  filter(group == -1, type == "BIC_RLW")

test2 <- wilcox.test(dat3$value, dat4$value)
test2

#calculate Z statistic
Z <- qnorm(test2$p.value/2)
Z

#calculate effect size
effect <- abs(Z)/sqrt(50)
effect

dat5 <- 
  dat %>% 
  filter(type == "BIC", group == 1)

dat6 <- 
  dat %>% 
  filter(type == "BIC_RLW", group == 1)

test3 <- wilcox.test(dat5$value, dat6$value)
test3

#calculate Z statistic
Z <- qnorm(test3$p.value/2)
Z

#calculate effect size
effect <- abs(Z)/sqrt(50)
effect

dat7 <- 
  dat %>% 
  filter(type == "BIC", group == -1)

dat8 <- 
  dat %>% 
  filter(type == "BIC_RLW", group == -1)

test4 <- wilcox.test(dat7$value, dat8$value)
test4

#calculate Z statistic
Z <- qnorm(test4$p.value/2)
Z

#calculate effect size
effect <- abs(Z)/sqrt(50)
effect
  
###################################################### ANALYSIS OF MODEL PARAMETERS

#compare fitted learning rates and temperatures of both groups
df_fit_compare <-
  df_fit_social %>% 
  select(alpha_fit_RLW, id, group, beta_fit_RLW, weight_fit_RLW)

#compare fitted learning rates of both groups
ggplot(aes(y = alpha_fit_RLW, fill = group, x = group), data = df_fit_compare) +
  geom_boxplot(outlier.size = 3, alpha = 0.7) +
  scale_fill_manual(values = c("sienna1", "dodgerblue3")) +
  scale_x_discrete(labels = c("RL", "RLW")) +
  scale_y_continuous(breaks = seq(0, 1, .2)) +
  xlab("Phenotype group") +
  ylab(expression(paste("Fitted ", alpha, " values"))) +
  theme_classic() +
  theme(axis.title.x = element_text(size = 15)) + 
  theme(axis.title.y = element_text(size = 15))+
  theme(axis.text = element_text(size = 13, colour = "black")) +
  theme(legend.title = element_text(size = 15)) +
  theme(legend.text = element_text(size = 13)) +
  theme(legend.position = "none")

#compare fitted temperatures of both groups
ggplot(aes(y = beta_fit_RLW, x = group, fill = group), data = df_fit_compare) +
  geom_boxplot(outlier.size = 3, alpha = 0.7) +
  scale_fill_manual(values = c("sienna1", "dodgerblue3")) +
  scale_x_discrete(labels = c("RL", "RLW")) +
  scale_y_continuous(breaks = seq(0, 10, 2)) +
  xlab("Phenotype group") +
  ylab(expression(paste("Fitted ", beta, " values"))) +
  theme_classic() +
  theme(axis.title.x = element_text(size = 15)) + 
  theme(axis.title.y = element_text(size = 15))+
  theme(axis.text = element_text(size = 13, colour = "black")) +
  theme(legend.title = element_text(size = 15)) +
  theme(legend.text = element_text(size = 13)) +
  theme(legend.position = "none")

#compare fitted weights of both groups
ggplot(aes(y = weight_fit_RLW, fill = group, x = group), data = df_fit_compare) +
  geom_boxplot(outlier.size = 3, alpha = 0.7) +
  scale_fill_manual(values = c("sienna1", "dodgerblue3")) +
  scale_x_discrete(labels = c("RL", "RLW")) +
  scale_y_continuous(breaks = seq(-1, 1, .5)) +
  xlab("Phenotype group") +
  ylab(expression(paste("Fitted ", omega, " values"))) +
  theme_classic() +
  theme(axis.title.x = element_text(size = 15)) + 
  theme(axis.title.y = element_text(size = 15))+
  theme(axis.text = element_text(size = 13, colour = "black")) +
  theme(legend.title = element_text(size = 15)) +
  theme(legend.text = element_text(size = 13)) +
  theme(legend.position = "none")


df <- df_fit_compare


hist(df$alpha_fit_RLW)

#like a paired t test for not normally distributed data
alpha_test <- wilcox.test(alpha_fit_RLW ~ group, data = df, paired = F, exact = F)
alpha_test

check <-
  df %>% 
  filter(group == 1)

median(check$alpha_fit_RLW)

check <-
  df %>% 
  filter(group == -1)

median(check$alpha_fit_RLW)

#calculate Z statistic
Z <- qnorm(alpha_test$p.value/2)
Z

#calculate effect size
effect <- abs(Z)/sqrt(50)
effect

hist(df$beta_fit_RLW)

beta_test <- wilcox.test(beta_fit_RLW ~ group, data = df, paired = F, exact = F)
beta_test

#calculate Z statistic
Z <- qnorm(beta_test$p.value/2)
Z

#calculate effect size
effect <- abs(Z)/sqrt(50)
effect

check2 <-
  df %>% 
  filter(group == 1)

median(check2$beta_fit_RLW)

check2 <-
  df %>% 
  filter(group == -1)

median(check2$beta_fit_RLW)


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

check3 <-
  df %>% 
  filter(group == 1)

median(check3$weight_fit_RLW)

check3 <-
  df %>% 
  filter(group == -1)

median(check3$weight_fit_RLW)

  