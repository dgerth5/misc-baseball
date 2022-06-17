library(ggplot2)

set.seed(5)

# safe int
int_safe1 = sample(c(0,1,2), 1000, replace = TRUE, prob = c(0.85,0.1,0.05))
int_safe2 = sample(c(0,1,2), 1000, replace = TRUE, prob = c(0.85,0.1,0.05))
int_safe3 = sample(c(0,1,2), 1000, replace = TRUE, prob = c(0.85,0.1,0.05))
int_safe4 = sample(c(0,1,2), 1000, replace = TRUE, prob = c(0.85,0.1,0.05))
int_safe5 = sample(c(0,1,2), 1000, replace = TRUE, prob = c(0.85,0.1,0.05))

df_int_safe = data.frame(int_safe1,int_safe2,int_safe3,int_safe4,int_safe5)

# risky int
int_risk1 = sample(c(0,1,2), 1000, replace = TRUE, prob = c(0.60,0.25,0.15))
int_risk2 = sample(c(0,1,2), 1000, replace = TRUE, prob = c(0.60,0.25,0.15))

df_int_risk = data.frame(int_risk1,int_risk2)

# safe dom
dom_safe1 = sample(c(0,1,2), 1000, replace = TRUE, prob = c(0.55,0.45,0.05))
dom_safe2 = sample(c(0,1,2), 1000, replace = TRUE, prob = c(0.55,0.45,0.05))
dom_safe3 = sample(c(0,1,2), 1000, replace = TRUE, prob = c(0.55,0.45,0.05))

df_dom_safe = data.frame(dom_safe1,dom_safe2,dom_safe3)

# risky dom
dom_risk1 = sample(c(0,1,2), 1000, replace = TRUE, prob = c(0.6,0.2,0.2))
dom_risk2 = sample(c(0,1,2), 1000, replace = TRUE, prob = c(0.6,0.2,0.2))
dom_risk3 = sample(c(0,1,2), 1000, replace = TRUE, prob = c(0.6,0.2,0.2))

df_dom_risk = data.frame(dom_risk1,dom_risk2,dom_risk3)

# compare safe/safe vs risk/risk

safe = data.frame(df_dom_safe, df_int_safe)
safe$tot_war = Reduce("+", safe[,1:8])
safe$fac_tot_war = as.factor(safe$tot_war)

risk = data.frame(df_dom_risk, df_int_risk)
risk$tot_war = Reduce("+", risk[,1:5])
risk$fac_tot_war = as.factor(risk$tot_war)

safe$row_mean = apply(X = safe[,1:8],MARGIN = 1, FUN = mean)
risk$row_mean = apply(X = risk[,1:5],MARGIN = 1, FUN = mean)

ss <- as.data.frame(table(safe$fac_tot_war) / length(safe$fac_tot_war))
ss$type <- "safe"
rr <- as.data.frame(table(risk$fac_tot_war) / length(safe$fac_tot_war))
rr$type <- "risk"
df <- rbind(ss,rr)
colnames(df)[1] <- "WAR"

ggplot(df, aes(WAR, Freq, fill = type)) +
  geom_bar(position = "dodge", stat = "identity")

mean(safe$tot_war) / sd(safe$tot_war)
mean(risk$tot_war) / sd(risk$tot_war)
