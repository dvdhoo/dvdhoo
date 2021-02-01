library(readxl)
library(mice)
data_missing <- read_excel(file.choose(), sheet = "Missing")

md.pattern(data_missing)

imputed_data <- mice(data_missing, m = 5, meth = 'pmm', maxit = 30)

reg <- with(imputed_data,
            lm(Y ~ X1 + X2 + X3 + X4 + X5))
reg_pool <- pool(reg)
summary(reg_pool)


l_df <- mice::complete(imputed_data, "long", include = F)
AIC <- c()
logLik <- c()
AIC1 <- c()
logLik1 <- c()
m <- max(l_df$.imp)


for(i in 1:m) {
  imputed_model_temp <- lm(Y ~ X1 + X2 + X3 + X4 + X5, data = l_df[which(l_df$.imp == i),])
  AIC[i] <- AIC(imputed_model_temp)
  logLik[i] <- logLik(imputed_model_temp)
}

for(i in 1:m) {
  imputed_model_temp <- lm(Y ~ X1 + X3 + X4 + X5, data = l_df[which(l_df$.imp == i),])
  AIC1[i] <- AIC(imputed_model_temp)
  logLik1[i] <- logLik(imputed_model_temp)
}

mean(AIC) < mean(AIC1)

