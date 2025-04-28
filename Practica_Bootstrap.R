## Técnica Bootstrap
set.seed(12345)
reclam = rlnorm(20, meanlog = 2.5, sdlog = 2.5)

bootstrap_mse = function(data, n_bootstrap){
  mse_values = numeric(n_bootstrap)
  for(i in 1:n_bootstrap){
    sample_data = sample(data, replace = TRUE)
    mse_values[i] = mean((sample_data - mean(sample_data))^2)
  }
  return(list(
    media_mse = mean(mse_values),
    sd_mse = sd(mse_values), 
    msval = mse_values
  ))
}

# Estimación del MSE usando bootstrap
n_bootstrap = 100000
mse_estimate <- bootstrap_mse(reclam, n_bootstrap)

cat("El MSE es:", format(mse_estimate$media_mse, big.mark = ","), "\n")

# Intervalos de confianza bajo normalidad
alpha = 0.05
z = qnorm(1-alpha/2)
ci_lower = mse_estimate[[1]] - z*mse_estimate[[2]]
ci_upper = mse_estimate[[1]] + z*mse_estimate[[2]]

cat("Los intervalos de confianza bajo normalidad son: [", ci_lower, ", ", ci_upper, "]\n")

## Intervalo de confianza bajo cuantiles 

ci_qua_low <- quantile(mse_estimate$msval,0.025)
ci_qua_upp <- quantile(mse_estimate$msval, 0.975) 

cat("Los intervalos de confianza bajo cuantiles son: [", ci_qua_low, ", ", ci_qua_upp, "]\n")

## Histograma 

hist(mse_estimate$msval, 
     breaks = 50, 
     probability = TRUE,
     col = "lightblue", 
     main = "Histograma de Densidad del MSE (Bootstrap)", 
     xlab = "MSE")
abline(v = mean(mse_estimate$msval), col = "red", lwd = 2)
abline(v = ci_qua_low, col = "green" , lwd =2 )
abline(v = ci_qua_upp, col = "green" , lwd =2 )

"-------------------- Con datos obtenidos del excel -------------------------------------"

rec <-  c(144,134,185,141,205,126,123,152,123,215,170,165,180,175,160,185,168,172,178,169)

mse1_estimate <- bootstrap_mse(rec, n_bootstrap)

cat("El MSE es:", format(mse1_estimate$media_mse, big.mark = ","), "\n")

# Intervalos de confianza bajo normalidad
alpha = 0.05
z = qnorm(1-alpha/2)
ci1_lower = mse1_estimate[[1]] - z*mse1_estimate[[2]]
ci1_upper = mse1_estimate[[1]] + z*mse1_estimate[[2]]
cat("Los intervalos de confianza bajo normalidad son: [", ci1_lower, ", ", ci1_upper, "]\n")

## Intervalo de confianza bajo cuantiles 

ci1_qua_low <- quantile(mse1_estimate$msval,0.025)
ci1_qua_upp <- quantile(mse1_estimate$msval, 0.975) 
cat("Los intervalos de confianza bajo cuantiles son: [", ci1_qua_low, ", ", ci1_qua_upp, "]\n")

## Histograma 

hist(mse1_estimate$msval, 
     breaks = 50, 
     probability = TRUE,
     col = "lightblue", 
     main = "Histograma de Densidad del MSE (Bootstrap)", 
     xlab = "MSE")
abline(v = mean(mse1_estimate$msval), col = "red", lwd = 2)
abline(v = ci1_qua_low, col = "green" , lwd =2 )
abline(v = ci1_qua_upp, col = "green" , lwd =2 )