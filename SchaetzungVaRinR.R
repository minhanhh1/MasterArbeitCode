library(MASS)
library(matrixcalc)
library(RiskPortfolios)
library(ggplot2)
library(scales)
library(dplyr)
#Dataframes definieren
returns <- read.csv2('/Users/minhanhle/Documents/MasterArbeit/MasterArbeitCode/returns1.csv', sep = ',')
datum <- returns['Datum'] 
returns['Datum'] <- NULL

returns <- as.data.frame(returns)
returns <- as.data.frame(lapply(returns, as.numeric))
# Dataframes definieren
normalPortfolioSigmas <- data.frame()
daily_shrinkageVaR_999 <- data.frame()
daily_normalVaR_999 <- data.frame()
shrinkageVaR999_250 <- data.frame()
normalVaR999_250 <- data.frame()

daily_shrinkageVaR_95 <- data.frame()
daily_normalVaR_95 <- data.frame()
shrinkageVaR95_250 <- data.frame()
normalVaR95_250 <- data.frame()

daily_shrinkageVaR_99 <- data.frame()
daily_normalVaR_99 <- data.frame()
shrinkageVaR99_250 <- data.frame()
normalVaR99_250 <- data.frame()

rolling_daily_returns_cor <- data.frame()


shrinkage_constant_values = list()
frobenius_distance_values = list()

# Funktion zur Berechnung der Shrinkage-Kovarianzmatrix
# covCor <- function(Y) {
#   # Dimension der Matrix Nxp
#   N <- nrow(Y)
#   p <- ncol(Y)
#   
#   # Berechne die Shrinkage-Kovarianzmatrix nach Ledoit und Wolf
#   shrinkage_result <- covShrink(Y, method = "ledoitWolf")
#   
#   shrinkage <- shrinkage_result$shrinkage
#   sigmahat <- shrinkage_result$covmat
#   
#   list(shrinkage = shrinkage, sigmahat = sigmahat)
# }

# VaR Konfidenzniveau
pVaR <- c(0.999, 0.99, 0.95)
zScore <- qnorm(pVaR)
weights <- rep(1 / ncol(returns), ncol(returns))

sampleSize = nrow(returns)-1
windowSize = 249
testWindowSize = sampleSize-windowSize

for (t in c(1:testWindowSize)){
  k = windowSize + t
  # Returns filtern
  returnsData <- returns[t:k, ]
  
  N <- nrow(returnsData)
  p <- ncol(returnsData)
  mean <- colMeans(returnsData)
  
  Y <- sweep(returnsData, 2, mean)                   
  #Y <- as.matrix(Y)
  n = N-1                       
  #Stichproben VKM berechnen
  sample_cov <- (t(as.matrix(Y)) %*% as.matrix(Y)) / n
  covMatrix = sample_cov
  
  #shrinkage_constant, shrinkage_cov = covCor(returnsData)
  #shrinkage_constant_values.append(shrinkage_constant)
  shrinkage_cov <- covEstimation(as.matrix(returnsData), control = list(type = 'cor'))
  v= weights
  v_trans = t(v)
  
  # Berechne Portfolio-Standardabweichung mit Stichproben VKM
  # Berechne Portfolio-Standardabweichung mit Stichproben VKM
  sigma <- sqrt(v_trans %*% covMatrix %*% v)
  normalPortfolioSigmas[t, 'Sigma'] <- sigma
  
  # Berechne Portfolio-Standardabweichung mit Shrinkage VKM
  sigmahat <- sqrt(v_trans %*% shrinkage_cov %*% v)
  
  # Berechne die täglichen VaR
  daily_normalVaR_999[t, 'VaR'] <- -zScore[1] * sigma
  daily_shrinkageVaR_999[t, 'VaR'] <- -zScore[1] * sigmahat
  
  daily_normalVaR_99[t, 'VaR'] <- -zScore[2] * sigma
  daily_shrinkageVaR_99[t, 'VaR'] <- -zScore[2] * sigmahat
  
  daily_normalVaR_95[t, 'VaR'] <- -zScore[3] * sigma
  daily_shrinkageVaR_95[t, 'VaR'] <- -zScore[3] * sigmahat
  
  # Berechnung des 250 Tage VaR
  normalVaR999_250[t, 'VaR'] <- -zScore[1] * sigma * sqrt(250)
  shrinkageVaR999_250[t, 'VaR'] <- -zScore[1] * sigmahat * sqrt(250)
  
  normalVaR99_250[t, 'VaR'] <- -zScore[2] * sigma * sqrt(250)
  shrinkageVaR99_250[t, 'VaR'] <- -zScore[2] * sigmahat * sqrt(250)
  
  normalVaR95_250[t, 'VaR'] <- -zScore[3] * sigma * sqrt(250)
  shrinkageVaR95_250[t, 'VaR'] <- -zScore[3] * sigmahat * sqrt(250)
  
  # Berechne die Gewinn-Verlust-Werte
  rolling_daily_returns_cor[t, 'GV'] <- sum(returns[k, ] * weights)
  
  # Frobenius
  frobenius_distance <- norm(sample_cov - shrinkage_cov, type = "F")
  frobenius_distance_values <- c(frobenius_distance_values, frobenius_distance)
}
returns <- read.csv2('/Users/minhanhle/Documents/MasterArbeit/MasterArbeitCode/returns1.csv', sep = ',')  
# Sicherstellen, dass die Indizes als Datum interpretiert werden
daily_normalVaR_999 <- daily_normalVaR_999 %>% 
  `rownames<-`(as.Date(datum[251:(250 + nrow(daily_normalVaR_999)),]))

daily_shrinkageVaR_999 <- daily_shrinkageVaR_999 %>% 
  `rownames<-`(as.Date(datum[251:(250 + nrow(daily_shrinkageVaR_999)),]))

# Überprüfen des neuen Indexes von daily_normalVaR und daily_shrinkageVaR
cat("Erster Eintrag im Index von daily_normalVaR:", rownames(daily_normalVaR_999)[1], "\n")
cat("Letzter Eintrag im Index von daily_normalVaR:", rownames(daily_normalVaR_999)[length(daily_normalVaR_999)], "\n")
cat("Erster Eintrag im Index von daily_shrinkageVaR:", rownames(daily_shrinkageVaR_999)[1], "\n")
cat("Letzter Eintrag im Index von daily_shrinkageVaR:", rownames(daily_shrinkageVaR_999)[length(daily_shrinkageVaR_999)], "\n")

# Plotten des Subsets mit Beschriftung der X-Achse auf Jahre reduziert
library(ggplot2)
library(scales)

df <- data.frame(
  Datum = as.Date(rownames(daily_normalVaR_999)),
  normalVaR = -daily_normalVaR_999$VaR,
  shrinkageVaR = -daily_shrinkageVaR_999$VaR
)

ggplot(df, aes(x = Datum)) +
  geom_line(aes(y = normalVaR, color = 'Stichproben-VaR')) +
  geom_line(aes(y = shrinkageVaR, color = 'Shrinkage-VaR')) +
  scale_x_date(date_breaks = "1 year", date_labels = "%Y") +
  labs(x = 'Datum', y = 'VaR', title = 'Schätzung des VaRs zum 99.9%-Konfidenzniveau') +
  scale_color_manual(values = c('Stichproben-VaR' = 'black', 'Shrinkage-VaR' = 'red')) +
  theme_minimal() +
  theme(legend.title = element_blank()) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  theme(panel.grid.major = element_line(color = 'gray', linewidth = 0.5)) # Updated here

##Frobenius 
df_frob <- data.frame(
  Iteration = 1:length(frobenius_distance_values),
  Frobenius_Distance = unlist(frobenius_distance_values)
)
ggplot(df_frob, aes(x = Iteration, y = Frobenius_Distance)) +
  geom_line() +
  geom_point() +
  labs(x = 'Iteration', y = 'Frobenius Distance', title = 'Frobenius Distance Over Time') +
  theme_minimal()

#vergleich 
shrinkage_py <- read.csv2('/Users/minhanhle/Documents/MasterArbeit/MasterArbeitCode/daily_shrinkageVaR_999.csv', sep = ',')
normal_y <- read.csv2('/Users/minhanhle/Documents/MasterArbeit/MasterArbeitCode/daily_normalVaR_999.csv', sep = ',')