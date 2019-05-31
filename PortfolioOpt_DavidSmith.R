library(quantmod)
library(GA)
library(ggplot2)
library(tsbox)
library(reshape2)
library(xts)
library(plotrix)

finalStocks <-c('JPM', 'FBNC', 'EE', 'ATO', 'AMZN', 'HSY', 'BP', 'JNJ','BOE', 'GE', 'AAPL', 'NFLX', 'VOD', 'BLL', 'HCP')

stocks<-do.call(merge, (lapply(finalStocks, function(sym) {monthlyReturn(na.omit(getSymbols
                         (sym, from='2017-01-01', to='2018-01-01', auto.assign=FALSE)))})))
colnames(stocks) <- finalStocks

future_stocks<-do.call(merge, (lapply(finalStocks, function(sym) {monthlyReturn(na.omit(getSymbols
                         (sym, from='2018-01-01', to='2019-01-01', auto.assign = FALSE)))})))
colnames(future_stocks) <- finalStocks

# Getting returns

mean_returns<-c(sapply(stocks, function(x) mean(x)))
future_mean_returns <- c(sapply(future_stocks, function(x) mean(x)))    # Used later for portfolio performance test

returns <- function(mean_returns, weights){
  portfolio_return <- sum(data.frame(mapply(`*`, (mean_returns), weights)))
  return(portfolio_return)
}

# Risk

portfolio_risks <- function(stocks, weights){
  mat <- cbind(stocks)
  w=weights
  m=cov(mat)
  risk=sum(t(m * w) * w)
  return(risk)
}

normalise <- function(chromosome){
  total = sum(chromosome)
  weights = chromosome/total   
  return(weights)
}

fitness <- function(chromosome){
  weights = normalise(chromosome)
  f1 = returns(mean_returns, weights)
  f2 = portfolio_risks(stocks, weights)
  f =  ((currentWeight) * (f1)) + ((1-currentWeight) * (-f2))

  return(f)
}

weightings <- seq(from=0,to=1,by=0.1)
currentWeight<-0
gens <- 1000
popSize <- 100
hallofFame <- vector('list', length(weightings))
fitnesses <- list()
lower <- c(rep(0, length(mean_returns)))
upper <- c(rep(1, length(mean_returns)))

for(i in 1:length(weightings)){print(paste0("Run ", i))
                               currentWeight<-weightings[i]
                               GA <- ga(type='real-valued', fitness=fitness,
                                     lower=lower, upper=upper, maxiter=gens,
                                     pcrossover=0.80, pmutation=0.05,
                                     popSize=popSize, seed=123)
                               hallofFame[[i]]=(normalise(GA@solution))
                               fitnesses[[i]]=GA@fitnessValue
}

hallofFame
fitnesses

plotFront <- function(hallofFame){
  ranruns <- 500
  risks <- vector('list', length(weightings))
  returns <- vector('list', length(weightings))
  ranRisks <- vector('list', ranruns)         # Random comparison 
  ranRets <- vector('list', ranruns)
  
  for(i in 1:length(hallofFame)){
    solution_weight=unlist(hallofFame[i])
    risk=portfolio_risks(stocks, solution_weight)
    return=returns(mean_returns, solution_weight)
    risks[i]=risk
    returns[i]=return
  }
  
  equalWeight <- normalise(rep(1, length(mean_returns)))          # Uniform weight comparison
  equalRisk <- portfolio_risks(stocks, equalWeight)
  equalReturn <- returns(mean_returns, equalWeight)

  for(i in 1:ranruns){
    random_weight=normalise(runif(length(mean_returns), 0, 1))
    rand_risk=portfolio_risks(stocks, random_weight)
    rand_return=returns(mean_returns, random_weight)
    ranRisks[i]=rand_risk
    ranRets[i]=rand_return
  }
  
  ranResults <- as.data.frame(cbind(as.numeric(ranRisks), as.numeric(ranRets)))
  gaResults <- as.data.frame(cbind(as.numeric(risks), as.numeric(returns)))

  # Comparison plot
  
  ggplot() + geom_point(data=ranResults, aes(x=ranResults$V1, y=ranResults$V2), size=0.00001, col='blue') +
  geom_point(data=gaResults, aes(x=gaResults$V1, y=gaResults$V2), col='black') + labs(x='Risk', y='Return') +
  ggtitle('Portfolio Optimisation: GA, Random and Equal Weights') + scale_y_continuous(labels= scales::percent) + scale_x_continuous(labels=scales::percent) +
  geom_point(aes(x=equalRisk, y=equalReturn), col='red') +
  theme(legend.position = "none") + theme(plot.title = element_text(hjust = 0.5))

  # Just GA plot 
  
  ggplot() + geom_point(data=gaResults, aes(x=gaResults$V1, y=gaResults$V2), col='black') + labs(x='Risk', y='Return') +
  ggtitle('Portfolio Optimisation: GA Pareto Front') + scale_y_continuous(labels= scales::percent) + scale_x_continuous(labels=scales::percent) +
  geom_point(shape=4, aes(x = unlist(risks[2]), y = unlist(returns[2])), size=3) +
  geom_abline(intercept=0.011, slope=27.65, size=0.3, linetype='dashed') +
  theme(legend.position = "none") + theme(plot.title = element_text(hjust = 0.5)) 
  
  # Spread of solutions for different risk/return objective weight
  
  ggplot() + geom_smooth(data=gaResults, aes(x=gaResults$V1, y=gaResults$V2), col='black') + labs(x='Risk', y='Return') +
  ggtitle('Portfolio Optimisation: Solution Distribution') + scale_y_continuous(labels= scales::percent) + scale_x_continuous(labels=scales::percent) +
  theme(legend.position = "none") + theme(plot.title = element_text(hjust = 0.5))
}

plotFront(hallofFame)
fitnesses
# Plotting portfolio performance over time period

x.df <- ts_df(stocks);x.df
y.df <- x.df[1:84,]
z.df <- x.df[85:180,]
ggplot(data=y.df, aes(x=y.df$time, y=y.df$value, color=y.df$id, group=y.df$id), size=0.00001, col='blue') + 
              geom_point() + geom_line() + scale_y_continuous(labels= scales::percent) +
              ggtitle('Portfolio Performance 1') + labs(x='2017 Quarters', y='Return (%)') +
              theme(legend.title = element_blank()) + theme(plot.title = element_text(hjust = 0.5)) 

ggplot(data=z.df, aes(x=z.df$time, y=z.df$value, color=z.df$id, group=z.df$id), size=0.00001, col='blue') + 
              geom_point() + geom_line() + scale_y_continuous(labels= scales::percent) +
              ggtitle('Portfolio Performance 2') + labs(x='2017 Quarters', y='Return (%)') +
              theme(legend.title = element_blank()) + theme(plot.title = element_text(hjust = 0.5))


# Chosen solution portfolio breakdown 

best_sol <- unlist(hallofFame[2]);best_sol
z<-(best_sol*100)
data <- data.frame(finalStocks, z)
df <- data[order(data$z,decreasing = TRUE),]
barplot(df$z,names.arg = df$finalStocks,cex.names=0.8, main='GA Solution Portfolio',
        xlab='Asset', ylab='Relative Investment (%)', col='blue')

# Future performance 

# Riskiest GA solution (highest return, highest risk)

futureHighReturn<-returns(future_mean_returns, unlist(hallofFame[11]));futureHighReturn
futureHighRisk <-portfolio_risks(future_stocks, unlist(hallofFame[11]));futureHighRisk 

# Chosen GA solution 

futurePortReturn<-returns(future_mean_returns, unlist(hallofFame[2]));futurePortReturn
futurePortRisk <-portfolio_risks(future_stocks, unlist(hallofFame[2]));futurePortRisk 

# Low risk GA solution (lowest return, lowest risk)

futureLowReturn<-returns(future_mean_returns, unlist(hallofFame[1]));futureLowReturn
futureLowRisk <-portfolio_risks(future_stocks, unlist(hallofFame[1]));futureLowRisk

# Actual stock return and risk for 2018 

mean(future_mean_returns)
sum(cov(cbind(future_stocks)))