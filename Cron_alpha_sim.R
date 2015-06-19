#Ben Dykstra
#replicating excel file with random values to compute alpha and use Pearson's estimation fit
library(PearsonDS)
library(MASS)

RandDF = function(RandomNumbers){
  #rownames = c("iter1", "iter2", "iter3", "iter4", "iter5", "iter6", "iter7", "iter8", "iter9", "iter10")
  colnames = c("cell1", "cell2", "cell3", "cell4", "cell5")
  myDF = data.frame(matrix(RandomNumbers, 1000, 5)) #CHANGE ROWS TO MATCH TOTAL ON LINE 29
  names(myDF) = colnames
  
  return(myDF)
}

#function for calculating Cronbach's Alpha - takes in a data frame as a parameter
CronAlpha = function(dataframe){
  k = nrow(dataframe) #number of iterations
  RowVar = apply(dataframe, 1, FUN = var) #variance across cells for each iteration
  ColMeans = apply(dataframe, 2, FUN = mean) #mean across all iterations for each cell
  Num = sum(RowVar)
  Den = k^2 * var(ColMeans)
  print(Num)
  print(Den)
  alpha = (k/(k-1))*(1 -(Num/Den))
  return(alpha)
}


static_rand = rnorm(5000, mean = 0, sd = 1) #CHANGE TOTAL NUMBER OF KNOWN POINTS - MATCH WITH LINE 9
static_df = RandDF(static_rand)

static_est_df = data.frame(matrix( ,100000, 5)) #CHANGE NUMBER OF ROWS - MATCH ROW # W/ LINE 40
names(static_est_df) = c("cell1", "cell2", "cell3", "cell4", "cell5")


#for loop for test points
for( i in 1:ncol(static_df)){
  momen = empMoments(static_df[,i]) #calculates moments for each column -- mean, var, skew, kurtosis
  #y = pearsonFitM(moments = momen) #finds pearson distribution with those moments/parameters for column
  test_points = rpearson(100000, moments = momen) #generates fitted test points 
  static_est_df[ , i] = test_points
}

#alpha calculations
alpha = CronAlpha(static_df)
est_alpha = CronAlpha(static_est_df)


#estimated plots
est_dat = data.frame(cols = c(static_est_df[1:nrow(static_est_df),1], 
                              static_est_df[1:nrow(static_est_df),2], 
                              static_est_df[1:nrow(static_est_df),3], 
                              static_est_df[1:nrow(static_est_df),4],
                              static_est_df[1:nrow(static_est_df),5]), 
                 lines = rep(c("1", "2","3", "4", "5"), each = nrow(static_est_df)))
ggplot(est_dat, aes(x = cols, fill = lines)) + geom_density() + ggtitle("Estimated Data")

#known plots
dat = data.frame(cols = c(static_df[1:nrow(static_df),1], static_df[1:nrow(static_df),2], 
                              static_df[1:nrow(static_df),3], static_df[1:nrow(static_df),4],
                              static_df[1:nrow(static_df),5]), 
                     lines = rep(c("1", "2","3", "4", "5"), each = nrow(static_df)))
ggplot(dat, aes(x = cols, fill = lines)) + geom_density() + ggtitle("Known Data")

#combined plots
comb_dat = data.frame(cols = c(static_est_df[ 1:1000,5], 
                               static_df[ 1:1000,5]),  
                     lines = rep(c("est5", "5"), each = 1000))
ggplot(comb_dat, aes(x = cols, fill = lines)) + geom_density() + ggtitle("est vs. known")


print(alpha)
print(est_alpha)
perc_diff = (alpha - est_alpha)/alpha
print(perc_diff)

