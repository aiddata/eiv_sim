#Ben Dykstra
#replicating excel file with random values to compute alpha and use Pearson's estimation fit


RandDF = function(RandomNumbers){
  rownames = c("iter1", "iter2", "iter3", "iter4", "iter5", "iter6", "iter7", "iter8", "iter9", "iter10")
  colnames = c("cell1", "cell2", "cell3", "cell4", "cell5", "cell6", "cell7", "cell8", "cell9")
  myDF = data.frame(matrix(RandomNumbers, 10, 9), row.names = rownames)
  names(myDF) = colnames
  
  return(myDF)
}
x = RandDF(rand)
x

#function for calculating Cronbach's Alpha - takes in a data frame as a parameter
CronAlpha = function(dataframe){
  k = nrow(dataframe) #number of iterations
  RowVar = apply(dataframe, 1, FUN = var) #variance across cells for each iteration
  ColMeans = apply(dataframe, 2, FUN = mean) #mean across all iterations for each cell
  Num = sum(RowVar)
  Den = k^2 * var(ColMeans)
  alpha = (k/(k-1))*(1 -(Num/Den))
  return(alpha)
}


nrep = 1000
x = c(1:1000) #some place holding variables to fill with alpha values
z = c(1:1000)
#for loop for test points
for( i in 1:nrep){
  rand = rnorm(90)
  moments = empMoments(rand) #gives the first four moments
  y = pearsonFitM(moments = moments) #finds pearson distribution with those moments/parameters
  test_points = rpearson(90, params = y) #generates fitted test points
  est_df = RandDF(test_points)
  myAlpha = CronAlpha(est_df)
  x[i] = myAlpha
  
  #uses regular non-fitted points to generate alphas to compare to the fitted ones
  not_est_df = RandDF(rand)
  not_est_Alpha = CronAlpha(not_est_df)
  z[i] = not_est_Alpha
}


hist(x)
hist(z)
mean_diff = mean(x-z)
print(mean_diff)
  
