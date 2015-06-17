#Ben Dykstra
#replicating excel file with random values to compute alpha and run kernel estimation on


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


nrep = 100
x = c(1:100)
for( i in 1:nrep){
  rand = runif(90, min = 0, max = 1)
  df = RandDF(rand)
  myAlpha = CronAlpha(df)
  x[i] = myAlpha
}

plot(x)


  
  
  