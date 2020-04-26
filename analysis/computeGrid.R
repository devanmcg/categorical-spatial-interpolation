# specify function which is executed for each tile of the grid
computeGrid <- function(grid, dialects_train, knn) {
  require(kknn)
  # create empty result data frame
  dialects_result <- data.frame(dialect = as.factor(NA), 
                                lon = st_coordinates(grid)[, 1], 
                                lat = st_coordinates(grid)[, 2])
  # run KKNN
  dialects_kknn <- kknn::kknn(dialect ~ ., 
                              train = dialects_train, 
                              test = dialects_result, 
                              kernel = "gaussian", 
                              k = knn)
  # bring back to result data frame
  # only retain the probability of the dominant dialect at that grid cell
  dialects_result %<>%
    # extract the interpolated dialect at each grid cell with the 
    # kknn::fitted function
    mutate(dialect = fitted(dialects_kknn),
           # only retain the probability of the interpolated dialect,
           # discard the other 7
           prob = apply(dialects_kknn$prob, 
                        1, 
                        function(x) max(x)))
  return(dialects_result)
}