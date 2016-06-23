
  ## makeCacheMatrix - function creates a special "matrix" object that can cache its inverse
  ## For this assignment, assume that the matrix supplied is always invertible.
  ## What function does:
  ##    1. set the matrix (use <<- to assign to global environment)
  ##    2. get the matrix
  ##    3. set the inverse
  ##    4. get the inverse
  ##    5. creates list used as the input to cacheSolve()

makeCacheMatrix <- function(x = matrix()) {
  i_csh <- NULL
  set   <- function(y) {
    x     <<- y
    i_csh <<- NULL
  }
  get    <- function() x
  setinv <- function(inverse) i_csh <<- inverse 
  getinv <- function() i_csh
  list(set=set, get=get, setinv=setinv, getinv=getinv)
}

  ## cacheSolve - function computes the inverse of the special "matrix" 
  ## returned by makeCacheMatrix.
  ## If the inverse has already been calculated (and the matrix has not changed),
  ## then the cachesolve should retrieve the inverse from the cache.
  ## For this assignment, assume that the matrix supplied is always invertible.

cacheSolve <- function(x, ...) {
  i_csh <- x$getinv()
  if(!is.null(i_csh)) {
    message("getting cached data.")
    return(i_csh)
  }
  data <- x$get()
  i_csh <- solve(data)
  x$setinv(i_csh)
  i_csh
}
 ## Test results and procedures
 ## 1. Creste revertable matrix
 ## > x <- matrix(rnorm(16),4,4)
 ## 
 ## 2. Run makeCacheMatrix
 ## > o<-makeCacheMatrix(x)
 ## 
 ## 3. Run cacheSolve - First time, it should cach it
 ## > cacheSolve(o)
 ##           [,1]       [,2]       [,3]      [,4]
 ##[1,]  2.181912 -0.8773386  1.3173053 -2.060567
 ##[2,] -1.994796 -0.7814952 -0.3842933  1.065329
 ##[3,] -7.547529 -1.0271182 -4.5962425  6.645827
 ##[4,] -9.550373 -1.6391783 -4.7739054  9.060037
 ## 
 ## 4. cacheSolve - Second time, it should take it from cach
 ## > cacheSolve(o)
 ## getting cached data.
 ##         [,1]       [,2]       [,3]      [,4]
 ##[1,]  2.181912 -0.8773386  1.3173053 -2.060567
 ##[2,] -1.994796 -0.7814952 -0.3842933  1.065329
 ##[3,] -7.547529 -1.0271182 -4.5962425  6.645827
 ##[4,] -9.550373 -1.6391783 -4.7739054  9.060037
 ##
 ## end #########################################################
 
