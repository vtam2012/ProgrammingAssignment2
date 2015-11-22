# Sometimes, taking the inverse of a matrix is a time consuming operation.
# Therefore, it could be more beneficial to caching the inverse of a matrix
# than to re-calculate the inverse every single time it is needed.
# The functions makeCacheMatrix and cacheSolve can thus be used to cache the inverse of a given matrix.

#The first function, makeCacheMatrix creates a list containing a function to
# 1) set the value of the matrix
# 2) get the value of the matrix
# 3) set the value of the inverse of the matrix
# 4) get the value of the inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
  I <- NULL
  setI <- function (y){
    x <<- y
    I <<- NULL
  }
  getI <- function() x
  setinverse <- function(inverse) I <<- inverse
  getinverse <- function() I
  list(set=setI, get=getI, setinverse=setinverse, getinverse=getinverse)
}

# The  function cacheSolve returns the inverse of the matrix. 
# First, it checks to see if the inverse has already been computed. 
# If it has been computed, it then simply returns the resulting inverse from the cache.
# If not, it will then compute the inverse and set the value in the cache.
# Additionally, be wary that this function assumes that the matrix is always invertible.

cacheSolve <- function(x, ...) {
  I <- x$getinverse()
  if(!is.null(I)) {
    message("getting cached data")
    return(I)
  }
  data <- x$get()
  I <- solve(data, ...)
  x$setinverse(I)
  I
}
  
# Example:
# create a matrix and store in variable x
#> x<-matrix(c(1,2,2,1), nrow= 2, ncol= 2)
#> x
#[,1] [,2]
#[1,]    1    2
#[2,]    2    1

# cache the matrix and allow functions getinverse, setinverse, set, and get
#> M <-makeCacheMatrix(x)
#> M$get()
#[,1] [,2]
#[1,]    1    2
#[2,]    2    1

# retrive the inverse of matrix x and store in cache
#> cacheSolve(M)
#[,1]       [,2]
#[1,] -0.3333333  0.6666667
#[2,]  0.6666667 -0.3333333 

# The second time the function is called, it retrieves resulting matrix
# from cache and did not recalculate the values.
#> cacheSolve(M)
#getting cached data
#[,1]       [,2]
#[1,] -0.3333333  0.6666667
#[2,]  0.6666667 -0.3333333
  
