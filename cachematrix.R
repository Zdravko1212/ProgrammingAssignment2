#Assignment: catching the inverse of a matrixless
#make cacheMatrix & cacheSolve 
#the two functions are useful tools in catching the inverse of a matrix.As matrix inversion takes a lot of space
#these functions will help to compute only once and get the result

#makecacheMatrix creates a special "matrix" object that can cache the inverse
#cachesolve computes the inverse of the special "matrix" from line 3. it is only a simply variable of type matrix
#makecacheMatrix consists of four functions

#solution
makeCacheMatrix <- function(x = matrix()){
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setinver <- function(inverse) inv <<- inverse
  getinver <- function() inv
  list(set = set, get = get, 
       setinver = setinver, 
       getinver = getinver)
}


#cacheSolve is a function that computes the inverse of the special "matrix" returned by the makeCacheMatrix
#if the inverse has already been calculated (and matrix not changed), then cacheSolve should retrieve the inverse from the cache
#cacheSolve helps computing the reverse once makeCacheMatrix is created.

cacheSolve <- function(x, ...) {
  inv <-x$getinver()
  if(!is.null(inv)) {
    message("get the cached data")
    return(inv)
  }
  data <- x$get()
  inv <- solve(data, ...)
  x$setinver(inv)
  inv
}
#testing
#creating new matrix with normal distribution
new_mat <- makeCacheMatrix(matrix(1:4, 2, 2))
new_mat$get()
#my matrix with geninver (get inverse)
new_mat$getinver()
#it returns NULL
#moving on to cacheSolve
cacheSolve(new_mat)
new_mat$getinver()
#setting new values for my matrix and printing the new values in the next line
new_mat$set(matrix(c(12, 16, 20, 24), 2, 2))
cacheSolve(new_mat)
#trying cache solve again
new_mat$getinver()
#it returns the matrix
