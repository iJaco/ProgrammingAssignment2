## Put comments here that give an overall description of what your
## functions do

##The first function, `makeCacheMatrix` creates a special "matrix", which is
##really a list containing a function to

##1.  set the value of the matrix
##2.  get the value of the matrix
##3.  set the inverse value of the matrix
##4.  get the inverse value of the matrix

makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        
        ##create the set function -> set the matrix
        set <- function(y) {
          x <<- y
          m <<- NULL
        }
        
        ##create the get function -> returns the matrix
        get <- function() x
        
        ##setsolve ->  solve command is used to calculate the inverse of the matrix
        setsolve <- function(solve) m <<- solve
        
        ##getsolve -> return the inversed matrix
        getsolve <- function() m
        
        ## create a list with the functions created
        list(set = set, get = get, setsolve = setsolve, getsolve = getsolve )
}


# The following function calculates the inverse of the special "matrix"
# created with the above function. However, it first checks to see if the
# inverse has already been calculated. If so, it `get`s the inverse matrix from the
# cache and skips the computation. Otherwise, it calculates the inverse matrix of
# the data and sets the value of the mean in the cache via the `setsolve`
# function.


cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        m <- x$getsolve()
        
        ##verify if the inversed matrix is cached or not. If cached retrun cache.
        
        if(!is.null(m)) {
          message("getting cached data")
          return(m)
        }
        
        ##else calculate the inverse and return.
        data <- x$get()
        m <- solve(data, ...)
        x$setsolve(m)
        m
}
