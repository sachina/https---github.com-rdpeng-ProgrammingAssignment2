## Put comments here that give an overall description of what your
## functions do

## This function creates a special "matrix" object that can cache its inverse.
# set the matrix
# get the matrix
# set the inverse of the matrix
# get the inverse of the matrix
makeCacheMatrix = function(x = matrix()) {
    minv <- NULL
    set <- function(y) {
        x <<- y
        minv <<- NULL
    }
    get <- function() x
    setinv <- function(m) minv <<- m
    getinv <- function() minv
    list(set = set, get = get,setinv = setinv,getinv = getinv)
}


## This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
#If the inverse has already been calculated (and the matrix has not changed), then cacheSolve 
#should retrieve the inverse from the cache.

cacheSolve = function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    
    minv <- x$getinv()
    if(!is.null(minv)) {
        message("getting cached data")
        return(minv)
    }
    data <- x$get()
    minv <- solve(data,...)
    x$setinv(minv)
    minv
}


#Test code
mat = matrix(rnorm(1000000),1000,1000)

start = proc.time()
temp = makeCacheMatrix(mat)
end = proc.time()
elapsed = end - start
elapsed

start = proc.time()
cacheSolve(temp)
end = proc.time()
elapsed = end - start
elapsed

#get cached matrix
start = proc.time()
cacheSolve(temp)
end = proc.time()
elapsed = end - start
elapsed


