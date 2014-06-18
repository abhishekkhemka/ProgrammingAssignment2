## These fucntions will cache the inverse of a martix. If the the matrix hasnt 
## changed then the cached version (if available) will be retunred..


## Function to check if two matrices are equal
## Can only compare matrices of same dimenstion.
matrixEqual <- function(x,y) {
    is.matrix(x) && is.matrix(y) && dim(x) == dim(y) && all(x == y)
}


## This function will take a matrix and return a list containg
## functions to set and get the matrix and its inverse

# makeCacheMatrix creates a list containing a function to
# 1. set the value of the matrix
# 2. get the value of the matrix
# 3. set the value of inverse of the matrix
# 4. get the value of inverse of the matrix
# @return: An object containg functions to get and set
#          values of the matrix and its inverse.
makeCacheMatrix <- function(x = matrix()) {
    
    inv <- NULL
    set <- function(y) {
        #No Need to update the matrix if it hasnt changed.
        if (!matrixEqual(x,y)) {
            x <<- y
            inv <<- solve(y)
            
        }
    }
    get <- function() x
    setInverse <- function(invValue) inv <<- invValue
    getInverse <- function() inv
    list(set = set, get = get,
         setmean = setmean,
         getmean = getmean)
    
}


## Write a short comment describing this function
#Pass in the object returned from the 
##makeCacheMatrix function.
# @return Inverse of the matrix
cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    inv <- x$getInverse()
    if(!is.null(inv)) {
        message("getting cached inverse")
        return(inv)
    }
    data <- solve(x$get)
    x$setInverse(data)
    data
    
}
