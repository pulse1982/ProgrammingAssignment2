##makeCacheMatrix: This function creates a special "matrix" object that can cache its inverse.
##
##cacheSolve: This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
##            If the inverse has already been calculated (and the matrix has not changed), 
##            then the cachesolve should retrieve the inverse from the cache.


## makeCacheMatrix:
## function input: 'x' is an invertible matrix;
## function output: creates a list containing a function to
##                  1. set the matrix
##                  2. get the matrix
##                  3. set the inverse
##                  4. get the inverse
##  Output list is used as the input to cacheSolve()Write function

makeCacheMatrix <- function(x = matrix()) {
        
        inv = NULL
        set = function(y) {
                
                
                x <<- y                #  <<- operator which can be used to assign a value to an object   
                inv <<- NULL           # in an environment that is different from the current environment.
        }
        get = function() x
        setinv = function(inverse) inv <<- inverse 
        getinv = function() inv
        list(set=set, get=get, setinv=setinv, getinv=getinv) #return list of functions
        
}

## cacheSolve:
## function input: 'x' is an output list of makeCacheMatrix()
## function output: inverse of the original matrix input to makeCacheMatrix()

cacheSolve <- function(x, ...) {
        
        inv = x$getinv()
        
        
        if (!is.null(inv)){               # if inverse already calculated
                message("getting cached data")
                return(inv)                     # then get it from cache and skip computation.
        }
        
        
        data = x$get()               
        inv = solve(data, ...)        # else calculate the inverse
        
        
        x$setinv(inv)        # and set the value of the inverse in the cache via the setinv function.
        
        inv
}
