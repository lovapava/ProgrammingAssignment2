## Two functions that are used to create a special object that stores a matrix 
## and cache's its inverse


## Creating a special "matrix" object

makeCacheMatrix <- function(x = matrix()) {
        
        i <- NULL
        set <-function(y){
                x <<- y
                i <<- NULL
        }
        
        get <- function()x
        setInverse <- function(inverse)i <<- inverse
        getInverse <- function()i
        
        list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)
}


## Computing the inverse of the special "matrix" object

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        i <- x$getInverse()
        if(!is.null(i)){
                message("getting cache date")
                return(i)
        }
        data <- x$get()
        i <- solve(data, ...)
        x$setInverse(i)
        i
        
}
