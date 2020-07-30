## Function that helps caching the inverse of a matrix instead of computting it repeatedly.

## This function creates a special "matrix" object that can cache its inverse.Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()){
        inver <- NULL  
        
        ## Setting matrix value
        set <- function(y){
                x <<- y
                inver <<- NULL
        }
        ## Matrix and Inverse values      
        get <- function() {x}
        setInver <- function(inverse) {inver <<- inverse}
        getInver <- function() {inver}
        list(set = set, get = get, setInver = setInver, getInver = getInver)
}


## This function computes the inverse of the special "matrix" returned by makeCacheMatrix function.
## Cachesolve should retrieve the inverse from the cache If the inverse has already been calculated.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        inver <- x$getInver()
        if(!is.null(inver)){
                message("Inverse returned from cache data")
                return(inver)
        }
        mat <- x$get()
        inver <- solve(mat, ...)
        x$setInver(inver)
        inver
}

