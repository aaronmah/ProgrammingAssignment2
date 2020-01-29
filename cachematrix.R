## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {

        #instantiate the matrix to store
        cache <- matrix()
        
        #solve it
        solveMatrix <- function() {
                solve(x) 
                x
        }
        
        #get cache
        getCache <- function() cache
        
        #setCache
        setCache <- function(solved) {
                cache <<- solved 
                cache
                } 

        cache
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        #see if there's a value in cache
        #if there is, return it
        #if there isn't, call solve function and set it
        #store in cache
        #return solve
        
        cache <- x$getCache()
        
        if(!is.na(cache)) return(cache)
        
        solved <- x$solveMatrix()
        
        x$setCache(solved)
        
        solved
}

makeVector <- function(x = numeric()) {
        m <- NULL
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        get <- function() x
        setmean <- function(mean) m <<- mean
        getmean <- function() m
        list(set = set, get = get,
             setmean = setmean,
             getmean = getmean)
}

cachemean <- function(x, ...) {
        m <- x$getmean()
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        data <- x$get()
        m <- mean(data, ...)
        x$setmean(m)
        m
}
