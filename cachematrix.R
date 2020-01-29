## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {

        #instantiate the matrix to store
        cache <- NULL
        
        #solve it
        solveMatrix <- function(y) {
                solve(y) 
        }
        
        #get cache
        getCache <- function() cache
        
        #get data
        getData <- function() x
        
        #setCache
        setCache <- function(y) {
                cache <<- solveMatrix(y) 
                } 

        list (solveMatrix=solveMatrix, getCache=getCache, setCache=setCache, getData = getData)
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
        
        if(!is.null(cache)){
                message("returning cache")
                return(cache)
        }
        
        data <- x$getData()
        
        solved <- x$solveMatrix(data)
        
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
