##Two functions: first one returns list of functions that
## invert matrix, gets the cache, sets the cache and gets all data
## the second function uses the first to solve the matrix and store
## the values

## Write a short comment describing this function
##functions to solve matrix, get cache, set cache and get data

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
## get cache; if not null, return it. if null, solve & store in cache

cacheSolve <- function(x, ...) {
        
        cache <- x$getCache()
        
        if(!is.null(cache)){
                message("returning cache")
                return(cache)
        }
        
        data <- x$getData()
        
        solved <- x$solveMatrix(data)
        
        x$setCache(data)
        
        solved
}

