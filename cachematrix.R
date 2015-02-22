# This program designed to demonstrate lexical scoping. It contains two 
# functions, makeCacheMatrix, which will create a matrix object to cache its 
# inverse and another, cacheSolve, that computes the inverse of the special 
# matrix sent to it by the makeCacheMatrix function.

makeCacheMatrix <- function(x = matrix()) {
    # This function creates a special matrix object that will be sent to cache
    # its inverse. In addition, it will declare a number of functions 
    # 
    # Arguments:
    #   x: This variable represents the matrix defined by the user
    
    cache <- NULL # initializes the cache variable.
           
    getMatrix <- function(){
        # This function returns the user defined matrix
        # 
        # Returns:
        #   x: The user defined matrix
        
        return(x)
    }
    
    setCache <- function(inverse) {
        # This function stores the inverse of the matrix to the cache through
        # lexical scoping
        # 
        # Arguments:
        #   inverse: The inversed matrix
        
        cache <<- inverse
    } 
    
    getCache <- function() {
        # This function is used to retrieve the cached inverse matrix
        # 
        # Returns:
        #   cache: The cached inverse matrix
        
        return(cache)
    } 
        
    z = list(getMatrix = getMatrix, setCache = setCache, getCache = getCache)
    # This list stores the actionable nested functions of makeCacheMatrix
    
    cacheSolve(x, z) # Calls the cacheSolve function to invert the matrix
    
    print(cache) # Prints the contents of the cache
    
    cacheSolve(x, z) # Calls the cacheSolve again to test cache recovery
    
    print(cache) # Prints the contents of the cache
}

cacheSolve <- function(x, z) {
    # This function computes the inverse of the matrix provided to it. If the 
    # inverse already exists and the matrix has not changed, the function will
    # retrieve the inverse from cache.
    # 
    # Arguments:
    #   x: Contains the user defined matrix
    #   z: Contains the list of nested functions of makeCacheMatrix
     
    cacheData <- z$getCache() # Retrieve the cache
    
    data <- z$getMatrix() # Retrieve the previous matrix
    
    if(!is.null(cacheData) && identical(x, data)) {
        # Tests to see if the cache is not empty and if the matrix is the same.
        # If so, it will return the cache 
        
        message("Duplicate matrix. Printing cached data")
        
        return(cacheData)
    }
    
    cacheData <- solve(data) # Computes the new inverse
    
    z$setCache(cacheData) # Caches the inverse
    
 }
