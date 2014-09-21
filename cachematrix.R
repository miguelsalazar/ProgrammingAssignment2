## @author: Miguel Salazar (miguelsalazarg@gmail.com)

## This function makes a special 'matrix' object which contains three states:
## 1. Original Matrix: A matrix M for which we'll calcualte the inverse.
## 2. Cached Matrix: A matrix C whose inverse has been already calculated.
## 3. Inversed Matrix: The inverse of matrix M.

makeCacheMatrix <- function(x = matrix()) {
    inverse <- NULL
    cache <- NULL
    
    setmatrix <- function(y) {
        x <<- y
        inverse <<- NULL
    }
    
    getmatrix <- function() 
        x
    
    setcache <- function(y)
        cache <<- y
    
    getcache <- function()
        cache
    
    setinverse <- function(solve) 
        inverse <<- solve
    
    getinverse <- function() 
        inverse
    
    list(setmatrix = setmatrix, 
         getmatrix = getmatrix,
         setcache = setcache,
         getcache = getcache,
         setinverse = setinverse,
         getinverse = getinverse)
}

## Solves the inverse of a matrix. 
## Initially, this function calculates the inverse of a matrix and stores its result in a cache.
## For further calculations, it first verifies if the matrix hasn't been previously calcualted.
## If the matrix has been previously calcualted, it returns the result from the cache.
## The function also verifies that the matrix for which it cached a result, is the same as the one that will be returned.
## If there isn't concordance, it recalcualtes the inverse of the matrix.

cacheSolve <- function(x, ...) {
    cached_matrix <- x$getcache()
    target_matrix <- x$getmatrix()
    
    m <- x$getinverse()
    
    if(!is.null(m)) {
        message("getting cached data")
        #checks if the cached matrix has not changed.
        #if the cached matrix has not changed, it returns the previously calculated value.
        if(dim(cached_matrix) == dim(target_matrix) && all(cached_matrix == target_matrix)) {
            return (m)    
        }
        
    }
    
    #if the cached matrix has changed, it calculates the inverse again.
    
    # Retrieves the matrix object.
    data <- x$getmatrix()
    
    # Solves the inverse matrix.
    message("calculating inverse of matrix")
    m <- solve(data)
    
    # Stores the inverse matrix in the cache.
    x$setinverse(m)
    
    # Stores the original matrix in the cache
    x$setcache(data)
    
    m
}