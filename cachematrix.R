## 150314 RS rprog-012 assignment2 Caching the Inverse of a Matrix
## 
## Two functions for caching the inverse of a matrix
##

## makeCacheMatrix
## creates a special "matrix" object that can cache its inverse
## containing a list of functions to
## 1. set the value of the matrix
## 2. get the value of the matrix
## 3. set the value of the inverse
## 4. get the value of the inverse
##

makeCacheMatrix <- function(x = matrix()) {
    # returning list with four functions
    mi <- NULL;
    
    # set function for caching values
    set <- function(y) {
        x <<- y;
        mi <<- NULL;
    }
    
    # get function x
    get <- function() x;
    
    # calculate inverse of a matrix with inverse
    setinverse <- function(inverse) mi <<- inverse;
    
    #get inverse
    getinverse <- function() mi
    
    # creating return list
    list(set = set, get = get, setinverse = setinverse, getinverse = getinverse);
}

## cacheSolve
## calculates or retreave from cache the inverse of the special "matrix" created with the above function
##

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    mi<-x$getinverse();
    if(!is.null(mi)) {
        message("getting cached mi$data");
        return(mi)
    }
    
    data<-x$get();
    # calculate inverse matrix
    mi<-solve(data, ...);
    # store invers matrix in cache;
    x$setinverse(mi);
    # return m
    mi;  
}
