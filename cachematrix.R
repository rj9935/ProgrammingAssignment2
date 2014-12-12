## These 2 functions demonstrate the performance improvement possible by using
## caching to speed up calls to a matrix inversion function.

## The makeCacheMatrix function takes a matrix as input and creates an object 
## containing the matrix, its inverse and a list of functions that can be applied:
##
## (1) set() - overwrites the cached matrix and sets the cached inverse to NULL
## (2) get() - gets the matrix from cache
## (3) getinverse() - retrieves the inverse matrix from cache 
## (4) setinverse() - overwrites the inverse matrix in cache with a new inverse
##
## NB - set() & setinverse() use the superassignment operator (<<-) to 
## overwrite cached values in the object created by makeCacheMatrix (and not 
## make purely local copies within these functions only)

makeCacheMatrix <- function(x = matrix()) {
    invX <- NULL
    
    set <- function(y){
        x <<- y
        invX <<- NULL
        }
    
    get <- function() x    
    setinverse <- function(inverse) invX <<- inverse
    getinverse <- function() invX

    list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}

## The cacheSolve function takes an object created by makeCacheMatrix and
## returns the inverse of the matrix contained in the object. The function
## checks to see if the inverse is already held in cache within the object and 
## returns the stored value if it is. If not, it calls solve() to invert the 
## matrix, puts the newly generated inverse in the cache and returns it. It does 
## not check if the matrix has been altered since the last time the inverse
## was cached since the inverse is set to NULL when the object is created and
## reset to NULL when altered using the set() function. Ie this implicitly
## assumes any modifications to the original matrix cached in the object will 
## only be effected using the objects set() method. It also doesn’t protect
## against anyone calling x$setinverse(value) directly.

cacheSolve <- function(x, ...) {
    invX <- x$getinverse()
    
    ## Tests to see if matrix inversion has already been performed 
    if(!is.null(invX)){
        message("getting cached data")
        return(invX)
    }
    ## If matrix inversion has not already been performed then use solve()
    message("inverting matrix")
    mydata <- x$get()    
    invX <- solve(mydata, ...)
    
    x$setinverse(invX)
    invX
}

## A simple test procedure showing caching performance gain is:
##
## > m <- matrix(rnorm(4000000),2000,2000)
## > cm <- makeCacheMatrix(m)
## > icm <- cacheSolve(cm)
## > icm <- cacheSolve(cm)
## > cm$set(matrix(rnorm(4000000),2000,2000))
## > icm <- cacheSolve(cm)
## > icm <- cacheSolve(cm)
##
## As well as the different messages, there’s a big difference in 
## processing times for the consecutive cacheSolve calls.
