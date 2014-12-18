## These 2 functions demonstrate the performance improvement possible by using
## caching. From the programming assignment specification, it is not clear if
## additional protection against changes in the cached matrix beyond those in
## the example code are required. Therefore a rudimentary checksum is used here 
## to provide some protection against changes to the cached matrix - NB to 
## protect against malicious changes a more sophisticated checksum (then the 
## grand sum implemented here) will be required (and will have to be applied
## to the cached inverse matrix as well). A simple test procedure testing all
## of this functionality is given below at the foot of this file.

## The makeCacheMatrix function takes a matrix as input and creates an object 
## containing the matrix, the matrix checksum, the inverse matrix (initially 
## set to NULL)  and a list of functions that can be applied to these items:
##
## (1) set() - overwrites the matrix & checksum & sets the inverse to NULL
## (2) get() - retreives the matrix from cache
## (3) getcs() - retreives the checksum from cache
## (4) getinverse() - retrieves the inverse matrix from cache 
## (5) setinverse() - overwrites the cached inverse matrix with a new inverse
## (6) change() - overwrites the matrix (only) - this function allows testing of
##                the changed matrix detection functionality 
##
## NB - set(), setinverse() & change() use the superassignment operator (<<-) 
## to overwrite cached values in the object (not make local copies)
makeCacheMatrix <- function(x = matrix()) {

    ## On instantiation the inverse matrix has not yet been computed
    invX <- NULL
    
    ## Compute and cache the matrix checksum (simple grand sum in this case)
    cs <- sum(rowSums(x))
    
    set <- function(y){
        ## Cache the new matrix
        x <<- y
        ## If the matrix is changed then the inverse is incorrect
        invX <<- NULL
        ## The checksum also requires to be recomputed and cached
        cs <<- sum(rowSums(x))
        }
    
    get <- function() x  
    getcs <- function() cs    
    setinverse <- function(inverse) invX <<- inverse
    getinverse <- function() invX
    
    change <- function(y){
        x <<- y   
        ## Since this function simulates unexpected changes in the values of the
        ## cached matrix elements the invX & cs items are left unchanged
        }
    
    list(set = set, get = get, getcs = getcs,
         setinverse = setinverse, getinverse = getinverse, change = change)
}

## The cacheSolve function takes an object created by makeCacheMatrix and
## returns the inverse of the matrix contained in the object. The function
## checks to see if the cached matrix has been changed since the checksum was
## computed and if it has prompts the user to re-enter the matrix and then 
## returns NULL for the the inverse. if not, then the function checks if inverse
## is already held in cache and returns the stored value if it is. If not, it 
## calls solve() to invert the matrix, puts the newly generated inverse in the 
## cache and returns it. 
cacheSolve <- function(x, ...) {

    ## Get the cached values of the matrix, the inverse & the checksum
    myData <- x$get()    
    myinvX <- x$getinverse()
    mycs <- x$getcs()
        
    ## Test if the computed checksum for the matrix agrees with the cached value
    if(abs(mycs-sum(rowSums(myData)))<1.0E-10){

        ## Test to see if the inverse is in cache
        if(!is.null(myinvX)){
            ## Return the cached inverse matrix
            message("getting cached data")
            return(myinvX)

            } else {
            ## Inverse matrix is not in cache so compute, cache and return it
            message("inverting matrix")
            myinvX <- solve(myData, ...)
            x$setinverse(myinvX)
            return(myinvX)
        }

    } else {
        ## Cached matrix has been changed - prompt to re-enter and return NULL 
        ## for the inverse since this forces the user to re-enter whatever is
        ## the correct value for the matrix (since we can't know whether the 
        ## changes were intentional or accidental)
        message('matrix altered - re-enter using $set()')
        return(NULL)
    }
}

## A simple but effective test procedure:
##
## > m <- matrix(rnorm(4000000),2000,2000)
## > cm <- makeCacheMatrix(m)
## > icm <- cacheSolve(cm)
## > icm <- cacheSolve(cm)
## > cm$set(matrix(rnorm(4000000),2000,2000))
## > icm <- cacheSolve(cm)
## > icm <- cacheSolve(cm)
## > cm$change (matrix(rnorm(4000000),2000,2000))
## > icm <- cacheSolve(cm)
## > icm <- cacheSolve(cm)
## > cm$set(matrix(rnorm(4000000),2000,2000))
## > icm <- cacheSolve(cm)
## > icm <- cacheSolve(cm)
##
## As well as the different messages output, you'll notice a big difference in 
## processing times for the consecutive cacheSolve calls.

