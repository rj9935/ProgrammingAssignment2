## These functions demonstrate the performance improvement possible by using
## caching. In the programming assignment it's not clear if more protection 
## against cached matrix changes than those in the example code are needed. So a 
## rudimentary checksum is used here. To protect against malicious changes a 
## better checksum could be used (and applied to the inverse matrix too). 
## A simple test procedure for all this is at the foot of this file.

## The computeCs function computes and returns a checksum - the one
## implemented here is very simple (grand sum) but there are R packages with 
## more sophisticated implementations (eg MD5) if needed
computeCs <- function(x){
    ## Compute and return the checksum from the matrix x - NB the assignment
    ## operator is used here (not superassignment) since it is vital not to 
    ## overwrite the cached value of cs in the cachedMatrix object.
    cs <- sum(rowSums(x)) 
}

## The makeCacheMatrix function inputs a matrix and creates an object 
## containing the matrix, the matrix checksum, the inverse matrix (initially 
## set to NULL)  and a list of functions that can be applied to these items:
##
## (1) set() - overwrites the matrix & checksum & sets the inverse to NULL
## (2) get() - retreives the matrix from cache
## (3) getcs() - retreives the checksum from cache
## (4) getinverse() - retrieves the inverse matrix from cache 
## (5) setinverse() - overwrites the cached inverse matrix with a new inverse
##
## NB - set() & setinverse() use the superassignment operator (<<-) to overwrite 
## cached values in the object (not make local copies)
makeCacheMatrix <- function(x = matrix()) {

    ## On instantiation the inverse matrix has not yet been computed
    invX <- NULL
    
    ## On instantiation compute and cache the matrix checksum
    cs <- computeCs(x)
    
    set <- function(y){
        ## Cache the new matrix
        x <<- y
        ## If the matrix is changed then the inverse is incorrect
        invX <<- NULL
        ## The checksum also requires to be recomputed and cached
        cs <<- computeCs(x)
        }
    
    get <- function() x  
    getcs <- function() cs    
    setinverse <- function(inverse) invX <<- inverse
    getinverse <- function() invX    
            
    list(set = set, get = get, getcs = getcs,
         setinverse = setinverse, getinverse = getinverse)
}

## The cacheSolve function takes a makeCacheMatrix object and returns the 
## inverse of the matrix cached in it. It checks to see if the cached matrix has
## changed since the checksum was computed and if so, prompts the user to 
## re-enter the matrix and returns NULL for the the inverse. if not, then it
## checks if the inverse is already in cache and, if so, returns the cached value
## If not, it calls solve() to invert the matrix, caches and returns the inverse
cacheSolve <- function(x, ...) {

    ## Get the cached values of the matrix, the inverse & the checksum
    myData <- x$get()    
    myinvX <- x$getinverse()
    mycs <- x$getcs()
        
    ## Test if the checksum for the current matrix agrees with the cached value
    if(abs(mycs-computeCs(myData))<1.0E-10){

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
## > env <- environment(cm$getinverse) # gets the environment of the matrix x
## > assign("x", matrix(rnorm(4000000),2000,2000), envir = env)  # corrupts x
## > icm <- cacheSolve(cm)
## > icm <- cacheSolve(cm)
## > cm$set(matrix(rnorm(4000000),2000,2000))
## > icm <- cacheSolve(cm)
## > icm <- cacheSolve(cm)
##
## As well as the different messages output, you'll notice a big difference in 
## processing times for the consecutive cacheSolve calls.

