## These functions demonstrate the performance improvement possible by using
## caching. In the programming assignment it's not clear if more protection 
## against cached value changes than those in the example code are needed. So a 
## rudimentary checksum is used here. To protect against malicious changes a 
## better hash code/checksum could be used such as the the MD5 (see 
## {digest}digest(..., algo="md5"). A test procedure is at the foot of this file.

## The computeCS function computes and returns a matrix checksum - the one
## implemented here is very simple (grand sum) but there are R packages with 
## more sophisticated implementations (eg MD5 in {digest}digest()) if needed.
computeCS 			<- function(Input){ Checksum <- sum(rowSums(Input)) }

## The makeCacheMatrix function inputs a matrix and creates an object 
## containing the matrix, the matrix checksum, the inverse matrix (initially 
## set to NULL), its checksum  and a list of functions that can be applied:
##
## (1) setmatrix() - overwrites the matrix & checksum & sets the inverse to NULL
## (2) getmatrix() - retreives the matrix from cache
## (3) getmatrixCS() - retreives the matrix checksum from cache
## (4) getinverse() - retrieves the inverse matrix from cache 
## (5) setinverse() - overwrites the cached inverse matrix with a new inverse
## (6) getinverseCS() - retrieves the inverse matrix checksum from cache 
## (7) setinverseCS() - overwrites the cached inverse matrix checksum
##
## NB - setmatrix(), setinverse() & setinverseCS() use the super assignment
## operator (<<-) to overwrite cached values in the object (not local copies)
makeCacheMatrix 	<- function(Matrix = matrix()) {

    ## On instantiation the inverse matrix has not yet been computed
    Inverse 		<- NULL
    ## As the inverse matrix has not yet been computed set its checksum to NULL
    InverseCS 		<- NULL
    ## On instantiation compute and cache the direct matrix checksum
    MatrixCS 		<- computeCS(Matrix)
    
    setmatrix 		<- function(Input){
        ## Cache the new matrix
        Matrix 		<<- Input
        ## If the matrix is changed then the inverse is incorrect
        Inverse 	<<- NULL
        ## The inverse matrix checksum is also now incorrect
        InverseCS 	<- NULL
        ## The direct matrix checksum also requires to be recomputed and cached
        MatrixCS 	<<- computeCS(Matrix)
        }
    
    getmatrix 		<- function() Matrix 
    getmatrixCS 	<- function() MatrixCS   
    setinverse 		<- function(Input) Inverse <<- Input
    getinverse 		<- function() Inverse 
    setinverseCS 	<- function(Input) InverseCS <<- Input
    getinverseCS 	<- function() InverseCS
            
    list(setmatrix = setmatrix, getmatrix = getmatrix, 
         getmatrixCS = getmatrixCS, setinverse = setinverse, 
         getinverse = getinverse, setinverseCS = setinverseCS, 
         getinverseCS = getinverseCS)
}

## The cacheSolve function takes a makeCacheMatrix object and returns the 
## inverse of the matrix cached in it. It checks to see if the cached matrix has
## changed since the checksum was computed and if so, prompts the user to 
## re-enter the matrix and returns NULL for the the inverse. if not, then it
## checks if the inverse is already in cache and if the inverse checksum is
## correct, if so, it returns the cached value of the inverse. If not, it calls 
## solve() to invert the matrix, computes the inverse checksum, caches these and 
## returns the inverse.
cacheSolve 			<- function(Object, ...) {

    ## Get the cached values of the matrix, its inverse and the checksums
    myMatrix 		<- Object$getmatrix()    
    myInverse 		<- Object$getinverse()
    myMatrixCS 		<- Object$getmatrixCS()
    myInverseCS 	<- Object$getinverseCS()
    
    ## Test if the checksum for the cached matrix agrees with the cached value
    if(abs(myMatrixCS-computeCS(myMatrix)) < 1.0E-10){

        ## Test to see if the inverse is in cache & its checksum is correct
        if((!is.null(myInverse)) && 
               (abs(myInverseCS-computeCS(myInverse)) < 1.0E-10)){
            ## Return the cached inverse matrix
            message("getting cached data")
            return(myInverse)

            } else {
            ## Either the inverse matrix is not in cache or its checksum is 
            ## incorrect, either way recompute & cache both and return inverse
            message("inverting matrix")
            myInverse 		<- solve(myMatrix, ...)
            myInverseCS 	<- computeCS(myInverse)
            Object$setinverse(myInverse)
            Object$setinverseCS(myInverseCS)
            return(myInverse)
        }

    } else {
        ## Cached matrix has been changed - prompt to re-enter and return NULL 
        ## for the inverse since this forces the user to re-enter whatever is
        ## the correct value for the matrix (since we can't know whether the 
        ## changes were intentional or accidental)
        message('matrix altered - re-enter using $setmatrix()')
        return(NULL)
    }
}

## A simple but effective test procedure:
##
## > m <- matrix(rnorm(4000000),2000,2000)
## > cm <- makeCacheMatrix(m)
## > icm <- cacheSolve(cm)
## > icm <- cacheSolve(cm)
## > cm$setmatrix(matrix(rnorm(4000000),2000,2000))
## > icm <- cacheSolve(cm)
## > icm <- cacheSolve(cm)
##      Get the environment ID of the cached data in the cm object
## > env <- environment(cm$getinverse) 
##      Corrupt the cached Matrix in the cm object
## > assign("Matrix", matrix(rnorm(4000000),2000,2000), envir = env)
## > icm <- cacheSolve(cm)
## > icm <- cacheSolve(cm)
## > cm$setmatrix(matrix(rnorm(4000000),2000,2000))
## > icm <- cacheSolve(cm)
## > icm <- cacheSolve(cm)
##      Corrupt the cached Inverse in the cm object
## > assign("Inverse", matrix(rnorm(4000000),2000,2000), envir = env)
## > icm <- cacheSolve(cm)
## > icm <- cacheSolve(cm)
##
## As well as the different messages output, you'll notice a big difference in 
## processing times for the consecutive cacheSolve calls.

