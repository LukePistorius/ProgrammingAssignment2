## The below 2 functions work together: the first on you enter the matrix
## it sets a default (NULL) for i --> if it's the first time this
## matrix has been inversed this will be NULL. 
## it also creates a list.
## (after using this before using cachesolve x$get = the matrix)

## the second function will do a lookup to X$getinverse() if it's NULL then
## we know it hasn't been cached if there is a value it will return the inverse.
## it will then get the x matrix and inverse it with the solve function
## after that it will store it 

# 1) setting the defaults and x
# 2) save it to the parent environment and not in the functions environment (Caching)
# 3) creating a list with 4 functions in it

makeCacheMatrix <- function(x = matrix()) {
        i <- NULL
        set <- function(y) {
                x <<- y
                i <<- NULL
        }
        get <- function() x
        setinverse <- function(solve) i <<- solve
        getinverse <- function() i
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}



# it takes the list created from above as an entree
# checks if i = NULL, if not then it has been calculated before and it needs to come from the cash
# if it's the first time, it needs to be calculated and saved

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        

                i <- x$getinverse()
                if(!is.null(i)) {
                        message("getting cached data")
                        return(i)
                }
                data <- x$get()
                i <- solve(data, ...)
                x$setinverse(i)
                i
 }
        
