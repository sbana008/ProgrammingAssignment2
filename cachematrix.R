## Please read the bottom comments for getting the output


 makeCacheMatrix <- function(x = matrix()){  ## my 1st function
           inv <- NULL   
           ## below are the 4 functions referring the vector mean problem. 
           set <- function(y){                   
                  x <<- y                             
                 inv <<- NULL                       
             }
            get <- function() x                     
           
            setInverse <- function(inverse) inv <<- inverse  
            getInverse <- function() inv 
            
            ## the below list can be sent to cacheSolve().
            list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)  
                                                                                           
        }
      
cacheSolve <- function(x, ...) {  ## my 2nd function
         
             inv <- x$getInverse()
             
             ## if statement to check if the cached inverse is present or not.
              if(!is.null(inv)) {
                    message("getting cached data")
                    return(inv)
                }
             data <- x$get()
              inv <- solve(data, ...)
              x$setInverse(inv)
              inv
}

## for output please try below squared matrix  to pass into makeCacheMatrix function and 
## assign it to x and then pass the x into cacheSolve function or else it shows invalid atomic error.

## use the following example to try the output
## matrix(c(1, 0, 5, 2, 1, 6, 3, 5, 0), 3,3)
## IF not some squared matrices are producing errors like 
## Error in solve.default(x) : 
#  Lapack routine dgesv: system is exactly singular: U[3,3] = 0
