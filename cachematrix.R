#write a pair of functions that caches the inverse of a matrix

##This function creates a special "matrix" object that can cache its inverse

makeCacheMatrix <- function(x = matrix()) { ##setting all initial values to NULL
  i  <- NULL 
set  <- function(y){ #set values in matrix
 x <<- y #caches inputted matrix
i <<- NULL  
 } 

get  <- function() x #get values of matrix
setinverse  <- function(inverse) i  <<- inverse #get inverse of values
getinverse  <- function() i #get values of set inverse and cahce
list(set= set, get = get,setinverse = setinverse, getinverse = getinverse) 
  

}


## This function computes the inverse of the special "matrix" returned by  makeCacheMatrix  above. If the inverse has already 
##been calculated (and the matrix has not changed), then  cacheSolve  should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
  i  <- x$getinverse() #if inversed is already cached than get it
  
 if (!is.null(i)){ #if matrix is still the same and hasnt been cached return getting data
 message("getting cached data") 
 return(i) 
   } 
 # if matrix isnt already there
data  <- x$get() #get matrix
 i  <- solve(data, ...) #compute inverse
  x$setinverse(i) #cache inverse
        i #give inverse
  
}
