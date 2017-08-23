## To use it :
## 0) source("cachematrix.R")
## Remark : we assume matrices to be always invertible in this exercise

## makeCacheMatrix : this function creates a special "matrix" object that can cache
## its inverse

## 1) Create a "makeCacheMatrix" object called mcm :
##        mcm <- makeCacheMatrix()
## Now, mcm is an empty "makeCacheMatrix" object where I can set/get a matrix and set/get 
## the inverse of this matrix
## If I type :
##        mcm$get()
## I will get :
##            [,1]
##        [1,]  NA
## because nothing is stored in yet
## It's the same if I do :
##        mcm$getinv()
## I will get :
##        NULL


## 2) Let's create a matrix m1 :
##        m1 <- matrix(1:4, 2, 2)
## If I type m1, I will get :
##            [,1] [,2]
##        [1,]    1    3
##        [2,]    2    4
## Now, I set m1 into mcm :
##        mcm$set(m1)
## I could also put this m1 matrix at the creation of mcm like this :
##        mcm <- makeCacheMatrix(m1)

## 3) If I want to see which matrix is stored in mcm :
##        mcm$get()
## I will get :
##             [,1] [,2]
##        [1,]    1    3
##        [2,]    2    4

## 4) If I want to store m1's inverse in mcm :
##        mcm$setinv(solve(m1))

## 5) If I want to get m1's inverse from mcm :
##        mcm$getinv()
## I will get :
##             [,1] [,2]
##        [1,]   -2  1.5
##        [2,]    1 -0.5


makeCacheMatrix<- function(x = matrix()) {
  inv<- NULL
  set <- function(y){
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setinv<- function(inverse) inv <<- inverse
  getinv<- function() inv
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)
  
}


## cacheSolve : this function computes the inverse of the special "matrix" returned
## by makeCacheMatrix above. If the inverse has already been 
## calculated (and the matrix has not changed), then the cachesolve 
## should retrieve the inverse from the cache.

## 1) Let's call cacheSolve with the mcm object created above :
##        cacheSolve(mcm)
## I get :
##        getting cached data
##             [,1] [,2]
##        [1,]   -2  1.5
##        [2,]    1 -0.5
## Meaning it used the inverse I stored in the cache before.

## On the other hand, If I create a new "makeCacheMatrix" called mcm2 :
##        mcm2 <- makeCacheMatrix(matrix(5:8, 2, 2))
## I didn't set any value for the inverse of this matrix. So, when I do :
##        cacheSolve(mcm2)
## I get :
##             [,1] [,2]
##        [1,]   -4  3.5
##        [2,]    3 -2.5
## I didn't get the "getting cached data" message this time because it's the firt time
## it's been calculated. But If I do it again, I will get the message, as by now, it's 
## stored into the mcm2 object.



cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  inv <- x$getinv()
  if(!is.null(inv)){
    message("getting cached data")
    return(inv)
  }
  data <- x$get()
  inv <- solve(data, ...)
  x$setinv(inv)
  inv
}