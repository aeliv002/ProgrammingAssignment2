## makeCacheMatrix function creates cached object - matrix. Also returns functions for later usage.
## cacheSolve function returns inverseMatrix if possible from makeCacheMatrix object. 
## If inverseMatrix object was created. It returns it from the cache.

##  makeCacheMatrix
##    input parameter:
##      inputMatrix is any dimension matrix.
##
##  makeCacheMatrix cached objects:
##    matrix: Type matrix. Desc: Input matrix or null if inputMatrix is not matrix object
##    inverseMatrix: Type matrix. Desc: inversed matrix or null. NULL conditions: 
##                  * function doInverseMatrix was not called.
##                  * function doInverseMatrix was called, but it is imposible to solve matrix.
##                  * function doInverseMatrix was called, but matrix is not squared.
##
##  returns 4 functions:
##    setMatrix: set matrix object
##    getMatrix: getter for matrix object
##    doInverseMatrix: function which tries to create inverseMatrix if possible.
##    getInverseMatrix: getter for inverseMatrix

makeCacheMatrix <- function(inputMatrix){
  
  matrix <- NULL
  inverseMatrix <- NULL
  
  ## Setting matrix if inputMatrix is matrix object.
  
  setMatrix <- function (inputMatrix){
    if (is.matrix(inputMatrix)){
      matrix <<-  inputMatrix
    }
    else{
      message(paste("Input is not a matrix object."))
    }
  }
  
  getMatrix <- function (){
    matrix
  }
  
  ## doInversion:
  ##   TryCatchBlock.
  ##      solve(matrix) - called expression.
  ##      if error inverseMatrix is set to NULL and error message is printed to console.  
  doInverseMatrix <-function (){
    
    tryCatch(
      expr={
          inverseMatrix <<- solve(matrix)
      },
      error=
        function(e){
          message("Cannot inverse matrix. Returning NULL value. The problem is:")
          message(paste("ERROR:",e[[1]]))
          inverseMatrix <<- NULL
        },
      finnaly={}
    )
  }

  getInverseMatrix <- function (){
    inverseMatrix
  }

  ## When function makeCacheMatrix, setMatrix function is called. 
  ## calling setMatrix function assures that inputMatrix is matrix object.
  setMatrix(inputMatrix)

  # Return list of available functions
  list(
    setMatrix=setMatrix,
    getMatrix=getMatrix,
    doInverseMatrix=doInverseMatrix,
    getInverseMatrix=getInverseMatrix
  )
}

## cacheSolve
##   input parameter makeCacheMatrixObject
## Returns:
##   inverse matrix.
##
## This function tries to get makeCacheMatrixObject.
## If inverseMatrix allready exist (is not NULL) it gets it from cache.
## If inverseMatrix does not exist (is NULL) function calls makeCacheMatrixObject$doInverseMatrix().

cacheSolve <- function(makeCacheMatrixObject=NULL){

  get <- function (makeCacheMatrixObject){
    if (
      is.null(makeCacheMatrixObject$getInverseMatrix())){
      # trying to get inverseMatrix
      makeCacheMatrixObject$doInverseMatrix()
    }
    else{
      # if inverseMatrix exists print message
      message("getting from cashe");
    }
    
    # Returns inverseMatrix
    makeCacheMatrixObject$getInverseMatrix()
  }
  
  ## Get function is wrapped in tryCatch function.
  ## It is done, because makeCacheMatrixObject can be not makeCacheMatrixObject but something else.
  tryCatch(
    expr={
      get(makeCacheMatrixObject)
    },
    # anonymous function
    error = function(e){
      message("Something is wrong with input parameter. Error message:")
      message(paste("ERROR:",e[[1]]))
      return(NULL)
    },
    finnaly={}
  )
}