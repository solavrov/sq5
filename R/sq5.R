#' Function
#' @export
addElement <- function(givenList, elementToAdd) {

  givenList[[length(givenList) + 1]] <- elementToAdd

  return(givenList)

}

#' Function
#' @export
getAllDots <- function(dimOfSquare) {

  allDots <- list()

  for (j in 1:dimOfSquare) {
    for (i in 1:dimOfSquare) {
      allDots <- addElement(allDots,c(j,i))
    }
  }

  return(allDots)

}

#' Function
#' @export
buildSquareWithRandomDots <- function(dimOfSquare, numOfDots) {

  square <- matrix(numeric(dimOfSquare^2), nrow = dimOfSquare)

  markedDots <- sample(getAllDots(dimOfSquare), numOfDots)

  for (i in 1:numOfDots) {
    dot <- markedDots[[i]]
    square[dot[1], dot[2]] <- 1
  }

  return(square)

}

#' Function
#' @export
sumAlongLine <- function(givenSquare, givenLine) {

  sum <- 0

  for (i in 1:length(givenLine)) {
    dot <- givenLine[[i]]
    sum <- sum + givenSquare[dot[1], dot[2]]
  }

  return (sum)

}

#' Function
#' @export
getVerticalLines <- function(dimOfSquare) {

  setOfLines <- list()

  for (j in 1:dimOfSquare) {

    aLine <- list()

    for (i in 1:dimOfSquare) {
      aLine <- addElement(aLine, c(i,j))
    }

    setOfLines <- addElement(setOfLines, aLine)

  }

  return(setOfLines)

}

#' Function
#' @export
getHorizontalLines <- function(dimOfSquare) {

  setOfLines <- list()

  for (j in 1:dimOfSquare) {

    aLine <- list()

    for (i in 1:dimOfSquare) {
      aLine <- addElement(aLine, c(j,i))
    }

    setOfLines <- addElement(setOfLines, aLine)

  }

  return(setOfLines)

}

#' Function
#' @export
isDotIn <- function(dimOfSquare, givenDot) {

  all(givenDot >= 1) && all(givenDot <= dimOfSquare)

}

#' Function
#' @export
drawLine <- function(dimOfSuare, startDot, dirVector) {

  aLine <- list()
  dot <- startDot

  while (isDotIn(dimOfSuare,dot)) {
    aLine <- addElement(aLine, dot)
    dot <- dot + dirVector
  }

  return (aLine)

}

#' Function
#' @export
getUpperLeftEdge <- function(dimOfSquare) {

  anEdge <- addElement(list(), c(1,1))

  for (i in 2:dimOfSquare) {
    anEdge <- addElement(anEdge, c(1,i))
    anEdge <- addElement(anEdge, c(i,1))
  }

  return (anEdge)

}

#' Function
#' @export
getUpperRightEdge <- function(dimOfSquare) {

  anEdge <- addElement(list(), c(1, dimOfSquare))

  for (i in 1:(dimOfSquare-1)) {
    anEdge <- addElement(anEdge, c(1, dimOfSquare - i))
    anEdge <- addElement(anEdge, c(i + 1, dimOfSquare))
  }

  return (anEdge)

}

#' Function
#' @export
getDiagLines <- function(dimOfSquare, minNumOfDots, dirVector) {

  refVector <- c(dirVector[1], -dirVector[2])

  setOfLines <- list()

  anEdge <- getUpperLeftEdge(dimOfSquare)

  for (dot in anEdge) {

    aLine <- drawLine(dimOfSquare,dot,dirVector)

    if (length(aLine) >= minNumOfDots) {
      setOfLines <- addElement(setOfLines, aLine)
    }

  }

  anEdge <- getUpperRightEdge(dimOfSquare)

  for (dot in anEdge) {

    aLine <- drawLine(dimOfSquare,dot,refVector)

    if (length(aLine) >= minNumOfDots) {
      setOfLines <- addElement(setOfLines, aLine)
    }

  }

  return (setOfLines)

}

#' Function that looks for 5x5 solutions and print them
#' @export
square5 <- function(numOfTries = 10^5) {

  cat("Problem: square 5x5, find 10 cells, no more than 2 on one line\n")
  cat("Number of tries =",numOfTries,"\n")

  setOfLines <- list()
  setOfLines <- append(setOfLines, getVerticalLines(5))
  setOfLines <- append(setOfLines, getHorizontalLines(5))
  setOfLines <- append(setOfLines, getDiagLines(5, 3, c(1,1)))
  setOfLines <- append(setOfLines, getDiagLines(5, 3, c(1,2)))
  setOfLines <- append(setOfLines, getDiagLines(5, 3, c(2,1)))

  isFound <- FALSE
  isFoundOnce <- FALSE
  solutionNum <- 1

  t0 <- as.numeric(Sys.time()) * 1000

  for (i in 1:numOfTries) {

    isFound <- TRUE

    square <- buildSquareWithRandomDots(5,10)

    for (aLine in setOfLines) {
      if(sumAlongLine(square,aLine) > 2) {
        isFound <- FALSE
        break
      }
    }

    if (isFound) {
      cat("solution #", solutionNum,"\n")
      print(square)
      solutionNum <- solutionNum + 1
      isFoundOnce <- TRUE
    }

  }

  t1 <- as.numeric(Sys.time()) * 1000
  dt <- round(t1 - t0)

  cat("We finished. It took",dt,"millisecs\n")

  if (!isFoundOnce) cat("Not lucky in finding solution this time :-( Try again!")

}













