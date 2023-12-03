#AoC 2023 Day 3

library(dplyr)
library(stringr)

test <- here::here('2023/Day3/test.txt') # Test data
input <- here::here('2023/Day3/input.txt') # Input data

df <- read.delim(input, sep ="", header = FALSE, col.names = 'code') # Read data
l <- str_length(df$code[1]) # Get length (# of columns)

mat <- str_split_fixed(df$code, pattern = "", n=l) # transform data to matrix

# Extract all the special characters
specialVec <- c()
for(i in 1:nrow(mat)) {
  for(j in 1:ncol(mat)) {
    if(mat[i,j] != "." & is.na(as.numeric(mat[i,j]))){
      specialVec <- c(specialVec, mat[i,j])
    }
  }
}
special <- unique(specialVec)

# Pad matrix with periods
newRow <- matrix(rep('.', l), nrow=1, ncol=l)
newCol <- matrix(rep('.', l+2), nrow=(l+2), ncol=1)
mat2 <- cbind(newCol, rbind(newRow, mat, newRow), newCol)

# Store dimensions of padded matrix
nRow <- dim(mat2)[1]
nCol <- dim(mat2)[2]

# Define function to check for special characters around the current element
checkSafe <- function(mat, p, q){
  safe <- 0
  if(
    mat[p-1, q-1] %in% special |
    mat[p-1, q] %in% special |
    mat[p-1, q+1] %in% special |
    mat[p, q-1] %in% special |
    mat[p, q+1] %in% special |
    mat[p+1, q-1] %in% special |
    mat[p+1, q] %in% special |
    mat[p+1, q+1] %in% special
  ) {safe <- 1}
  return(safe)
}

# Initialise vectors
numVec <- c()
safeVec <- c()
number <- ""
safe <- 0

# Loop
for (i in 2:(nRow-1)) { # Loop through rows

  number <- "" # Reset number to blank at start of each row
  safe <- 0

  for (j in 2:(nCol-1)) { # Loop through column element

      # If you come to the end of a consecutive number post the number + current safe status
      if(mat2[i,j] %in% c(".", special) & number != ""){
        numVec <- c(numVec, number)
        safeVec <- c(safeVec, safe)
        number <- ""
        safe <- 0
      }

      # If current element is numeric append to current number and update safe status
      if(!is.na(as.numeric(mat2[i,j]))) {

        number <- str_c(number, mat2[i,j])

          if(checkSafe(mat2, i, j)==1){
            safe <- 1
          }

      }
    }

  }

# Compile number and safe status and calculate sum
dt <- data.frame(num = as.numeric(numVec), safe = safeVec)

filter(dt, safe==1) |>
  summarise(ans=sum(num))
