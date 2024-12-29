#' Training and Testing Partitioning
#'
#' This function is intended for splitting a dataframe into a training set and a
#' testing set given a binary response variable. It creates a training set by
#' randomly sampling half of the instances where the specified variable has a
#' value of `1` and half of theinstances where it has a value of `0`.
#' The function returns the indices of the selected samples for further use.
#'
#' @param df A data frame containing the data to be split.
#' @param var A character string specifying the name of the binary response variable
#' (column) in the data frame. The values in this column should be `1` and `0`.
#'
#' @return A vector of indices representing a random sample of half of the rows
#' where the specified variable is `1` and half where it is `0`.
#'
#' @examples
#' # Example usage with a data frame df and a binary column "target"
#' df <- data.frame(target = c(1, 0, 1, 0, 1, 1, 0, 0))
#' train_indices <- splitting(df, "target")
#' train <- df[train_indices,]
#' test <- df[-train_indices,]
#'
#' @export
splitting <- function(df, var){
  # partition into majority and minority classes
  ones <- which(df[[var]]==1) # indices of 1's
  zeros <- which(df[[var]]==0) # indices of 0's
  train_inds <- c(sample(ones,size = length(ones)/2, replace = FALSE),
                  sample(zeros,size = length(zeros)/2, replace = FALSE))
  return(train_inds)
}

