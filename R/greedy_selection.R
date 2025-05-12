library(purrr)

greedy_set_cover_na <- function(matrix, category_indices) {
  # Input:
  # - matrix: Matrix (rows = individuals, columns = markers). 
  #           1 = favorable allele, 0/NA = unfavorable/missing.
  # - category_indices: Row indices of individuals in the target category.
  #
  # Output:
  # List with:
  # - selected: Indices of selected individuals (from category_indices).
  # - covered: Column indices of covered markers (excluding NA).
  
  n_markers <- ncol(matrix)
  covered <- logical(n_markers)  # Tracks covered markers (FALSE initially)
  selected <- integer(0)          # Selected individuals
  remaining <- category_indices   # Candidates not yet selected
  
  while (!all(covered) && length(remaining) > 0) {
    # Calculate new coverage for each candidate, ignoring NA
    new_coverage <- sapply(remaining, function(i) {
      sum(
        (matrix[i, ] == 1) & !covered,  # Count 1s that are not NA and not covered
        na.rm = TRUE
      )
    })
    
    # Stop if no candidate provides new coverage
    if (max(new_coverage, na.rm = TRUE) == 0) break
    
    # Select the best individual (first one in case of ties)
    best_idx <- which.max(new_coverage)
    best_individual <- remaining[best_idx]
    
    # Update selected individuals
    selected <- c(selected, best_individual)
    
    # Update covered markers (only 1s, ignoring NA)
    new_covered <- (matrix[best_individual, ] == 1) & !is.na(matrix[best_individual, ])
    covered <- covered | new_covered
    
    # Remove selected individual from remaining candidates
    remaining <- remaining[-best_idx]
  }
  
  # Return results
  list(
    selected = selected,
    covered = which(covered)
  )
}

# USAGE

#dat <- lapply(1:50, function(i) sample(c(0,1, NA), 10, prob=c(.5, .4, 0.1),
#                                       replace=TRUE))
#mt <- do.call(rbind, dat)
#greedy_set_cover_na(mt, 1:50)

