#' Generate and format a correlation table for publication.
#' @param x            Tibble or dataframe. Numeric variables as columns, observations as rows.
#' @param use          Character. "pairwise" is the default value and will do pairwise deletion of cases. "complete" will select just complete cases.
#' @param method       Character. "pearson" is the default value. The alternatives to be passed to cor are "spearman" and "kendall"
#' @param digits       Numeric. Number of digits to be displayed in the formated table.
#' @param variables    Character vector. Names of the variables in rows (if different from names in the dataframe).
#' @param addpval      Logical. Whether p-values should be added below the correlations.
#' @return             A correlation table formatted for publication.
#' @importFrom psych corr.test
#' @importFrom tibble as_tibble
#' @importFrom dplyr select
#' @importFrom stats median
#' @importFrom stats sd
#' @importFrom psych corr.test
#' @export


stat_bivariate <- function(x,
                           use = "pairwise",
                           method = "pearson",
                           digits = 3,
                           variables = NULL,
                           addpval = T) {

  # Reformat to allow processing
  x <- as.data.frame(x)

  # Get the correlations and their significance
  ct <- psych::corr.test(x, y = NULL, use, method) # compute correlations
  r <- ct$r # get correlation coefs
  r[upper.tri(r, diag = TRUE)] <- NA
  p <- ct$p # get p-values
  p[upper.tri(p, diag = TRUE)] <- NA

  # Generate significance star
  stars <- ifelse(
    is.na(p), NA, ifelse(
      p < .001, "***", ifelse(
        p < .01, "** ", ifelse(
          p < .05, "*  ", "   "
        )
      )
    )
  )

  # create empty matrix and add names to rows and columns
  m <- matrix(NA, nrow = nrow(r) * 2, ncol = ncol(r) + 1)
  if (is.null(variables)) {
    rlab <- names(x)
  } else {
    rlab <- variables
  }
  rlab <- paste0(seq_len(length(x)), ". ", rlab)
  clab <- paste0("(", seq_len(length(x)), ")")
  rows <- seq_len(nrow(m)) # row indices
  cols <- 2:ncol(m) # column indices
  odd <- rows %% 2 == 1 # odd rows
  even <- rows %% 2 == 0 # even rows
  m[odd, 1] <- rlab # add variable names
  m[even, 1] <- rep("", sum(even)) # add blank
  colnames(m) <- c("Correlations", clab) # add colnames

  # Fill in the matrix with the formated correlations and significance and return a tibble.
  m[odd, cols] <- ifelse(is.na(r), NA, paste0(format(round(r, digits), nsmall = digits), stars)) # add r coefs
  if (addpval == T) {
    m[even, cols] <- ifelse(is.na(p), NA, paste0("(", format(round(p, digits), nsmall = digits), ")  ")) # add p-values
  } else {
    m <- m[odd, ]
  }
  m <- m %>%
    tibble::as_tibble()
  m <- m %>% dplyr::select(-length(m))

  m
}
