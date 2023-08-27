#' @name table_bivariate
#' @title Report bivariate statistics
#' @author Nicolas Mangin
#' @description Generate and format a correlation table for publication.
#' @param x            Tibble or dataframe. Numeric variables as columns, observations as rows.
#' @param use          Character. "pairwise" is the default value and will do pairwise deletion of cases. "complete" will select just complete cases.
#' @param method       Character. "pearson" is the default value. The alternatives to be passed to cor are "spearman" and "kendall"
#' @param digits       Numeric. Number of digits to be displayed in the formated table.
#' @param variables    Character vector. Names of the variables in rows (if different from names in the dataframe).
#' @param addpval      Logical. Whether p-values should be added below the correlations.
#' @return             A correlation table formatted for publication.
#' @importFrom dplyr select
#' @importFrom psych corr.test
#' @importFrom tibble as_tibble
#' @export


table_bivariate <- function(x,
                           use = "pairwise",
                           method = "pearson",
                           digits = 3,
                           variables = NULL,
                           addpval = T) {

  # Reformat to allow processing
  x <- base::as.data.frame(x)

  # Get the correlations and their significance
  ct <- psych::corr.test(x, y = NULL, use, method) # compute correlations
  r <- ct$r # get correlation coefs
  r[base::upper.tri(r, diag = TRUE)] <- NA
  p <- ct$p # get p-values
  p[base::upper.tri(p, diag = TRUE)] <- NA

  # Generate significance star
  stars <- base::ifelse(
    base::is.na(p), NA, base::ifelse(
      p < .001, "***", base::ifelse(
        p < .01, "** ", base::ifelse(
          p < .05, "*  ", "   "
        )
      )
    )
  )

  # create empty matrix and add names to rows and columns
  m <- base::matrix(NA, nrow = base::nrow(r) * 2, ncol = base::ncol(r) + 1)
  if (base::is.null(variables)) {
    rlab <- base::names(x)
  } else {
    rlab <- variables
  }
  rlab <- base::paste0(base::seq_len(base::length(x)), ". ", rlab)
  clab <- base::paste0("(", base::seq_len(base::length(x)), ")")
  rows <- base::seq_len(base::nrow(m)) # row indices
  cols <- 2:base::ncol(m) # column indices
  odd <- rows %% 2 == 1 # odd rows
  even <- rows %% 2 == 0 # even rows
  m[odd, 1] <- rlab # add variable names
  m[even, 1] <- base::rep("", base::sum(even)) # add blank
  base::colnames(m) <- c("Correlations", clab) # add colnames

  # Fill in the matrix with the formated correlations and significance and return a tibble.
  m[odd, cols] <- base::ifelse(base::is.na(r), NA, base::paste0(base::format(base::round(r, digits), nsmall = digits), stars)) # add r coefs
  if (addpval == T) {
    m[even, cols] <- base::ifelse(base::is.na(p), NA, base::paste0("(", base::format(base::round(p, digits), nsmall = digits), ")  ")) # add p-values
  } else {
    m <- m[odd, ]
  }
  m <- m |>
    tibble::as_tibble()
  m <- m |> dplyr::select(-base::length(m))

  m
}
