#' Format in-text reporting of confidence intervals or p-values around an estimate.
#' @param coefficient Character. Name or identifier of the coefficient.
#' @param estimate    Numeric. Estimate of the coefficient.
#' @param std.error   Numeric. Standard error arounf the estimate.
#' @param threshold   Numeric. Probability of 0 contained in the confidence interval.
#' @param digits      Integer. Number of digits for rounding.
#' @param stats       Character vector. Statistics to report: "cf" for the coefficient, "ci" for the confidence internal, "pv" for the p-value
#' @return Character vector with estimates formatted with confidence intervals or p-values for publication.
#' @importFrom dplyr case_when
#' @importFrom stats qnorm
#' @importFrom stats pnorm
#' @export

stat_report <- function(coefficient = "$\beta$",
                        estimate = NULL,
                        std.error = NULL,
                        threshold = 0.05,
                        digits = 3,
                        stats = c("cf", "ci", "pv")) {
  if ("cf" %in% stats) {
    cf <- paste0(coefficient, " = ", round(estimate, digits))
  } else {
    cf <- ""
  }

  if ("ci" %in% stats) {
    ci <- dplyr::case_when(
      threshold == 0.01 ~ "99% CI ",
      threshold == 0.05 ~ "95% CI ",
      threshold == 0.10 ~ "90% CI ",
      TRUE ~ ""
    )

    ll <- round(estimate - qnorm(1 - threshold / 2) * std.error, digits)
    ul <- round(estimate + qnorm(1 - threshold / 2) * std.error, digits)

    ci <- paste0(ci, "[", ll, ", ", ul, "]")
  } else {
    ci <- ""
  }


  if ("pv" %in% stats) {
    pval <- 2 * pnorm(-abs((estimate - 0) / (std.error)))

    pv <- dplyr::case_when(
      pval <= 0.01 ~ "P < 0.01",
      pval <= 0.05 ~ "P < 0.05",
      pval <= 0.1 ~ "P < 0.1",
      TRUE ~ "n.s."
    )
  } else {
    pv <- ""
  }

  report <- dplyr::case_when(
    "cf" %in% stats & "ci" %in% stats & "pv" %in% stats ~ paste0("(", cf, ", ", ci, ", ", pv, ")"),
    "ci" %in% stats & "pv" %in% stats ~ paste0("(", ci, ", ", pv, ")"),
    "cf" %in% stats & "pv" %in% stats ~ paste0("(", cf, ", ", pv, ")"),
    "cf" %in% stats & "ci" %in% stats ~ paste0("(", cf, ", ", ci, ")"),
    "cf" %in% stats ~ paste0("(", cf, ")"),
    "ci" %in% stats ~ paste0("(", ci, ")"),
    "pv" %in% stats ~ paste0("(", pv, ")")
  )

  return(report)
}
