\name{data.table-condition-classes}
\alias{data.table-condition-classes}
\title{Condition Handling with Classed Conditions}
\description{
\code{data.table} provides specific condition classes for common operations, making it easier to handle conditions programmatically. This is particularly useful when writing robust code or packages that use \code{data.table}. Relying on the exact text of condition messages is fragile (it is not uncommon to change the wording slightly, or for the user's session not to be in English); prefer using the signal class where possible.
}
\details{
\subsection{Available Condition Classes}{
\code{data.table} provides the following specific condition classes:

Error Classes:
\itemize{
  \item \code{dt_missing_column_error}: When referencing columns that don't exist
  \item \code{dt_invalid_input_error}: When providing invalid input types or empty required arguments
  \item \code{dt_unsortable_type_error}: When trying to sort/key unsupported types
  \item \code{dt_join_type_mismatch_error}: When column types are incompatible in joins/set operations
  \item \code{dt_invalid_let_error}: When using assignment operators incorrectly
}

Warning Classes:
\itemize{
  \item \code{dt_missing_fun_aggregate_warning}: When aggregation function is missing in operations that require it
}
}

\subsection{Backward Compatibility}{
All condition classes inherit from base R's condition system, so existing \code{tryCatch(..., error = ...)} code continues to work unchanged. The new classes simply provide more specific handling options when needed.
}
}
\examples{
    
# Handle missing column errors specifically
DT <- data.table(a = 1:3, b = 4:6)
tryCatch({
  setkey(DT, nonexistent_col)
}, dt_missing_column_error = function(e) {
  cat("Missing column detected:", conditionMessage(e), "\n")
}, error = function(e) {
  cat("Other error:", conditionMessage(e), "\n")
})
}
\seealso{
\code{\link{tryCatch}}, \code{\link{test}}, \url{https://adv-r.hadley.nz/conditions.html}
}
