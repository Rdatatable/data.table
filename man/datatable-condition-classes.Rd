\name{data.table-error-handling}
\alias{data.table-error-handling}
\title{Error Handling with Classed Conditions}
\description{
\code{data.table} provides specific condition classes for common operations, making it easier to handle errors programmatically. This is particularly useful when writing robust code or packages that use \code{data.table}. Relying on the exact text of error messages is fragile (it is not uncommon to change the wording slightly, or for the user's session not to be in English); prefer using the signal class where possible.
}
\details{
\subsection{Available Error Classes}{
\code{data.table} provides four specific error classes:

\itemize{
  \item \code{dt_missing_column_error}: When referencing columns that don't exist
  \item \code{dt_invalid_input_error}: When providing invalid input types or empty required arguments
  \item \code{dt_unsortable_type_error}: When trying to sort/key unsupported types
  \item \code{dt_join_type_mismatch_error}: When column types are incompatible in joins/set operations
}
}

\subsection{Backward Compatibility}{
All error classes inherit from base R's condition system, so existing \code{tryCatch(..., error = ...)} code continues to work unchanged. The new classes simply provide more specific handling options when needed.
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
\code{\link{tryCatch}}, \url{https://adv-r.hadley.nz/conditions.html}
}
