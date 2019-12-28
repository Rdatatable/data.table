\name{fread_output{data.table}}
\alias{fread_output}
\title{
  Fast and friendly file finagler: Output styled data.table/data.frame 
}
\description{
fread() cannot directly read strings(data.frames and data.tables) copied and pasted from R terminal. fread_output() is a viable alternative
}
\usage{
fread_output(str,
             stringsAsFactors = FALSE,
             showProgress=getOption("datatable.showProgress", interactive()),
             data.table=getOption("datatable.fread.datatable", TRUE),
             logical01=getOption("datatable.logical01", FALSE)
}

\arguments{
  \item{str}
  {
  A single character String(containing addtional numeric row index) to be converted
  }
  \item{stringsAsFactors}
  {
  Convert all character columns to factors?
  }
  \item{showProgress}
  {
    TRUE displays progress on the console if the ETA is greater than 3 seconds.
  }
  \item{data.table}
  {
    FALSE returns a data.frame, TRUE returns data.table
  }
  \item{logical01}
  {
    if TRUE a column containing only 0s and 1s will be read as logical, otherwise as integer
  }
}
\details{
  The function creates an additonal column fread_output_TEMP_COLUMN_NAME to store the index names, implicitly calls fread() and then deletes the additional column
}
\value{
Returns corresponding data.table object if data.table argument is set to TRUE otherwise returns data.frame object
}

\examples{
fread_output("ID  time nTest
 1: 1650 6.714    91
 2: 1652 6.376    91
 3: 1648 6.338    91
 4: 1509 5.446     1
 5: 1779 4.541    13
 6: 1646 4.060    91
 7: 1644 3.572    91
 8: 1642 3.416    91
 9: 1437 2.261   528
 10: 1151 1.828     2")
}

\keyword{ data }
\keyword{ output }
