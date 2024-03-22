\name{setDTthreads}
\alias{setDTthreads}
\alias{getDTthreads}
\alias{openMP}
\alias{openmp}
\title{ Set or get number of threads that data.table should use }
\description{
  Set and get number of threads to be used in \code{data.table} functions that are parallelized with OpenMP. The number of threads is initialized when \code{data.table} is first loaded in the R session using optional environment variables. Thereafter, the number of threads may be changed by calling \code{setDTthreads}. If you change an environment variable using \code{Sys.setenv} you will need to call \code{setDTthreads} again to reread the environment variables.
}
\usage{
  setDTthreads(threads = NULL, restore_after_fork = NULL, percent = NULL, throttle = NULL)
  getDTthreads(verbose = getOption("datatable.verbose"))
}
\arguments{
  \item{threads}{ NULL (default) rereads environment variables. 0 means to use all logical CPUs available. Otherwise a number >= 1 }
  \item{restore_after_fork}{ Should data.table be multi-threaded after a fork has completed? NULL leaves the current setting unchanged which by default is TRUE. See details below. }
  \item{percent}{ If provided it should be a number between 2 and 100; the percentage of logical CPUs to use. By default on startup, 50\%. }
  \item{throttle}{ 1024 (default) means that, roughly speaking, a single thread will be used when nrow(DT)<=1024, 2 threads when nrow(DT)<=2048, etc. The throttle is to speed up small data tasks (especially when repeated many times) by not incurring the overhead of managing multiple threads. Hence the number of threads is throttled (restricted) for small tasks. }
  \item{verbose}{ Display the value of relevant OpenMP settings plus the \code{restore_after_fork} internal option. }
}
\value{
  A length 1 \code{integer}. The old value is returned by \code{setDTthreads} so you can store that prior value and pass it to \code{setDTthreads()} again after the section of your code where you control the number of threads.
}
\details{
  \code{data.table} automatically switches to single threaded mode upon fork (the mechanism used by \code{parallel::mclapply} and the foreach package). Otherwise, nested parallelism would very likely overload your CPUs and result in much slower execution. As \code{data.table} becomes more parallel internally, we expect explicit user parallelism to be needed less often. The \code{restore_after_fork} option controls what happens after the explicit fork parallelism completes. It needs to be at C level so it is not a regular R option using \code{options()}. By default \code{data.table} will be multi-threaded again; restoring the prior setting of \code{getDTthreads()}. But problems have been reported in the past on Mac with Intel OpenMP libraries whereas success has been reported on Linux. If you experience problems after fork, start a new R session and change the default behaviour by calling \code{setDTthreads(restore_after_fork=FALSE)} before retrying. Please raise issues on the data.table GitHub issues page.

  The number of logical CPUs is determined by the OpenMP function \code{omp_get_num_procs()} whose meaning may vary across platforms and OpenMP implementations. \code{setDTthreads()} will not allow more than this limit. Neither will it allow more than \code{omp_get_thread_limit()} nor the current value of \code{Sys.getenv("OMP_THREAD_LIMIT")}. Note that CRAN's daily test system (results for data.table \href{https://cran.r-project.org/web/checks/check_results_data.table.html}{here}) sets \code{OMP_THREAD_LIMIT} to 2 and should always be respected; e.g., if you have written a package that uses data.table and your package is to be released on CRAN, you should not change \code{OMP_THREAD_LIMIT} in your package to a value greater than 2.

  Some hardware allows CPUs to be removed and/or replaced while the server is running. If this happens, our understanding is that \code{omp_get_num_procs()} will reflect the new number of processors available. But if this happens after data.table started, \code{setDTthreads(...)} will need to be called again by you before data.table will reflect the change. If you have such hardware, please let us know your experience via GitHub issues / feature requests.

  Use \code{getDTthreads(verbose=TRUE)} to see the relevant environment variables, their values and the current number of threads data.table is using. For example, the environment variable \code{R_DATATABLE_NUM_PROCS_PERCENT} can be used to change the default number of logical CPUs from 50\% to another value between 2 and 100. If you change these environment variables using `Sys.setenv()` after data.table and/or OpenMP has initialized then you will need to call \code{setDTthreads(threads=NULL)} to reread their current values. \code{getDTthreads()} merely retrieves the internal value that was set by the last call to \code{setDTthreads()}. \code{setDTthreads(threads=NULL)} is called when data.table is first loaded and is not called again unless you call it.

  \code{setDTthreads()} affects \code{data.table} only and does not change R itself or other packages using OpenMP. We have followed the advice of section 1.2.1.1 in the R-exts manual: "\ldots or, better, for the regions in your code as part of their specification\ldots num_threads(nthreads)\ldots That way you only control your own code and not that of other OpenMP users." Every parallel region in data.table contain a \code{num_threads(getDTthreads())} directive. This is mandated by a \code{grep} in data.table's quality control script.

  \code{setDTthreads(0)} is the same as \code{setDTthreads(percent=100)}; i.e. use all logical CPUs, subject to \code{Sys.getenv("OMP_THREAD_LIMIT")}. Please note again that CRAN's daily test system sets \code{OMP_THREAD_LIMIT} to 2, so developers of CRAN packages should never change \code{OMP_THREAD_LIMIT} inside their package to a value greater than 2.

  Internally parallelized code is used in the following places:

  \itemize{
    \item\file{between.c} - \code{\link{between}()}
    
    Parallelism is used here to speed up range checks that are performed over the elements of the supplied vector. OpenMP is used here to parallelize:
    \itemize{
      \item The loops that check if each element of the vector provided is between the specified \code{lower} and \code{upper} bounds, for integer (\code{INTSXP}) and real (\code{REALSXP}) types
      \item Checking and handling of undefined values (such as NaNs)
    }
    
    \item\file{cj.c} - \code{\link{CJ}()}
    
    Parallelism is used here to expedite the creation of all combinations of the input vectors over the cross-product space. Better speedup can be expected when dealing with large vectors or a multitude of combinations. OpenMP is used here to parallelize:

    \itemize{
      \item Element assignment in vectors
      \item Memory copying operations (blockwise replication of data using \code{memcpy})
    }
    
    \item\file{coalesce.c} - \code{\link{fcoalesce}()}
    
    Parallelism is used here to reduce the time taken to fill NA values with the first non-NA value from other vectors. Operates over columns to replace NAs. OpenMP is used here to parallelize:
    \itemize{
      \item The operation that iterates over the rows to coalesce the data (which can be of type integer, real, or complex)
      \item The replacement of NAs with non-NA values from subsequent vectors
      \item The conditional checks within parallelized loops
      }
    
    \item\file{fifelse.c} - \code{\link{fifelse}()}
    
    For logical, integer, and real types, OpenMP is being used here to parallelize loops that perform conditional checks along with assignment operations over the elements of the supplied logical vector based on the condition (\code{test}) and values provided for the remaining arguments (\code{yes}, \code{no}, and \code{na}).
    
    \item\file{fread.c} - \code{\link{fread}()}
    
    Parallelism is used here to read and process data in chunks (blocks of lines/rows). Expect significant speedup for large files, as I/O operations benefit greatly from parallel processing. OpenMP is used here to:

    \itemize{
      \item Avoid race conditions or concurrent writes to the output \code{data.table} by having atomic operations on the string data 
      \item Managing synchronized updates to the progress bar and serializing the output to the console
      }
    There are no explicit pragmas for parallelizing loops, and instead the use of OpenMP here is in controlling access to shared resources (with the use of critical sections, for instance) in a multi-threaded environment.
    
    \item\file{forder.c}, \file{fsort.c}, and \file{reorder.c} - \code{\link{forder}()} and related
    \item\file{froll.c}, \file{frolladaptive.c}, and \file{frollR.c} - \code{\link{froll}()} and family
    \item\file{fwrite.c} - \code{\link{fwrite}()}

    OpenMP is primarily used here to parallelize the process of writing rows to the output file. Error handling and compression (if enabled) are also managed within this parallel region, with special attention to thread safety and synchronization, especially in the ordered sections where output to the file and handling of errors is serialized to maintain the correct sequence of rows.
    
    \item\file{gsumm.c} - GForce in various places, see \link{GForce}
    
    Functions with GForce optimization are internally parallelized to speed up grouped summaries over a large \code{data.table}. OpenMP is used here to parallelize operations involved in calculating group-wise statistics like sum, mean, and median (implying faster computation of \code{sd}, \code{var}, and \code{prod} as well). The input data is split into batches (groups), and each thread processes a subset of the data based on them.
    
    \item\file{nafill.c} - \code{\link{nafill}()}
    
    Parallelism is used here for faster filling of missing values. OpenMP is being used here to parallelize the loop that achieves the same, over columns of the input data. This includes handling different data types (double, integer, and integer64) and applying the designated filling method (constant, last observation carried forward, or next observation carried backward) to each column in parallel.
    
    \item\file{subset.c} - Used in \code{\link[=data.table]{[.data.table}} subsetting
    
    Parallelism is used her to expedite the filtering of data. OpenMP is utilized here to parallelize the process of subsetting vectors that have sufficient elements to warrant multi-threaded processing.
    
    \item\file{types.c} - Internal testing usage
    
    Parallelism is being used here for enhancing the performance of internal tests (not impacting any user-facing operations or functions). OpenMP is being used here to test a message printing function inside a nested loop which has been collapsed into a single loop of the combined iteration space using \code{collapse(2)}, along with specification of dynamic scheduling for distributing the iterations in a way that can balance the workload among the threads.
  }
}
\examples{
  getDTthreads(verbose=TRUE)
}
\keyword{ data }
