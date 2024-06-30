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

    OpenMP is used here to parallelize:
    \itemize{
      \item The loops that check if each element of the vector provided is between the specified \code{lower} and \code{upper} bounds, for integer (\code{INTSXP}) and real (\code{REALSXP}) types
      \item The checking and handling of undefined values (such as NaNs)
    }

    Since this function is used to find rows where a column's value falls within a specific range, it benefits more from parallelization when the input data consists of a large number of rows.

    \item\file{cj.c} - \code{\link{CJ}()}

    OpenMP is used here to parallelize:

    \itemize{
      \item The element assignment in vectors
      \item The memory copying operations (blockwise replication of data using \code{memcpy})
      \item The creation of all combinations of the input vectors over the cross-product space
    }

    Given that the number of combinations increases exponentially as more columns are added, better speedup can be expected when dealing with a large number of columns.

    \item\file{coalesce.c} - \code{\link{fcoalesce}()}

    OpenMP is used here to parallelize:
    \itemize{
      \item The operation that iterates over the rows to coalesce the data (which can be of type integer, real, or complex)
      \item The replacement of NAs with non-NA values from subsequent vectors
      \item The conditional checks within parallelized loops
      }

    Significant speedup can be expected for more number of columns here, given that this function operates efficiently across multiple columns to find non-NA values.

    \item\file{fifelse.c} - \code{\link{fifelse}()}

    For logical, integer, and real types, OpenMP is being used here to parallelize loops that perform conditional checks along with assignment operations over the elements of the supplied logical vector based on the condition (\code{test}) and values provided for the remaining arguments (\code{yes}, \code{no}, and \code{na}).

    Better speedup can be expected for more number of columns here as well, given that this function operates column-wise with independent vector operations.

    \item\file{fread.c} - \code{\link{fread}()}

    OpenMP is used here to:

    \itemize{
      \item Parallelize the reading of data in chunks
      \item Avoid race conditions or concurrent writes to the output \code{data.table} by having atomic operations on the string data 
      \item Manage synchronized updates to the progress bar and serialize the output to the console
      }

    This function is highly optimized in reading and processing data with both large numbers of rows and columns, but the efficiency is more pronounced across rows.

    \item\file{forder.c}, \file{fsort.c}, and \file{reorder.c} - \code{\link{forder}()} and related

    OpenMP is used here to parallelize multiple operations that come together to sort a \code{data.table} using the Radix algorithm. These include:

    \itemize{
      \item The counting of unique values and recursively sorting subsets of data across different threads (specific to \file{forder.c})
      \item The process of finding the range and distribution of data for efficient grouping and sorting (applies to both \file{forder.c} and \file{fsort.c})
      \item Creation of histograms which are used to sort data based on significant bits (each thread processes a separate batch of the data, computes the MSB of each element, and then increments the corresponding bins), with the distribution and merging of buckets (specific to \file{fsort.c})
      \item The process of reordering a vector or each column in a list of vectors (such as in a \code{data.table}) based on a given vector that dictates the new ordering of elements (specific to \file{reorder.c})
      }

    Better speedups can be expected when the input data contains a large number of rows as the sorting complexity increases with more rows.

    \item\file{froll.c}, \file{frolladaptive.c}, and \file{frollR.c} - \code{\link{froll}()} and family

    OpenMP is used here to parallelize the loops that compute the rolling means (\code{frollmean}) and sums (\code{frollsum}) over a sliding window for each position in the input vector.

    These functions benefit more in terms of speedup when the data has a large number of columns, primarily due to the efficient memory access patterns (cache-friendly) used when processing the data for each column sequentially in memory to compute the rolling statistic.

    \item\file{fwrite.c} - \code{\link{fwrite}()}

    OpenMP is used here primarily to parallelize the process of writing rows to the output file, but error handling and compression (if enabled) are also managed within the parallel region. Special attention is paid to thread safety and synchronization, especially in the ordered sections where output to the file and handling of errors is serialized to maintain the correct sequence of rows.
    
    Similar to \code{\link{fread}()}, this function is highly efficient in parallely processing data with large numbers of both rows and columns, but it has more notable speedups with an increased number of rows.

    \item\file{gsumm.c} - GForce in various places, see \link{GForce}

    Functions with GForce optimization are internally parallelized to speed up grouped summaries over a large \code{data.table}. OpenMP is used here to parallelize operations involved in calculating group-wise statistics like sum, mean, and median (implying faster computation of \code{sd}, \code{var}, and \code{prod} as well). 

    These optimized grouping operations benefit more in terms of speedup if the input data contains a large number of groups since they leverage parallelization more efficiently by eliminating the overhead of individual group evaluations.

    \item\file{nafill.c} - \code{\link{nafill}()}

    OpenMP is being used here to parallelize the loop that fills missing values over columns of the input data. This includes handling different data types (double, integer, and integer64) and applying the designated filling method (constant, last observation carried forward, or next observation carried backward) to each column in parallel. 

    Given its optimization for column-wise operations, better speedups can be expected when the input data consists of a large number of columns.

    \item\file{subset.c} - Used in \code{\link[=data.table]{[.data.table}} subsetting

    OpenMP is used here to parallelize the loops that perform the subsetting of vectors, with conditional checks and filtering of data. 

    Since subset operations tend to be usually row-dependent, better speedups can be expected when dealing with a large number of rows. However, it also depends on whether the computations are focused on rows or columns (as dictated by the subsetting criteria).

    \item\file{types.c} - Internal testing usage

    This caters to internal tests (not impacting any user-facing operations or functions), and OpenMP is being used here to test a message printing function inside a nested loop which has been collapsed into a single loop of the combined iteration space using \code{collapse(2)}, along with specification of dynamic scheduling for distributing the iterations in a way that can balance the workload among the threads.
  }

In general, or as applicable to all the aforementioned use cases, better speedup can be expected when dealing with large datasets.

Having such data when using \code{\link{fread}()} or \code{\link{fwrite}()} (ones with significant speedups for larger file sizes) also means that while one part of the data is being read from or written to disk (I/O operations), another part can be simultaneously processed using multiple cores (parallel computations). This overlap reduces the total time taken for the read or write operation (as the system can perform computations during otherwise idle I/O time).

Apart from increasing the size of the input data, function-specific parameters when considered can benefit more from parallelization or lead to an increase in speedup. For instance, these can be:

    \itemize{
      \item Having a large number of groups when using \code{\link{forder}()} or a multitude of combinations when using \code{\link{CJ}()}
      \item Having several missing values in your data when using \code{\link{fcoalesce}()} or \code{\link{nafill}()}
      \item Using larger window sizes and/or time series data when using \code{\link{froll}()}
      \item Having more and/or complex conditional logic when using \code{\link{fifelse}()} or \code{\link{subset}()}
      }

Note: The information above is based on implementation-specific details as of March 2024.

}

\examples{
  getDTthreads(verbose=TRUE)
}