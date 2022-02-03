fread_output <- function(str,
                         stringsAsFactors = FALSE,
                         showProgress=getOption("datatable.showProgress", interactive()),
                         data.table=getOption("datatable.fread.datatable", TRUE),
                         logical01=getOption("datatable.logical01", FALSE)
)
{
        s = strsplit(str, '\n', fixed = TRUE)[[1L]]
        s <- sapply(s, trimws);
        s[1] <- paste("fread_output_TEMP_COLUMN_NAME", s[1]);
        str <- s[1];
        ans <- fread(text = s, stringsAsFactors = stringsAsFactors, showProgress = showProgress,
                     data.table = data.table,logical01 = logical01);
        if(data.table==TRUE)
        {
                ans[,fread_output_TEMP_COLUMN_NAME:=NULL];
        }
        else
        {
                ans$fread_output_TEMP_COLUMN_NAME <- NULL;
        }
        return(ans);
}
