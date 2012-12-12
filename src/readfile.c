#include <R.h>
#define USE_RINTERNALS
#include <Rinternals.h>
#include <Rdefines.h>
#include <ctype.h>

/*****
TO DO:
Quote protection in the main data read step needs implementing
Remove strtok? and formatcopy?  Add %n to buffer read to save strlen. Change to p++. Speed regressed for 2008.csv.
Add a way to pick out particular columns only, by name or position.
A way for user to override type, for particular columns only.
Read middle and end to check types
Whitespace at the end of the line before \n should be ignored (warn about non white unprotected by comment char)
test using at least "grep read.table ...Rtrunk/tests/
loop fscanf in batches of 32 (or 128) to remove column limit
test if increasing p from 32 to 128 decreases performance for 10 columns.
secondary separator for list() columns, such as columns 11 and 12 in BED.
Allow logical columns (currently read as character). T/True/TRUE/true are allowed in main/src/util.c
Clear up slightly over-allocated rows on heap during gc().
Allow 2 character separators (some companies have internal formats that do that)
*****/

extern int sizes[];
char sep, sep2;
FILE *f;
extern double currentTime();

static void protprint(char *str)   
{
    // protected print i.e. \n is protected by replacing with \\n
    // for tracing problems via verbose=TRUE
    char *p=str;
    while (*p!='\0') {
        if (*p=='\n') Rprintf("\\n");
        else if (*p=='\t') Rprintf("_");
        else if (*p=='\r') Rprintf("\\r");
        else Rprintf("%c",*p);
        p++;
    }
}

static int countfields()
{
    int ncol=0;
    char ch, protected='0';
    ch = getc(f);
    if (ch!='\n' && ch!=EOF) ncol=1;
    while (ch!='\n' && ch!=EOF) {
        if (ch=='\"' || ch=='\'') {
            if (protected=='0') protected = ch;
            else if (protected==ch) protected = '0';
            continue;
        }
        ncol += (protected=='0' ? ch==sep : 0);
        ch = getc(f);
    }
    return(ncol);
}

SEXP readfile(SEXP fnam, SEXP nrowsarg, SEXP headerarg, SEXP nastrings, SEXP verbosearg, SEXP fsize)
{
    SEXP thiscol, ans, thisstr;
    R_len_t i, j, k, protecti=0, nrow=0, ncharcol=0, ncol=0, estn, nline, flines, nonblank;
    int size[32], type[32], thistype, charcol[32], buflen;
    char *ansbuffer[32];
    char *p[32], *format, *formatcopy, *fmttok[32], *q, ch;
    long pos, pos1, pos2;
    Rboolean verbose=LOGICAL(verbosearg)[0], header=LOGICAL(headerarg)[0], allchar;
    int nnastrings = length(nastrings);
    
    double t0 = currentTime();
    f=fopen(CHAR(STRING_ELT(fnam,0)),"r");
    if (f == NULL) error("file not found");
    //setvbuf(f,NULL,_IOFBF,8096);
    //setbuf(f,NULL);
    
    // ********************************************************************************************
    // Auto skip human readable banners / panels
    // ********************************************************************************************
    double t1 = currentTime();
    nline = 0;
    nonblank = 0;
    ch = '\0';
    pos = 0;
    while (nline<30 && ch!=EOF) {
        i = 0;
        pos1 = ftell(f);
        while ((ch = getc(f))!=EOF && ch!='\n') i += !isspace(ch);
        nline++;
        if (i) pos=pos1, nonblank=nline;
    }
    if (nonblank==0) error("File is either empty or fully whitespace in the first 30 rows");
    nline = nonblank;   // nline>nonblank for short files (under 30 rows) with trailing newlines
    if (verbose) Rprintf("Starting format detection on line %d (the last non blank line in the first 30)\n", nline);
    fseek(f,pos,SEEK_SET);
    
    // ********************************************************************************************
    // Auto detect separator and number of fields
    // ********************************************************************************************
    
    // If the first column is protected (quoted character column) then skip over it in case it contains a different sep
    if ((ch=getc(f)) == '\"') while(getc(f)!='\"');
    else if (ch=='\'') while(getc(f)!='\'');
    
    if (fscanf(f,"%*[^, \t|:;~\n]")==EOF) sep='\n'; else sep = getc(f);
    if (verbose && sep=='\n') Rprintf("Line %d ends before any separator was found. Deducing this is a single column file. Otherwise, please specify 'sep' manually, see ?fread.\n", nline);
    // TO DO: Test single column files where there is no sep.
    // TO DO: Test files where line 30 is the last line and doesn't end \n
    fseek(f,pos,SEEK_SET);  // back to beginning of line
    ncol = countfields();
    if (verbose && sep!='\n') {
        if (sep=='\t') Rprintf("Detected sep as '\\t' and %d columns ('\\t' will be printed as '_' below)\n", ncol);
        else Rprintf("Detected sep as '%c' and %d columns\n", sep, ncol);
    }
    if (ncol>32) error("Currently limited to 32 columns just for dev. Will be increased.");
    fseek(f,pos,SEEK_SET);
    
    // ********************************************************************************************
    // Make informed guess at column types
    // ********************************************************************************************
    
    char typef[3][7] = {"%9d%n","%lld%n","%lg%n"};
    // We'll promote types from long long => double => character
    // R is C99 so ll ok => bit64::integer64. We don't use %d, as the result is undefined in C99 for large numbers that
    // overflow int hence protected field width 9. Where int wider than 9 chars but < INT_MAX, we'll detect that later.
    
    int nread;
    for (i=0; i<ncol; i++) type[i]=0;
    char strf[] = "%[^,\n\r]";
    strf[3] = sep;  // replace comma with sep, in case sep isn't comma
    char *buffer = malloc(sizeof(char) * 1024);
    double v;
    flines = 0;
    pos1 = ftell(f);
    do {
        for (i=0;i<ncol;i++) {
            *buffer='\0';
            if (fscanf(f,strf,buffer)==EOF) {
                if (i==0) {ch=EOF; break;}  // last empty line
                if (i<ncol-1) error("File finishes on line %d in field %d, before all %d fields have been read",nline+flines,i+1,ncol);
            }
            // Rprintf("Field contents are '%s'\n", buffer);
            if (*buffer=='\"') {type[i]=3; if (fscanf(f,"%*[^\n\"\r]")==EOF) error("Premature EOF 2"); getc(f); }
            else if (*buffer=='\'') {type[i]=3; if (fscanf(f,"%*[^'\n\r]")==EOF) error("Premature EOF 3"); getc(f); }
            else if (*buffer!='\0' && strcmp(buffer,"NA")!=0) { // ,, or ,NA, doesn't help to detect type, skip
                // Rprintf("Testing type, type before = %d with format %s\n", type[i],typef[type[i]]);
                while (type[i]<3 && (sscanf(buffer,typef[type[i]],&v,&nread)==0 || nread<strlen(buffer))) type[i]++;
                // Rprintf("Type after = %d with nread=%d\n", type[i], nread);
                // if (type[i]==0) Rprintf("Int value read=%lld with errno=%d\n",*(long long *)&v,errno);
            }
            ch = getc(f);
            if (i<ncol-1 && ch!=sep) {
                if (i==0 && (ch=='\n' || ch==EOF)) break;
                error("Unexpected character (%d) ending field %d of line %d.", ch, i+1, nline+flines);
            }
        }
        if (i==ncol) flines++;
        // Rprintf("Read line and ch is %d with \\n=%d\n",ch,'\n');
        while (ch!='\n' && ch!=EOF) ch=getc(f); //, Rprintf("Next ch is %d\n",ch);
    } while( flines<10 && ch!=EOF );
    pos2 = ftell(f);
    // TO DO: If user has overridden any types, change these now. Not if user has selected a lesser type, though, with warning.
    
    q = format = malloc(sizeof(char)*1024);
    char typef2[4][10] = {"%9d","%lld","%lg","%[^,\n\r]"};
    typef2[3][3] = sep;
    int typesxp[4] = {INTSXP,REALSXP,REALSXP,STRSXP};
    q += sprintf(q, "%s", typef2[type[0]]);
    strcpy(strf, ",%s");
    strf[0] = sep;
    for (i=1; i<ncol; i++) q += sprintf(q, strf, typef2[type[i]]);
    if (verbose) {Rprintf("Format = "); protprint(format); Rprintf("\n");}
    formatcopy = malloc(sizeof(char)*strlen(format));
    strcpy(formatcopy,format);
    strf[1] = '\0';  // now just sep in char * form with ending \0 that strtok needs
    fmttok[i=0] = q = strtok(formatcopy, strf);
    while (q != NULL) {     // TO DO: why not just populate q when building format?  fmttok needs to be [] so it can be changed later
        if (q[1] == '[') {
            q[strlen(q)] = sep;
            strtok(NULL, strf);
        }
        fmttok[++i] = q = strtok(NULL, strf);
    }
    if (verbose) {Rprintf("Fsplit ="); for (j=0; j<i; j++){Rprintf(" "); protprint(fmttok[j]);}; Rprintf("\n");}
    if (i!=ncol) error("Internal error: Fsplit i (%d) != ncol (%d)", i, ncol);
    
    // ********************************************************************************************
    // Detect and assign column names
    // ********************************************************************************************
    
    rewind(f);  // To the start
    nline = 1;
    pos = ftell(f);
    while (countfields()!=ncol) nline++,pos=ftell(f); // discard human readable header (if any). No need for 'skip'.
    
    if (verbose) Rprintf("Found first row with %d fields occuring on line %d (either column names or first row of data)\n", ncol, nline);
    if (header!=NA_LOGICAL) Rprintf("'header' changed by user from 'auto' to %s\n", header?"TRUE":"FALSE");
    fseek(f,pos,SEEK_SET);  // back to the start of (likely) header row
    SEXP names = PROTECT(allocVector(STRSXP, ncol));
    protecti++;
    i=0; allchar=TRUE;
    while (fscanf(f, fmttok[i], buffer)==1) {ch=getc(f); if (type[i]<3) allchar=FALSE; i++;}
    // TO DO discard any whitespace after last column name on first row before the \n
    if (ch!='\n' && ch!=EOF) error("Not positioned correctly after testing format of header row. ch=%d",ch);
    if (i==ncol && header!=TRUE && (nonblank>nline || !allchar)) {
        if (verbose) {
            if (nonblank==nline) Rprintf("There is just one data row and no header and this single row has some numeric fields. Treating as a data row and using default column names.\n");
            else Rprintf("Fields in header row are the same type as the data rows, and 'header' is either 'auto' or FALSE. Treating as the first data row and using default column names.\n");
        }
        for (i=0; i<ncol; i++) {
            sprintf(buffer,"V%d",i+1);
            SET_STRING_ELT(names, i, mkChar(buffer));
        }
        fseek(f,pos,SEEK_SET);  // back to start of first row. Treat as first data row, no column names present.
    } else {
        if (verbose) {
            if (nonblank==nline) Rprintf("Single data row has all character fields, treating as column names of empty table\n");
            else if (i<ncol) {
                Rprintf("Fields in header row aren't the same type as data rows. Treating as column names");
                if (header==FALSE) Rprintf(", despite header=FALSE (its types means it can't be treated as data row).\n"); 
                else Rprintf(".\n");
            } 
            else Rprintf("Fields in header row are the same type as the data rows but 'header' is TRUE. Treating as column names.");
        }
        fseek(f,pos,SEEK_SET);
        strcpy(strf, "%[^,\n\r]");
        strf[3] = sep;
        for (i=0; i<ncol; i++) {
            if (fscanf(f,strf,buffer)!=1) error("Format error when reading column name %d", i+1);
            // TO DO: remove quote protection if present
            SET_STRING_ELT(names, i, mkChar(buffer));
            ch = getc(f);
        }
        while (ch!='\n' && ch!=EOF) ch=getc(f);
    }
    
    // ********************************************************************************************
    // Estimate number of rows
    // ********************************************************************************************

    pos = ftell(f);  // start of the first data row
    estn = (R_len_t)ceil(1.05 * flines * (REAL(fsize)[0]-pos) / (pos2-pos1)) +5;  // +5 for small files
    if (verbose) Rprintf("Estimated nrows: %d ( 1.05*%d*(%ld-%ld)/(%ld-%ld) )\n",estn,flines,(long)REAL(fsize)[0],pos,pos2,pos1);
    // fsize passed in from R level for simplicity since R handles LFS, calls stat and stat64 appropriately on
    // each platform, and returns fsize as double accordingly.
    i = INTEGER(nrowsarg)[0];
    if (i>-1 && i<estn) {
        estn = i;
        if (verbose) Rprintf("estn limited to nrows passed in (%d)\n", estn);
    }

    // ********************************************************************************************
    // Now read data
    // ********************************************************************************************
    
    for (i=0; i<32; i++) p[i] = 0;
    ans=PROTECT(allocVector(VECSXP,ncol));  // TO DO, could over-allocate here directly, or maybe safer to go via alloccol.
    protecti++;
    setAttrib(ans,R_NamesSymbol,names);
    for (i=0; i<ncol; i++) {
        thistype  = typesxp[ type[i] ];  // map from 0-3 to INTSXP,REALSXP and STRSXP.
        thiscol = PROTECT(allocVector(thistype,estn));
        protecti++;
        if (type[i]==1) setAttrib(thiscol, R_ClassSymbol, ScalarString(mkChar("integer64")));
        SET_TRUELENGTH(thiscol, estn);
        SET_VECTOR_ELT(ans,i,thiscol);
        if (thistype == STRSXP) {
            charcol[ncharcol] = i;
            p[i] = ansbuffer[ncharcol] = malloc(sizeof(char)*1024);   // TO DO tie in %1024s and return value from fscanf
            size[i] = 0;
            ncharcol++;
        } else {
            p[i] = (char *)DATAPTR(thiscol);
            size[i] = sizes[thistype];
        }
    }
    double t2=currentTime(), t3, t4=0.0, tCoerce0, tCoerce=0.0;
    double nexttime = t1+2;
    // pos is still at the start of the first data row at this point.
    if (EOF > -1) error("Internal error. EOF is not -1 or less\n");
    while (TRUE) {
        i = fscanf(f, format,
            p[0], p[1], p[2], p[3], p[4], p[5], p[6], p[7],       // see footnote 1
            p[8], p[9], p[10],p[11],p[12],p[13],p[14],p[15],      // bulk read in one call
            p[16],p[17],p[18],p[19],p[20],p[21],p[22],p[23],      // could replace with p++, p++, etc, then could be looped.
            p[24],p[25],p[26],p[27],p[28],p[29],p[30],p[31]);     // a smaller batch size here, could mean less field iterating
        // TO DO: if next char isn't \n, or there's any non white at the end then reset to the start, too
        // Rprintf("%d %d\n",i,nrow);
        if (i<ncol) {
            if (i==EOF) break;
            t3 = currentTime();
            // some missings, NAs, or protected sep
            // back to start of line, now at normal speed
            // ***  TO DO: can we do away with strtok now?  ***
            if (i==0 && getc(f)=='\n') continue;  // skip blank lines
            fseek(f,pos,SEEK_SET);
            for (i=0; i<ncol; i++) {
                *buffer='\0';
                buflen = 0;
                if (fscanf(f,strf,buffer)==EOF) error("Premature EOF!");  
                //***** buflen = strlen(buffer);   // TO DO: faster to use %n !!, and/or count up !isspace  "3.14  " should be 3.14 ok.
                q = buffer;
                while (*q!='\0') if (isspace(*q++)==0) buflen++;
                
                // Rprintf("Field contents are '%s' for field %d\n", buffer, i);
                /*
                if (*buffer=='\"') {type[i]=3; if (fscanf(f,"%*[^\n\"]")==EOF) error("Premature EOF"); getc(f); }
                else if (*buffer=='\'') {type[i]=3; if (fscanf(f,"%*[^'\n]")==EOF) error("Premature EOF"); getc(f); }
                else if (*buffer!='\0' && strcmp(buffer,"NA")!=0) { // ,, or ,NA, doesn't help to detect type, skip
                */
                if (type[i]<3) {
                    if (buflen==0 || strcmp(buffer,"NA")==0) {  // where buflen is non white with beginning and end stripped.
                        if (type[i])
                            *(double *)p[i] = NA_REAL;  // 1 (integer64) or 2 (REALSXP)
                        else
                            *(int *)p[i] = NA_INTEGER;  // 0 (INTSXP)
                    } else {
                        nread=0;
                        if (sscanf(buffer,typef[type[i]],p[i],&nread)==0 || nread<buflen) {
                            type[i]++;
                            tCoerce0 = currentTime();
                            while (type[i]<3 && (sscanf(buffer,typef[type[i]],&v,&nread)==0 || nread<buflen)) type[i]++;
                            if (verbose) Rprintf("Column %d bumped to code %d on line %d, field contains '%s'\n", i+1, type[i], nrow+1, buffer);
                            if (type[i]==1) error("Coercing int to integer64 needs to be implemeted");
                            if (type[i]==2 && TYPEOF(VECTOR_ELT(ans,i))==REALSXP) error("Coercing integer64 to real needs to be implemented");
                            thistype  = typesxp[ type[i] ];
                            thiscol = coerceVector(VECTOR_ELT(ans, i), thistype);
                            // TO DO: specialize coerceVector to only coerce up to the point so far.
                            SET_VECTOR_ELT(ans, i, thiscol);
                            if (thistype == STRSXP) {
                                for (j=0; j<nrow; j++) if (STRING_ELT(thiscol,j)==NA_STRING) SET_STRING_ELT(thiscol,j,R_BlankString);
                                // If a character column starts with blanks, they'll have been coerced to NAs. Set back to "" and
                                // these will be converted back to NA at the end if na.strings contains "".
                                charcol[ncharcol] = i;
                                p[i] = ansbuffer[ncharcol] = malloc(sizeof(char)*1024);
                                size[i] = 0;
                                ncharcol++;
                                SET_STRING_ELT(VECTOR_ELT(ans, i), nrow, mkChar(buffer));
                            } else {
                                p[i] = (char *)DATAPTR(VECTOR_ELT(ans, i));
                                size[i] = sizes[thistype];
                                p[i] += nrow*size[i];
                                memcpy(p[i],&v,size[i]);
                            }
                            tCoerce += currentTime() - tCoerce0;
                        }
                    }
                } else {
                    SET_STRING_ELT(VECTOR_ELT(ans, i), nrow, mkChar(buffer));
                }
                ch = getc(f);
                if (i+1<ncol && ch!=sep) {
                    if (i==0 && (ch=='\n' || ch==EOF)) break;
                    error("Unexpected character (%d) ending field %d of line %d.", ch, i+1, nline+nrow);
                }
            }
            t4 += currentTime()-t3;
        } else {
            // bulk read was successful, just deal with the character fields
            for (i=0; i<ncharcol; i++)
                SET_STRING_ELT(VECTOR_ELT(ans, charcol[i]), nrow, mkChar(ansbuffer[i]));
            ch = getc(f);
        }
        //if (ch != '\n') error("Non \\n at end of line %d\n", nrow);
        while (ch!='\n' && ch!=EOF) ch=getc(f);  // count !isspace here
        for (i=0; i<ncol; i++) p[i]+=size[i];
        pos=ftell(f);
        if (++nrow == estn) {
            if (INTEGER(nrowsarg)[0]==-1) warning("Filled all estn rows, even after estimating 5%% more. Reallocation at this point fairly trivial but not yet implemented. Returning the rows read so far and discarding the rest (if any), for now.");
            break;
        } else if (nrow%10000==0 && currentTime()>nexttime) {  // %10000 && saves a little bit (apx 0.2 in 4.9 secs, 5%)
            Rprintf("%.1f%%\r", 100.0*nrow/estn);
            nexttime = currentTime()+1;
            R_CheckUserInterrupt();
        }
    }
    double t5=currentTime();
    fclose(f);
    for (i=0; i<ncol; i++) SETLENGTH(VECTOR_ELT(ans,i), nrow);
    for (k=0; k<nnastrings; k++) {
        thisstr = STRING_ELT(nastrings,k);
        for (j=0; j<ncharcol; j++) {
            thiscol = VECTOR_ELT(ans,charcol[j]);
            for (i=0; i<nrow; i++) {
                if (STRING_ELT(thiscol,i)==thisstr) SET_STRING_ELT(thiscol, i, NA_STRING);
            }
        }
    }
    for (i=0; i<ncharcol; i++) free(ansbuffer[i]);
    free(formatcopy);
    free(buffer);
    free(format);
    UNPROTECT(protecti);
    if (verbose) {
        double tn = currentTime(), tot=tn-t0;
        Rprintf("%8.3fs (%3.0f%%) Opening file\n", t1-t0, 100*(t1-t0)/tot);
        Rprintf("%8.3fs (%3.0f%%) Format detection\n", t2-t1, 100*(t2-t1)/tot);
        Rprintf("%8.3fs (%3.0f%%) Bulk reading\n", t5-t2-t4, 100*(t5-t2-t4)/tot);
        Rprintf("%8.3fs (%3.0f%%) Tokenized reading (any rows with missing data or fields with a protected sep)\n", t4-tCoerce, 100*(t4-tCoerce)/tot);
        Rprintf("%8.3fs (%3.0f%%) Bumping column type midread and coercing data already read\n", tCoerce, 100*tCoerce/tot);
        if (nnastrings) Rprintf("%8.3fs (%3.0f%%) Changing na.strings to NA\n", tn-t5, 100*(tn-t5)/tot);
        Rprintf("%8.3fs        Total\n", tot);
    }
    return(ans);
}

/*
Footnote 1.
Often superfluous and ugly p[*] repetition is deliberate and required.
It isn't possible in C to construct a vargs and pass it to fscanf in C or C99. A '...' can
be passed on, but not created, unless we depend on a specialist library or go to asm.
Assumption is that fscanf is optimised internally for navigating the format, and is faster than character by
character code with branching and function calls at the C level (such as in R's scan.c).
*/


