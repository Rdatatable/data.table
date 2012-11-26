#include <R.h>
#define USE_RINTERNALS
#include <Rinternals.h>
#include <Rdefines.h>
#include <fcntl.h>
#include <stdio.h>
#include <errno.h>

/*****
TO DO:
Read bit64 directly without going via character
Read middle and end to check types
Whitespace at the end of the line before \n should be ignored (warn about non white unprotected by comment char)
Invalid but not ,, and not NA should promote type and coerce existing (reporting on which line it did that).
test using "grep read.table ...Rtrunk/tests/
loop fscanf in batches of 32 (or 128) to remove column limit
test if increasing p from 32 to 128 decreases performance for 10 columns.
secondary separator for list() columns, such as columns 11 and 12 in BED.
Allow logical columns (currently will be character). T/True/TRUE/true are allowed in main/src/util.c
*****/

extern int sizes[];
char sep, sep2;

static void protprint(char *str)   // protected print i.e. \n is protected by replacing with \\n
{
    char *p=str;
    while (*p!='\0') {
        if (*p=='\n') Rprintf("\\n");
        else Rprintf("%c",*p);
        p++;
    }
}

SEXP readfile(SEXP fnam, SEXP formatarg, SEXP types, SEXP skip, SEXP estnarg, SEXP verbosearg)
{
    SEXP thiscol, ans;
    R_len_t i, protecti=0, nrow=0, ncol, estn;
    int size[32], type[32], thistype, nc=0, charcol[32], c1, c2, c3;
    char *buffer[32];
    char *p[32], *format, *formatcopy, *fmttok[32], *q, ch;
    long pos=0;
    Rboolean verbose = LOGICAL(verbosearg)[0];
    if (LENGTH(types)>32) error("Currently limited to 32 columns just for dev. Will be increased.");
    for (i=0; i<32; i++) p[i] = 0;
    FILE *f=fopen(CHAR(STRING_ELT(fnam,0)),"r");
    if (f == NULL) error("file not found");
    estn = INTEGER(estnarg)[0];
    ncol = LENGTH(types);
    ans=PROTECT(allocVector(VECSXP,ncol));  // TO DO, could over-allocate here directly, or maybe safer to go via alloccol.
    protecti++;
    for (i=0; i<ncol; i++) {
        thistype = type[i] = TYPEOF(VECTOR_ELT(types,i));
        thiscol = PROTECT(allocVector(thistype,estn));
        protecti++;
        SET_TRUELENGTH(thiscol, estn);
        SET_VECTOR_ELT(ans,i,thiscol);
        if (thistype == STRSXP) {
            charcol[nc] = i;
            p[i] = buffer[nc] = malloc(sizeof(char)*1024);   // TO DO tie in %1024s and return value from fscanf
            size[i] = 0;
            nc++;
        } else {
            p[i] = (char *)DATAPTR(thiscol);
            size[i] = sizes[thistype];
        }
    }
    for (i=0; i<INTEGER(skip)[0]; i++) while (fgetc(f) != '\n');
    format = (char *)CHAR(STRING_ELT(formatarg,0));
    formatcopy = malloc(sizeof(char)*strlen(format));
    strcpy(formatcopy,format);
    fmttok[i=0] = q = strtok(formatcopy,",");
    while (q != NULL) {
        if (q[1] == '[') {
            q[strlen(q)] = ',';
            strtok(NULL,",");
        }
        fmttok[++i] = q = strtok(NULL,",");
    }
    if (verbose) {Rprintf("Split format:"); for (i=0; i<ncol; i++){Rprintf(" "); protprint(fmttok[i]);}; Rprintf("\n");}
    while ((i=fscanf(f, format,
            p[0], p[1], p[2], p[3], p[4], p[5], p[6], p[7],            // see footnote 1
            p[8], p[9], p[10],p[11],p[12],p[13],p[14],p[15],
            p[16],p[17],p[18],p[19],p[20],p[21],p[22],p[23],
            p[24],p[25],p[26],p[27],p[28],p[29],p[30],p[31])) != EOF) {
        while (i<ncol) {
            switch(type[i]) {              // this switch could be avoided for speed using memcpy, but less readable
            case INTSXP:                   // and this only runs for each invalid or missing field anyway (i.e. rarely)
               *(int *)p[i] = NA_INTEGER;
               break;
            case REALSXP:
               *(double *)p[i] = NA_REAL;
               if (ftell(f)>pos) {                          // see footnote 2. dealing with potential scanf bug, not needed otherwise
                   c3 = getc(f); fseek(f,-3,SEEK_CUR); c1 = getc(f); c2 = getc(f);   
                   if ((c1=='A' || c1=='a') && c2==',' &&   // has just potentially over-read comma of "NA,"
                       (pos || (i &&                        // pos>0 => not the first NA in this row. ftell below has just run and
                                                            //   the one-by-one fscanf did just advance (i.e. didn't just read ",,")
                                                            //   i to ensure i-1 is valid, but probably not needed (keep for safety)
                        (c3!=',' || type[i-1]!=STRSXP))))   // if the all-on-one fscanf above just read NA into a character column
                                                            //   but a float ",," is next, the comma wasn't over read so don't rewind
                       fseek(f,-1,SEEK_CUR);                // rewind the over read comma when NA appears in a float column
               }
               break;
            case STRSXP:
               *p[i] = '\0';
               break;
            default:
               error("Unsupported type");
            }
            while((ch=getc(f))!=','   // discard remainder of invalid input in this field (if any) and read next
                   && ch!='\n');      // separator. TO DO: promote type and ensure invalid data not lost without at least warning.
            while (++i<ncol && (pos=ftell(f)) && fscanf(f, fmttok[i], p[i]) && getc(f)!=EOF);   // read remaining fields one by one
            // TO DO - cater for EOF here on last line of file if there's a missing on last row.
        }
        for (i=0; i<ncol; i++) p[i]+=size[i];
        for (i=0; i<nc; i++) SET_STRING_ELT(VECTOR_ELT(ans, charcol[i]), nrow, mkChar(buffer[i]));
        pos=0;
        nrow++;
    }
    fclose(f);
    for (i=0; i<ncol; i++) SETLENGTH(VECTOR_ELT(ans,i), nrow);
    UNPROTECT(protecti);
    for (i=0; i<nc; i++) free(buffer[i]);
    free(formatcopy);
    return(ans);
}

FILE *f;

static int countfields()
{
    int ncols=0;
    char ch, protected='0';
    ch = getc(f);
    if (ch!='\n') ncols=1;
    while (ch!='\n') {   // TO DO: add tests of this.
        if (ch=='\"' || ch=='\'') {
            if (protected=='0') protected = ch;
            else if (protected==ch) protected = '0';
            continue;
        }
        ncols += (protected=='0' ? ch==sep : 0);
        ch = getc(f);
    }
    return(ncols);
}

SEXP finddelim(SEXP fnam)
{
    int nline=0, ncols=0, i;
    char ch, *format, *p;
    long pos=0;
    Rboolean verbose=TRUE;
    f=fopen(CHAR(STRING_ELT(fnam,0)),"r");
    
    while (nline<30 && (ch=getc(f))!=EOF) nline += ch=='\n';   // skip first 30 lines (in case of human readable banner)
    Rprintf("Found line %d\n", nline);
    pos = ftell(f);
    
    // If the first column is protected (quoted character column) then skip over it in case it contains a different sep
    if ((ch=getc(f)) == '\"') while(getc(f)!='\"');
    else if (ch=='\'') while(getc(f)!='\'');
    
    if (fscanf(f,"%*[^, \t|:;~\n]")==EOF) error("File ends on line %d before any separator was found", nline);
    sep = getc(f);
    if (sep=='\n') error("No separator found on line %d. Please specify 'sep' manually, see ?read.", nline);
    if (verbose) Rprintf("Detected sep as '%c'\n", sep);
    // TO DO: Test single column files where there is no sep.
    // TO DO: Test files where line 30 is the last line and doesn't end \n
    fseek(f,pos,SEEK_SET);  // back to beginning of line
    ncols = countfields();
    if (verbose) Rprintf("Detected %d columns\n", ncols);
    rewind(f);  // To the start
    
    nline = 1;
    pos = ftell(f);
    while (countfields()!=ncols) nline++,pos=ftell(f); // discard human readable header (if any)
    
    if (verbose) Rprintf("First row with %d fields detected on line %d\n", ncols, nline);
    // if there is a header row, it will have just been read. Start on 2nd row regardless to detect types
    // Now keep looping until all fields are character, or we have seen 10 non-NA values in all columns.
    // Potentially, could scan whole file here (an entirely NA column). TO DO: limit that and test middle and end of file.
    
    char typef[3][7] = {"%9d%n","%lld%n","%lg%n"};
    // We'll promote types from long long => double => character
    // R is C99 so ll ok => bit64::integer64. We don't use %d, as the result is undefined in C99 for large numbers that
    // overflow int hence protected field width 9. Where int wider than 9 chars but < INT_MAX, we'll detect that later.
    
    int typei[ncols], typec[ncols], nread;  // typec = count of non-NA data in that col
    for (i=0; i<ncols; i++) typei[i]=0, typec[i]=0;
    char strf[] = "%[^,\n]";
    strf[3] = sep;  // replace comma with sep, in case sep isn't comma
    char *buffer = malloc(sizeof(char) * 1024);
    Rboolean needMoreObs;
    double v;
    do {   // all non character columns need 10 non-NA items for good guess. Type character can stop there.
        needMoreObs = FALSE;
        for (i=0;i<ncols;i++) {
            *buffer='\0';
            if (fscanf(f,strf,buffer)==EOF) error("Premature EOF!");
            // Rprintf("Field contents are '%s'\n", buffer);
            if (*buffer=='\"') {typei[i]=3; if (fscanf(f,"%*[^\n\"]")==EOF) error("Premature EOF"); getc(f); }
            else if (*buffer=='\'') {typei[i]=3; if (fscanf(f,"%*[^'\n]")==EOF) error("Premature EOF"); getc(f); }
            else if (*buffer!='\0' && strcmp(buffer,"NA")!=0) { // ,, or ,NA, doesn't help to detect type, skip
                // Rprintf("Testing type, type before = %d with format %s\n", typei[i],typef[typei[i]]);
                while (typei[i]<3 && (sscanf(buffer,typef[typei[i]],&v,&nread)==0 || nread<strlen(buffer))) typei[i]++;
                // Rprintf("Type after = %d with nread=%d\n", typei[i], nread);
                // if (typei[i]==0) Rprintf("Int value read=%lld with errno=%d\n",*(long long *)&v,errno);
                typec[i]++;
            }
            ch = getc(f);
            if (i+1<ncols && ch!=sep) error("Unexpected separator ending field %d of line %d", i+1, nline);
            if (typei[i]<3 && typec[i]<10) needMoreObs=TRUE;
        }
        while (ch!='\n') ch = getc(f);
    } while( needMoreObs && ch != EOF );
    
    for (i=0; i<ncols; i++) Rprintf("Type %d = %d\n", i, typei[i]);
    
    // TO DO: If user has overridden any types, change these now. Not if user has selected a lesser type, though, with warning.
    
    p = format = malloc(sizeof(char)*1024);
    char typef2[4][7] = {"%9d","%lld","%lg","%[^,\n]"};
    p += sprintf(p, "%s", typef2[typei[0]]);
    strcpy(strf, ",%s");
    strf[0] = sep;
    for (i=1; i<ncols; i++) p += sprintf(p, strf, typef2[typei[i]]);
    Rprintf("Format on C side = "); protprint(format); Rprintf("\n");
    
    fseek(f,pos,SEEK_SET);  // back to the start of (likely) header row
    /* Read the header row using the split format into v.
    If header row fails (nfields read < ncol) then it's the header row, otherwise that's the first data row (unless header has
    been set) then proceed as before ...
    */
    fclose(f);
    free(buffer);
    free(format);
    return(R_NilValue);
}

/*
Footnote 1.
Often superfluous and ugly p[*] repetition is deliberate and required.
It isn't possible in C to construct a vargs and pass it to fscanf in C or C99. A '...' can
be passed on, but not created, unless we depend on a specialist library or go to asm.
Assumption is that fscanf is optimised internally for navigating the format, and is faster than character by
character code with branching and function calls at the C level (such as in R's scan.c).

Footnote 2.
The internet has many (usually false) claims of bug finds in fscanf, but this _might_ actually
be one. NAN and INF (case insensitive) are (the only) acceptable text values under IEEE Binary Floating Point
format (vs hexadecimal floating-point) to %g (and %e %f %a equivalents). That capabability is
deep in the OS or hardware (exactly which can vary) and we like taking advantage of that, by using fscanf.
However, fscanf is supposed to stop reading the field on invalid character
and leave that invalid character unread. It does this for all other invalid character input to %g. But
when it reads "NA", apparently, it expects N next (to form NAN) realises "," isn't valid and stops ok but doesn't leave that comma
unread (inconsistent with all other invalid input to %g).  Hence the rewind and special case in the C code above.
Unfortunately, R historically defaulted to writing out "NA" for NA, so we have to cope with
that. Just blank (i.e. ,,) would have been easier. It's not so bad as the extra test only runs when
a missing or invalid input to %lg actually occurs; i.e., for fully populated files not written by R, that'll never run.
Note the test on getc only needs to test for "A," or "a,", since the only way that could happen if there was an N (or n) before that; i.e., it really is only a special case for this apparent bug in scanf with NA (not NAN) input.
The ftell(f)>pos is to deal with ",NA,," to detect that the 2nd field hasn't advanced (without that if, it would correct the comma overread bug in scanf a 2nd time for the same particular comma, and a mess would ensue).

TO DO: link to gcc/S.O. query about %d and %lld overread of comma after NA.
*/


