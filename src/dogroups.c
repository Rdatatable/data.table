#include "data.table.h"
#include <Rdefines.h>
#include <fcntl.h>
#include <time.h>


void setSizes() {
  // called by init.c
  int i;
  for (i=0;i<100;i++) sizes[i]=0;
  // only these types are currently allowed as column types :
  sizes[INTSXP] = sizeof(int);     // integer and factor
  sizes[LGLSXP] = sizeof(int);     // logical
  sizes[REALSXP] = sizeof(double); // numeric
  sizes[STRSXP] = sizeof(SEXP *);  // character
  sizes[VECSXP] = sizeof(SEXP *);  // a column itself can be a list()
  for (i=0;i<100;i++) {
    if (sizes[i]>8) error("Type %d is sizeof() greater than 8 bytes on this machine. We haven't tested on any architecture greater than 64bit, yet.", i);
    // One place we need the largest sizeof (assumed to be 8 bytes) is the working memory malloc in reorder.c
  }
  SelfRefSymbol = install(".internal.selfref");
}

SEXP dogroups(SEXP dt, SEXP dtcols, SEXP groups, SEXP grpcols, SEXP jiscols, SEXP xjiscols, SEXP grporder, SEXP order, SEXP starts, SEXP lens, SEXP jexp, SEXP env, SEXP lhs, SEXP newnames, SEXP on, SEXP verbose)
{
  R_len_t i, j, k, rownum, ngrp, nrowgroups, njval=0, ngrpcols, ansloc=0, maxn, estn=-1, r, thisansloc, grpn, thislen, igrp, vlen, origIlen=0, origSDnrow=0;
  int protecti=0;
  SEXP names, names2, xknames, bynames, dtnames, ans=NULL, jval, thiscol, SDall, BY, N, I, GRP, iSD, xSD, rownames, s, RHS, listwrap, target, source, tmp;
  Rboolean wasvector, firstalloc=FALSE, NullWarnDone=FALSE, recycleWarn=TRUE;
  size_t size; // must be size_t, otherwise bug #5305 (integer overflow in memcpy)
  clock_t tstart=0, tblock[10]={0}; int nblock[10]={0};

  if (!isInteger(order)) error("Internal error: order not integer vector");
  if (TYPEOF(starts) != INTSXP) error("Internal error: starts not integer");
  if (TYPEOF(lens) != INTSXP) error("Internal error: lens not integer");
  // starts can now be NA (<0): if (INTEGER(starts)[0]<0 || INTEGER(lens)[0]<0) error("starts[1]<0 or lens[1]<0");
  if (!isNull(jiscols) && LENGTH(order) && !LOGICAL(on)[0]) error("Internal error: jiscols not NULL but o__ has length");
  if (!isNull(xjiscols) && LENGTH(order) && !LOGICAL(on)[0]) error("Internal error: xjiscols not NULL but o__ has length");
  if(!isEnvironment(env)) error("’env’ should be an environment");
  ngrp = length(starts);  // the number of groups  (nrow(groups) will be larger when by)
  ngrpcols = length(grpcols);
  nrowgroups = length(VECTOR_ELT(groups,0));
  // fix for longstanding FR/bug, #495. E.g., DT[, c(sum(v1), lapply(.SD, mean)), by=grp, .SDcols=v2:v3] resulted in error.. the idea is, 1) we create .SDall, which is normally == .SD. But if extra vars are detected in jexp other than .SD, then .SD becomes a shallow copy of .SDall with only .SDcols in .SD. Since internally, we don't make a copy, changing .SDall will reflect in .SD. Hopefully this'll workout :-).
  SDall = PROTECT(findVar(install(".SDall"), env)); protecti++;  // PROTECT for rchk

  defineVar(sym_BY, BY = PROTECT(allocVector(VECSXP, ngrpcols)), env); protecti++;  // PROTECT for rchk
  bynames = PROTECT(allocVector(STRSXP, ngrpcols));  protecti++;   // TO DO: do we really need bynames, can we assign names afterwards in one step?
  for (i=0; i<ngrpcols; i++) {
    j = INTEGER(grpcols)[i]-1;
    SET_VECTOR_ELT(BY, i, allocVector(TYPEOF(VECTOR_ELT(groups, j)),
      nrowgroups ? 1 : 0)); // TODO: might be able to be 1 always but 0 when 'groups' are integer(0) seem sensible. #2440 was involved in the past.
    // Fix for #5437, by cols with attributes when also used in `j` lost the attribute.
    copyMostAttrib(VECTOR_ELT(groups, j), VECTOR_ELT(BY,i));  // not names, otherwise test 778 would fail
    SET_STRING_ELT(bynames, i, STRING_ELT(getAttrib(groups,R_NamesSymbol), j));
    defineVar(install(CHAR(STRING_ELT(bynames,i))), VECTOR_ELT(BY,i), env);      // by vars can be used by name in j as well as via .BY
    if (SIZEOF(VECTOR_ELT(BY,i))==0)
      error("Unsupported type '%s' in column %d of 'by'", type2char(TYPEOF(VECTOR_ELT(BY, i))), i+1);
  }
  setAttrib(BY, R_NamesSymbol, bynames); // Fix for #5415 - BY doesn't retain names anymore
  R_LockBinding(sym_BY, env);
  if (isNull(jiscols) && (length(bynames)!=length(groups) || length(bynames)!=length(grpcols))) error("!length(bynames)[%d]==length(groups)[%d]==length(grpcols)[%d]",length(bynames),length(groups),length(grpcols));
  // TO DO: check this check above.

  N =   PROTECT(findVar(install(".N"), env));   protecti++; // PROTECT for rchk
  GRP = PROTECT(findVar(install(".GRP"), env)); protecti++;
  iSD = PROTECT(findVar(install(".iSD"), env)); protecti++; // 1-row and possibly no cols (if no i variables are used via JIS)
  xSD = PROTECT(findVar(install(".xSD"), env)); protecti++;
  R_len_t maxGrpSize = 0;
  for (R_len_t i=0; i<LENGTH(lens); i++) {
    if (INTEGER(lens)[i] > maxGrpSize) maxGrpSize = INTEGER(lens)[i];
  }
  defineVar(install(".I"), I = PROTECT(allocVector(INTSXP, maxGrpSize)), env); protecti++;
  R_LockBinding(install(".I"), env);

  dtnames = PROTECT(getAttrib(dt, R_NamesSymbol)); protecti++; // added here to fix #4990 - `:=` did not issue recycling warning during "by"
  // fetch rownames of .SD.  rownames[1] is set to -thislen for each group, in case .SD is passed to
  // non data.table aware package that uses rownames
  for (s = ATTRIB(SDall); s != R_NilValue && TAG(s)!=R_RowNamesSymbol; s = CDR(s));
  // getAttrib0 basically but that's hidden in attrib.c
  if (s==R_NilValue) error("row.names attribute of .SD not found");
  rownames = CAR(s);
  if (!isInteger(rownames) || LENGTH(rownames)!=2 || INTEGER(rownames)[0]!=NA_INTEGER) error("row.names of .SD isn't integer length 2 with NA as first item; i.e., .set_row_names(). [%s %d %d]",type2char(TYPEOF(rownames)),LENGTH(rownames),INTEGER(rownames)[0]);

  // fetch names of .SD and prepare symbols. In case they are copied-on-write by user assigning to those variables
  // using <- in j (which is valid, useful and tested), they are repointed to the .SD cols for each group.
  names = PROTECT(getAttrib(SDall, R_NamesSymbol)); protecti++;
  if (length(names) != length(SDall)) error("length(names)!=length(SD)");
  SEXP *nameSyms = (SEXP *)R_alloc(length(names), sizeof(SEXP));
  for(i = 0; i < length(SDall); i++) {
    if (SIZEOF(VECTOR_ELT(SDall, i))==0)
      error("Type %d in .SD column %d", TYPEOF(VECTOR_ELT(SDall, i)), i);
    nameSyms[i] = install(CHAR(STRING_ELT(names, i)));
    // fixes http://stackoverflow.com/questions/14753411/why-does-data-table-lose-class-definition-in-sd-after-group-by
    copyMostAttrib(VECTOR_ELT(dt,INTEGER(dtcols)[i]-1), VECTOR_ELT(SDall,i));  // not names, otherwise test 778 would fail
  }

  origIlen = length(I);  // test 762 has length(I)==1 but nrow(SD)==0
  if (length(SDall)) origSDnrow = length(VECTOR_ELT(SDall, 0));

  xknames = getAttrib(xSD, R_NamesSymbol);
  if (length(xknames) != length(xSD)) error("length(xknames)!=length(xSD)");
  SEXP *xknameSyms = (SEXP *)R_alloc(length(xknames), sizeof(SEXP));
  for(i = 0; i < length(xSD); i++) {
    if (SIZEOF(VECTOR_ELT(xSD, i))==0)
      error("Type %d in .xSD column %d", TYPEOF(VECTOR_ELT(xSD, i)), i);
    xknameSyms[i] = install(CHAR(STRING_ELT(xknames, i)));
  }

  if (length(iSD)!=length(jiscols)) error("length(iSD)[%d] != length(jiscols)[%d]",length(iSD),length(jiscols));
  if (length(xSD)!=length(xjiscols)) error("length(xSD)[%d] != length(xjiscols)[%d]",length(xSD),length(xjiscols));

  PROTECT(listwrap = allocVector(VECSXP, 1));
  protecti++;
  Rboolean jexpIsSymbolOtherThanSD = (isSymbol(jexp) && strcmp(CHAR(PRINTNAME(jexp)),".SD")!=0);  // test 559

  ansloc = 0;
  for(i=0; i<ngrp; i++) {   // even for an empty i table, ngroup is length 1 (starts is value 0), for consistency of empty cases

    if (INTEGER(starts)[i]==0 && (i<ngrp-1 || estn>-1)) continue;
    // Previously had replaced (i>0 || !isNull(lhs)) with i>0 to fix #5376
    // The above is now to fix #1993, see test 1746.
    // In cases were no i rows match, '|| estn>-1' ensures that the last empty group creates an empty result.
    // TODO: revisit and tidy

    if (!isNull(lhs) &&
        (INTEGER(starts)[i] == NA_INTEGER ||
         (LENGTH(order) && INTEGER(order)[ INTEGER(starts)[i]-1 ]==NA_INTEGER)))
      continue;
    grpn = INTEGER(lens)[i];
    INTEGER(N)[0] = INTEGER(starts)[i] == NA_INTEGER ? 0 : grpn;
    // .N is number of rows matched to ( 0 even when nomatch is NA)
    INTEGER(GRP)[0] = i+1;  // group counter exposed as .GRP

    for (j=0; j<length(iSD); j++) {   // either this or the next for() will run, not both
      size = SIZEOF(VECTOR_ELT(iSD,j));
      memcpy((char *)DATAPTR(VECTOR_ELT(iSD,j)),  // ok use of memcpy. Loop'd through columns not rows
             (char *)DATAPTR(VECTOR_ELT(groups,INTEGER(jiscols)[j]-1))+i*size,
             size);
    }
    // igrp determines the start of the current group in rows of dt (0 based).
    // if jiscols is not null, we have a by = .EACHI, so the start is exactly i.
    // Otherwise, igrp needs to be determined from starts, potentially taking care about the order if present.
    igrp = !isNull(jiscols) ? i : (length(grporder) ? INTEGER(grporder)[INTEGER(starts)[i]-1]-1 : INTEGER(starts)[i]-1);
    if (igrp>=0 && nrowgroups) for (j=0; j<length(BY); j++) {    // igrp can be -1 so 'if' is important, otherwise memcpy crash
      size = SIZEOF(VECTOR_ELT(BY,j));
      memcpy((char *)DATAPTR(VECTOR_ELT(BY,j)),  // ok use of memcpy size 1. Loop'd through columns not rows
             (char *)DATAPTR(VECTOR_ELT(groups,INTEGER(grpcols)[j]-1))+igrp*size,
             size);
    }
    if (INTEGER(starts)[i] == NA_INTEGER || (LENGTH(order) && INTEGER(order)[ INTEGER(starts)[i]-1 ]==NA_INTEGER)) {
      for (j=0; j<length(SDall); j++) {
        switch (TYPEOF(VECTOR_ELT(SDall, j))) {
        case LGLSXP :
          LOGICAL(VECTOR_ELT(SDall,j))[0] = NA_LOGICAL;
          break;
        case INTSXP :
          INTEGER(VECTOR_ELT(SDall,j))[0] = NA_INTEGER;
          break;
        case REALSXP :
          REAL(VECTOR_ELT(SDall,j))[0] = NA_REAL;
          break;
        case STRSXP :
          SET_STRING_ELT(VECTOR_ELT(SDall,j),0,NA_STRING);
          break;
        case VECSXP :
          SET_VECTOR_ELT(VECTOR_ELT(SDall,j),0,R_NilValue);
          break;
        default:
          error("Logical error. Type of column should have been checked by now");
        }
      }
      grpn = 1;  // it may not be 1 e.g. test 722. TODO: revisit.
      SETLENGTH(I, grpn);
      INTEGER(I)[0] = 0;
      for (j=0; j<length(xSD); j++) {
        switch (TYPEOF(VECTOR_ELT(xSD, j))) {
        case LGLSXP :
          LOGICAL(VECTOR_ELT(xSD,j))[0] = NA_LOGICAL;
          break;
        case INTSXP :
          INTEGER(VECTOR_ELT(xSD,j))[0] = NA_INTEGER;
          break;
        case REALSXP :
          REAL(VECTOR_ELT(xSD,j))[0] = NA_REAL;
          break;
        case STRSXP :
          SET_STRING_ELT(VECTOR_ELT(xSD,j),0,NA_STRING);
          break;
        case VECSXP :
          SET_VECTOR_ELT(VECTOR_ELT(xSD,j),0,R_NilValue);
          break;
        default:
          error("Logical error. Type of column should have been checked by now");
        }
      }
    } else {
      if (LOGICAL(verbose)[0]) tstart = clock();
      SETLENGTH(I, grpn);
      if (LENGTH(order)==0) {
        if (grpn) rownum = INTEGER(starts)[i]-1; else rownum = -1;  // not ternary to pass strict-barrier
        for (j=0; j<grpn; j++) INTEGER(I)[j] = rownum+j+1;
        if (rownum>=0) {
          for (j=0; j<length(SDall); j++) {
            size = SIZEOF(VECTOR_ELT(SDall,j));
            memcpy((char *)DATAPTR(VECTOR_ELT(SDall,j)),  // direct memcpy best here, for usually large size groups. by= each row is slow and not recommended anyway, so we don't mind there's no switch here for grpn==1
                   (char *)DATAPTR(VECTOR_ELT(dt,INTEGER(dtcols)[j]-1))+rownum*size,
                   grpn*size);
            // SD is our own alloc'd memory, and the source (DT) is protected throughout, so no need for SET_* overhead
          }
          for (j=0; j<length(xSD); j++) {
            size = SIZEOF(VECTOR_ELT(xSD,j));
            memcpy((char *)DATAPTR(VECTOR_ELT(xSD,j)),  // ok use of memcpy. Loop'd through columns not rows
                   (char *)DATAPTR(VECTOR_ELT(dt,INTEGER(xjiscols)[j]-1))+rownum*size,
                   size);
          }
        }
        if (LOGICAL(verbose)[0]) { tblock[0] += clock()-tstart; nblock[0]++; }
      } else {
        // Fairly happy with this block. No need for SET_* here. See comment above.
        for (k=0; k<grpn; k++) INTEGER(I)[k] = INTEGER(order)[ INTEGER(starts)[i]-1 + k ];
        for (j=0; j<length(SDall); j++) {
          size = SIZEOF(VECTOR_ELT(SDall,j));
          target = VECTOR_ELT(SDall,j);
          source = VECTOR_ELT(dt,INTEGER(dtcols)[j]-1);
          if (size==4) {
            for (k=0; k<grpn; k++) {
              rownum = INTEGER(I)[k]-1;
              INTEGER(target)[k] = INTEGER(source)[rownum]; // on 32bit, copies pointers too
            }
          } else {  // size 8
            for (k=0; k<grpn; k++) {
              rownum = INTEGER(I)[k]-1;
              REAL(target)[k] = REAL(source)[rownum];       // on 64bit, copies pointers too
            }
          }
        }
        if (LOGICAL(verbose)[0]) { tblock[1] += clock()-tstart; nblock[1]++; }
        // The two blocks have separate timing statements to make sure which is running
      }
    }
    INTEGER(rownames)[1] = -grpn;  // the .set_row_names() of .SD. Not .N when nomatch=NA and this is a nomatch
    for (j=0; j<length(SDall); j++) {
      SETLENGTH(VECTOR_ELT(SDall,j), grpn);
      defineVar(nameSyms[j], VECTOR_ELT(SDall, j), env);
      // In case user's j assigns to the columns names (env is static) (tests 387 and 388)
      // nameSyms pre-stored to save repeated install() for efficiency.
    }
    for (j=0; j<length(xSD); j++) {
      defineVar(xknameSyms[j], VECTOR_ELT(xSD, j), env);
    }

    if (LOGICAL(verbose)[0]) tstart = clock();  // call to clock() is more expensive than an 'if'
    PROTECT(jval = eval(jexp, env));

    if (LOGICAL(verbose)[0]) { tblock[2] += clock()-tstart; nblock[2]++; }

    if (isNull(jval))  {
      // j may be a plot or other side-effect only
      UNPROTECT(1);
      continue;
    }
    if ((wasvector = (isVectorAtomic(jval) || jexpIsSymbolOtherThanSD))) {  // see test 559
      // Prior to 1.8.1 the wrap with list() was done up in R after the first group had tested.
      // This wrap is done to make the code below easier to loop through. listwrap avoids copying jval.
      SET_VECTOR_ELT(listwrap,0,jval);
      jval = listwrap;
    } else {
      if (!isNewList(jval))    // isNewList tests for VECSXP. isList tests for (old) LISTSXP
        error("j evaluates to type '%s'. Must evaluate to atomic vector or list.",type2char(TYPEOF(jval)));
      if (!LENGTH(jval)) {
        UNPROTECT(1);
        continue;
      }
      for (j=0; j<LENGTH(jval); j++) {
        thiscol = VECTOR_ELT(jval,j);
        if (!isNull(thiscol) && (!isVector(thiscol) || isFrame(thiscol) || isArray(thiscol) ))
          error("All items in j=list(...) should be atomic vectors or lists. If you are trying something like j=list(.SD,newcol=mean(colA)) then use := by group instead (much quicker), or cbind or merge afterwards.");
      }
    }
    if (!isNull(lhs)) {
      R_len_t origncol = LENGTH(dt);
      for (j=0; j<length(lhs); j++) {
        target = VECTOR_ELT(dt,INTEGER(lhs)[j]-1);
        RHS = VECTOR_ELT(jval,j%LENGTH(jval));
        if (isNull(target)) {
          // first time adding to new column
          if (isNull(RHS)) error("RHS is NULL when grouping :=. Makes no sense to delete a column by group. Perhaps use an empty vector instead.");
          if (TRUELENGTH(dt) < INTEGER(lhs)[j]) error("Internal error: Trying to add new column by reference but tl is full; alloc.col should have run first at R level before getting to this point in dogroups");
          tmp = PROTECT(allocNAVector(TYPEOF(RHS), LENGTH(VECTOR_ELT(dt,0))));
          // increment length only if the allocation passes, #1676
          SETLENGTH(dtnames, LENGTH(dtnames)+1);
          SETLENGTH(dt, LENGTH(dt)+1);
          SET_VECTOR_ELT(dt, INTEGER(lhs)[j]-1, tmp);
          UNPROTECT(1);
          // Even if we could know reliably to switch from allocNAVector to allocVector for slight speedup, user code could still contain a switched halt, and in that case we'd want the groups not yet done to have NA rather than uninitialized or 0.
          // dtnames = getAttrib(dt, R_NamesSymbol); // commented this here and added it on the beginning to fix #4990
          SET_STRING_ELT(dtnames, INTEGER(lhs)[j]-1, STRING_ELT(newnames, INTEGER(lhs)[j]-origncol-1));
          target = VECTOR_ELT(dt,INTEGER(lhs)[j]-1);
        }
        if (TYPEOF(target)!=TYPEOF(RHS)) error("Type of RHS ('%s') must match LHS ('%s'). To check and coerce would impact performance too much for the fastest cases. Either change the type of the target column, or coerce the RHS of := yourself (e.g. by using 1L instead of 1)", type2char(TYPEOF(RHS)), type2char(TYPEOF(target)));
        size = SIZEOF(target);
        vlen = length(RHS);
        if (vlen==0) continue;
        if (grpn>0 && vlen>grpn && j<LENGTH(jval)) warning("RHS %d is length %d (greater than the size (%d) of group %d). The last %d element(s) will be discarded.", j+1, vlen, grpn, i+1, vlen-grpn);
        // fix for #4990 - `:=` did not issue recycling warning during "by" operation.
        if (vlen<grpn && vlen>0 && grpn%vlen != 0)
          warning("Supplied %d items to be assigned to group %d of size %d in column '%s' (recycled leaving remainder of %d items).",vlen,i+1,grpn,CHAR(STRING_ELT(dtnames,INTEGER(lhs)[j]-1)),grpn%vlen);

        memrecycle(target, order, INTEGER(starts)[i]-1, grpn, RHS);

        copyMostAttrib(RHS, target);  // not names, otherwise test 778 would fail.
        /* OLD FIX: commented now. The fix below resulted in segfault on factor columns because I dint set the "levels"
           Instead of fixing that, I just removed setting class if it's factor. Not appropriate fix.
           Correct fix of copying all attributes (except names) added above. Now, everything should be alright.
           Test 1144 (#5104) will provide the right output now. Modified accordingly.
        OUTDATED: if (!isFactor(RHS)) setAttrib(target, R_ClassSymbol, getAttrib(RHS, R_ClassSymbol));
        OUTDATED: // added !isFactor(RHS) to fix #5104 (side-effect of fixing #2531)
           See also #155 and #36 */

      }
      UNPROTECT(1);
      continue;
    }
    maxn = 0;
    if (njval==0) njval = LENGTH(jval);   // for first group, then the rest (when non 0) must conform to the first >0 group
    if (njval!=LENGTH(jval)) error("j doesn't evaluate to the same number of columns for each group");  // this would be a problem even if we unlisted afterwards. This way the user finds out earlier though so he can fix and rerun sooner.
    for (j=0; j<njval; j++) {
      k = length(VECTOR_ELT(jval,j));  // might be NULL, so length not LENGTH
      maxn = k>maxn ? k : maxn;
    }
    if (ansloc + maxn > estn) {
      if (estn == -1) {
        // Given first group and j's result on it, make a good guess for size of result required.
        if (grpn==0)
          estn = maxn = 0;   // empty case e.g. test 184. maxn is 1 here due to sum(integer()) == 0L
        else if (maxn==1) // including when grpn==1 we default to assuming it's an aggregate
          estn = LENGTH(starts);
          // Common case 1 : j is a list of simple aggregates i.e. list of atoms only
        else if (maxn >= grpn) {
          estn = 0;
          for (j=0;j<LENGTH(lens);j++) estn+=INTEGER(lens)[j];
          // Common case 2 : j returns as many rows as there are in the group (maybe a join)
          // TO DO: this might over allocate if first group has 1 row and j is actually a single row aggregate
          //        in cases when we're not sure could wait for the first few groups before deciding.
        } else  // maxn < grpn
          estn = maxn * LENGTH(starts);
          // Common case 3 : head or tail of .SD perhaps
        if (estn<maxn) estn=maxn;  // if the result for the first group is larger than the table itself(!) Unusual case where a join is being done in j via .SD and the 1-row table is an edge case of bigger picture.
        PROTECT(ans = allocVector(VECSXP, ngrpcols + njval));
        protecti++;
        firstalloc=TRUE;
        for(j=0; j<ngrpcols; j++) {
          thiscol = VECTOR_ELT(groups, INTEGER(grpcols)[j]-1);
          SET_VECTOR_ELT(ans, j, allocVector(TYPEOF(thiscol), estn));
          copyMostAttrib(thiscol, VECTOR_ELT(ans,j));  // not names, otherwise test 778 would fail
        }
        for(j=0; j<njval; j++) {
          thiscol = VECTOR_ELT(jval, j);
          if (isNull(thiscol))
            error("Column %d of j's result for the first group is NULL. We rely on the column types of the first result to decide the type expected for the remaining groups (and require consistency). NULL columns are acceptable for later groups (and those are replaced with NA of appropriate type and recycled) but not for the first. Please use a typed empty vector instead, such as integer() or numeric().", j+1);
          if (LOGICAL(verbose)[0] && !isNull(getAttrib(thiscol, R_NamesSymbol))) {
            if (wasvector) {
              Rprintf("j appears to be a named vector. The same names will likely be created over and over again for each group and slow things down. Try and pass a named list (which data.table optimizes) or an unnamed list() instead.");
            } else {
              Rprintf("Column %d of j is a named vector (each item down the rows is named, somehow). Please remove those names for efficiency (to save creating them over and over for each group). They are ignored anyway.", j+1);
            }
          }
          SET_VECTOR_ELT(ans, ngrpcols+j, allocVector(TYPEOF(thiscol), estn));
          copyMostAttrib(thiscol, VECTOR_ELT(ans,ngrpcols+j));  // not names, otherwise test 276 would fail
        }
        names = getAttrib(jval, R_NamesSymbol);
        if (!isNull(names)) {
          if (LOGICAL(verbose)[0]) Rprintf("The result of j is a named list. It's very inefficient to create the same names over and over again for each group. When j=list(...), any names are detected, removed and put back after grouping has completed, for efficiency. Using j=transform(), for example, prevents that speedup (consider changing to :=). This message may be upgraded to warning in future.\n");  // e.g. test 104 has j=transform().
          // names of result come from the first group and the names of remaining groups are ignored (all that matters for them is that the number of columns (and their types) match the first group.
          names2 = PROTECT(allocVector(STRSXP,ngrpcols+njval));
          protecti++;
          //  for (j=0; j<ngrpcols; j++) SET_STRING_ELT(names2, j, STRING_ELT(bynames,j));  // These get set back up in R
          for (j=0; j<njval; j++) SET_STRING_ELT(names2, ngrpcols+j, STRING_ELT(names,j));
          setAttrib(ans, R_NamesSymbol, names2);
          // setAttrib(SD, R_NamesSymbol, R_NilValue); // so that lapply(.SD,mean) is unnamed from 2nd group on
        }
      } else {
        estn = ((double)ngrp/i)*1.1*(ansloc+maxn);
        if (LOGICAL(verbose)[0]) Rprintf("dogroups: growing from %d to %d rows\n", length(VECTOR_ELT(ans,0)), estn);
        if (length(ans) != ngrpcols + njval) error("dogroups: length(ans)[%d]!=ngrpcols[%d]+njval[%d]",length(ans),ngrpcols,njval);
        for (j=0; j<length(ans); j++) SET_VECTOR_ELT(ans, j, growVector(VECTOR_ELT(ans,j), estn));
      }
    }
    // Now copy jval into ans ...
    for (j=0; j<ngrpcols; j++) {
      target = VECTOR_ELT(ans,j);
      source = VECTOR_ELT(groups, INTEGER(grpcols)[j]-1);  // target and source the same type by construction above
      if (SIZEOF(target)==4) for (r=0; r<maxn; r++)
        INTEGER(target)[ansloc+r] = INTEGER(source)[igrp];
      else for (r=0; r<maxn; r++)
        REAL(target)[ansloc+r] = REAL(source)[igrp];
      // Shouldn't need SET_* to age objects here sice groups, TO DO revisit.
    }
    for (j=0; j<njval; j++) {
      thisansloc = ansloc;
      source = VECTOR_ELT(jval,j);
      thislen = length(source);
      target = VECTOR_ELT(ans, j+ngrpcols);
      if (thislen == 0) {
        // including NULL and typed empty vectors, fill with NA
        // A NULL in the first group's jval isn't allowed; caught above after allocating ans
        if (!NullWarnDone && maxn>1) {  // maxn==1 in tests 172,280,281,282,403,405 and 406
          warning("Item %d of j's result for group %d is zero length. This will be filled with %d NAs to match the longest column in this result. Later groups may have a similar problem but only the first is reported to save filling the warning buffer.", j+1, i+1, maxn);
          NullWarnDone = TRUE;
        }
        switch (TYPEOF(target)) {     // rarely called so no need to optimize this switch
        case LGLSXP :
        case INTSXP :
          for (r=0; r<maxn; r++) INTEGER(target)[thisansloc+r] = NA_INTEGER;
          break;
        case REALSXP :
          for (r=0; r<maxn; r++) REAL(target)[thisansloc+r] = NA_REAL;
          break;
        case STRSXP :
          for (r=0; r<maxn; r++) SET_STRING_ELT(target,thisansloc+r,NA_STRING);
          break;
        case VECSXP :
          for (r=0; r<maxn; r++) SET_VECTOR_ELT(target,thisansloc+r,R_NilValue);
          break;
        default:
          error("Logical error. Type of column should have been checked by now");
        }
        continue;
      }
      if (TYPEOF(source) != TYPEOF(target))
        error("Column %d of result for group %d is type '%s' but expecting type '%s'. Column types must be consistent for each group.", j+1, i+1, type2char(TYPEOF(source)), type2char(TYPEOF(target)));
      if (recycleWarn && maxn%thislen != 0) {
        warning("Column %d of result for group %d is length %d but the longest column in this result is %d. Recycled leaving remainder of %d items. This warning is once only for the first group with this issue.",j+1,i+1,thislen,maxn,maxn%thislen);
        recycleWarn = FALSE;
      }
      memrecycle(target, R_NilValue, thisansloc, maxn, source);
    }
    ansloc += maxn;
    if (firstalloc) {
      protecti++;          //  remember the first jval. If we UNPROTECTed now, we'd unprotect
      firstalloc = FALSE;  //  ans. The first jval is needed to create the right size and type of ans.
      // TO DO: could avoid this last 'if' by adding a dummy PROTECT after first alloc for this UNPROTECT(1) to do.
    }
    else UNPROTECT(1);  // the jval. Don't want them to build up. The first jval can stay protected till the end ok.
  }
  if (isNull(lhs) && ans!=NULL) {
    if (ansloc < LENGTH(VECTOR_ELT(ans,0))) {
      if (LOGICAL(verbose)[0]) Rprintf("Wrote less rows (%d) than allocated (%d).\n",ansloc,LENGTH(VECTOR_ELT(ans,0)));
      for (j=0; j<length(ans); j++) SET_VECTOR_ELT(ans, j, growVector(VECTOR_ELT(ans,j), ansloc));
      // shrinks (misuse of word 'grow') back to the rows written, otherwise leak until ...
      // ... TO DO: set truelength to LENGTH(VECTOR_ELT(ans,0)), length to ansloc and enhance finalizer to handle over-allocated rows.
    }
  } else ans = R_NilValue;
  // Now reset length of .SD columns and .I to length of largest group, otherwise leak if the last group is smaller (often is).
  for (j=0; j<length(SDall); j++) SETLENGTH(VECTOR_ELT(SDall,j), origSDnrow);
  SETLENGTH(I, origIlen);
  if (LOGICAL(verbose)[0]) {
    if (nblock[0] && nblock[1]) error("Internal error: block 0 [%d] and block 1 [%d] have both run", nblock[0], nblock[1]);
    int w = nblock[1]>0;
    Rprintf("\n  %s took %.3fs for %d groups\n", w ? "collecting discontiguous groups" : "memcpy contiguous groups",
                          1.0*tblock[w]/CLOCKS_PER_SEC, nblock[w]);
    Rprintf("  eval(j) took %.3fs for %d calls\n", 1.0*tblock[2]/CLOCKS_PER_SEC, nblock[2]);
  }
  UNPROTECT(protecti);
  return(ans);
}

SEXP keepattr(SEXP to, SEXP from)
{
  // Same as R_copyDFattr in src/main/attrib.c, but that seems not exposed in R's api
  // Only difference is that we reverse from and to in the prototype, for easier calling above
  SET_ATTRIB(to, ATTRIB(from));
  IS_S4_OBJECT(from) ?  SET_S4_OBJECT(to) : UNSET_S4_OBJECT(to);
  SET_OBJECT(to, OBJECT(from));
  return to;
}

SEXP growVector(SEXP x, R_len_t newlen)
{
  // Similar to EnlargeVector in src/main/subassign.c, with the following changes :
  // * replaced switch and loops with one memcpy for INTEGER and REAL, but need to age CHAR and VEC.
  // * no need to cater for names
  // * much shorter and faster
  SEXP newx;
  R_len_t i, len = length(x);
  if (isNull(x)) error("growVector passed NULL");
  PROTECT(newx = allocVector(TYPEOF(x), newlen));   // TO DO: R_realloc(?) here?
  if (newlen < len) len=newlen;   // i.e. shrink
  switch (TYPEOF(x)) {
  case STRSXP :
    for (i=0; i<len; i++)
      SET_STRING_ELT(newx, i, STRING_ELT(x, i));
    // TO DO. Using SET_ to ensure objects are aged, rather than memcpy. Perhaps theres a bulk/fast way to age CHECK_OLD_TO_NEW
    break;
  case VECSXP :
    for (i=0; i<len; i++)
      SET_VECTOR_ELT(newx, i, VECTOR_ELT(x, i));
    // TO DO: Again, is there bulk op to avoid this loop, which still respects older generations
    break;
  default :
    memcpy((char *)DATAPTR(newx), (char *)DATAPTR(x), len*SIZEOF(x));   // SIZEOF() returns size_t (just as sizeof()) so * shouldn't overflow
  }
  // if (verbose) Rprintf("Growing vector from %d to %d items of type '%s'\n", len, newlen, type2char(TYPEOF(x)));
  // Would print for every column if here. Now just up in dogroups (one msg for each table grow).
  keepattr(newx,x);
  UNPROTECT(1);
  return newx;
}


// benchmark timings for #481 fix:
// old code - no changes, R v3.0.3
// > system.time(dt[, list(list(y)), by=x])
//    user  system elapsed
//  82.593   0.936  84.314
// > system.time(dt[, list(list(y)), by=x])
//    user  system elapsed
//  34.558   0.628  35.658
// > system.time(dt[, list(list(y)), by=x])
//    user  system elapsed
//  37.056   0.315  37.668
//
// All new changes in place, R v3.0.3
// > system.time(dt[, list(list(y)), by=x])
//    user  system elapsed
//  82.852   0.952  84.575
// > system.time(dt[, list(list(y)), by=x])
//    user  system elapsed
//  34.600   0.356  35.173
// > system.time(dt[, list(list(y)), by=x])
//    user  system elapsed
//  36.865   0.514  37.901

// old code - no changes, R v3.1.0 --- BUT RESULTS ARE WRONG!
// > system.time(dt[, list(list(y)), by=x])
//    user  system elapsed
//  11.022   0.352  11.455
// > system.time(dt[, list(list(y)), by=x])
//    user  system elapsed
//  10.397   0.119  10.600
// > system.time(dt[, list(list(y)), by=x])
//    user  system elapsed
//  10.665   0.101  11.013

// All new changes in place, R v3.1.0
// > system.time(dt[, list(list(y)), by=x])
//    user  system elapsed
//  83.279   1.057  89.856
// > system.time(dt[, list(list(y)), by=x])
//    user  system elapsed
//  30.569   0.633  31.452
// > system.time(dt[, list(list(y)), by=x])
//    user  system elapsed
//  30.827   0.239  32.306

