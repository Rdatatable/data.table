#include "data.table.h"
#include <Rdefines.h>
#include <fcntl.h>
#include <time.h>

SEXP dogroups(SEXP dt, SEXP dtcols, SEXP groups, SEXP grpcols, SEXP jiscols, SEXP xjiscols, SEXP grporder, SEXP order, SEXP starts, SEXP lens, SEXP jexp, SEXP env, SEXP lhs, SEXP newnames, SEXP on, SEXP verboseArg)
{
  R_len_t ngrp, nrowgroups, njval=0, ngrpcols, ansloc=0, maxn, estn=-1, thisansloc, grpn, thislen, igrp, origIlen=0, origSDnrow=0;
  int nprotect=0;
  SEXP ans=NULL, jval, thiscol, BY, N, I, GRP, iSD, xSD, rownames, s, RHS, target, source;
  Rboolean wasvector, firstalloc=FALSE, NullWarnDone=FALSE;
  clock_t tstart=0, tblock[10]={0}; int nblock[10]={0};
  const bool verbose = LOGICAL(verboseArg)[0]==1;

  if (!isInteger(order)) error(_("Internal error: order not integer vector")); // # nocov
  if (TYPEOF(starts) != INTSXP) error(_("Internal error: starts not integer")); // # nocov
  if (TYPEOF(lens) != INTSXP) error(_("Internal error: lens not integer")); // # nocov
  // starts can now be NA (<0): if (INTEGER(starts)[0]<0 || INTEGER(lens)[0]<0) error(_("starts[1]<0 or lens[1]<0"));
  if (!isNull(jiscols) && LENGTH(order) && !LOGICAL(on)[0]) error(_("Internal error: jiscols not NULL but o__ has length")); // # nocov
  if (!isNull(xjiscols) && LENGTH(order) && !LOGICAL(on)[0]) error(_("Internal error: xjiscols not NULL but o__ has length")); // # nocov
  if(!isEnvironment(env)) error(_("'env' should be an environment"));
  ngrp = length(starts);  // the number of groups  (nrow(groups) will be larger when by)
  ngrpcols = length(grpcols);
  nrowgroups = length(VECTOR_ELT(groups,0));
  // fix for longstanding FR/bug, #495. E.g., DT[, c(sum(v1), lapply(.SD, mean)), by=grp, .SDcols=v2:v3] resulted in error.. the idea is, 1) we create .SDall, which is normally == .SD. But if extra vars are detected in jexp other than .SD, then .SD becomes a shallow copy of .SDall with only .SDcols in .SD. Since internally, we don't make a copy, changing .SDall will reflect in .SD. Hopefully this'll workout :-).
  SEXP SDall = PROTECT(findVar(install(".SDall"), env)); nprotect++;  // PROTECT for rchk
  SEXP SD = PROTECT(findVar(install(".SD"), env)); nprotect++;

  defineVar(sym_BY, BY = PROTECT(allocVector(VECSXP, ngrpcols)), env); nprotect++;  // PROTECT for rchk
  SEXP bynames = PROTECT(allocVector(STRSXP, ngrpcols));  nprotect++;   // TO DO: do we really need bynames, can we assign names afterwards in one step?
  for (int i=0; i<ngrpcols; ++i) {
    int j = INTEGER(grpcols)[i]-1;
    SET_VECTOR_ELT(BY, i, allocVector(TYPEOF(VECTOR_ELT(groups, j)),
      nrowgroups ? 1 : 0)); // TODO: might be able to be 1 always but 0 when 'groups' are integer(0) seem sensible. #2440 was involved in the past.
    // Fix for #36, by cols with attributes when also used in `j` lost the attribute.
    copyMostAttrib(VECTOR_ELT(groups, j), VECTOR_ELT(BY,i));  // not names, otherwise test 778 would fail
    SET_STRING_ELT(bynames, i, STRING_ELT(getAttrib(groups,R_NamesSymbol), j));
    defineVar(install(CHAR(STRING_ELT(bynames,i))), VECTOR_ELT(BY,i), env);      // by vars can be used by name in j as well as via .BY
    if (SIZEOF(VECTOR_ELT(BY,i))==0)
      error(_("Internal error: unsupported size-0 type '%s' in column %d of 'by' should have been caught earlier"), type2char(TYPEOF(VECTOR_ELT(BY, i))), i+1); // #nocov
  }
  setAttrib(BY, R_NamesSymbol, bynames); // Fix for #42 - BY doesn't retain names anymore
  R_LockBinding(sym_BY, env);
  if (isNull(jiscols) && (length(bynames)!=length(groups) || length(bynames)!=length(grpcols))) error(_("!length(bynames)[%d]==length(groups)[%d]==length(grpcols)[%d]"),length(bynames),length(groups),length(grpcols));
  // TO DO: check this check above.

  N =   PROTECT(findVar(install(".N"), env));   nprotect++; // PROTECT for rchk
  GRP = PROTECT(findVar(install(".GRP"), env)); nprotect++;
  iSD = PROTECT(findVar(install(".iSD"), env)); nprotect++; // 1-row and possibly no cols (if no i variables are used via JIS)
  xSD = PROTECT(findVar(install(".xSD"), env)); nprotect++;
  R_len_t maxGrpSize = 0;
  const int *ilens = INTEGER(lens), n=LENGTH(lens);
  for (R_len_t i=0; i<n; ++i) {
    if (ilens[i] > maxGrpSize) maxGrpSize = ilens[i];
  }
  defineVar(install(".I"), I = PROTECT(allocVector(INTSXP, maxGrpSize)), env); nprotect++;
  R_LockBinding(install(".I"), env);

  SEXP dtnames = PROTECT(getAttrib(dt, R_NamesSymbol)); nprotect++; // added here to fix #91 - `:=` did not issue recycling warning during "by"
  // fetch rownames of .SD.  rownames[1] is set to -thislen for each group, in case .SD is passed to
  // non data.table aware package that uses rownames
  for (s = ATTRIB(SD); s != R_NilValue && TAG(s)!=R_RowNamesSymbol; s = CDR(s));  // getAttrib0 basically but that's hidden in attrib.c
  if (s==R_NilValue) error(_("row.names attribute of .SD not found"));
  rownames = CAR(s);
  if (!isInteger(rownames) || LENGTH(rownames)!=2 || INTEGER(rownames)[0]!=NA_INTEGER) error(_("row.names of .SD isn't integer length 2 with NA as first item; i.e., .set_row_names(). [%s %d %d]"),type2char(TYPEOF(rownames)),LENGTH(rownames),INTEGER(rownames)[0]);

  // fetch names of .SD and prepare symbols. In case they are copied-on-write by user assigning to those variables
  // using <- in j (which is valid, useful and tested), they are repointed to the .SD cols for each group.
  SEXP names = PROTECT(getAttrib(SDall, R_NamesSymbol)); nprotect++;
  if (length(names) != length(SDall)) error(_("length(names)!=length(SD)"));
  SEXP *nameSyms = (SEXP *)R_alloc(length(names), sizeof(SEXP));
  for(int i=0; i<length(SDall); ++i) {
    if (SIZEOF(VECTOR_ELT(SDall, i))==0)
      error(_("Internal error: size-0 type %d in .SD column %d should have been caught earlier"), TYPEOF(VECTOR_ELT(SDall, i)), i); // #nocov
    nameSyms[i] = install(CHAR(STRING_ELT(names, i)));
    // fixes http://stackoverflow.com/questions/14753411/why-does-data-table-lose-class-definition-in-sd-after-group-by
    copyMostAttrib(VECTOR_ELT(dt,INTEGER(dtcols)[i]-1), VECTOR_ELT(SDall,i));  // not names, otherwise test 778 would fail
  }

  origIlen = length(I);  // test 762 has length(I)==1 but nrow(SD)==0
  if (length(SDall)) origSDnrow = length(VECTOR_ELT(SDall, 0));

  SEXP xknames = PROTECT(getAttrib(xSD, R_NamesSymbol)); nprotect++;
  if (length(xknames) != length(xSD)) error(_("length(xknames)!=length(xSD)"));
  SEXP *xknameSyms = (SEXP *)R_alloc(length(xknames), sizeof(SEXP));
  for(int i=0; i<length(xSD); ++i) {
    if (SIZEOF(VECTOR_ELT(xSD, i))==0)
      error(_("Internal error: type %d in .xSD column %d should have been caught by now"), TYPEOF(VECTOR_ELT(xSD, i)), i); // #nocov
    xknameSyms[i] = install(CHAR(STRING_ELT(xknames, i)));
  }

  if (length(iSD)!=length(jiscols)) error(_("length(iSD)[%d] != length(jiscols)[%d]"),length(iSD),length(jiscols));
  if (length(xSD)!=length(xjiscols)) error(_("length(xSD)[%d] != length(xjiscols)[%d]"),length(xSD),length(xjiscols));

  SEXP listwrap = PROTECT(allocVector(VECSXP, 1)); nprotect++;
  Rboolean jexpIsSymbolOtherThanSD = (isSymbol(jexp) && strcmp(CHAR(PRINTNAME(jexp)),".SD")!=0);  // test 559

  ansloc = 0;
  const int *istarts = INTEGER(starts);
  const int *iorder = INTEGER(order);
  for(int i=0; i<ngrp; ++i) {   // even for an empty i table, ngroup is length 1 (starts is value 0), for consistency of empty cases

    if (istarts[i]==0 && (i<ngrp-1 || estn>-1)) continue;
    // Previously had replaced (i>0 || !isNull(lhs)) with i>0 to fix #49
    // The above is now to fix #1993, see test 1746.
    // In cases were no i rows match, '|| estn>-1' ensures that the last empty group creates an empty result.
    // TODO: revisit and tidy

    if (!isNull(lhs) &&
        (istarts[i] == NA_INTEGER ||
         (LENGTH(order) && iorder[ istarts[i]-1 ]==NA_INTEGER)))
      continue;
    grpn = ilens[i];
    INTEGER(N)[0] = istarts[i] == NA_INTEGER ? 0 : grpn;
    // .N is number of rows matched to ( 0 even when nomatch is NA)
    INTEGER(GRP)[0] = i+1;  // group counter exposed as .GRP
    INTEGER(rownames)[1] = -grpn;  // the .set_row_names() of .SD. Not .N when nomatch=NA and this is a nomatch
    for (int j=0; j<length(SDall); ++j) {
      SETLENGTH(VECTOR_ELT(SDall,j), grpn);  // before copying data in otherwise assigning after the end could error R API checks
      defineVar(nameSyms[j], VECTOR_ELT(SDall, j), env);
      // Redo this defineVar for each group in case user's j assigned to the column names (env is static) (tests 387 and 388)
      // nameSyms pre-stored to save repeated install() for efficiency, though.
    }
    for (int j=0; j<length(xSD); ++j) {
      defineVar(xknameSyms[j], VECTOR_ELT(xSD, j), env);
    }

    if (length(iSD) && length(VECTOR_ELT(iSD, 0))/*#4364*/) for (int j=0; j<length(iSD); ++j) {   // either this or the next for() will run, not both
      memrecycle(VECTOR_ELT(iSD,j), R_NilValue, 0, 1, VECTOR_ELT(groups, INTEGER(jiscols)[j]-1), i, 1, j+1, "Internal error assigning to iSD");
      // we're just use memrecycle here to assign a single value
    }
    // igrp determines the start of the current group in rows of dt (0 based).
    // if jiscols is not null, we have a by = .EACHI, so the start is exactly i.
    // Otherwise, igrp needs to be determined from starts, potentially taking care about the order if present.
    igrp = !isNull(jiscols) ? i : (length(grporder) ? INTEGER(grporder)[istarts[i]-1]-1 : istarts[i]-1);
    if (igrp>=0 && nrowgroups) for (int j=0; j<length(BY); ++j) {    // igrp can be -1 so 'if' is important, otherwise memcpy crash
      memrecycle(VECTOR_ELT(BY,j), R_NilValue, 0, 1, VECTOR_ELT(groups, INTEGER(grpcols)[j]-1), igrp, 1, j+1, "Internal error assigning to BY");
    }
    if (istarts[i] == NA_INTEGER || (LENGTH(order) && iorder[ istarts[i]-1 ]==NA_INTEGER)) {
      for (int j=0; j<length(SDall); ++j) {
        writeNA(VECTOR_ELT(SDall, j), 0, 1);
        // writeNA uses SET_ for STR and VEC, and we always use SET_ to assign to SDall always too. Otherwise,
        // this writeNA could decrement the reference for the old value which wasn't incremented in the first place.
        // Further, the eval(jval) could feasibly assign to SD although that is currently caught and disallowed. If that
        // became possible, that assign from user's j expression would decrement the reference which wasn't incremented
        // in the first place. And finally, upon release of SD, values will be decremented, where they weren't incremented
        // in the first place. All in all, too risky to write behind the barrier in this section.
        // Or in the words, this entire section, and this entire dogroups.c file, is now write-barrier compliant from v1.12.10
        // and we hope that reference counting on by default from R 4.0 will avoid costly gc()s.
      }
      grpn = 1;  // it may not be 1 e.g. test 722. TODO: revisit.
      SETLENGTH(I, grpn);
      INTEGER(I)[0] = 0;
      for (int j=0; j<length(xSD); ++j) {
        writeNA(VECTOR_ELT(xSD, j), 0, 1);
      }
    } else {
      if (verbose) tstart = clock();
      SETLENGTH(I, grpn);
      int *iI = INTEGER(I);
      if (LENGTH(order)==0) {
        const int rownum = grpn ? istarts[i]-1 : -1;
        for (int j=0; j<grpn; ++j) iI[j] = rownum+j+1;
        if (rownum>=0) {
          for (int j=0; j<length(SDall); ++j)
            memrecycle(VECTOR_ELT(SDall,j), R_NilValue, 0, grpn, VECTOR_ELT(dt, INTEGER(dtcols)[j]-1), rownum, grpn, j+1, "Internal error assigning to SDall");
          for (int j=0; j<length(xSD); ++j)
            memrecycle(VECTOR_ELT(xSD,j), R_NilValue, 0, 1, VECTOR_ELT(dt, INTEGER(xjiscols)[j]-1), rownum, 1, j+1, "Internal error assigning to xSD");
        }
        if (verbose) { tblock[0] += clock()-tstart; nblock[0]++; }
      } else {
        for (int k=0; k<grpn; ++k) iI[k] = iorder[ istarts[i]-1 + k ];
        for (int j=0; j<length(SDall); ++j) {
          // this is the main non-contiguous gather, and is parallel (within-column) for non-SEXP
          subsetVectorRaw(VECTOR_ELT(SDall,j), VECTOR_ELT(dt,INTEGER(dtcols)[j]-1), I, /*anyNA=*/false);
        }
        if (verbose) { tblock[1] += clock()-tstart; nblock[1]++; }
        // The two blocks have separate timing statements to make sure which is running
      }
    }

    if (verbose) tstart = clock();  // call to clock() is more expensive than an 'if'
    PROTECT(jval = eval(jexp, env));
    if (verbose) { tblock[2] += clock()-tstart; nblock[2]++; }

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
        error(_("j evaluates to type '%s'. Must evaluate to atomic vector or list."),type2char(TYPEOF(jval)));
      if (!LENGTH(jval)) {
        UNPROTECT(1);
        continue;
      }
      for (int j=0; j<LENGTH(jval); ++j) {
        thiscol = VECTOR_ELT(jval,j);
        if (!isNull(thiscol) && (!isVector(thiscol) || isFrame(thiscol) || isArray(thiscol) ))
          error(_("All items in j=list(...) should be atomic vectors or lists. If you are trying something like j=list(.SD,newcol=mean(colA)) then use := by group instead (much quicker), or cbind or merge afterwards."));
      }
    }
    if (!isNull(lhs)) {
      R_len_t origncol = LENGTH(dt);
      // check jval first before proceeding to add columns, so that if error on the first group, the columns aren't added
      for (int j=0; j<length(lhs); ++j) {
        RHS = VECTOR_ELT(jval,j%LENGTH(jval));
        if (isNull(RHS))
          error(_("RHS of := is NULL during grouped assignment, but it's not possible to delete parts of a column."));
        int vlen = length(RHS);
        if (vlen>1 && vlen!=grpn) {
          SEXP colname = isNull(VECTOR_ELT(dt, INTEGER(lhs)[j]-1)) ? STRING_ELT(newnames, INTEGER(lhs)[j]-origncol-1) : STRING_ELT(dtnames,INTEGER(lhs)[j]-1);
          error(_("Supplied %d items to be assigned to group %d of size %d in column '%s'. The RHS length must either be 1 (single values are ok) or match the LHS length exactly. If you wish to 'recycle' the RHS please use rep() explicitly to make this intent clear to readers of your code."),vlen,i+1,grpn,CHAR(colname));
          // e.g. in #91 `:=` did not issue recycling warning during grouping. Now it is error not warning.
        }
      }
      int n = LENGTH(VECTOR_ELT(dt, 0));
      for (int j=0; j<length(lhs); ++j) {
        int colj = INTEGER(lhs)[j]-1;
        target = VECTOR_ELT(dt, colj);
        RHS = VECTOR_ELT(jval,j%LENGTH(jval));
        if (isNull(target)) {
          // first time adding to new column
          if (TRUELENGTH(dt) < colj+1) error(_("Internal error: Trying to add new column by reference but tl is full; setalloccol should have run first at R level before getting to this point in dogroups")); // # nocov
          target = PROTECT(allocNAVectorLike(RHS, n));
          // Even if we could know reliably to switch from allocNAVectorLike to allocVector for slight speedup, user code could still
          // contain a switched halt, and in that case we'd want the groups not yet done to have NA rather than 0 or uninitialized.
          // Increment length only if the allocation passes, #1676. But before SET_VECTOR_ELT otherwise attempt-to-set-index-n/n R error
          SETLENGTH(dtnames, LENGTH(dtnames)+1);
          SETLENGTH(dt, LENGTH(dt)+1);
          SET_VECTOR_ELT(dt, colj, target);
          UNPROTECT(1);
          SET_STRING_ELT(dtnames, colj, STRING_ELT(newnames, colj-origncol));
          copyMostAttrib(RHS, target); // attributes of first group dominate; e.g. initial factor levels come from first group
        }
        const char *warn = memrecycle(target, order, INTEGER(starts)[i]-1, grpn, RHS, 0, -1, 0, "");
        // can't error here because length mismatch already checked for all jval columns before starting to add any new columns
        if (warn)
          warning(_("Group %d column '%s': %s"), i+1, CHAR(STRING_ELT(dtnames, colj)), warn);
      }
      UNPROTECT(1); // jval
      continue;
    }
    maxn = 0;
    if (njval==0) njval = LENGTH(jval);   // for first group, then the rest (when non 0) must conform to the first >0 group
    if (njval!=LENGTH(jval)) error(_("j doesn't evaluate to the same number of columns for each group"));  // this would be a problem even if we unlisted afterwards. This way the user finds out earlier though so he can fix and rerun sooner.
    for (int j=0; j<njval; ++j) {
      int k = length(VECTOR_ELT(jval,j));  // might be NULL, so length not LENGTH
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
          for (int j=0; j<LENGTH(lens); ++j) estn+=ilens[j];
          // Common case 2 : j returns as many rows as there are in the group (maybe a join)
          // TO DO: this might over allocate if first group has 1 row and j is actually a single row aggregate
          //        in cases when we're not sure could wait for the first few groups before deciding.
        } else  // maxn < grpn
          estn = maxn * LENGTH(starts);
          // Common case 3 : head or tail of .SD perhaps
        if (estn<maxn) estn=maxn;  // if the result for the first group is larger than the table itself(!) Unusual case where a join is being done in j via .SD and the 1-row table is an edge case of bigger picture.
        PROTECT(ans = allocVector(VECSXP, ngrpcols + njval));
        nprotect++;
        firstalloc=TRUE;
        for(int j=0; j<ngrpcols; ++j) {
          thiscol = VECTOR_ELT(groups, INTEGER(grpcols)[j]-1);
          SET_VECTOR_ELT(ans, j, allocVector(TYPEOF(thiscol), estn));
          copyMostAttrib(thiscol, VECTOR_ELT(ans,j));  // not names, otherwise test 778 would fail
        }
        for(int j=0; j<njval; ++j) {
          thiscol = VECTOR_ELT(jval, j);
          if (isNull(thiscol))
            error(_("Column %d of j's result for the first group is NULL. We rely on the column types of the first result to decide the type expected for the remaining groups (and require consistency). NULL columns are acceptable for later groups (and those are replaced with NA of appropriate type and recycled) but not for the first. Please use a typed empty vector instead, such as integer() or numeric()."), j+1);
          if (verbose && !isNull(getAttrib(thiscol, R_NamesSymbol))) {
            if (wasvector) {
              Rprintf(_("j appears to be a named vector. The same names will likely be created over and over again for each group and slow things down. Try and pass a named list (which data.table optimizes) or an unnamed list() instead.\n"));
            } else {
              Rprintf(_("Column %d of j is a named vector (each item down the rows is named, somehow). Please remove those names for efficiency (to save creating them over and over for each group). They are ignored anyway.\n"), j+1);
            }
          }
          SET_VECTOR_ELT(ans, ngrpcols+j, allocVector(TYPEOF(thiscol), estn));
          copyMostAttrib(thiscol, VECTOR_ELT(ans,ngrpcols+j));  // not names, otherwise test 276 would fail
        }
        SEXP jvalnames = PROTECT(getAttrib(jval, R_NamesSymbol));
        if (!isNull(jvalnames)) {
          if (verbose) Rprintf(_("The result of j is a named list. It's very inefficient to create the same names over and over again for each group. When j=list(...), any names are detected, removed and put back after grouping has completed, for efficiency. Using j=transform(), for example, prevents that speedup (consider changing to :=). This message may be upgraded to warning in future.\n"));  // e.g. test 104 has j=transform().
          // names of result come from the first group and the names of remaining groups are ignored (all that matters for them is that the number of columns (and their types) match the first group.
          SEXP names2 = PROTECT(allocVector(STRSXP,ngrpcols+njval));
          //  for (j=0; j<ngrpcols; j++) SET_STRING_ELT(names2, j, STRING_ELT(bynames,j));  // These get set back up in R
          for (int j=0; j<njval; ++j) SET_STRING_ELT(names2, ngrpcols+j, STRING_ELT(jvalnames,j));
          setAttrib(ans, R_NamesSymbol, names2);
          UNPROTECT(1); // names2
          // setAttrib(SD, R_NamesSymbol, R_NilValue); // so that lapply(.SD,mean) is unnamed from 2nd group on
        }
        UNPROTECT(1); // jvalnames
      } else {
        estn = ((double)ngrp/i)*1.1*(ansloc+maxn);
        if (verbose) Rprintf(_("dogroups: growing from %d to %d rows\n"), length(VECTOR_ELT(ans,0)), estn);
        if (length(ans) != ngrpcols + njval) error(_("dogroups: length(ans)[%d]!=ngrpcols[%d]+njval[%d]"),length(ans),ngrpcols,njval);
        for (int j=0; j<length(ans); ++j) SET_VECTOR_ELT(ans, j, growVector(VECTOR_ELT(ans,j), estn));
      }
    }
    // write the group values to ans, recycled to match the nrow of the result for this group ...
    for (int j=0; j<ngrpcols; ++j) {
      memrecycle(VECTOR_ELT(ans,j), R_NilValue, ansloc, maxn, VECTOR_ELT(groups, INTEGER(grpcols)[j]-1), igrp, 1, j+1, "Internal error recycling group values");
    }
    // Now copy jval into ans ...
    for (int j=0; j<njval; ++j) {
      thisansloc = ansloc;
      source = VECTOR_ELT(jval,j);
      thislen = length(source);
      target = VECTOR_ELT(ans, j+ngrpcols);
      if (thislen == 0) {
        // including NULL and typed empty vectors, fill with NA
        // A NULL in the first group's jval isn't allowed; caught above after allocating ans
        if (!NullWarnDone && maxn>1) {  // maxn==1 in tests 172,280,281,282,403,405 and 406
          warning(_("Item %d of j's result for group %d is zero length. This will be filled with %d NAs to match the longest column in this result. Later groups may have a similar problem but only the first is reported to save filling the warning buffer."), j+1, i+1, maxn);
          NullWarnDone = TRUE;
        }
        writeNA(target, thisansloc, maxn);
      } else {
        // thislen>0
        if (TYPEOF(source) != TYPEOF(target))
          error(_("Column %d of result for group %d is type '%s' but expecting type '%s'. Column types must be consistent for each group."), j+1, i+1, type2char(TYPEOF(source)), type2char(TYPEOF(target)));
        if (thislen>1 && thislen!=maxn && grpn>0) {  // grpn>0 for grouping empty tables; test 1986
          error(_("Supplied %d items for column %d of group %d which has %d rows. The RHS length must either be 1 (single values are ok) or match the LHS length exactly. If you wish to 'recycle' the RHS please use rep() explicitly to make this intent clear to readers of your code."), thislen, j+1, i+1, maxn);
        }
        memrecycle(target, R_NilValue, thisansloc, maxn, source, 0, -1, 0, "");
      }
    }
    ansloc += maxn;
    if (firstalloc) {
      nprotect++;          //  remember the first jval. If we UNPROTECTed now, we'd unprotect
      firstalloc = FALSE;  //  ans. The first jval is needed to create the right size and type of ans.
      // TO DO: could avoid this last 'if' by adding a dummy PROTECT after first alloc for this UNPROTECT(1) to do.
    }
    else UNPROTECT(1);  // the jval. Don't want them to build up. The first jval can stay protected till the end ok.
  }
  if (isNull(lhs) && ans!=NULL) {
    if (ansloc < LENGTH(VECTOR_ELT(ans,0))) {
      if (verbose) Rprintf(_("Wrote less rows (%d) than allocated (%d).\n"),ansloc,LENGTH(VECTOR_ELT(ans,0)));
      for (int j=0; j<length(ans); j++) SET_VECTOR_ELT(ans, j, growVector(VECTOR_ELT(ans,j), ansloc));
      // shrinks (misuse of word 'grow') back to the rows written, otherwise leak until ...
      // ... TO DO: set truelength to LENGTH(VECTOR_ELT(ans,0)), length to ansloc and enhance finalizer to handle over-allocated rows.
    }
  } else ans = R_NilValue;
  // Now reset length of .SD columns and .I to length of largest group, otherwise leak if the last group is smaller (often is).
  for (int j=0; j<length(SDall); ++j) SETLENGTH(VECTOR_ELT(SDall,j), origSDnrow);
  SETLENGTH(I, origIlen);
  if (verbose) {
    if (nblock[0] && nblock[1]) error(_("Internal error: block 0 [%d] and block 1 [%d] have both run"), nblock[0], nblock[1]); // # nocov
    int w = nblock[1]>0;
    Rprintf(_("\n  %s took %.3fs for %d groups\n"), w ? "collecting discontiguous groups" : "memcpy contiguous groups",
                          1.0*tblock[w]/CLOCKS_PER_SEC, nblock[w]);
    Rprintf(_("  eval(j) took %.3fs for %d calls\n"), 1.0*tblock[2]/CLOCKS_PER_SEC, nblock[2]);
  }
  UNPROTECT(nprotect);
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

SEXP growVector(SEXP x, const R_len_t newlen)
{
  // Similar to EnlargeVector in src/main/subassign.c, with the following changes :
  // * replaced switch and loops with one memcpy for INTEGER and REAL, but need to age CHAR and VEC.
  // * no need to cater for names
  // * much shorter and faster
  SEXP newx;
  R_len_t len = length(x);
  if (isNull(x)) error(_("growVector passed NULL"));
  PROTECT(newx = allocVector(TYPEOF(x), newlen));   // TO DO: R_realloc(?) here?
  if (newlen < len) len=newlen;   // i.e. shrink
  switch (TYPEOF(x)) {
  case RAWSXP:  memcpy(RAW(newx),     RAW(x),     len*SIZEOF(x)); break;
  case LGLSXP:  memcpy(LOGICAL(newx), LOGICAL(x), len*SIZEOF(x)); break;
  case INTSXP:  memcpy(INTEGER(newx), INTEGER(x), len*SIZEOF(x)); break;
  case REALSXP: memcpy(REAL(newx),    REAL(x),    len*SIZEOF(x)); break;
  case CPLXSXP: memcpy(COMPLEX(newx), COMPLEX(x), len*SIZEOF(x)); break;
  case STRSXP : {
    const SEXP *xd = SEXPPTR_RO(x);
    for (int i=0; i<len; ++i)
      SET_STRING_ELT(newx, i, xd[i]);
  } break;
  case VECSXP : {
    const SEXP *xd = SEXPPTR_RO(x);
    for (int i=0; i<len; ++i)
      SET_VECTOR_ELT(newx, i, xd[i]);
  } break;
  default :
    error(_("Internal error: growVector doesn't support type '%s'"), type2char(TYPEOF(x)));  // # nocov
  }
  // if (verbose) Rprintf(_("Growing vector from %d to %d items of type '%s'\n"), len, newlen, type2char(TYPEOF(x)));
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
