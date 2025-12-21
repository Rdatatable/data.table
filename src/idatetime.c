#include "data.table.h"

static const int YEARS400 = 146097;
static const int YEARS100 = 36524;
static const int YEARS4 = 1461;
static const int YEARS1 = 365;

typedef enum { YDAY, WDAY, MDAY, WEEK, MONTH, QUARTER, YEAR, YEARMON, YEARQTR} datetype;

static inline bool isLeapYear(int year) {
    return (year % 100 != 0 || year % 400 == 0) && year % 4 == 0;
}

void convertSingleDate(int x, datetype type, void *out)
{
    static const char months[] = {31, 30, 31, 30, 31, 31, 30, 31, 30, 31, 31, 29};
    static const int quarter[] = {31, 91, 92, 92, 60};

    if (x == NA_INTEGER) {
        if (type == YEARMON || type == YEARQTR) *(double *)out = NA_REAL;
        else *(int *)out = NA_INTEGER;
        return;
    }

    if (type == WDAY) {
        int wday = (x + 4) % 7;
        if (wday < 0) wday += 7;
        *(int *)out = ++wday;
        return;
    }

    int days = x - 11017;

    int years400 = days / YEARS400;
    days %= YEARS400;
    if (days < 0) {
        days += YEARS400;
        years400--;
    }

    int years100 = days / YEARS100;
    days %= YEARS100;

    int years4 = days / YEARS4;
    days %= YEARS4;

    int years1 = days / YEARS1;
    days %= YEARS1;

    int year = 2000 + years1 + 4*years4 + 100*years100 + 400*years400;
    if (days > 305)
        ++year;

    if (type == YEAR) {
        *(int *)out = year;
        return;
    }

    int leap = !years1 && (years4 || !years100);

    if (type == YDAY) {
        int yday = days + 31 + 28 + leap;
        if (yday >= YEARS1 + leap)
            yday -= YEARS1 + leap;
        *(int *)out = ++yday;
        return;
    }

    if (type == MONTH || type == YEARMON) {
        int i;
        if (days==0 && !leap && isLeapYear(year)) {
            i = 1;
        } else {
            i = 2;
            while (months[i-2] <= days) {
                days -= months[i-2];
                i++;
            }
        }
        if (i >= 12)
            i -= 12;

        if (type == MONTH) {
            *(int *)out = i + 1;
        } else {
            *(double *)out = year + i / 12.0;
        }
        return;
    }

    if (type == MDAY) {
        if (days==0 && !leap && isLeapYear(year)) {
            *(int *)out = 29;
            return;
        }
        int i = 0;
        while (months[i] <= days) {
            days -= months[i];
            i++;
        }
        *(int *)out = ++days;
        return;
    }

    if (type == QUARTER || type == YEARQTR) {
        int i = 0;
        while (quarter[i] <= days) {
            days -= quarter[i];
            i++;
        }
        if (i >= 4)
            i -= 4;
        if (type == QUARTER) {
            *(int *)out = i + 1;
        } else {
            *(double *)out = year + (i / 4.0);
        }
        return;
    }
}

SEXP convertDate(SEXP x, SEXP type)
{
    if (!isInteger(x)) error(_("x must be an integer vector"));
    const int *ix = INTEGER_RO(x);
    const int n = length(x);
    if (!isString(type) || length(type) != 1)
        internal_error(__func__, "invalid type for, should have been caught before"); // # nocov
    datetype ctype = 0;
    bool ansint = true;

    const char* ctype_str = CHAR(STRING_ELT(type, 0));
    if (!strcmp(ctype_str, "yday")) ctype = YDAY;
    else if (!strcmp(ctype_str, "wday")) ctype = WDAY;
    else if (!strcmp(ctype_str, "mday")) ctype = MDAY;
    else if (!strcmp(ctype_str, "week")) ctype = WEEK;
    else if (!strcmp(ctype_str, "month")) ctype = MONTH;
    else if (!strcmp(ctype_str, "quarter")) ctype = QUARTER;
    else if (!strcmp(ctype_str, "year")) ctype = YEAR;
    else if (!strcmp(ctype_str, "yearmon")) { ctype = YEARMON; ansint = false; }
    else if (!strcmp(ctype_str, "yearqtr")) { ctype = YEARQTR; ansint = false; }
    else internal_error(__func__, "invalid type, should have been caught before"); // # nocov
    
    if (ctype == WEEK) {
        SEXP ans = PROTECT(allocVector(INTSXP, n));
        int *ansp = INTEGER(ans);

        SEXP opt = GetOption1(install("datatable.week"));
        const char *mode = isString(opt) && length(opt) == 1 ? CHAR(STRING_ELT(opt, 0)) : "default";

        bool use_sequential = !strcmp(mode, "sequential");
        bool use_legacy = !strcmp(mode, "legacy");
        bool can_warn = !use_sequential && !use_legacy;

        for (int i = 0; i < n; i++) {
            if (ix[i] == NA_INTEGER) {
                ansp[i] = NA_INTEGER;
                continue;
            }
            int yday;
            convertSingleDate(ix[i], YDAY, &yday);
            int new_week = ((yday - 1) / 7) + 1;

            if (use_sequential) {
                ansp[i] = new_week;
            } else {
                int old_week = (yday / 7) + 1;
                ansp[i] = old_week;
                if (can_warn && new_week != old_week) {
                    warning(_("The default behavior of week() is changing. Previously ('legacy' mode), week numbers advanced every 7th day of the year. The new 'sequential' mode ensures the first week always has 7 days. For example, as.IDate('2023-01-07') returns week 2 in legacy mode but week 1 in sequential mode (week 2 starts on '2023-01-08'). To adopt the new behavior now, set options(datatable.week = 'sequential'). To keep the old results and silence this warning, set options(datatable.week = 'legacy'). See https://github.com/Rdatatable/data.table/issues/2611"));
                    can_warn = false;
                }
            }
        }
        UNPROTECT(1);
        return ans;
    }

    if (ansint) {
        SEXP ans = PROTECT(allocVector(INTSXP, n));
        int *ansp = INTEGER(ans);
        for (int i = 0; i < n; i++) {
            convertSingleDate(ix[i], ctype, &ansp[i]);
        }
        UNPROTECT(1);
        return ans;
    } else {
        SEXP ans = PROTECT(allocVector(REALSXP, n));
        double *ansp = REAL(ans);
        for (int i = 0; i < n; i++) {
            convertSingleDate(ix[i], ctype, &ansp[i]);
        }
        UNPROTECT(1);
        return ans;
    }
}
