#include "data.table.h"

#define YEARS400 146097
#define YEARS100 36524
#define YEARS4 1461
#define YEARS1 365

typedef enum { YDAY, WDAY, MDAY, WEEK, MONTH, QUARTER, YEAR, YEARMON, YEARQTR} datetype;

int convertDate_int(int x, datetype type);
double convertDate_double(int x, datetype type);

SEXP convertDate(SEXP x, SEXP type)
{
    if (!isInteger(x)) error(_("x must be an integer vector"));
    const int *ix = INTEGER(x);
    const int n = length(x);
    if (!isString(type) || length(type) != 1)
        error(_("Internal error: invalid type for convertDate(), should have been caught before. please report to data.table issue tracker")); // # nocov
    datetype ctype;
    bool ansint = true;
    if (!strcmp(CHAR(STRING_ELT(type, 0)), "yday")) ctype = YDAY;
    else if (!strcmp(CHAR(STRING_ELT(type, 0)), "wday")) ctype = WDAY;
    else if (!strcmp(CHAR(STRING_ELT(type, 0)), "mday")) ctype = MDAY;
    else if (!strcmp(CHAR(STRING_ELT(type, 0)), "week")) ctype = WEEK;
    else if (!strcmp(CHAR(STRING_ELT(type, 0)), "month")) ctype = MONTH;
    else if (!strcmp(CHAR(STRING_ELT(type, 0)), "quarter")) ctype = QUARTER;
    else if (!strcmp(CHAR(STRING_ELT(type, 0)), "year")) ctype = YEAR;
    else if (!strcmp(CHAR(STRING_ELT(type, 0)), "yearmon")) { ctype = YEARMON; ansint = false; }
    else if (!strcmp(CHAR(STRING_ELT(type, 0)), "yearqtr")) { ctype = YEARQTR; ansint = false; }
    else error(_("Internal error: invalid type for convertDate, should have been caught before. please report to data.table issue tracker")); // # nocov

    SEXP ans;
    if (ansint) {
        ans = PROTECT(allocVector(INTSXP, n));
        for (int i=0; i < n; ++i) {
            INTEGER(ans)[i] = convertDate_int(ix[i], ctype);
        }
    } else {
        ans = PROTECT(allocVector(REALSXP, n));
        for (int i=0; i < n; ++i) {
            REAL(ans)[i] = convertDate_double(ix[i], ctype);
        }
    }
    UNPROTECT(1);
    return ans;
}

int convertDate_int(int x, datetype type)
{
    static const char months[] = {31, 30, 31, 30, 31, 31, 30, 31, 30, 31, 31, 29};
    static const int quarter[] = {31, 91, 92, 92, 60};

    if (type == WDAY) {
        int wday = (x + 4) % 7;
        if (wday < 0) wday += 7;
        return wday+1;
    }

    if (type == WEEK) {
        return (convertDate_int(x, YDAY) / 7) + 1;
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

    if (type == YEAR) {
        return year + (days > 305);
    }

    int leap = !years1 && (years4 || !years100);

    if (type == YDAY) {
        int yday = days + 31 + 28 + leap;
        if (yday >= YEARS1 + leap)
            yday -= YEARS1 + leap;
        yday++;
        return yday;
    }

    if (type == MONTH) {
        int i;
        if (days==0 && !leap && years1%4==0) {
            i = 11;
        } else {
            i = 0;
            while (months[i] <= days) {
                days -= months[i];
                i++;
            }
        }
        return (i+2) % 12 + 1;
    }

    if (type == MDAY) {
        if (days==0 && !leap && years1%4==0) return 29;
        int i = 0;
        while (months[i] <= days) {
            days -= months[i];
            i++;
        }
        return days+1;
    }

    if (type == QUARTER) {
        int i = 0;
        while (quarter[i] <= days) {
            days -= quarter[i];
            i++;
        }
        return i % 4 + 1;
    }
}

double convertDate_double(int x, datetype type)
{
    static const char months[] = {31, 30, 31, 30, 31, 31, 30, 31, 30, 31, 31, 29};
    static const int quarter[] = {31, 91, 92, 92, 60};

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
    if (days > 305) ++year;

    int leap = !years1 && (years4 || !years100);

    if (type == YEARMON) {
        int i;
        if (days==0 && !leap && years1%4==0) {
            i = 11;
        } else {
            i = 0;
            while (months[i] <= days) {
                days -= months[i];
                i++;
            }
        }
        return year + ((i+2) % 12) / 12.0;
    }

    if (type == YEARQTR) {
        int i = 0;
        while (quarter[i] <= days) {
            days -= quarter[i];
            i++;
        }
        return year + ((i % 4) / 4.0);
    }
}