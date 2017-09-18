
typedef void (*writer_fun_t)(void *, int64_t, char **);

writer_fun_t writeBool8;
writer_fun_t writeBool32;
writer_fun_t writeBool32AsString;
writer_fun_t writeInt32;
writer_fun_t writeInt64;
writer_fun_t writeFloat64;
writer_fun_t writeITime;
writer_fun_t writeDateInt32;
writer_fun_t writeDateFloat64;
writer_fun_t writePOSIXct;
writer_fun_t writeNanotime;
writer_fun_t writeString;
writer_fun_t writeCategString;
//writer_fun_t writeList123;

extern void write_chars(const char *source, char **dest);

void fwriteMain();

