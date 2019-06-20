#include<stdint.h>

typedef struct ans_t {
  int32_t *int_v;             // used in nafill
  double *dbl_v;              // used in froll, nafill
  int64_t *int64_v;           // not used yet
  uint8_t status;             // 0:ok, 1:message, 2:warning, 3:error; unix return signal: {0,1,2}=0, {3}=1
  char message[4][4096];      // STDOUT: output, STDERR: message, warning, error
// implicit n_message limit discussed here: https://github.com/Rdatatable/data.table/issues/3423#issuecomment-487722586
} ans_t;
