#include<stdint.h>

typedef struct double_ans_t {
  double *ans;
  uint8_t status;   // 0:ok, 1:message, 2:warning, 3:error; unix return signal: {0,1,2}=0, {3}=1
  char message[4][256]; // STDOUT: output, STDERR: message, warning, error
} double_ans_t;
