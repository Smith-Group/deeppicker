#ifndef DEEP_OUTPUT_C_H
#define DEEP_OUTPUT_C_H

#ifdef __cplusplus
extern "C" {
#endif

void deep_output_error_printf(const char *format, ...);

#ifdef __cplusplus
}
#endif

#define DEEP_FPRINTF_STDERR(...) deep_output_error_printf(__VA_ARGS__)

#endif
