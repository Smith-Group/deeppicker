#ifndef DEEP_OUTPUT_H
#define DEEP_OUTPUT_H

#include <cstdarg>
#include <iosfwd>

namespace deep_output {

using c_error_writer_t = void (*)(const char *message);

std::ostream &out();
std::ostream &err();

void set_out_stream(std::ostream *stream);
void set_err_stream(std::ostream *stream);
std::ostream *out_stream();
std::ostream *err_stream();
void reset_streams();

void set_c_error_writer(c_error_writer_t writer);
c_error_writer_t c_error_writer();
void reset_c_error_writer();

void set_verbose(bool verbose);
bool verbose();

void error_vprintf(const char *format, va_list args);
void error_printf(const char *format, ...);

} // namespace deep_output

extern "C" void deep_output_error_printf(const char *format, ...);

#define DEEP_OUT deep_output::out()
#define DEEP_ERR deep_output::err()

#endif
