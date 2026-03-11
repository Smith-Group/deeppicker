#include "deep_output.h"

#include <cstring>
#include <ostream>
#include <streambuf>
#include <string>
#include <unistd.h>

namespace {

class DeepFdBuffer : public std::streambuf {
public:
    explicit DeepFdBuffer(int fd) : fd_(fd) {}

protected:
    int overflow(int c) override
    {
        if (c != traits_type::eof()) {
            char ch = static_cast<char>(c);
            if (::write(fd_, &ch, 1) != 1) {
                return traits_type::eof();
            }
        }
        return c;
    }

    std::streamsize xsputn(const char *s, std::streamsize n) override
    {
        const ssize_t written = ::write(fd_, s, static_cast<size_t>(n));
        return written < 0 ? 0 : written;
    }

private:
    int fd_;
};

class DeepFdStream : public std::ostream {
public:
    explicit DeepFdStream(int fd) : std::ostream(&buffer_), buffer_(fd) {}

private:
    DeepFdBuffer buffer_;
};

class DeepNullBuffer : public std::streambuf {
public:
    int overflow(int c) override { return c; }
};

class DeepNullStream : public std::ostream {
public:
    DeepNullStream() : std::ostream(&buffer_) {}

private:
    DeepNullBuffer buffer_;
};

void default_c_error_writer(const char *message)
{
    ::write(2, message, std::strlen(message));
}

bool g_verbose = true;
DeepFdStream g_default_out(1);
DeepFdStream g_default_err(2);
DeepNullStream g_null_stream;
std::ostream *g_out_stream = nullptr;
std::ostream *g_err_stream = nullptr;
deep_output::c_error_writer_t g_c_error_writer = default_c_error_writer;

} // namespace

namespace deep_output {

std::ostream &out()
{
    if (!g_verbose) {
        return g_null_stream;
    }

    return g_out_stream == nullptr ? static_cast<std::ostream &>(g_default_out) : *g_out_stream;
}

std::ostream &err()
{
    if (!g_verbose) {
        return g_null_stream;
    }

    return g_err_stream == nullptr ? static_cast<std::ostream &>(g_default_err) : *g_err_stream;
}

void set_out_stream(std::ostream *stream)
{
    g_out_stream = stream;
}

void set_err_stream(std::ostream *stream)
{
    g_err_stream = stream;
}

std::ostream *out_stream()
{
    return g_out_stream;
}

std::ostream *err_stream()
{
    return g_err_stream;
}

void reset_streams()
{
    g_out_stream = nullptr;
    g_err_stream = nullptr;
}

void set_c_error_writer(c_error_writer_t writer)
{
    g_c_error_writer = writer == nullptr ? default_c_error_writer : writer;
}

c_error_writer_t c_error_writer()
{
    return g_c_error_writer;
}

void reset_c_error_writer()
{
    g_c_error_writer = default_c_error_writer;
}

void set_verbose(bool verbose)
{
    g_verbose = verbose;
}

bool verbose()
{
    return g_verbose;
}

void error_vprintf(const char *format, va_list args)
{
    if (!g_verbose) {
        return;
    }

    va_list args_copy;
    va_copy(args_copy, args);
    const int needed = std::vsnprintf(nullptr, 0, format, args_copy);
    va_end(args_copy);

    if (needed < 0) {
        return;
    }

    std::string buffer(static_cast<size_t>(needed) + 1, '\0');
    std::vsnprintf(buffer.data(), buffer.size(), format, args);

    g_c_error_writer(buffer.c_str());
}

void error_printf(const char *format, ...)
{
    va_list args;
    va_start(args, format);
    error_vprintf(format, args);
    va_end(args);
}

} // namespace deep_output

extern "C" void deep_output_error_printf(const char *format, ...)
{
    va_list args;
    va_start(args, format);
    deep_output::error_vprintf(format, args);
    va_end(args);
}
