#include "deeppicker_output.h"

#include <R_ext/Print.h>

#include <ostream>
#include <streambuf>
#include <string>

namespace {

class RConsoleBuffer : public std::streambuf {
public:
    using writer_t = void (*)(const char *, ...);

    explicit RConsoleBuffer(writer_t writer) : writer_(writer) {}

protected:
    int overflow(int c) override
    {
        if (c != traits_type::eof()) {
            buffer_.push_back(static_cast<char>(c));
            if (c == '\n') {
                flush_buffer();
            }
        }
        return c;
    }

    int sync() override
    {
        flush_buffer();
        return 0;
    }

private:
    void flush_buffer()
    {
        if (!buffer_.empty()) {
            writer_("%s", buffer_.c_str());
            buffer_.clear();
        }
    }

    writer_t writer_;
    std::string buffer_;
};

class RConsoleStream : public std::ostream {
public:
    explicit RConsoleStream(RConsoleBuffer::writer_t writer) : std::ostream(&buffer_), buffer_(writer) {}

private:
    RConsoleBuffer buffer_;
};

void r_error_writer(const char *message)
{
    REprintf("%s", message);
}

RConsoleStream g_r_out(Rprintf);
RConsoleStream g_r_err(REprintf);

} // namespace

DeepOutputScope::DeepOutputScope(bool verbose)
    : old_verbose_(deep_output::verbose()),
      old_out_stream_(deep_output::out_stream()),
      old_err_stream_(deep_output::err_stream()),
      old_c_error_writer_(deep_output::c_error_writer())
{
    deep_output::set_out_stream(&g_r_out);
    deep_output::set_err_stream(&g_r_err);
    deep_output::set_c_error_writer(r_error_writer);
    deep_output::set_verbose(verbose);
}

DeepOutputScope::~DeepOutputScope()
{
    deep_output::set_out_stream(old_out_stream_);
    deep_output::set_err_stream(old_err_stream_);
    deep_output::set_c_error_writer(old_c_error_writer_);
    deep_output::set_verbose(old_verbose_);
}
