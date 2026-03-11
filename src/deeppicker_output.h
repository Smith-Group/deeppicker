#ifndef DEEPPICKER_OUTPUT_H
#define DEEPPICKER_OUTPUT_H

#include "deep/deep_output.h"

class DeepOutputScope
{
public:
    explicit DeepOutputScope(bool verbose);
    ~DeepOutputScope();

private:
    bool old_verbose_;
    std::ostream *old_out_stream_;
    std::ostream *old_err_stream_;
    deep_output::c_error_writer_t old_c_error_writer_;
};

#endif
