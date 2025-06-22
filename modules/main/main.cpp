module;

#include <iostream>

#include <libassert/assert.hpp>

export module Atem.Main;

export auto main() -> int {
    std::cout << "Hello, World!" << std::endl;
    DEBUG_ASSERT(false);
    return 0;
}
