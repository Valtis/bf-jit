#include "parser.h"
#include "interpreter.h"
#include "compiler.h"
#include <cstdio>
#include <cstring>



int main(int argc, const char **argv) {
    if (argc == 1) {
        std::fprintf(stderr, "Give the BF file as command line argument\n");
        return 1;
    }

    auto opcodes = parse(argv[1]);


    if (argc == 2 ) {
       interpret(opcodes);
    } else if (argc >= 2 && !strcmp(argv[2], "fast")) {
        Code code{opcodes};
        code.compile();
        code.run();
    } else if (argc >= 2 && !strcmp(argv[2], "dump")) {
        Code code{opcodes};
        code.compile();
        code.print_memory();
    }
}