Brainfuck interpreter and jit-compiler  
=======================================

Executes bf-files either by interpreting or jit-compiling the file.

Jit-compiler assumes x86-64 and sysvabi 64-bit calling convention (Linux, basically).

Usage: ./bf file (fast|dump)

Note that the arguments must be given exactly in this order.

File points to the bf-file. 
By default, the file is executed by the interpreter. To enable jit, use 'fast' argument.
To dump the jitted memory, use 'dump'.

License applies to source/header files and the Makefile only. The bf source-files are copyrighted/licensed by their respective owners

