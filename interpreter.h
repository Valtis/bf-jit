#pragma once
#include "opcode.h"
#include <vector>


void interpret(std::vector<Opcode> opcodes);
void print_opcodes(const std::vector<Opcode> &opcodes);