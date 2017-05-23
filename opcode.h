#pragma once
#include <cstdint>
enum class Opcode : uint32_t {
    NOP, MOVE_LEFT_ONE, MOVE_RIGHT_ONE, MOVE_LEFT, MOVE_RIGHT, INC, DEC, ADD,
    SUB, ZERO_CELL, PRINT, INPUT, JUMP_IF_FALSE, JUMP_IF_TRUE, STOP
};