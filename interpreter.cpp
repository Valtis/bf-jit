#include "interpreter.h"
#include "tape_size.h"
#include <cstdio>
#include <cstdlib>

void interpret(std::vector<Opcode> opcodes) {
    const int32_t tape_len = TAPE_SIZE;

    std::vector<uint8_t> tape;
    tape.resize(tape_len);

    for (int32_t i = 0; i < tape_len; ++i) {
        tape[i] = 0;
    }

    opcodes.push_back(Opcode::STOP);

    int32_t tape_pos = 0;
    uint32_t pc = 0;

    static void *opcode_array[] = {
        &&OPCODE_NOP,
        &&OPCODE_MOVE_LEFT_ONE,
        &&OPCODE_MOVE_RIGHT_ONE,
        &&OPCODE_MOVE_LEFT,
        &&OPCODE_MOVE_RIGHT,
        &&OPCODE_INC,
        &&OPCODE_DEC,
        &&OPCODE_ADD,
        &&OPCODE_SUB,
        &&OPCODE_ZERO_CELL,
        &&OPCODE_PRINT,
        &&OPCODE_INPUT,
        &&OPCODE_JUMP_IF_FALSE,
        &&OPCODE_JUMP_IF_TRUE,
        &&OPCODE_STOP };

    #define DISPATCH() do { goto *opcode_array[(uint32_t)opcodes[pc++]]; } while (false)

    DISPATCH();

    OPCODE_NOP:
        DISPATCH();

    OPCODE_MOVE_LEFT_ONE:
        --tape_pos;
        DISPATCH();

    OPCODE_MOVE_RIGHT_ONE:
        ++tape_pos;
        DISPATCH();

    OPCODE_MOVE_LEFT:
        tape_pos -= (uint32_t)opcodes[pc++];
        DISPATCH();

    OPCODE_MOVE_RIGHT:
        tape_pos += (uint32_t)opcodes[pc++];
        DISPATCH();

    OPCODE_INC:
        ++tape[tape_pos];
        DISPATCH();

    OPCODE_DEC:
        --tape[tape_pos];
        DISPATCH();

   OPCODE_ADD:
        tape[tape_pos] += (uint32_t)opcodes[pc++];
        DISPATCH();

    OPCODE_SUB:
        tape[tape_pos] -= (uint32_t)opcodes[pc++];
        DISPATCH();

    OPCODE_ZERO_CELL:
        tape[tape_pos] = 0;
        DISPATCH();

    OPCODE_PRINT:
        std::putc(tape[tape_pos], stdout);
        DISPATCH();

    OPCODE_INPUT:
        tape[tape_pos] = (uint8_t)getchar();
        // flush any extra bytes + newline from the stream
      /*  if (tape[tape_pos] == '\n') {
            DISPATCH();
        }
        // if the initial character wasn't a newline,
        //  keep reading until we see one
        while (getchar() != '\n');*/
        DISPATCH();

    OPCODE_JUMP_IF_FALSE:
        if (tape[tape_pos] == 0) {
            pc = (uint32_t)opcodes[pc];
        } else {
            ++pc;
        }
        DISPATCH();

    OPCODE_JUMP_IF_TRUE:
        if (tape[tape_pos] != 0) {
            pc = (uint32_t)opcodes[pc];
        } else {
            ++pc;
        }
        DISPATCH();

    OPCODE_STOP:
        return;

}



void print_opcodes(const std::vector<Opcode> &opcodes) {

    for (uint32_t i = 0; i < opcodes.size(); ++i) {
        auto opcode = opcodes[i];
        printf("%04d ", i);
        switch (opcode) {

            case Opcode::NOP:
                printf("NOP\n");
                break;

            case Opcode::MOVE_RIGHT_ONE:
                printf("MOVE RIGHT ONE STEP\n");
                break;

            case Opcode::MOVE_LEFT_ONE:
                printf("MOVE LEFT ONE STEP\n");
                break;
            case Opcode::MOVE_LEFT:
                printf("MOVE LEFT %d STEPS\n", (uint32_t)opcodes[++i]);
                break;
            case Opcode::MOVE_RIGHT:
                printf("MOVE RIGHT %d STEPS\n", (uint32_t)opcodes[++i]);
                break;
            case Opcode::ZERO_CELL:
                printf("ZERO CELL\n");
                break;
            case Opcode::INC:
                printf("INC\n");
                break;
            case Opcode::DEC:
                printf("DEC\n");
                break;
            case Opcode::ADD:
                printf("ADD %d\n", (uint32_t)opcodes[++i]);
                break;
            case Opcode::SUB:
                printf("SUB %d\n", (uint32_t)opcodes[++i]);
                break;
            case Opcode::PRINT:
                printf("PRINT\n");
                break;
            case Opcode::INPUT:
                printf("INPUT\n");
                break;
            case Opcode::JUMP_IF_FALSE:
                printf("JUMP IF FALSE %d\n", (int32_t)opcodes[++i]);
                break;
          case Opcode::JUMP_IF_TRUE:
                printf("JUMP IF TRUE %d\n", (int32_t)opcodes[++i]);
                break;
            default:
                printf("Invalid opcode %d\n", (int)opcode);
        }
    }
}