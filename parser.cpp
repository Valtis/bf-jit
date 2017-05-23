#include "parser.h"
#include <cstdio>
#include <cstdlib>
#include <stack>

std::vector<Opcode> parse(const char *fname) {
    std::FILE *handle = std::fopen(fname, "r");

    if (handle == nullptr) {
        fprintf(stderr, "Failed to open file %s\n", fname);
        exit(1);
    }

    int val;

    int successive_incs = 0;
    int successive_decs = 0;

    int successive_lefts = 0;
    int successive_rights = 0;

    bool terminate = false;

    std::vector<Opcode> opcodes;
    std::stack<size_t> patch_location;


    while ((val = fgetc(handle)) != EOF) {
        char c = (char)val;

        if (c != '+' && successive_incs != 0) {
            if (successive_incs == 1) {
                opcodes.push_back(Opcode::INC);
            } else {
                opcodes.push_back(Opcode::ADD);
                opcodes.push_back((Opcode)successive_incs);
            }
            successive_incs = 0;

        }

        if (c != '-' && successive_decs != 0) {
            if (successive_decs == 1) {
                opcodes.push_back(Opcode::DEC);
            } else {
                opcodes.push_back(Opcode::SUB);
                opcodes.push_back((Opcode)successive_decs);
            }

            successive_decs = 0;
        }

        if (c != '>' && successive_rights != 0) {
            if (successive_rights == 1) {
                opcodes.push_back(Opcode::MOVE_RIGHT_ONE);
            } else {
                opcodes.push_back(Opcode::MOVE_RIGHT);
                opcodes.push_back((Opcode)successive_rights);
            }

            successive_rights = 0;
        }

        if (c != '<' && successive_lefts != 0) {
            if (successive_lefts == 1) {
                opcodes.push_back(Opcode::MOVE_LEFT_ONE);
            } else {
                opcodes.push_back(Opcode::MOVE_LEFT);
                opcodes.push_back((Opcode)successive_lefts);
            }

            successive_lefts = 0;
        }


        if (terminate) {
            break;
        }

        switch (c) {

            case '>':
                ++successive_rights;
               // opcodes.push_back(Opcode::RIGHT);
                break;

            case '<':
                ++successive_lefts;
                //opcodes.push_back(Opcode::LEFT);
                break;

            case '+':
                ++successive_incs;
                break;

            case '-':
                ++successive_decs;
                break;

            case '.':
                opcodes.push_back(Opcode::PRINT);
                break;

            case ',':
                opcodes.push_back(Opcode::INPUT);
                break;

            case '[':

                opcodes.push_back(Opcode::JUMP_IF_FALSE);
                patch_location.push(opcodes.size());
                opcodes.push_back(Opcode::NOP);

                break;

            case ']':
                if (patch_location.size() == 0) {
                    fclose(handle);
                    fprintf(stderr, "] without matching [ found");
                    exit(1);
                }
                // optimization: Replace [-] with zeroing the cell
                if (opcodes.size() >= 3 &&
                    opcodes[opcodes.size() - 1 ] == Opcode::DEC &&
                    // intentional: opcodes.size() - 2 contains the jump target
                    opcodes[opcodes.size() - 3 ] == Opcode::JUMP_IF_FALSE) {

                    opcodes.pop_back();
                    opcodes.pop_back();
                    opcodes.pop_back();
                    patch_location.pop();
                    opcodes.push_back(Opcode::ZERO_CELL);

                } else {
                    size_t location = patch_location.top();
                    patch_location.pop();

                    opcodes.push_back(Opcode::JUMP_IF_TRUE);
                    opcodes.push_back((Opcode)(location + 1));
                    opcodes[location] = (Opcode)(uint32_t)opcodes.size();
                }
                break;

            case '!':
                terminate = true;
                break;
            default:
               continue;
        }

    }

    std::fclose(handle);
    return opcodes;
}