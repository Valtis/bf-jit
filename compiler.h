#pragma once
#include "opcode.h"
#include "tape_size.h"
#include <vector>
#include <stack>
#include <cstddef>

#define EXT_REGS 0b0000'1000

// only 64 bit registers used, extended regs not used
enum class Register : uint32_t {
    RAX, RCX, RDX, RBX, RSP, RBP, RSI, RDI
};

enum class Register_8bit : uint32_t {
    AL, CL, DL, BL, AH, CH, DH, BH,
    SPL = 0b1100, BPL, SIL, DIL // use only low 3 bits when encoding
};

class Code {
public:
    Code(std::vector<Opcode> opcodes);
    ~Code();

    void compile();
    void print_memory();
    void run();

private:
    std::vector<uint8_t> compile_opcodes();




    void emit_move_tape_left_one_step(std::vector<uint8_t> &code);
    void emit_move_tape_right_one_step(std::vector<uint8_t> &code);
    void emit_move_tape_left(uint32_t steps, std::vector<uint8_t> &code);
    void emit_move_tape_right(uint32_t steps, std::vector<uint8_t> &code);
    void emit_inc_cell(std::vector<uint8_t> &code);
    void emit_dec_cell(std::vector<uint8_t> &code);
    void emit_add_to_cell(const uint32_t val, std::vector<uint8_t> &code);
    void emit_sub_from_cell(const uint32_t val, std::vector<uint8_t> &code);
    void emit_jump_if_false(
        std::stack<uint32_t> &jump_placeholders,
        std::vector<uint8_t> &code);
    void emit_jump_if_true(
        std::stack<uint32_t> &jump_placeholders,
        std::vector<uint8_t> &code);
    void emit_zero_cell(std::vector<uint8_t> &code);
    void emit_print(std::vector<uint8_t> &code);
    void emit_read(std::vector<uint8_t> &code);

    void emit_prologue(std::vector<uint8_t> &code);
    void emit_epilogue(std::vector<uint8_t> &code);


    uint8_t get_rex_prefix(
        const bool operand_size_64_bit,
        const bool modrm_reg_extension,
        const bool sib_index_extension,
        const bool rm_or_sib_extension);

    uint8_t get_sib_byte(
        uint8_t scale,
        Register index,
        Register base);

    void emit_reg_push(const Register reg, std::vector<uint8_t> &code);
    void emit_reg_pop(const Register reg, std::vector<uint8_t> &code);

    void emit_lea(
        const Register dest,
        const Register base,
        const Register add,
        std::vector<uint8_t> &code);

    void emit_mov_reg_direct(
        const Register dest,
        const size_t value,
        std::vector<uint8_t> &code);

    void emit_mov_immediate_byte_ptr(
        const Register base,
        const Register index,
        const uint8_t val,
        std::vector<uint8_t> &code);

    void emit_mov_byte_ptr_reg(
        const Register base,
        const Register index,
        const Register_8bit dest,
        std::vector<uint8_t> &code);

    void emit_mov_reg_byte_ptr(
        const Register base,
        const Register index,
        const Register_8bit src,
        std::vector<uint8_t> &code);

    void emit_inc_byte_ptr(
        const Register base,
        const Register index,
        std::vector<uint8_t> &code);

    void emit_dec_byte_ptr(
        const Register base,
        const Register index,
        std::vector<uint8_t> &code);

    void emit_add_immediate_byte_ptr(
        const Register base,
        const Register index,
        const uint8_t val,
        std::vector<uint8_t> &code);

    void emit_add_immediate_reg(
        const Register destination,
        const uint32_t val,
        std::vector<uint8_t> &code);

    void emit_sub_immediate_byte_ptr(
        const Register base,
        const Register index,
        const uint8_t val,
        std::vector<uint8_t> &code);

    void emit_sub_immediate_reg(
        const Register destination,
        const uint32_t val,
        std::vector<uint8_t> &code);

    void emit_inc_register(
        const Register reg,
        std::vector<uint8_t> &code);

    void emit_dec_register(
        const Register reg,
        std::vector<uint8_t> &code);

    void emit_cmp_immediate_byte_ptr(
        const Register base,
        const Register index,
        const uint8_t val,
        std::vector<uint8_t> &code);

    void emit_cmp_immediate_reg(
        const Register reg,
        const uint8_t val,
        std::vector<uint8_t> &code);

    void emit_jump_zero(
        const uint32_t offset,
        std::vector<uint8_t> &code);

    void emit_jump_not_zero(
        const uint32_t offset,
        std::vector<uint8_t> &code);

    void emit_jump(
        const uint32_t offset,
        std::vector<uint8_t> &code);

    void emit_indirect_function_call(
        const Register reg,
        std::vector<uint8_t> &code);

    void update_false_branch_jump_target(
        std::stack<uint32_t> jump_placeholders,
        std::vector<uint8_t> &code);

    uint32_t calculate_jump_target_for_true_branch(
        std::stack<uint32_t> jump_placeholders,
        const std::vector<uint8_t> &code);

    uint32_t calculate_jump_target_for_false_branch(
        std::stack<uint32_t> jump_placeholders,
        const std::vector<uint8_t> &code);

    void create_executable_section(const std::vector<uint8_t> &code);
    std::vector<Opcode> m_opcodes;
    uint8_t *m_tape;
    void *m_code;

    uint32_t m_code_size;
};

