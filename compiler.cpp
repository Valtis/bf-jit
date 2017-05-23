#include "compiler.h"
#include <cstdlib>
#include <sys/mman.h>
#include <cstdio>

typedef void (*jitFunction)(void);

#define WORK_REGISTER Register::RBX

// base and index are stored in separate registers, as this would allow
// easy bounds checking (although not implemented right now)
#define TAPE_BASE_REGISTER Register::RCX
#define TAPE_INDEX_REGISTER Register::RDX

#define REGISTER_INDIRECT_MODE 0b0000'0000
#define ONE_BYTE_DISPLACEMENT_MODE 0b0100'0000
#define FOUR_BYTE_DISPLACEMENT_MODE 0b1000'0000
#define REGISTER_DIRECT_MODE 0b1100'0000

#define REGISTER_INDIRECT_SIB_ONLY 0b100
#define REGISTER_INDIRECT_DISPLACEMENT_ONLY 0b101
/*

    x86 instruction encoding for dummies:

    General instruction form, consisting of 1 - 14 bytes:
    <prefixes> <opcode> <modrm_byte> <sib_byte> <immediate operand>

    only rex-prefix used here, other prefixes do exist though.

    rex_prefix: 0b0100'wrxb:
        w = if 1, 64 bit operand size. Otherwise instruction default size
        r = extends modrm.reg-field (yyy-bits below in modrm_byte)
        x = extends sib.index-field (yyy-bits below in sib_byte)
        z = extends modrm.rm-field or sib.base-field (zzz-bits below in both)

        extensions extend the 3-bit field into 4 bits (8 -> 16 values),
        allowing the usage of the new x86-64 registers (R8 - R15)

        Rules exist on when (not) to use this, but are out of scope
        for this little note. Generally if you want to use the extensions, you
        need this byte. Some instructions seem to require it even if wrxb are
        all zero.

    opcode:
        instruction opcode. Can be found from e.g. Intel manuals

    modrm_byte: 0bxxyy'yzzz

        xx selects mode:
            00 = register indirect addressing, sib with no displacement when
                zzz-bits are 100, or displacement only addressing when
                zzz-bits are 101
            01 = one-byte signed displacement following mod-rm, sib
            10 = four-byte signed displacement following mod-rm, sib
            11 = direct register addressing mode

            indirect is basically pointer access (register/result of sib-byte
            calculations contains address to the actual value), direct uses
            values directly in the register

        yyy, reg field, is either a register operand or opcode extension.
            Typically contains destination register, although this may vary
            depending on instruction
        zzz, rm-field, typically source register (direct or indirect addressing)
            or may contain additional mode selection information.
            Again, depends on instruction

        instruction opcode may have direction-bit (d-bit) which decides whether
        yyy or zzz is the destination register

    sib_byte: 0bxxyy'yzzz
        (sib = scale, index, base)

        generally encodes indirect addressing in the following way:
            [base + (index*2^scale) + displacement]
        where displacement byte(s) may or may not follow sib, depending on
        the selected mode in modrm-byte.

        This byte may or may not be present, depending on the mode selected by
        the modrm_byte. Displacement may or may not follow this byte, depending
        again on the selected addressing mode.

        this is useful for pointer arithmetics: e.g.

            int a[10];
            a[4] = 2;

            The address calculation a[4] can be encoded in the sib byte
            [<base address of a> + <index=4>*2^<scale=2> + 0]
            where both base value and the index value 4 will be stored in
            registers. Scale is used to encode the operand size, which in this
            case is 4 bytes (assuming 4 byte integer), so scale will have value
            of 2 (2^2 -> 4).
            Effectively encodes base + 4*sizeof(int) in this case

        xx = scale bits, used to encode operand size, 2^scale
            allows following operand sizes:
            00 = 2^0 = 1
            01 = 2^1 = 2
            10 = 2^2 = 4
            11 = 2^3 = 8
            (internally does a bit shift to achieve this)

        yyy = encodes register holding the index value
        zzz = encodes register holding the base value.


        Note: Due to 0b100 (also encoding for RSP register) and 0b101 (also
            encoding for RBP register) being used as mode encoding in
            indirect modrm-byte (zzz-values, see above), this complicates
            instruction encoding somewhat.

            That is, for example
                mov BYTE PTR [rdx], 5 (move 5 into byte-sized value pointed by
                                       rdx, [] indicates indirect/pointer access)
            can be encoded as
                0xc6 0x00 0x05
            where opcode is 0xc6 (0b1100'0110), mod-rm is 0x02 (0b0000'0010) and
            0x05 is the immediate value.

            mod-rm is 0b|00|000|010, where first 00 selects indirect access,
            second 000 is opcode extension (all zeros in this case), and the
            third 010 encodes the destination register (RDX)
            Note that SIB-byte is not used, as we do not have index value, only
            what effectively would be a base register in the SIB encoding.

            For similar instruction
                mov BYTE PTR [rsp], 5
            we get encoding
                0xc6 0x04 0x24 0x05
            where 0xc6 is again the opcode, 0x04 is the modrm-byte (0b0000'0100),
            0x24 is a sib-byte (0b0010'0100,  was not used in previous example)
            and 0x05 is again the immediate value

            Mod-rm-byte is similar to above, where first two zeros selects the
            addressing mode and next three zeros are opcode extension. The last
            three bits are 0b100, which is the RSP encoding. The architecture
            designers however have decided to use this value to encode the
            fact that we have a sib byte, but no displacement value.

            This means that even though we have no index-register, so in theory
            sib byte is not needed, we still need to use the sib-byte.
            This byte is 0x24, 0b|00|100|100. First 00 selects the scale,
            which is 1. Next 100 encodes the index-register, which in theory
            should be RSP. However, again, the 100 is a special value, which
            means that the field is actually unused. This means RSP can't be
            used as an index register, as its encoding is again used for
            something else. The final 100 encodes the base register, which is
            actually the RSP, no special meaning this time.

            Similar encoding happens when rbp is used without index. RBP is
            valid index register however, unlike rsp.

            x86 encoding is fun, isn't it.
            (unrelated fun fact: mov-instruction is turing complete)

    immediate_operand: Holds immediate operand (if any) used by the instruction.


    Register encoding (64 bit register names used here):
        when rex-prefix extension bit is 0:

            000 RAX
            001 RCX
            010 RDX
            011 RBX
            100 RSP
            101 RBP
            110 RSI
            111 RDI
        when rex-prefix extension bit is 1:
            000 R8
            001 R9
            010 R10
            011 R11
            100 R12
            101 R13
            110 R14
            111 R15

*/

Code::Code(std::vector<Opcode> opcodes) :
    m_opcodes{opcodes},
    m_tape{nullptr},
    m_code{nullptr},
    m_code_size{0} {

    m_tape = (uint8_t *)malloc(TAPE_SIZE);
    if (m_tape == nullptr) {
        fprintf(stderr, "Failed to allocate memory for the tape\n");
        exit(1);
    }

    for (int i = 0; i < TAPE_SIZE; ++i) {
        m_tape[i] = 0;
    }
}

Code::~Code() {
    if (m_code != nullptr) {
        munmap(m_code, m_code_size);
    }

    free(m_tape);
}

void Code::print_memory() {
    if (m_code == nullptr) {
        printf("No code has been compiled\n");
    } else {
        uint8_t *ptr = (uint8_t *)m_code;
        for (size_t i = 0; i < m_code_size; ++i) {
            printf("%02x", *ptr++);
        }
        printf("\n");
    }
}

void Code::compile() {
    auto code = compile_opcodes();
    create_executable_section(code);
}

std::vector<uint8_t> Code::compile_opcodes() {

    std::vector<uint8_t> code;
    code.reserve(m_opcodes.size()*2);
    std::stack<uint32_t> jump_placeholders;
    emit_prologue(code);

    for (size_t i = 0; i < m_opcodes.size(); ++i) {
        const auto opcode = m_opcodes[i];

        switch (opcode) {

            case Opcode::MOVE_LEFT_ONE:
                emit_move_tape_left_one_step(code);
                break;

            case Opcode::MOVE_RIGHT_ONE:
                emit_move_tape_right_one_step(code);
                break;

            case Opcode::MOVE_LEFT:
                emit_move_tape_left((uint32_t)m_opcodes[++i], code);
                break;

            case Opcode::MOVE_RIGHT:
                emit_move_tape_right((uint32_t)m_opcodes[++i], code);
                break;

            case Opcode::ZERO_CELL:
                emit_zero_cell(code);
                break;

            case Opcode::INC:
                emit_inc_cell(code);
                break;

            case Opcode::DEC:
                emit_dec_cell(code);
                break;
            case Opcode::ADD:
                emit_add_to_cell((uint32_t)m_opcodes[++i], code);
                break;
            case Opcode::SUB:
                emit_sub_from_cell((uint32_t)m_opcodes[++i], code);
                break;

            case Opcode::JUMP_IF_FALSE:
                ++i; // discard next value, not used in compiled mode
                emit_jump_if_false(jump_placeholders, code);
                break;

            case Opcode::JUMP_IF_TRUE:
                ++i; // discard next value, not used in compiled mode
                emit_jump_if_true(jump_placeholders, code);
                break;

            case Opcode::PRINT:
                emit_print(code);
                break;
            case Opcode::INPUT:
                emit_read(code);
                break;

            default:
                fprintf(stderr, "Unknown opcode %d\n", (uint32_t)opcode);
                exit(1);
        }
    }

    emit_epilogue(code);
    return code;
}

void Code::emit_inc_cell(std::vector<uint8_t> &code) {
    /*
    emit machine code to increase current pointed cell by one

    emits the following assembly
        inc BYTE PTR [TAPE_BASE_REGISTER + TAPE_INDEX_REGISTER]
    */
    emit_inc_byte_ptr(
        TAPE_BASE_REGISTER,
        TAPE_INDEX_REGISTER,
        code);
}

void Code::emit_dec_cell(std::vector<uint8_t> &code) {
    /*
    emit machine code to decrease current pointed cell by one

    emits the following assembly
        dec BYTE PTR [TAPE_BASE_REGISTER + TAPE_INDEX_REGISTER]
    */
    emit_dec_byte_ptr(
        TAPE_BASE_REGISTER,
        TAPE_INDEX_REGISTER,
        code);
}


void Code::emit_add_to_cell(const uint32_t val, std::vector<uint8_t> &code) {
    /*
    emit machine code to add a constant to the cell

    emits the following assembly
        add BYTE PTR [TAPE_BASE_REGISTER, TAPE_INDEX_REGISTER], constant
    */

    // NOTE: The constant val parameter gets truncated into 8-bit value
    emit_add_immediate_byte_ptr(
        TAPE_BASE_REGISTER,
        TAPE_INDEX_REGISTER,
        val,
        code);
}

void Code::emit_sub_from_cell(const uint32_t val, std::vector<uint8_t> &code) {
    /*
    emit machine code to sub a constant from the cell

    emits the following assembly
        sub BYTE PTR [TAPE_BASE_REGISTER + TAPE_INDEX_REGISTER], constant
    */

    // NOTE: The constant val parameter gets truncated into 8-bit value
    emit_sub_immediate_byte_ptr(
        TAPE_BASE_REGISTER,
        TAPE_INDEX_REGISTER,
        val,
        code);
}


void Code::emit_move_tape_left_one_step(std::vector<uint8_t> &code) {
    /*
    emit machine code to decrease tape pointer by one

    emits the following assembly
        dec TAPE_INDEX_REGISTER

    */
    emit_dec_register(TAPE_INDEX_REGISTER, code);
}

void Code::emit_move_tape_right_one_step(std::vector<uint8_t> &code) {
    /*
    emit machine code to increase tape pointer by one

    emits the following assembly
        inc TAPE_INDEX_REGISTER

    */
    emit_inc_register(TAPE_INDEX_REGISTER, code);
}

void Code::emit_move_tape_left(
    uint32_t steps,
    std::vector<uint8_t> &code) {

    /*
    emit machine code to sub given number of steps to the tape pointer

    emits the following assembly
        sub TAPE_INDEX_REGISTER, <steps>
    */
    emit_sub_immediate_reg(TAPE_INDEX_REGISTER, steps, code);
}


void Code::emit_move_tape_right(
    uint32_t steps,
    std::vector<uint8_t> &code) {

    /*
    emit machine code to add given number of steps to the tape pointer

    emits the following assembly
        add TAPE_INDEX_REGISTER, <steps>
    */
    emit_add_immediate_reg(TAPE_INDEX_REGISTER, steps, code);
}

void Code::emit_jump_if_false(
    std::stack<uint32_t> &jump_placeholders,
    std::vector<uint8_t> &code) {
    /*
    emit machine code that compares current cell to zero, and jumps
    if said cell is zero. Jump location is a placeholder that gets updated
    by the jump_if_true operation

    emits the following assembly
        cmp BYTE PTR [TAPE_BASE_REGISTER + TAPE_INDEX_REGISTER], 0
        jz <32 bit placeholder value>
    */


    // while arithmetic operations set the zero flag, we can't be
    // sure that the operation that was executed was performed
    // on the current cell, and not on the tape index pointer
    // as such, we need to execute comparison to be sure
    emit_cmp_immediate_byte_ptr(
        TAPE_BASE_REGISTER,
        TAPE_INDEX_REGISTER,
        0,
        code);
    emit_jump_zero(0, code);

    uint32_t immediate_size = sizeof(uint32_t);
    // placeholder points to the start of the immediate operand
    jump_placeholders.push(code.size()-immediate_size);
}

void Code::emit_jump_if_true(
    /*
    emit machine code that compares current cell to zero, and jumps if said cell
    is non-zero. Jump target is the instruction after the matching jump_if_false
    instruction. Also updates the jump location for the jump_if_false, as there
    is now enough information to do so.

    Uses relative offset

    emits the following assembly
        cmp BYTE PTR [TAPE_BASE_REGISTER + TAPE_INDEX_REGISTER], 0
        jnz <
            matching '[' instruction>

    patches jump_if_false placeholder value to target the instruction after
    this jump instruction
    */

    std::stack<uint32_t> &jump_placeholders,
    std::vector<uint8_t> &code) {

    emit_cmp_immediate_byte_ptr(
        TAPE_BASE_REGISTER,
        TAPE_INDEX_REGISTER,
        0,
        code);

    auto target = calculate_jump_target_for_true_branch(
        jump_placeholders,
        code);

    emit_jump_not_zero(target, code);
    update_false_branch_jump_target(jump_placeholders, code);
    jump_placeholders.pop();
}

void Code::emit_zero_cell(
    std::vector<uint8_t> &code) {
    /*
    emit machine code to set the current cell in tape to zero

    emits the following assembly
        mov [TAPE_BASE_REGISTER + TAPE_INDEX_REGISTER], 0
    */
    emit_mov_immediate_byte_ptr(
        TAPE_BASE_REGISTER,
        TAPE_INDEX_REGISTER,
        0,
        code);
}

void Code::emit_print(std::vector<uint8_t> &code) {
    /*
    emit machine code to call putc-function on the current selected cell value

    emits the following assembly:

        push TAPE_INDEX_REGISTER
        push TAPE_BASE_REGISTER

        mov DIL, [TAPE_INDEX_REGISTER, TAPE_BASE_REGISTER]
        mov RSI, <constant value, pointer to stdout FILE object>
        mov WORK_REGISTER, <constant value, function pointer to putc)
        call WORK_REGISTER

        pop TAPE_BASE_REGISTER
        pop TAPE_INDEX_REGISTER
    */

    // bad things happen if work-register is one of these
    static_assert(
        WORK_REGISTER != Register::RSI,
        "Work register cannot be RSI, as this breaks function calls" );
    static_assert(WORK_REGISTER != Register::RDI,
        "Work register cannot be RDI, as this breaks function calls");
    // save tape regs, as the function does not preserve these and they might
    // be used during the function call

    emit_reg_push(TAPE_INDEX_REGISTER, code);
    emit_reg_push(TAPE_BASE_REGISTER, code);

    // sysvabi calling convention: first arg in RDI, second arg in RSI

    emit_mov_byte_ptr_reg(
        TAPE_BASE_REGISTER,
        TAPE_INDEX_REGISTER,
        Register_8bit::DIL,
        code);

    emit_mov_reg_direct(Register::RSI, (size_t)stdout, code);
    emit_mov_reg_direct(WORK_REGISTER, (size_t)&std::putc, code);
    emit_indirect_function_call(WORK_REGISTER, code);

    emit_reg_pop(TAPE_BASE_REGISTER, code);
    emit_reg_pop(TAPE_INDEX_REGISTER, code);
}

void Code::emit_read(std::vector<uint8_t> &code) {
    /*
    emit machine code to call getchar-function and store the result to the
    current cell.

    emits the following assembly:

        push TAPE_INDEX_REGISTER
        push TAPE_BASE_REGISTER

        mov WORK_REGISTER, <constant value, function pointer to getchar)
        call WORK_REGISTER

        pop TAPE_BASE_REGISTER
        pop TAPE_INDEX_REGISTER

    */

    // bad things happen if work-register is one of these
    static_assert(
        WORK_REGISTER != Register::RSI,
        "Work register cannot be RSI, as this breaks function calls" );
    static_assert(
        WORK_REGISTER != Register::RDI,
        "Work register cannot be RDI, as this breaks function calls");
    static_assert(
        WORK_REGISTER != Register::RAX,
        "Work register cannot be RAX, as this breaks function calls");

    emit_reg_push(TAPE_INDEX_REGISTER, code);
    emit_reg_push(TAPE_BASE_REGISTER, code);

    emit_mov_reg_direct(WORK_REGISTER, (size_t)&std::getchar, code);
    emit_indirect_function_call(WORK_REGISTER, code);

    emit_reg_pop(TAPE_BASE_REGISTER, code);
    emit_reg_pop(TAPE_INDEX_REGISTER, code);

    emit_mov_reg_byte_ptr(
        TAPE_BASE_REGISTER,
        TAPE_INDEX_REGISTER,
        Register_8bit::AL,
        code);
}

uint8_t Code::get_rex_prefix(
    const bool operand_size_64_bit,
    const bool modrm_reg_extension,
    const bool sib_index_extension,
    const bool rm_or_sib_extension) {

    uint8_t prefix = 0b0100'0000;

    if (operand_size_64_bit) {
        prefix = prefix | 0b0000'1000;
    }

    if (modrm_reg_extension) {
        prefix = prefix | 0b0000'0100;
    }

    if (sib_index_extension) {
        prefix = prefix | 0b0000'0010;
    }

    if (rm_or_sib_extension) {
        prefix = prefix | 0b0000'0001;
    }

    return prefix;
}

uint8_t Code::get_sib_byte(uint8_t scale, Register index, Register base) {
    if (index == Register::RSP) {
        fprintf(stderr, "RSP is not a valid index register for SIB byte");
        exit(1);
    }

    return (scale << 6) | ((uint8_t)index << 3) | ((uint8_t)base);
}

// push register into stack
void Code::emit_reg_push(
    const Register reg,
    std::vector<uint8_t> &code) {
    uint8_t reg_bits = (uint8_t)reg;
    code.push_back(0b0101'0000 | reg_bits);
}

// pop register from stack
void Code::emit_reg_pop(
    const Register reg,
    std::vector<uint8_t> &code) {
    uint8_t reg_bits = (uint8_t)reg;
    code.push_back(0b0101'1000 | reg_bits);
}

// move constant value into a register
void Code::emit_mov_reg_direct(
    const Register dest,
    const size_t value,
    std::vector<uint8_t> &code) {

    auto rex_prefix = get_rex_prefix(true, false, false, false);
    // variant of mov where destination reg is encoded in the opcode
    auto opcode = 0b1011'1000 | ((uint8_t)dest);
    code.push_back(rex_prefix);
    code.push_back(opcode);
    // immediate operand in little-endian form
    for (size_t i = 0; i < sizeof(uint64_t); ++i) {
        code.push_back((value >> 8*i) & 0xFF);
    }
}

// move constant, single byte value into address pointed by base and index
void Code::emit_mov_immediate_byte_ptr(
    const Register base,
    const Register index,
    const uint8_t value,
    std::vector<uint8_t> &code) {

    const uint8_t opcode = 0b1100'0110;
    const uint8_t opcode_ext = 0b000;

    const uint8_t modrm_byte =
        REGISTER_INDIRECT_MODE |
        (opcode_ext << 3) |
        REGISTER_INDIRECT_SIB_ONLY;

    // hardcoded scale for 1 byte operand size
    const uint8_t scale = 0b00;
    const uint8_t sib_byte = get_sib_byte(scale, index, base);


    code.push_back(opcode);
    code.push_back(modrm_byte);
    code.push_back(sib_byte);
    code.push_back(value);
}

// move single byte from address pointed by base and index into dest register
void Code::emit_mov_byte_ptr_reg(
    const Register base,
    const Register index,
    const Register_8bit dest,
    std::vector<uint8_t> &code) {

    // if SPL, BPL, SIL or DIL (8 bit versions of RSP, RBD, RSI, RDI)
    // are used, rex-prefix is required. This is because the bit-pattern
    // in mod-rm is already in use by other low/high reg encoding
    // (the first bit encodes low/high byte usage in the 16-bit portion of reg,
    //  register encoding that has this bit set requires the rex prefix)
    //

    // e.g.       0x40 0x8a 0x3c 0x11 encodes mov DIL, BYTE PTR [RCX + RDX]
    //-- ---------^^^^----- rex prefix
    // <no rex prefix> 0x8a 0x3c 0x11 encodes mov BH, BYTE PTR [RCX + RDX]

    const uint8_t opcode = 0b1000'1010;

    const uint8_t modrm_byte =
        REGISTER_INDIRECT_MODE |
        (((uint8_t)dest & 0b0000'0111) << 3) |
        REGISTER_INDIRECT_SIB_ONLY;

    // again, 1 byte operand so no scaling needed for the index
    uint8_t scale = 0b00;
    const uint8_t sib_byte = get_sib_byte(scale, index, base);

    if (dest == Register_8bit::SPL ||
        dest == Register_8bit::BPL ||
        dest == Register_8bit::SIL ||
        dest == Register_8bit::DIL) {

        const uint8_t rex_prefix = get_rex_prefix(false, false, false, false);
        code.push_back(rex_prefix);
    }

    code.push_back(opcode);
    code.push_back(modrm_byte);
    code.push_back(sib_byte);
}

// move single byte from source reg into address pointed by base and index regs
void Code::emit_mov_reg_byte_ptr(
    const Register base,
    const Register index,
    const Register_8bit src,
    std::vector<uint8_t> &code) {

    // if SPL, BPL, SIL or DIL (8 bit versions of RSP, RBD, RSI, RDI)
    // are used, rex-prefix is required. This is because the bit-pattern
    // in mod-rm is already in use by other low/high reg encoding
    // (the first bit encodes low/high byte usage in the 16-bit portion of reg,
    //  register encoding that has this bit set requires the rex prefix)
    //

    // e.g.       0x40 0x8a 0x3c 0x11 encodes mov DIL, BYTE PTR [RCX + RDX]
    //-- ---------^^^^----- rex prefix
    // <no rex prefix> 0x8a 0x3c 0x11 encodes mov BH, BYTE PTR [RCX + RDX]

    const uint8_t opcode = 0b1000'1000;

    const uint8_t modrm_byte =
        REGISTER_INDIRECT_MODE |
        (((uint8_t)src & 0b0000'0111) << 3) |
        REGISTER_INDIRECT_SIB_ONLY;

    // again, 1 byte operand so no scaling needed for the index
    uint8_t scale = 0b00;
    const uint8_t sib_byte = get_sib_byte(scale, index, base);

    if (src == Register_8bit::SPL ||
        src == Register_8bit::BPL ||
        src == Register_8bit::SIL ||
        src == Register_8bit::DIL) {

        const uint8_t rex_prefix = get_rex_prefix(false, false, false, false);
        code.push_back(rex_prefix);
    }

    code.push_back(opcode);
    code.push_back(modrm_byte);
    code.push_back(sib_byte);
}

// load effective address. Store result of base + index into dest
void Code::emit_lea(
    const Register dest,
    const Register base,
    const Register index,
    std::vector<uint8_t> &code) {

    auto prefix = get_rex_prefix(true, false, false, false);
    const uint8_t opcode = 0b1000'1101;
    const uint8_t modrm_byte =
        REGISTER_INDIRECT_MODE |
        (((uint8_t)dest) << 3) |
        REGISTER_INDIRECT_SIB_ONLY;

    // we only index byte arrays, so scale is always 0 (2^0 = 1, 1 byte size)
    const uint8_t scale = 0b00;

    const uint8_t sib_byte = get_sib_byte(scale, index, base);

    code.push_back(prefix);
    code.push_back(opcode);
    code.push_back(modrm_byte);
    code.push_back(sib_byte);
}

// increase value pointed by register by one
void Code::emit_inc_byte_ptr(
    const Register base,
    const Register index,
    std::vector<uint8_t> &code) {

    const uint8_t opcode = 0b1111'1110;
    const uint8_t opcode_ext = 0;

    const uint8_t modrm_byte =
        REGISTER_INDIRECT_MODE |
        (opcode_ext << 3) |
        REGISTER_INDIRECT_SIB_ONLY;

    const uint8_t scale = 0b00;
    const uint8_t sib_byte = get_sib_byte(scale, index, base);

    code.push_back(opcode);
    code.push_back(modrm_byte);
    code.push_back(sib_byte);
}

// decrease value pointed by register by one
void Code::emit_dec_byte_ptr(
    const Register base,
    const Register index,
    std::vector<uint8_t> &code) {

    const uint8_t opcode = 0b1111'1110;
    const uint8_t opcode_ext = 0b001;
    const uint8_t modrm_byte =
        REGISTER_INDIRECT_MODE |
        (opcode_ext << 3) |
        REGISTER_INDIRECT_SIB_ONLY;

    const uint8_t scale = 0b00;
    const uint8_t sib_byte = get_sib_byte(scale, index, base);

    code.push_back(opcode);
    code.push_back(modrm_byte);
    code.push_back(sib_byte);
}

// add byte constant to value pointed by base + index
void Code::emit_add_immediate_byte_ptr(
    const Register base,
    const Register index,
    const uint8_t val,
    std::vector<uint8_t> &code) {

    const uint8_t opcode = 0b1000'0000;
    const uint8_t opcode_ext = 0b000;

    const uint8_t modrm_byte =
        REGISTER_INDIRECT_MODE |
        (opcode_ext << 3) |
        REGISTER_INDIRECT_SIB_ONLY;

    uint8_t scale = 0b00; // 1 byte operand size
    const uint8_t sib_byte = get_sib_byte(scale, index, base);

    code.push_back(opcode);
    code.push_back(modrm_byte);
    code.push_back(sib_byte);
    code.push_back(val);
}

void Code::emit_add_immediate_reg(
    const Register destination,
    const uint32_t val,
    std::vector<uint8_t> &code) {

    uint8_t rex_prefix = get_rex_prefix(true, false, false, false);
    const uint8_t opcode = 0b1000'0001;
    const uint8_t opcode_ext = 0b000;

    const uint8_t modrm_byte =
        REGISTER_DIRECT_MODE |
        (opcode_ext << 3) |
        ((uint8_t)destination);

    code.push_back(rex_prefix);
    code.push_back(opcode);
    code.push_back(modrm_byte);

    const uint32_t bits_in_byte = 8;
    for (size_t i = 0; i < sizeof(val); ++i) {
        code.push_back((val >> i*bits_in_byte) & 0xFF);
    }
}

// subtract byte constant from value pointed by base + index
void Code::emit_sub_immediate_byte_ptr(
    const Register base,
    const Register index,
    const uint8_t val,
    std::vector<uint8_t> &code) {

    const uint8_t opcode = 0b1000'0000;
    const uint8_t opcode_ext = 0b101;

    const uint8_t modrm_byte =
        REGISTER_INDIRECT_MODE |
        (opcode_ext << 3) |
        REGISTER_INDIRECT_SIB_ONLY;

    uint8_t scale = 0b00; // 1 byte operand size
    const uint8_t sib_byte = get_sib_byte(scale, index, base);

    const uint8_t immediate = val & 0xFF;

    code.push_back(opcode);
    code.push_back(modrm_byte);
    code.push_back(sib_byte);
    code.push_back(immediate);
}

void Code::emit_sub_immediate_reg(
    const Register destination,
    const uint32_t val,
    std::vector<uint8_t> &code) {

    uint8_t rex_prefix = get_rex_prefix(true, false, false, false);
    const uint8_t opcode = 0b1000'0001;
    const uint8_t opcode_ext = 0b101;

    const uint8_t modrm_byte =
        REGISTER_DIRECT_MODE |
        (opcode_ext << 3) |
        ((uint8_t)destination);
    code.push_back(rex_prefix);
    code.push_back(opcode);
    code.push_back(modrm_byte);

    const uint32_t bits_in_byte = 8;
    for (size_t i = 0; i < sizeof(val); ++i) {
        code.push_back((val >> i*bits_in_byte) & 0xFF);
    }
}

// increase value in register by one
void Code::emit_inc_register(
    const Register reg,
    std::vector<uint8_t> &code) {

    const uint8_t rex_prefix = get_rex_prefix(true, false, false, false);
    const uint8_t opcode = 0b1111'1111;
    const uint8_t opcode_ext = 0;

    const uint8_t modrm_byte =
        REGISTER_DIRECT_MODE |
        (opcode_ext << 3) |
        (uint8_t)reg;

    code.push_back(rex_prefix);
    code.push_back(opcode);
    code.push_back(modrm_byte);
}

// decrease value in register by one
void Code::emit_dec_register(
    const Register reg,
    std::vector<uint8_t> &code) {

    const uint8_t rex_prefix = get_rex_prefix(true, false, false, false);
    const uint8_t opcode = 0b1111'1111;
    const uint8_t opcode_ext = 0b001;

    const uint8_t modrm_byte =
        REGISTER_DIRECT_MODE |
        (opcode_ext << 3) |
        (uint8_t)reg;

    code.push_back(rex_prefix);
    code.push_back(opcode);
    code.push_back(modrm_byte);
}

// compare value pointed by base + index to a byte constant
void Code::emit_cmp_immediate_byte_ptr(
    const Register base,
    const Register index,
    const uint8_t immediate,
    std::vector<uint8_t> &code) {

    const uint8_t opcode = 0b1000'0000;
    const uint8_t opcode_ext = 0b111;

    const uint8_t modrm_byte =
        REGISTER_INDIRECT_MODE |
        (opcode_ext << 3) |
        REGISTER_INDIRECT_SIB_ONLY;

    uint8_t scale = 0b00; // 1 byte operand size
    const uint8_t sib_byte = get_sib_byte(scale, index, base);

    code.push_back(opcode);
    code.push_back(modrm_byte);
    code.push_back(sib_byte);
    code.push_back(immediate);
}

// compare value pointed by base + index to a byte constant
void Code::emit_cmp_immediate_reg(
    const Register reg,
    const uint8_t immediate,
    std::vector<uint8_t> &code) {

    const uint8_t rex_prefix = get_rex_prefix(true, false, false, false);

    const uint8_t opcode = 0b1000'0011;
    const uint8_t opcode_ext = 0b111;

    const uint8_t modrm_byte =
        REGISTER_DIRECT_MODE |
        (opcode_ext << 3) |
        (uint8_t)reg;

    code.push_back(rex_prefix);
    code.push_back(opcode);
    code.push_back(modrm_byte);
    code.push_back(immediate);
}


// jump if zero flag is set to a given address
// (destination is a relative offset)
void Code::emit_jump_zero(
    const uint32_t offset,
    std::vector<uint8_t> &code) {

    const uint8_t opcode = 0b0000'1111;

    // has form tttn, where ttt select the comparison and n negates it
    // 0100 -> zero, 0101 -> not zero
    const uint8_t condition_test_field = 0b0000'0100;
    // this instruction expects the first 4 bits of modrm to be 1000
    const uint8_t modrm_byte =
        0b1000'0000 |
        condition_test_field;

    code.push_back(opcode);
    code.push_back(modrm_byte);

    const uint32_t bits_in_byte = 8;
    for (size_t i = 0; i < sizeof(offset); ++i) {
        code.push_back((offset >> bits_in_byte*i) & 0xFF);
    }
}
// jump if zero flag is not set to a given address
// (destination is a relative offset)
void Code::emit_jump_not_zero(
    const uint32_t offset,
    std::vector<uint8_t> &code) {

    const uint8_t opcode = 0b0000'1111;

    // has form tttn, where ttt select the comparison and n negates it
    // 0100 -> zero, 0101 -> not zero
    const uint8_t condition_test_field = 0b0000'0101;
    // this instruction expects the first 4 bits of modrm to be 1000
    const uint8_t modrm_byte =
        0b1000'0000 |
        condition_test_field;

    code.push_back(opcode);
    code.push_back(modrm_byte);

    const uint32_t bits_in_byte = 8;
    for (size_t i = 0; i < sizeof(offset); ++i) {
        code.push_back((offset >> bits_in_byte*i) & 0xFF);
    }
}

void Code::emit_jump(
    const uint32_t offset,
    std::vector<uint8_t> &code) {

    const uint8_t opcode = 0b1110'1001;

    code.push_back(opcode);
  const uint32_t bits_in_byte = 8;
    for (size_t i = 0; i < sizeof(offset); ++i) {
        code.push_back((offset >> bits_in_byte*i) & 0xFF);
    }
}


// emit indirect function call. Address of the function is stored in the reg
void Code::emit_indirect_function_call(
    const Register reg,
    std::vector<uint8_t> &code) {

    const uint8_t opcode = 0b1111'1111;
    const uint8_t opcode_ext = 0b010;
    const uint8_t modrm_byte =
        REGISTER_DIRECT_MODE |
        (opcode_ext << 3) |
        (uint8_t)reg;

    code.push_back(opcode);
    code.push_back(modrm_byte);
}

// calculates the offset for the true branch
// (decrease pc by n bytes)
uint32_t Code::calculate_jump_target_for_true_branch(
        std::stack<uint32_t> jump_placeholders,
        const std::vector<uint8_t> &code) {

    // 2 byte operation
    const uint32_t jump_instruction_size = 2;
    auto placeholder = jump_placeholders.top();
    return placeholder - code.size() - jump_instruction_size;
}

// update the placholder offset value for false branch
void Code::update_false_branch_jump_target(
    std::stack<uint32_t> jump_placeholders,
    std::vector<uint8_t> &code) {

    auto target = calculate_jump_target_for_false_branch(
        jump_placeholders,
        code);

    auto placeholder_start = jump_placeholders.top();

    const uint32_t bits_in_byte = 8;
    for (size_t i = 0; i < sizeof(target); ++i) {
        code[placeholder_start+i] = (target >> i*bits_in_byte) & 0xFF;
    }
}

// calculate the offset for the false branch
// (increase pc by n bytes)
uint32_t Code::calculate_jump_target_for_false_branch(
        std::stack<uint32_t> jump_placeholders,
        const std::vector<uint8_t> &code) {

    const uint32_t true_branch_immediate_size = sizeof(uint32_t);
    return code.size() - true_branch_immediate_size - jump_placeholders.top();
}



// emit function prologue. Store the value of registers we use
// (not necessarily required, calling convention dictates this but let's just
// store them unconditionally so we don't need to worry about this) and
// initialize the registers to correct values
void Code::emit_prologue(std::vector<uint8_t> &code) {
    emit_reg_push(WORK_REGISTER, code);
    emit_reg_push(TAPE_BASE_REGISTER, code);
    emit_reg_push(TAPE_INDEX_REGISTER, code);

    emit_mov_reg_direct(TAPE_BASE_REGISTER, (size_t)m_tape, code);
    emit_mov_reg_direct(WORK_REGISTER, 0, code);
    emit_mov_reg_direct(TAPE_INDEX_REGISTER, 0, code);

}

// emit function epilogue. Pop registers and emit return
void Code::emit_epilogue(std::vector<uint8_t> &code) {
   emit_reg_pop(TAPE_INDEX_REGISTER, code);
   emit_reg_pop(TAPE_BASE_REGISTER, code);
   emit_reg_pop(WORK_REGISTER, code);

   code.push_back(0xC3); // ret
}

// allocate and initialize executable memory region
void Code::create_executable_section(const std::vector<uint8_t> &code) {
    m_code_size = code.size();

    m_code = mmap(
       NULL,
       m_code_size,
       PROT_READ | PROT_WRITE,
       MAP_ANONYMOUS | MAP_PRIVATE,
       0,
       0);

    uint8_t *ptr = (uint8_t *)m_code;

    for (uint32_t i = 0; i < code.size(); ++i) {
        ptr[i] = code[i];
    }

    mprotect(m_code, m_code_size, PROT_READ | PROT_EXEC);
}

void Code::run() {
    auto func = (jitFunction)(m_code);
    func();
}