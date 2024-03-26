.cui
@exit_msg
.string "Please press any key to continue"
@err_nomem
.string "Error: cannot allocate memory\n"
@err_openfailure
.string "Error: cannot open file\n"
@err_badchar
.string "Error at line %d: bad character\n"
@err_notstring
.string "Error at line %d: string required here\n"
@err_badpseudoop
.string "Error at line %d: bad pseudo-op\n"
@err_undefined
.string "Error at line %d: label undefined\n"
@err_redefined
.string "Error at line %d: label redefined\n"
@err_inserr
.string "Error at line %d: unknown instruction\n"
@err_badalign
.string "Error at line %d: alignment out of range (1-8)\n"
@err_datasize
.string "Error at line %d: integer required here\n"

@msg_success
.string "Success\n"

@msg_infile
.string "Source file: "
@msg_outfile
.string "Output file: "

@fmode_r
.string "rb"
@fmode_w
.string "wb"

@pseudo_dllcall
.string ".dllcall"
@pseudo_byte
.string ".byte"
@pseudo_word
.string ".word"
@pseudo_long
.string ".long"
@pseudo_quad
.string ".quad"
@pseudo_string
.string ".string"
@pseudo_entry
.string ".entry"
@pseudo_align
.string ".align"
@pseudo_cui
.string ".cui"
@pseudo_datasize
.string ".datasize"

@dataseg_str
.string "@_$DATA"

@reg8
.string "al"
.string "cl"
.string "dl"
.string "bl"
.string "spl"
.string "bpl"
.string "sil"
.string "dil"
.string "r8b"
.string "r9b"
.string "r10b"
.string "r11b"
.string "r12b"
.string "r13b"
.string "r14b"
.string "r15b"
.string "ah"
.string "ch"
.string "dh"
.string "bh"
.byte 0
@reg16
.string "ax"
.string "cx"
.string "dx"
.string "bx"
.string "sp"
.string "bp"
.string "si"
.string "di"
.string "r8w"
.string "r9w"
.string "r10w"
.string "r11w"
.string "r12w"
.string "r13w"
.string "r14w"
.string "r15w"
.byte 0
@reg32
.string "eax"
.string "ecx"
.string "edx"
.string "ebx"
.string "esp"
.string "ebp"
.string "esi"
.string "edi"
.string "r8d"
.string "r9d"
.string "r10d"
.string "r11d"
.string "r12d"
.string "r13d"
.string "r14d"
.string "r15d"
.string "eip"
.byte 0
@reg64
.string "rax"
.string "rcx"
.string "rdx"
.string "rbx"
.string "rsp"
.string "rbp"
.string "rsi"
.string "rdi"
.string "r8"
.string "r9"
.string "r10"
.string "r11"
.string "r12"
.string "r13"
.string "r14"
.string "r15"
.string "rip"
.byte 0
@regxmm
.string "xmm10"
.string "xmm11"
.string "xmm12"
.string "xmm13"
.string "xmm14"
.string "xmm15"
.string "xmm0"
.string "xmm1"
.string "xmm2"
.string "xmm3"
.string "xmm4"
.string "xmm5"
.string "xmm6"
.string "xmm7"
.string "xmm8"
.string "xmm9"
.byte 0
@ins_format
.string "RB1"
.string "RW1"
.string "RL1"
.string "RQ1"
.string "RX1"
.string "RB2"
.string "RW2"
.string "RL2"
.string "RQ2"
.string "RX2"
.string "ADDR"
.string "IMM"
.string "CL"
.byte 0

@ins_list
.string "pushq "
.string "push "
.string "popq "
.string "pop "
.string "mov "
.string "movq "
.string "movl "
.string "movw "
.string "movb "
.string "movd "
.string "jmp "
.string "call "
.string "ret"
.string "je "
.string "jne "
.string "ja "
.string "jae "
.string "jb "
.string "jbe "
.string "jg "
.string "jge "
.string "jl "
.string "jle "
.string "cmp "
.string "cmpq "
.string "cmpl "
.string "cmpw "
.string "cmpb "
.string "add "
.string "addq "
.string "addl "
.string "addw "
.string "addb "
.string "sub "
.string "subq "
.string "subl "
.string "subw "
.string "subb "
.string "and "
.string "andq "
.string "andl "
.string "andw "
.string "andb "
.string "or "
.string "orq "
.string "orl "
.string "orw "
.string "orb "
.string "xor "
.string "xorq "
.string "xorl "
.string "xorw "
.string "xorb "
.string "test "
.string "testq "
.string "testl "
.string "testw "
.string "testb "
.string "inc "
.string "incq "
.string "incl "
.string "incw "
.string "incb "
.string "dec "
.string "decq "
.string "decl "
.string "decw "
.string "decb "
.string "mul "
.string "mulq "
.string "mull "
.string "mulw "
.string "mulb "
.string "imul "
.string "imulq "
.string "imull "
.string "imulw "
.string "imulb "
.string "div "
.string "divq "
.string "divl "
.string "divw "
.string "divb "
.string "idiv "
.string "idivq "
.string "idivl "
.string "idivw "
.string "idivb "
.string "not "
.string "notq "
.string "notl "
.string "notw "
.string "notb "
.string "neg "
.string "negq "
.string "negl "
.string "negw "
.string "negb "
.string "shl "
.string "shlq "
.string "shll "
.string "shlw "
.string "shlb "
.string "shr "
.string "shrq "
.string "shrl "
.string "shrw "
.string "shrb "
.string "sar "
.string "sarq "
.string "sarl "
.string "sarw "
.string "sarb "
.string "xchg "
.string "lea "
.string "movzbw "
.string "movzbl "
.string "movzbq "
.string "movzwl "
.string "movzwq "
.string "movsbw "
.string "movsbl "
.string "movsbq "
.string "movswl "
.string "movswq "
.string "movslq "
.string "movups "
.string "movss "
.string "movsd "
.string "addss "
.string "addsd "
.string "subss "
.string "subsd "
.string "mulss "
.string "mulsd "
.string "divss "
.string "divsd "
.string "comiss "
.string "comisd "
.string "addps "
.string "addpd "
.string "subps "
.string "subpd "
.string "mulps "
.string "mulpd "
.string "divps "
.string "divpd "
.string "shufps "
.string "cvtss2sd "
.string "cvtsd2ss "
.string "cvtsi2ss "
.string "cvtsi2sd "
.string "cvtss2si "
.string "cvtsd2si "
.byte 0

# meaning of instruction format
# TEXTFORMAT:OPTIMIZE:PREFIXES:REX:ENCODING
# ENCODING can be:
# a number
# i -- IMM8
# I -- IMM8 (signed 64-bit)
# j -- IMM16
# k -- IMM32
# K -- IMM32 (signed 64-bit)
# l -- IMM64
# A -- address
# R -- reg1
# r -- reg2
# s -- reg2<<3
# O, o -- used in JMP and CALL instructions
@ins_operands
.string "$IMM::0:0x6a I "
.string "$IMM::0:0x68 K "
.string "ADDR::0:0xff 0x30|A "
.string "%RQ1::0:0x50|R "
.string "ADDR::0:0x8f 0x00|A "
.string "%RQ1::0:0x58|R "
.string "$IMM,%RQ1::1:0xc7 0xc0|R K "
.string "$IMM,%RQ1::0:0xb8|R k "
.string "$IMM,%RQ1::1:0xb8|R l "
.string "$IMM,%RL1::0:0xb8|R k "
.string "$IMM,%RW1:0x66 :0:0xb8|R j "
.string "$IMM,%RB1::0:0xb0|R i "
.string "%RQ2,%RQ1::1:0x89 0xc0|R|s "
.string "%RL2,%RL1::0:0x89 0xc0|R|s "
.string "%RW2,%RW1:0x66 :0:0x89 0xc0|R|s "
.string "%RB2,%RB1::0:0x88 0xc0|R|s "
.string "%RQ2,ADDR::1:0x89 s|A "
.string "%RL2,ADDR::0:0x89 s|A "
.string "%RW2,ADDR:0x66 :0:0x89 s|A "
.string "%RB2,ADDR::0:0x88 s|A "
.string "ADDR,%RQ2::1:0x8b s|A "
.string "ADDR,%RL2::0:0x8b s|A "
.string "ADDR,%RW2:0x66 :0:0x8b s|A "
.string "ADDR,%RB2::0:0x8a s|A "
# movq
.string "$IMM,ADDR::1:0xc7 A k "
.string "%RQ1,%RX2:0x66 :1:0x0f 0x6e 0xc0|R|s "
.string "%RX2,%RQ1:0x66 :1:0x0f 0x7e 0xc0|R|s "
# movw
.string "$IMM,ADDR::0:0xc7 A k "
.string "$IMM,ADDR:0x66 :0:0xc7 A j "
.string "$IMM,ADDR::0:0xc6 A i "
# movd
.string "%RL1,%RX2:0x66 :0:0x0f 0x6e 0xc0|R|s "
.string "%RX2,%RL1:0x66 :0:0x0f 0x7e 0xc0|R|s "
# jmp
.string "IMM::0:0xeb o "
.string "IMM::0:0xe9 O "
.string "*%RQ1::0:0xff 0xe0|R "
.string "*ADDR::0:0xff 0x20|A "
# call
.string "IMM::0:0xe8 O "
.string "*%RQ1::0:0xff 0xd0|R "
.string "*ADDR::0:0xff 0x10|A "
# ret
.string "::0:0xc3 "
# jC
.string "IMM::0:0x74 o "
.string "IMM::0:0x0f 0x84 O "
.string "IMM::0:0x75 o "
.string "IMM::0:0x0f 0x85 O "
.string "IMM::0:0x77 o "
.string "IMM::0:0x0f 0x87 O "
.string "IMM::0:0x73 o "
.string "IMM::0:0x0f 0x83 O "
.string "IMM::0:0x72 o "
.string "IMM::0:0x0f 0x82 O "
.string "IMM::0:0x76 o "
.string "IMM::0:0x0f 0x86 O "
.string "IMM::0:0x7f o "
.string "IMM::0:0x0f 0x8f O "
.string "IMM::0:0x7d o "
.string "IMM::0:0x0f 0x8d O "
.string "IMM::0:0x7c o "
.string "IMM::0:0x0f 0x8c O "
.string "IMM::0:0x7e o "
.string "IMM::0:0x0f 0x8e O "
# cmp
.string "%RQ2,%RQ1::1:0x39 0xc0|R|s "
.string "%RL2,%RL1::0:0x39 0xc0|R|s "
.string "%RW2,%RW1:0x66 :0:0x39 0xc0|R|s "
.string "%RB2,%RB1::0:0x38 0xc0|R|s "
.string "%RQ2,ADDR::1:0x39 s|A "
.string "%RL2,ADDR::0:0x39 s|A "
.string "%RW2,ADDR:0x66 :0:0x39 s|A "
.string "%RB2,ADDR::0:0x38 s|A "
.string "ADDR,%RQ2::1:0x3b s|A "
.string "ADDR,%RL2::0:0x3b s|A "
.string "ADDR,%RW2:0x66 :0:0x3b s|A "
.string "ADDR,%RB2::0:0x3a s|A "
.string "$IMM,%RQ1::1:0x83 0xf8|R I "
.string "$IMM,%RQ1::1:0x81 0xf8|R K "
.string "$IMM,%RL1::0:0x83 0xf8|R I "
.string "$IMM,%RL1::0:0x81 0xf8|R k "
.string "$IMM,%RW1:0x66 :0:0x83 0xf8|R I "
.string "$IMM,%RW1:0x66 :0:0x81 0xf8|R j "
.string "$IMM,%RB1::0:0x80 0xf8|R i "
# cmpq
.string "$IMM,ADDR::1:0x83 0x38|A I "
.string "$IMM,ADDR::1:0x81 0x38|A K "
.string "$IMM,ADDR::0:0x83 0x38|A I "
.string "$IMM,ADDR::0:0x81 0x38|A k "
.string "$IMM,ADDR:0x66 :0:0x83 0x38|A I "
.string "$IMM,ADDR:0x66 :0:0x81 0x38|A j "
.string "$IMM,ADDR::0:0x80 0x38|A i "
# add
.string "%RQ2,%RQ1::1:0x01 0xc0|R|s "
.string "%RL2,%RL1::0:0x01 0xc0|R|s "
.string "%RW2,%RW1:0x66 :0:0x01 0xc0|R|s "
.string "%RB2,%RB1::0:0x00 0xc0|R|s "
.string "%RQ2,ADDR::1:0x01 s|A "
.string "%RL2,ADDR::0:0x01 s|A "
.string "%RW2,ADDR:0x66 :0:0x01 s|A "
.string "%RB2,ADDR::0:0x00 s|A "
.string "ADDR,%RQ2::1:0x03 s|A "
.string "ADDR,%RL2::0:0x03 s|A "
.string "ADDR,%RW2:0x66 :0:0x03 s|A "
.string "ADDR,%RB2::0:0x02 s|A "
.string "$IMM,%RQ1::1:0x83 0xc0|R I "
.string "$IMM,%RQ1::1:0x81 0xc0|R K "
.string "$IMM,%RL1::0:0x83 0xc0|R I "
.string "$IMM,%RL1::0:0x81 0xc0|R k "
.string "$IMM,%RW1:0x66 :0:0x83 0xc0|R I "
.string "$IMM,%RW1:0x66 :0:0x81 0xc0|R j "
.string "$IMM,%RB1::0:0x80 0xc0|R i "
# addq
.string "$IMM,ADDR::1:0x83 0x00|A I "
.string "$IMM,ADDR::1:0x81 0x00|A K "
.string "$IMM,ADDR::0:0x83 0x00|A I "
.string "$IMM,ADDR::0:0x81 0x00|A k "
.string "$IMM,ADDR:0x66 :0:0x83 0x00|A I "
.string "$IMM,ADDR:0x66 :0:0x81 0x00|A j "
.string "$IMM,ADDR::0:0x80 0x00|A i "
# sub
.string "%RQ2,%RQ1::1:0x29 0xc0|R|s "
.string "%RL2,%RL1::0:0x29 0xc0|R|s "
.string "%RW2,%RW1:0x66 :0:0x29 0xc0|R|s "
.string "%RB2,%RB1::0:0x28 0xc0|R|s "
.string "%RQ2,ADDR::1:0x29 s|A "
.string "%RL2,ADDR::0:0x29 s|A "
.string "%RW2,ADDR:0x66 :0:0x29 s|A "
.string "%RB2,ADDR::0:0x28 s|A "
.string "ADDR,%RQ2::1:0x2b s|A "
.string "ADDR,%RL2::0:0x2b s|A "
.string "ADDR,%RW2:0x66 :0:0x2b s|A "
.string "ADDR,%RB2::0:0x2a s|A "
.string "$IMM,%RQ1::1:0x83 0xe8|R I "
.string "$IMM,%RQ1::1:0x81 0xe8|R K "
.string "$IMM,%RL1::0:0x83 0xe8|R I "
.string "$IMM,%RL1::0:0x81 0xe8|R k "
.string "$IMM,%RW1:0x66 :0:0x83 0xe8|R I "
.string "$IMM,%RW1:0x66 :0:0x81 0xe8|R j "
.string "$IMM,%RB1::0:0x80 0xe8|R i "
# subq
.string "$IMM,ADDR::1:0x83 0x28|A I "
.string "$IMM,ADDR::1:0x81 0x28|A K "
.string "$IMM,ADDR::0:0x83 0x28|A I "
.string "$IMM,ADDR::0:0x81 0x28|A k "
.string "$IMM,ADDR:0x66 :0:0x83 0x28|A I "
.string "$IMM,ADDR:0x66 :0:0x81 0x28|A j "
.string "$IMM,ADDR::0:0x80 0x28|A i "
# and
.string "%RQ2,%RQ1::1:0x21 0xc0|R|s "
.string "%RL2,%RL1::0:0x21 0xc0|R|s "
.string "%RW2,%RW1:0x66 :0:0x21 0xc0|R|s "
.string "%RB2,%RB1::0:0x20 0xc0|R|s "
.string "%RQ2,ADDR::1:0x21 s|A "
.string "%RL2,ADDR::0:0x21 s|A "
.string "%RW2,ADDR:0x66 :0:0x21 s|A "
.string "%RB2,ADDR::0:0x20 s|A "
.string "ADDR,%RQ2::1:0x23 s|A "
.string "ADDR,%RL2::0:0x23 s|A "
.string "ADDR,%RW2:0x66 :0:0x23 s|A "
.string "ADDR,%RB2::0:0x22 s|A "
.string "$IMM,%RQ1::1:0x83 0xe0|R I "
.string "$IMM,%RQ1::1:0x81 0xe0|R K "
.string "$IMM,%RL1::0:0x83 0xe0|R I "
.string "$IMM,%RL1::0:0x81 0xe0|R k "
.string "$IMM,%RW1:0x66 :0:0x83 0xe0|R I "
.string "$IMM,%RW1:0x66 :0:0x81 0xe0|R j "
.string "$IMM,%RB1::0:0x80 0xe0|R i "
# andq
.string "$IMM,ADDR::1:0x83 0x20|A I "
.string "$IMM,ADDR::1:0x81 0x20|A K "
.string "$IMM,ADDR::0:0x83 0x20|A I "
.string "$IMM,ADDR::0:0x81 0x20|A k "
.string "$IMM,ADDR:0x66 :0:0x83 0x20|A I "
.string "$IMM,ADDR:0x66 :0:0x81 0x20|A j "
.string "$IMM,ADDR::0:0x80 0x20|A i "
# or
.string "%RQ2,%RQ1::1:0x09 0xc0|R|s "
.string "%RL2,%RL1::0:0x09 0xc0|R|s "
.string "%RW2,%RW1:0x66 :0:0x09 0xc0|R|s "
.string "%RB2,%RB1::0:0x08 0xc0|R|s "
.string "%RQ2,ADDR::1:0x09 s|A "
.string "%RL2,ADDR::0:0x09 s|A "
.string "%RW2,ADDR:0x66 :0:0x09 s|A "
.string "%RB2,ADDR::0:0x08 s|A "
.string "ADDR,%RQ2::1:0x0b s|A "
.string "ADDR,%RL2::0:0x0b s|A "
.string "ADDR,%RW2:0x66 :0:0x0b s|A "
.string "ADDR,%RB2::0:0x0a s|A "
.string "$IMM,%RQ1::1:0x83 0xc8|R I "
.string "$IMM,%RQ1::1:0x81 0xc8|R K "
.string "$IMM,%RL1::0:0x83 0xc8|R I "
.string "$IMM,%RL1::0:0x81 0xc8|R k "
.string "$IMM,%RW1:0x66 :0:0x83 0xc8|R I "
.string "$IMM,%RW1:0x66 :0:0x81 0xc8|R j "
.string "$IMM,%RB1::0:0x80 0xc8|R i "
# orq
.string "$IMM,ADDR::1:0x83 0x08|A I "
.string "$IMM,ADDR::1:0x81 0x08|A K "
.string "$IMM,ADDR::0:0x83 0x08|A I "
.string "$IMM,ADDR::0:0x81 0x08|A k "
.string "$IMM,ADDR:0x66 :0:0x83 0x08|A I "
.string "$IMM,ADDR:0x66 :0:0x81 0x08|A j "
.string "$IMM,ADDR::0:0x80 0x08|A i "
# xor
.string "%RQ2,%RQ1::1:0x31 0xc0|R|s "
.string "%RL2,%RL1::0:0x31 0xc0|R|s "
.string "%RW2,%RW1:0x66 :0:0x31 0xc0|R|s "
.string "%RB2,%RB1::0:0x30 0xc0|R|s "
.string "%RQ2,ADDR::1:0x31 s|A "
.string "%RL2,ADDR::0:0x31 s|A "
.string "%RW2,ADDR:0x66 :0:0x31 s|A "
.string "%RB2,ADDR::0:0x30 s|A "
.string "ADDR,%RQ2::1:0x33 s|A "
.string "ADDR,%RL2::0:0x33 s|A "
.string "ADDR,%RW2:0x66 :0:0x33 s|A "
.string "ADDR,%RB2::0:0x32 s|A "
.string "$IMM,%RQ1::1:0x83 0xf0|R I "
.string "$IMM,%RQ1::1:0x81 0xf0|R K "
.string "$IMM,%RL1::0:0x83 0xf0|R I "
.string "$IMM,%RL1::0:0x81 0xf0|R k "
.string "$IMM,%RW1:0x66 :0:0x83 0xf0|R I "
.string "$IMM,%RW1:0x66 :0:0x81 0xf0|R j "
.string "$IMM,%RB1::0:0x80 0xf0|R i "
# xorq
.string "$IMM,ADDR::1:0x83 0x30|A I "
.string "$IMM,ADDR::1:0x81 0x30|A K "
.string "$IMM,ADDR::0:0x83 0x30|A I "
.string "$IMM,ADDR::0:0x81 0x30|A k "
.string "$IMM,ADDR:0x66 :0:0x83 0x30|A I "
.string "$IMM,ADDR:0x66 :0:0x81 0x30|A j "
.string "$IMM,ADDR::0:0x80 0x30|A i "
# test
.string "%RQ2,%RQ1::1:0x85 0xc0|R|s "
.string "%RL2,%RL1::0:0x85 0xc0|R|s "
.string "%RW2,%RW1:0x66 :0:0x85 0xc0|R|s "
.string "%RB2,%RB1::0:0x84 0xc0|R|s "
.string "%RQ2,ADDR::1:0x85 s|A "
.string "%RL2,ADDR::0:0x85 s|A "
.string "%RW2,ADDR:0x66 :0:0x85 s|A "
.string "%RB2,ADDR::0:0x84 s|A "
.string "ADDR,%RQ2::1:0x85 s|A "
.string "ADDR,%RL2::0:0x85 s|A "
.string "ADDR,%RW2:0x66 :0:0x85 s|A "
.string "ADDR,%RB2::0:0x84 s|A "
.string "$IMM,%RQ1::1:0xf7 0xc0|R K "
.string "$IMM,%RL1::0:0xf7 0xc0|R k "
.string "$IMM,%RW1:0x66 :0:0xf7 0xc0|R j "
.string "$IMM,%RB1::0:0xf6 0xc0|R i "
# testq
.string "$IMM,ADDR::1:0xf7 0x00|A K "
.string "$IMM,ADDR::0:0xf7 0x00|A k "
.string "$IMM,ADDR:0x66 :0:0xf7 0x00|A j "
.string "$IMM,ADDR::0:0xf6 0x00|A i "
# inc
.string "%RQ1::1:0xff 0xc0|R "
.string "%RL1::0:0xff 0xc0|R "
.string "%RW1:0x66 :0:0xff 0xc0|R "
.string "%RB1::0:0xfe 0xc0|R "
.string "ADDR::1:0xff 0x00|A "
.string "ADDR::0:0xff 0x00|A "
.string "ADDR:0x66 :0:0xff 0x00|A "
.string "ADDR::0:0xfe 0x00|A "
# dec
.string "%RQ1::1:0xff 0xc8|R "
.string "%RL1::0:0xff 0xc8|R "
.string "%RW1:0x66 :0:0xff 0xc8|R "
.string "%RB1::0:0xfe 0xc8|R "
.string "ADDR::1:0xff 0x08|A "
.string "ADDR::0:0xff 0x08|A "
.string "ADDR:0x66 :0:0xff 0x08|A "
.string "ADDR::0:0xfe 0x08|A "
# mul
.string "%RQ1::1:0xf7 0xe0|R "
.string "%RL1::0:0xf7 0xe0|R "
.string "%RW1:0x66 :0:0xf7 0xe0|R "
.string "%RB1::0:0xf6 0xe0|R "
.string "ADDR::1:0xf7 0x20|A "
.string "ADDR::0:0xf7 0x20|A "
.string "ADDR:0x66 :0:0xf7 0x20|A "
.string "ADDR::0:0xf6 0x20|A "
# imul
.string "%RQ1::1:0xf7 0xe8|R "
.string "%RL1::0:0xf7 0xe8|R "
.string "%RW1:0x66 :0:0xf7 0xe8|R "
.string "%RB1::0:0xf6 0xe8|R "
.string "ADDR::1:0xf7 0x28|A "
.string "ADDR::0:0xf7 0x28|A "
.string "ADDR:0x66 :0:0xf7 0x28|A "
.string "ADDR::0:0xf6 0x28|A "
# div
.string "%RQ1::1:0xf7 0xf0|R "
.string "%RL1::0:0xf7 0xf0|R "
.string "%RW1:0x66 :0:0xf7 0xf0|R "
.string "%RB1::0:0xf6 0xf0|R "
.string "ADDR::1:0xf7 0x30|A "
.string "ADDR::0:0xf7 0x30|A "
.string "ADDR:0x66 :0:0xf7 0x30|A "
.string "ADDR::0:0xf6 0x30|A "
# idiv
.string "%RQ1::1:0xf7 0xf8|R "
.string "%RL1::0:0xf7 0xf8|R "
.string "%RW1:0x66 :0:0xf7 0xf8|R "
.string "%RB1::0:0xf6 0xf8|R "
.string "ADDR::1:0xf7 0x38|A "
.string "ADDR::0:0xf7 0x38|A "
.string "ADDR:0x66 :0:0xf7 0x38|A "
.string "ADDR::0:0xf6 0x38|A "
# not
.string "%RQ1::1:0xf7 0xd0|R "
.string "%RL1::0:0xf7 0xd0|R "
.string "%RW1:0x66 :0:0xf7 0xd0|R "
.string "%RB1::0:0xf6 0xd0|R "
.string "ADDR::1:0xf7 0x10|A "
.string "ADDR::0:0xf7 0x10|A "
.string "ADDR:0x66 :0:0xf7 0x10|A "
.string "ADDR::0:0xf6 0x10|A "
# neg
.string "%RQ1::1:0xf7 0xd8|R "
.string "%RL1::0:0xf7 0xd8|R "
.string "%RW1:0x66 :0:0xf7 0xd8|R "
.string "%RB1::0:0xf6 0xd8|R "
.string "ADDR::1:0xf7 0x18|A "
.string "ADDR::0:0xf7 0x18|A "
.string "ADDR:0x66 :0:0xf7 0x18|A "
.string "ADDR::0:0xf6 0x18|A "
# shl
.string "$IMM,%RQ1::1:0xc1 0xe0|R i "
.string "$IMM,%RL1::0:0xc1 0xe0|R i "
.string "$IMM,%RW1:0x66 :0:0xc1 0xe0|R i "
.string "$IMM,%RB1::0:0xc0 0xe0|R i "
.string "%CL,%RQ1::1:0xd3 0xe0|R "
.string "%CL,%RL1::0:0xd3 0xe0|R "
.string "%CL,%RW1:0x66 :0:0xd3 0xe0|R "
.string "%CL,%RB1::0:0xd2 0xe0|R "
.string "$IMM,ADDR::1:0xc1 0x20|A i "
.string "%CL,ADDR::1:0xd3 0x20|A "
.string "$IMM,ADDR::0:0xc1 0x20|A i "
.string "%CL,ADDR::0:0xd3 0x20|A "
.string "$IMM,ADDR:0x66 :0:0xc1 0x20|A i "
.string "%CL,ADDR:0x66 :0:0xd3 0x20|A "
.string "$IMM,ADDR::0:0xc0 0x20|A i "
.string "%CL,ADDR::0:0xd2 0x20|A "
# shr
.string "$IMM,%RQ1::1:0xc1 0xe8|R i "
.string "$IMM,%RL1::0:0xc1 0xe8|R i "
.string "$IMM,%RW1:0x66 :0:0xc1 0xe8|R i "
.string "$IMM,%RB1::0:0xc0 0xe8|R i "
.string "%CL,%RQ1::1:0xd3 0xe8|R "
.string "%CL,%RL1::0:0xd3 0xe8|R "
.string "%CL,%RW1:0x66 :0:0xd3 0xe8|R "
.string "%CL,%RB1::0:0xd2 0xe8|R "
.string "$IMM,ADDR::1:0xc1 0x28|A i "
.string "%CL,ADDR::1:0xd3 0x28|A "
.string "$IMM,ADDR::0:0xc1 0x28|A i "
.string "%CL,ADDR::0:0xd3 0x28|A "
.string "$IMM,ADDR:0x66 :0:0xc1 0x28|A i "
.string "%CL,ADDR:0x66 :0:0xd3 0x28|A "
.string "$IMM,ADDR::0:0xc0 0x28|A i "
.string "%CL,ADDR::0:0xd2 0x28|A "
# sar
.string "$IMM,%RQ1::1:0xc1 0xf8|R i "
.string "$IMM,%RL1::0:0xc1 0xf8|R i "
.string "$IMM,%RW1:0x66 :0:0xc1 0xf8|R i "
.string "$IMM,%RB1::0:0xc0 0xf8|R i "
.string "%CL,%RQ1::1:0xd3 0xf8|R "
.string "%CL,%RL1::0:0xd3 0xf8|R "
.string "%CL,%RW1:0x66 :0:0xd3 0xf8|R "
.string "%CL,%RB1::0:0xd2 0xf8|R "
.string "$IMM,ADDR::1:0xc1 0x38|A i "
.string "%CL,ADDR::1:0xd3 0x38|A "
.string "$IMM,ADDR::0:0xc1 0x38|A i "
.string "%CL,ADDR::0:0xd3 0x38|A "
.string "$IMM,ADDR:0x66 :0:0xc1 0x38|A i "
.string "%CL,ADDR:0x66 :0:0xd3 0x38|A "
.string "$IMM,ADDR::0:0xc0 0x38|A i "
.string "%CL,ADDR::0:0xd2 0x38|A "
# xchg
.string "%RQ2,%RQ1::1:0x87 0xc0|R|s "
.string "%RL2,%RL1::0:0x87 0xc0|R|s "
.string "%RW2,%RW1:0x66 :0:0x87 0xc0|R|s "
.string "%RB2,%RB1::0:0x86 0xc0|R|s "
.string "%RQ2,ADDR::1:0x87 0x00|R|s "
.string "%RL2,ADDR::0:0x87 0x00|R|s "
.string "%RW2,ADDR:0x66 :0:0x87 0x00|R|s "
.string "%RB2,ADDR::0:0x86 0x00|R|s "
# lea
.string "ADDR,%RQ2::1:0x8d s|A "
.string "ADDR,%RL2::0:0x8d s|A "
.string "ADDR,%RW2:0x66 :0:0x8d s|A "
# movx
.string "%RB1,%RW2:0x66 :0:0x0f 0xb6 0xc0|R|s "
.string "ADDR,%RW2:0x66 :0:0x0f 0xb6 s|A "
.string "%RB1,%RL2::0:0x0f 0xb6 0xc0|R|s "
.string "ADDR,%RL2::0:0x0f 0xb6 s|A "
.string "%RB1,%RQ2::1:0x0f 0xb6 0xc0|R|s "
.string "ADDR,%RQ2::1:0x0f 0xb6 s|A "
.string "%RW1,%RL2::0:0x0f 0xb7 0xc0|R|s "
.string "ADDR,%RL2::0:0x0f 0xb7 s|A "
.string "%RW1,%RQ2::1:0x0f 0xb7 0xc0|R|s "
.string "ADDR,%RQ2::1:0x0f 0xb7 s|A "
.string "%RB1,%RW2:0x66 :0:0x0f 0xbe 0xc0|R|s "
.string "ADDR,%RW2:0x66 :0:0x0f 0xbe s|A "
.string "%RB1,%RL2::0:0x0f 0xbe 0xc0|R|s "
.string "ADDR,%RL2::0:0x0f 0xbe s|A "
.string "%RB1,%RQ2::1:0x0f 0xbe 0xc0|R|s "
.string "ADDR,%RQ2::1:0x0f 0xbe s|A "
.string "%RW1,%RL2::0:0x0f 0xbf 0xc0|R|s "
.string "ADDR,%RL2::0:0x0f 0xbf s|A "
.string "%RW1,%RQ2::1:0x0f 0xbf 0xc0|R|s "
.string "ADDR,%RQ2::1:0x0f 0xbf s|A "
.string "%RL1,%RQ2::1:0x63 0xc0|R|s "
.string "ADDR,%RQ2::1:0x63 s|A "
# movups
.string "%RX1,%RX2::0:0x0f 0x10 0xc0|R|s "
.string "ADDR,%RX2::0:0x0f 0x10 s|A "
.string "%RX2,ADDR::0:0x0f 0x11 s|A "
# movss
.string "%RX1,%RX2:0xf3 :0:0x0f 0x10 0xc0|R|s "
.string "ADDR,%RX2:0xf3 :0:0x0f 0x10 s|A "
.string "%RX2,ADDR:0xf3 :0:0x0f 0x11 s|A "
# movsd
.string "%RX1,%RX2:0xf2 :0:0x0f 0x10 0xc0|R|s "
.string "ADDR,%RX2:0xf2 :0:0x0f 0x10 s|A "
.string "%RX2,ADDR:0xf2 :0:0x0f 0x11 s|A "
# addss
.string "%RX1,%RX2:0xf3 :0:0x0f 0x58 0xc0|R|s "
.string "ADDR,%RX2:0xf3 :0:0x0f 0x58 s|A "
# addsd
.string "%RX1,%RX2:0xf2 :0:0x0f 0x58 0xc0|R|s "
.string "ADDR,%RX2:0xf2 :0:0x0f 0x58 s|A "
# subss
.string "%RX1,%RX2:0xf3 :0:0x0f 0x5c 0xc0|R|s "
.string "ADDR,%RX2:0xf3 :0:0x0f 0x5c s|A "
# subsd
.string "%RX1,%RX2:0xf2 :0:0x0f 0x5c 0xc0|R|s "
.string "ADDR,%RX2:0xf2 :0:0x0f 0x5c s|A "
# mulss
.string "%RX1,%RX2:0xf3 :0:0x0f 0x59 0xc0|R|s "
.string "ADDR,%RX2:0xf3 :0:0x0f 0x59 s|A "
# mulsd
.string "%RX1,%RX2:0xf2 :0:0x0f 0x59 0xc0|R|s "
.string "ADDR,%RX2:0xf2 :0:0x0f 0x59 s|A "
# divss
.string "%RX1,%RX2:0xf3 :0:0x0f 0x5e 0xc0|R|s "
.string "ADDR,%RX2:0xf3 :0:0x0f 0x5e s|A "
# divsd
.string "%RX1,%RX2:0xf2 :0:0x0f 0x5e 0xc0|R|s "
.string "ADDR,%RX2:0xf2 :0:0x0f 0x5e s|A "
# comiss
.string "%RX1,%RX2::0:0x0f 0x2f 0xc0|R|s "
# comisd
.string "%RX1,%RX2:0x66 :0:0x0f 0x2f 0xc0|R|s "
# addps
.string "%RX1,%RX2::0:0x0f 0x58 0xc0|R|s "
.string "ADDR,%RX2::0:0x0f 0x58 s|A "
# addpd
.string "%RX1,%RX2:0x66 :0:0x0f 0x58 0xc0|R|s "
.string "ADDR,%RX2:0x66 :0:0x0f 0x58 s|A "
# subps
.string "%RX1,%RX2::0:0x0f 0x5c 0xc0|R|s "
.string "ADDR,%RX2::0:0x0f 0x5c s|A "
# subpd
.string "%RX1,%RX2:0x66 :0:0x0f 0x5c 0xc0|R|s "
.string "ADDR,%RX2:0x66 :0:0x0f 0x5c s|A "
# mulps
.string "%RX1,%RX2::0:0x0f 0x59 0xc0|R|s "
.string "ADDR,%RX2::0:0x0f 0x59 s|A "
# mulpd
.string "%RX1,%RX2:0x66 :0:0x0f 0x59 0xc0|R|s "
.string "ADDR,%RX2:0x66 :0:0x0f 0x59 s|A "
# divps
.string "%RX1,%RX2::0:0x0f 0x5e 0xc0|R|s "
.string "ADDR,%RX2::0:0x0f 0x5e s|A "
# divpd
.string "%RX1,%RX2:0x66 :0:0x0f 0x5e 0xc0|R|s "
.string "ADDR,%RX2:0x66 :0:0x0f 0x5e s|A "
# shufps
.string "$IMM,%RX1,%RX2::0:0x0f 0xc6 0xc0|R|s i "
# cvtss2sd
.string "%RX1,%RX2:0xf3 :0:0x0f 0x5a 0xc0|R|s "
# cvtsd2ss
.string "%RX1,%RX2:0xf2 :0:0x0f 0x5a 0xc0|R|s "
# cvtsi2ss
.string "%RL1,%RX2:0xf3 :0:0x0f 0x2a 0xc0|R|s "
.string "%RQ1,%RX2:0xf3 :1:0x0f 0x2a 0xc0|R|s "
# cvtsi2sd
.string "%RL1,%RX2:0xf2 :0:0x0f 0x2a 0xc0|R|s "
.string "%RQ1,%RX2:0xf2 :1:0x0f 0x2a 0xc0|R|s "
# cvtss2si
.string "%RX1,%RL2:0xf3 :0:0x0f 0x2d 0xc0|R|s "
.string "%RX1,%RQ2:0xf3 :1:0x0f 0x2d 0xc0|R|s "
# cvtsd2si
.string "%RX1,%RL2:0xf2 :0:0x0f 0x2d 0xc0|R|s "
.string "%RX1,%RQ2:0xf2 :1:0x0f 0x2d 0xc0|R|s "

.byte 0

.align 2
@ins_offsets
.long 3
.long 1
.long 1
.long 1
.long 18
# movq
.long 3
# movw
.long 1
.long 1
.long 1
# movd
.long 2
# jmp
.long 4
# call
.long 3
# ret
.long 1
# jC
.long 2
.long 2
.long 2
.long 2
.long 2
.long 2
.long 2
.long 2
.long 2
.long 2
# cmp
.long 19
# cmpq
.long 2
.long 2
.long 2
.long 1
# add
.long 19
# addq
.long 2
.long 2
.long 2
.long 1
# sub
.long 19
# subq
.long 2
.long 2
.long 2
.long 1
# and
.long 19
# andq
.long 2
.long 2
.long 2
.long 1
# or
.long 19
# orq
.long 2
.long 2
.long 2
.long 1
# xor
.long 19
# xorq
.long 2
.long 2
.long 2
.long 1
# test
.long 16
# testq
.long 1
.long 1
.long 1
.long 1
# inc
.long 4
.long 1
.long 1
.long 1
.long 1
# dec
.long 4
.long 1
.long 1
.long 1
.long 1
# mul
.long 4
.long 1
.long 1
.long 1
.long 1
# imul
.long 4
.long 1
.long 1
.long 1
.long 1
# div
.long 4
.long 1
.long 1
.long 1
.long 1
# idiv
.long 4
.long 1
.long 1
.long 1
.long 1
# not
.long 4
.long 1
.long 1
.long 1
.long 1
# neg
.long 4
.long 1
.long 1
.long 1
.long 1
# shl
.long 8
.long 2
.long 2
.long 2
.long 2
# shr
.long 8
.long 2
.long 2
.long 2
.long 2
# sar
.long 8
.long 2
.long 2
.long 2
.long 2
# xchg
.long 8
# lea
.long 3
# movx
.long 2
.long 2
.long 2
.long 2
.long 2
.long 2
.long 2
.long 2
.long 2
.long 2
.long 2
# movups
.long 3
# movss
.long 3
# movsd
.long 3
# addss
.long 2
# addsd
.long 2
# subss
.long 2
# subsd
.long 2
# mulss
.long 2
# mulsd
.long 2
# divss
.long 2
# divsd
.long 2
# comiss
.long 1
# comisd
.long 1
# addps
.long 2
# addpd
.long 2
# subps
.long 2
# subpd
.long 2
# mulps
.long 2
# mulpd
.long 2
# divps
.long 2
# divpd
.long 2
# shufps
.long 1
# cvtss2sd
.long 1
# cvtsd2ss
.long 1
# cvtsi2ss
.long 2
# cvtsi2sd
.long 2
# cvtss2si
.long 2
# cvtsd2si
.long 2

@dllcall_emit
.byte 0xb8,0x00,0x00,0x00,0x00,0xff,0x10

@mzdosheader
.quad 0x0000000300905a4d,0x0000ffff00000004
.quad 0x00000000000000b8,0x0000000000000040
.quad 0,0
.quad 0,0x0000008000000000
.quad 0xcd09b4000eba1f0e,0x685421cd4c01b821
.quad 0x72676f7270207369,0x6f6e6e6163206d61
.quad 0x6e75722065622074,0x20534f44206e6920
.quad 0x0a0d0d2e65646f6d,0x0000000000000024

@peheader
.long 0x4550,0x8664,0,0
.long 0,0x2300f0,0x20b,0
.long 0,0,0,0
.long 0x400000,0,0x1000,0x200
.long 0x4,0,0x20005,0
.long 0,0x200,0,0x2
.quad 0x400000,0x1000,0x100000,0x1000
.long 0,16
.quad 0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0
.quad 0,0,0,0,0
.quad 0,0,0,0,0
.quad 0,0,0,0,0
@peheader_end

@exit
sub $40,%rsp
mov $@exit_msg,%rcx
.dllcall "msvcrt.dll" "puts"
.dllcall "msvcrt.dll" "_getch"
xor %ecx,%ecx
.dllcall "msvcrt.dll" "exit"

@error
sub $40,%rsp
mov @_$DATA+32,%rdx
test %rdx,%rdx
je @error_noline
mov 48(%rdx),%rdx
@error_noline
.dllcall "msvcrt.dll" "printf"
call @exit

@nomem
mov $@err_nomem,%rcx
call @error
@openfailure
mov $@err_openfailure,%rcx
call @error
@inserr
mov $@err_inserr,%rcx
call @error

# string structure
# 0:8 -- length (N)
# 8:N -- contents
# 8+N:1 -- zero byte
@string_append_ch
push %r12
push %r13
sub $40,%rsp
mov %rdx,%r13
test %rcx,%rcx
jne @string_append_ch_empty
mov $9,%rcx
.dllcall "msvcrt.dll" "malloc"
test %rax,%rax
je @nomem
movq $0,(%rax)
movb $0,8(%rax)
mov %rax,%rcx
@string_append_ch_empty
mov %rcx,%r12
mov (%rcx),%rcx
inc %rcx
mov %rcx,(%r12)
lea 9(%rcx),%rdx
mov %r12,%rcx
.dllcall "msvcrt.dll" "realloc"
test %rax,%rax
je @nomem
mov (%rax),%rcx
mov %r13b,7(%rax,%rcx,1)
movb $0,8(%rax,%rcx,1)
add $40,%rsp
pop %r13
pop %r12
ret

@input_str
push %r12
sub $32,%rsp
xor %r12,%r12
@input_str_loop
.dllcall "msvcrt.dll" "getchar"
cmp $-1,%eax
je @input_str_end
cmp $10,%eax
je @input_str_end
mov %eax,%edx
mov %r12,%rcx
call @string_append_ch
mov %rax,%r12
jmp @input_str_loop
@input_str_end
mov %r12,%rax
add $32,%rsp
pop %r12
ret

@dup_str
push %r12
push %r13
sub $40,%rsp
xor %r12,%r12
mov %rcx,%r13
@dup_str_loop
mov (%r13),%dl
cmp $0,%dl
je @dup_str_end
mov %r12,%rcx
call @string_append_ch
mov %rax,%r12
inc %r13
jmp @dup_str_loop
@dup_str_end
mov %r12,%rax
add $40,%rsp
pop %r13
pop %r12
ret

@request_file
# arg1: msg
# arg2: mode
push %r12
sub $32,%rsp
mov %rdx,%r12
.dllcall "msvcrt.dll" "printf"
call @input_str
mov %r12,%rdx
mov %rax,%r12
lea 8(%r12),%rcx
.dllcall "msvcrt.dll" "fopen"
test %rax,%rax
je @openfailure
mov %r12,%rcx
mov %rax,%r12
.dllcall "msvcrt.dll" "free"
mov %r12,%rax
add $32,%rsp
pop %r12
ret

# line structure
# 0:8 -- next
# 8:8 -- flags: 1 -- needs recompiling
# 16:8 -- line content (pointer to string structure)
# 24:8 -- machine code (pointer to string structure)
# 32:8 -- DLL name (pointer to string structure)
# 40:8 -- DLL function name (pointer to string structure)
# 48:8 -- line number
# 56:72 -- reserved for future use

@load_line
push %r12
sub $32,%rsp
mov $128,%ecx
mov $1,%edx
.dllcall "msvcrt.dll" "calloc"
test %rax,%rax
je @nomem
mov %rax,%r12
@load_line_loop
mov @_$DATA+0,%rcx
.dllcall "msvcrt.dll" "fgetc"
cmp $-1,%eax
je @load_line_end
cmp $10,%eax
je @load_line_end2
test %eax,%eax
jne @load_line_zero_byte
mov $32,%al
@load_line_zero_byte
mov %eax,%edx
mov 16(%r12),%rcx
call @string_append_ch
mov %rax,16(%r12)
jmp @load_line_loop
@load_line_end
cmpq $0,16(%r12)
jne @load_line_end2
mov %r12,%rcx
.dllcall "msvcrt.dll" "free"
xor %r12d,%r12d
@load_line_end2
mov %r12,%rax
add $32,%rsp
pop %r12
ret

@load_file
push %r12
push %r13
sub $40,%rsp
xor %r12d,%r12d
xor %r13d,%r13d
@load_file_loop
call @load_line
test %rax,%rax
je @load_file_end
inc %r13
mov %r13,48(%rax)
test %r12,%r12
je @load_file_first
mov %rax,(%r12)
jmp @load_file_cond_end
@load_file_first
mov %rax,@_$DATA+16
@load_file_cond_end
mov %rax,%r12
jmp @load_file_loop
@load_file_end
add $40,%rsp
pop %r13
pop %r12
ret

@is_space
# %al -- character
# %eax (return) -- 1 if is space, 0 otherwise
cmp $32,%al
je @is_space_yes
cmp $'\n',%al
je @is_space_yes
cmp $'\t',%al
je @is_space_yes
cmp $'\v',%al
je @is_space_yes
cmp $'\r',%al
je @is_space_yes
xor %eax,%eax
ret
@is_space_yes
mov $1,%eax
ret

@is_valid_id
cmp $'_',%al
je @is_valid_id_yes
sub $'0',%al
jb @is_valid_id_no
cmp $9,%al
jbe @is_valid_id_yes
sub $'A'-'0',%al
jb @is_valid_id_no
cmp $25,%al
jbe @is_valid_id_yes
sub $'a'-'A',%al
jb @is_valid_id_no
cmp $25,%al
jbe @is_valid_id_yes
@is_valid_id_no
xor %eax,%eax
ret
@is_valid_id_yes
mov $1,%eax
ret

@skip_spaces
# arg1 -- pointer to string pointer
push %rax
push %r12
mov (%rcx),%r12
@skip_spaces_loop
mov (%r12),%al
call @is_space
test %eax,%eax
je @skip_spaces_loop_end
inc %r12
jmp @skip_spaces_loop
@skip_spaces_loop_end
mov %r12,(%rcx)
pop %r12
pop %rax
ret

@read_ch
# arg1 -- pointer to string pointer
# arg2 -- target character
push %rax
push %rbx
call @skip_spaces
mov (%rcx),%rbx
mov (%rbx),%al
cmp %dl,%al
jne @read_ch_end
incq (%rcx)
@read_ch_end
pop %rbx
pop %rax
ret

@read_str
# arg1 -- pointer to string pointer
# arg2 -- target string
push %rax
push %rbx
push %rdx
call @skip_spaces
mov (%rcx),%rbx
@read_str_loop
mov (%rbx),%al
mov (%rdx),%ah
cmp $0,%ah
je @read_str_end2
cmp %ah,%al
jne @read_str_end
inc %rbx
inc %rdx
jmp @read_str_loop
@read_str_end2
mov %rbx,(%rcx)
@read_str_end
pop %rdx
pop %rbx
pop %rax

@hexdig_to_value
# %al -- digit
# %eax (return) -- value
sub $'0',%al
jb @hexdig_to_value_error
cmp $9,%al
jbe @hexdig_to_value_end
sub $'A'-'0',%al
jb @hexdig_to_value_error
cmp $5,%al
jbe @hexdig_to_value_X1
sub $'a'-'A',%al
jb @hexdig_to_value_error
cmp $5,%al
ja @hexdig_to_value_error
@hexdig_to_value_X1
add $10,%al
@hexdig_to_value_end
movzbl %al,%eax
ret
@hexdig_to_value_error
mov $0xffffffff,%eax
ret

@read_char
# arg1 -- pointer to string pointer
# return -- character value
sub $40,%rsp
mov (%rcx),%rdx
cmpb $'\\',(%rdx)
jne @read_char_simple
inc %rdx
cmpb $'\'',(%rdx)
jne @read_char_X1
mov $'\'',%al
inc %rdx
jmp @read_char_end
@read_char_X1
cmpb $'\"',(%rdx)
jne @read_char_X2
mov $'\"',%al
inc %rdx
jmp @read_char_end
@read_char_X2
cmpb $'n',(%rdx)
jne @read_char_X3
mov $'\n',%al
inc %rdx
jmp @read_char_end
@read_char_X3
cmpb $'t',(%rdx)
jne @read_char_X4
mov $'\t',%al
inc %rdx
jmp @read_char_end
@read_char_X4
cmpb $'v',(%rdx)
jne @read_char_X5
mov $'\v',%al
inc %rdx
jmp @read_char_end
@read_char_X5
cmpb $'r',(%rdx)
jne @read_char_X6
mov $'\r',%al
inc %rdx
jmp @read_char_end
@read_char_X6
cmpb $'\\',(%rdx)
jne @read_char_X7
mov $'\\',%al
inc %rdx
jmp @read_char_end
@read_char_X7
cmpb $'x',(%rdx)
je @read_char_noerror
mov $@err_badchar,%rcx
call @error
@read_char_noerror
xor %esi,%esi
@read_char_loop
inc %rdx
mov (%rdx),%al
call @hexdig_to_value
cmp $0xffffffff,%eax
je @read_char_end2
shl $4,%esi
or %eax,%esi
jmp @read_char_loop
@read_char_end2
mov %esi,%eax
cmp $256,%eax
jb @read_char_end
mov $@err_badchar,%rcx
call @error
@read_char_end
mov %rdx,(%rcx)
add $40,%rsp
ret

@read_char_simple
mov (%rdx),%al
incq (%rcx)
add $40,%rsp
ret

@notstring
mov $@err_notstring,%rcx
call @error

@read_cstring
# arg1 -- pointer to string pointer
# return -- pointer to string structure
push %r12
push %r13
push %rcx
sub $48,%rsp
call @skip_spaces
mov (%rcx),%rsi
cmpb $'\"',(%rsi)
jne @notstring
mov %rcx,%r13
inc %rsi
mov %rsi,32(%rsp)
xor %r12d,%r12d
@read_cstring_loop
lea 32(%rsp),%rcx
mov (%rcx),%rsi
cmpb $0,(%rsi)
je @notstring
cmpb $'\"',(%rsi)
je @read_cstring_loop_end
call @read_char
mov %al,%dl
mov %r12,%rcx
call @string_append_ch
mov %rax,%r12
jmp @read_cstring_loop
@read_cstring_loop_end
inc %rsi
mov %rsi,(%r13)
mov %r12,%rax
add $48,%rsp
pop %rcx
pop %r13
pop %r12
ret

@read_number
# arg1 -- pointer to string pointer
# return -- value
push %r12
sub $32,%rsp
mov %rcx,%r12
mov (%rcx),%rcx
mov (%rcx),%al
cmp $'\'',%al
je @read_number_char
sub $'0',%al
cmp $9,%al
ja @read_number_end
cmp $0,%al
jne @read_number_dec
cmpb $'x',1(%rcx)
je @read_number_hex
xor %edx,%edx
@read_number_oct_loop
inc %rcx
mov (%rcx),%al
sub $'0',%al
cmp $7,%al
ja @read_number_oct_end
shl $3,%rdx
or %al,%dl
jmp @read_number_oct_loop
@read_number_oct_end
mov %rcx,(%r12)
mov %rdx,%rax
jmp @read_number_end
@read_number_char
incq (%r12)
mov %r12,%rcx
call @read_char
movzbl %al,%eax
mov (%r12),%rcx
cmpb $'\'',(%rcx)
je @read_number_char_noerror
mov $@err_badchar,%rcx
call @error
@read_number_char_noerror
incq (%r12)
jmp @read_number_end
@read_number_dec
movzbl %al,%eax
@read_number_dec_loop
inc %rcx
mov (%rcx),%dl
sub $'0',%dl
cmp $9,%dl
ja @read_number_dec_end
movzbl %dl,%edx
lea (%rax,%rax,4),%rax
shl $1,%rax
add %rdx,%rax
jmp @read_number_dec_loop
@read_number_dec_end
mov %rcx,(%r12)
jmp @read_number_end
@read_number_hex
xor %edx,%edx
inc %rcx
@read_number_hex_loop
inc %rcx
mov (%rcx),%al
call @hexdig_to_value
cmp $0xffffffff,%eax
je @read_number_hex_end
shl $4,%rdx
or %al,%dl
jmp @read_number_hex_loop
@read_number_hex_end
mov %rcx,(%r12)
mov %rdx,%rax
@read_number_end
add $32,%rsp
pop %r12
ret

@read_single_constant
# arg1 -- pointer to string pointer
# return -- value
push %r12
push %r13
sub $40,%rsp
mov %rcx,%r12
mov (%rcx),%r13
call @read_number
cmp %r13,(%r12)
jne @read_single_constant_end
mov %r12,%rcx
mov $@dataseg_str,%rdx
mov (%rcx),%r13
call @read_str
cmp %r13,(%r12)
je @read_single_constant_label
mov @_$DATA+32,%rax
orb $1,8(%rax)
mov @_$DATA+96,%rax
add $0x400000,%rax
jmp @read_single_constant_end
@read_single_constant_label
mov (%r12),%rcx
cmpb $'@',(%rcx)
jne @read_single_constant_end
inc %rcx
call @label_string_htab_get
test %rax,%rax
je @read_single_constant_label_end
mov 16(%rax),%rax
@read_single_constant_label_end
push %rax
mov (%r12),%rcx
@read_single_constant_label_skip
inc %rcx
mov (%rcx),%al
call @is_valid_id
test %eax,%eax
jne @read_single_constant_label_skip
pop %rax
mov %rcx,(%r12)
@read_single_constant_end
add $40,%rsp
pop %r13
pop %r12
ret

@read_constant
# arg1 -- pointer to string pointer
# return -- value
push %r12
push %r13
push %r14
push %r15
sub $40,%rsp
xor %r14d,%r14d
xor %r15d,%r15d
mov %rcx,%r12
mov (%rcx),%rcx
cmpb $'-',(%rcx)
jne @read_constant_neg
inc %r15d
incq (%r12)
@read_constant_neg
@read_constant_loop
mov %r12,%rcx
call @skip_spaces
mov %r12,%rcx
mov (%rcx),%r13
call @read_single_constant
cmp %r13,(%r12)
je @read_constant_end
test %r15d,%r15d
je @read_constant_X2
sub %rax,%r14
jmp @read_constant_X3
@read_constant_X2
add %rax,%r14
@read_constant_X3
mov %r12,%rcx
call @skip_spaces
mov (%r12),%rcx
xor %r15d,%r15d
cmpb $'+',(%rcx)
je @read_constant_X1
cmpb $'-',(%rcx)
jne @read_constant_end
inc %r15d
@read_constant_X1
incq (%r12)
jmp @read_constant_loop
@read_constant_end
mov %r14,%rax
add $40,%rsp
pop %r15
pop %r14
pop %r13
pop %r12
ret

@string_array_get_index
# arg1 -- pointer to string pointer
# arg2 -- string array
# return -- string index
push %r12
push %r13
push %r14
push %r15
sub $40,%rsp
mov %rcx,%r12
mov %rdx,%r13
xor %r14d,%r14d
mov (%rcx),%rax
mov %rax,32(%rsp)
@string_array_get_index_loop
mov %r13,%rcx
.dllcall "msvcrt.dll" "strlen"
test %rax,%rax
je @string_array_get_index_loop_err
mov %rax,%r15
mov %rax,%r8
mov (%r12),%rcx
mov %r13,%rdx
.dllcall "msvcrt.dll" "strncmp"
test %rax,%rax
je @string_array_get_index_loop_success
inc %r14d
add %r15,%r13
inc %r13
jmp @string_array_get_index_loop
@string_array_get_index_loop_err
mov 32(%rsp),%rax
mov %rax,(%r12)
mov $0xffffffff,%eax
jmp @string_array_get_index_loop_end
@string_array_get_index_loop_success
mov %r14d,%eax
add %r15,(%r12)
@string_array_get_index_loop_end
add $40,%rsp
pop %r15
pop %r14
pop %r13
pop %r12
ret

@get_reg8
sub $40,%rsp
mov $@reg8,%rdx
call @string_array_get_index
cmp $8,%eax
jge @get_reg8_X1
cmp $4,%eax
jl @get_reg8_X1
add $24,%eax
jmp @get_reg8_end
@get_reg8_X1
cmp $16,%eax
jl @get_reg8_end
add $4,%eax
@get_reg8_end
add $40,%rsp
ret

@get_reg16
sub $40,%rsp
mov $@reg16,%rdx
call @string_array_get_index
add $40,%rsp
ret

@get_reg32
sub $40,%rsp
mov $@reg32,%rdx
call @string_array_get_index
cmp $16,%eax
jl @get_reg32_end
add $8,%eax
@get_reg32_end
add $40,%rsp
ret

@get_reg64
sub $40,%rsp
mov $@reg64,%rdx
call @string_array_get_index
cmp $16,%eax
jl @get_reg64_end
add $8,%eax
@get_reg64_end
add $40,%rsp
ret

@get_regxmm
sub $40,%rsp
mov $@regxmm,%rdx
call @string_array_get_index
cmp $0,%eax
jl @get_regxmm_end2
cmp $6,%eax
jae @get_regxmm_end
add $16,%eax
@get_regxmm_end
sub $6,%eax
@get_regxmm_end2
add $40,%rsp
ret

# ins structure
# 0:2 -- reg1
# 2:2 -- reg2
# 4:2 -- index
# 6:2 -- scaler
# 8:8 -- offset
# 16:8 -- imm
# 30:1 -- valid regs
@get_addr
# arg1 -- pointer to string pointer
# arg2 -- ins structure
push %r12
push %r13
push %r14
sub $32,%rsp
mov %rcx,%r12
mov %rdx,%r13
mov (%rcx),%r14
call @read_constant
cmp %r14,(%r12)
je @get_addr_no_offset
mov %rax,8(%r13)
@get_addr_no_offset
mov (%r12),%rax
cmpb $0x28,(%rax)
jne @get_addr_end
inc %rax
mov %rax,(%r12)
cmpb $0x2c,(%rax)
je @get_addr_index
cmpb $0x25,(%rax)
jne @inserr
incq (%r12)
mov %r12,%rcx
call @get_reg64
cmp $0,%eax
jl @inserr
mov %ax,(%r13)
orb $1,30(%r13)
mov (%r12),%rax
cmpb $0x29,(%rax)
je @get_addr_end2
cmpb $0x2c,(%rax)
jne @inserr
@get_addr_index
cmpb $0x25,1(%rax)
jne @inserr
add $2,%rax
mov %rax,(%r12)
mov %r12,%rcx
call @get_reg64
cmp $0,%eax
jl @inserr
mov %ax,4(%r13)
orb $2,30(%r13)
mov (%r12),%rax
cmpb $0x2c,(%rax)
jne @inserr
inc %rax
cmpb $'1',(%rax)
jne @get_addr_scale1
movw $0,6(%r13)
jmp @get_addr_scale_end
@get_addr_scale1
cmpb $'2',(%rax)
jne @get_addr_scale2
movw $1,6(%r13)
jmp @get_addr_scale_end
@get_addr_scale2
cmpb $'4',(%rax)
jne @get_addr_scale4
movw $2,6(%r13)
jmp @get_addr_scale_end
@get_addr_scale4
cmpb $'8',(%rax)
jne @inserr
movw $3,6(%r13)
@get_addr_scale_end
inc %rax
cmpb $0x29,(%rax)
jne @inserr
@get_addr_end2
inc %rax
@get_addr_end
mov %rax,(%r12)
add $32,%rsp
pop %r14
pop %r13
pop %r12
ret

@emit_code
# arg1 -- ptr
# arg2 -- size
push %r12
push %r13
push %r14
sub $32,%rsp
mov %rcx,%r12
mov %rdx,%r13
xor %r14d,%r14d
@emit_loop
mov (%r12,%r14,1),%dl
mov @_$DATA+32,%rcx
mov 24(%rcx),%rcx
call @string_append_ch
mov @_$DATA+32,%rcx
mov %rax,24(%rcx)
inc %r14
dec %r13
jne @emit_loop
add $32,%rsp
pop %r14
pop %r13
pop %r12
ret

@fill_code_zero
# arg2 -- size
push %r13
sub $32,%rsp
mov %rcx,%r13
@emit_zero_loop
mov $0,%dl
mov @_$DATA+32,%rcx
mov 24(%rcx),%rcx
call @string_append_ch
mov @_$DATA+32,%rcx
mov %rax,24(%rcx)
dec %r13
jne @emit_zero_loop
add $32,%rsp
pop %r13
ret

@pad_image
sub $40,%rsp
@emit_loop3
mov @_$DATA+64,%rcx
testw $0x1ff,(%rcx)
je @emit_loop3_end
mov $0,%dl
mov @_$DATA+64,%rcx
call @string_append_ch
mov %rax,@_$DATA+64
jmp @emit_loop3
@emit_loop3_end
add $40,%rsp
ret

@write_image
# arg1 -- ptr
# arg2 -- size
push %r12
push %r13
push %r14
sub $32,%rsp
mov %rcx,%r12
mov %rdx,%r13
xor %r14d,%r14d
@emit_loop4
mov (%r12,%r14,1),%dl
mov @_$DATA+64,%rcx
call @string_append_ch
mov %rax,@_$DATA+64
inc %r14
dec %r13
jne @emit_loop4
add $32,%rsp
pop %r14
pop %r13
pop %r12
ret


@handle_pseudo_op
push %r12
sub $48,%rsp
mov @_$DATA+32,%rax
mov 16(%rax),%r12
add $8,%r12
lea 32(%rsp),%rcx
mov %r12,(%rcx)
mov $@pseudo_dllcall,%rdx
call @read_str
cmp %r12,32(%rsp)
je @pseudo_not_dllcall
call @read_cstring
mov @_$DATA+32,%rsi
mov %rax,32(%rsi)
call @read_cstring
mov @_$DATA+32,%rsi
mov %rax,40(%rsi)
mov $@dllcall_emit,%rcx
mov $7,%edx
call @emit_code
jmp @pseudo_end
@pseudo_not_dllcall

lea 32(%rsp),%rcx
mov $@pseudo_byte,%rdx
call @read_str
cmp %r12,32(%rsp)
je @pseudo_not_byte
@pseudo_byte_loop
lea 32(%rsp),%rcx
call @skip_spaces
lea 32(%rsp),%rcx
mov (%rcx),%r12
call @read_constant
mov %rax,40(%rsp)
cmp %r12,32(%rsp)
je @pseudo_end
lea 40(%rsp),%rcx
mov $1,%edx
call @emit_code
lea 32(%rsp),%rcx
call @skip_spaces
mov 32(%rsp),%rax
cmpb $0x2c,(%rax)
jne @pseudo_end
incq 32(%rsp)
jmp @pseudo_byte_loop
@pseudo_not_byte

lea 32(%rsp),%rcx
mov $@pseudo_word,%rdx
call @read_str
cmp %r12,32(%rsp)
je @pseudo_not_word
@pseudo_word_loop
lea 32(%rsp),%rcx
call @skip_spaces
lea 32(%rsp),%rcx
mov (%rcx),%r12
call @read_constant
mov %rax,40(%rsp)
cmp %r12,32(%rsp)
je @pseudo_end
lea 40(%rsp),%rcx
mov $2,%edx
call @emit_code
lea 32(%rsp),%rcx
call @skip_spaces
mov 32(%rsp),%rax
cmpb $0x2c,(%rax)
jne @pseudo_end
incq 32(%rsp)
jmp @pseudo_word_loop
@pseudo_not_word

lea 32(%rsp),%rcx
mov $@pseudo_long,%rdx
call @read_str
cmp %r12,32(%rsp)
je @pseudo_not_long
@pseudo_long_loop
lea 32(%rsp),%rcx
call @skip_spaces
lea 32(%rsp),%rcx
mov (%rcx),%r12
call @read_constant
mov %rax,40(%rsp)
cmp %r12,32(%rsp)
je @pseudo_end
lea 40(%rsp),%rcx
mov $4,%edx
call @emit_code
lea 32(%rsp),%rcx
call @skip_spaces
mov 32(%rsp),%rax
cmpb $0x2c,(%rax)
jne @pseudo_end
incq 32(%rsp)
jmp @pseudo_long_loop
@pseudo_not_long

lea 32(%rsp),%rcx
mov $@pseudo_quad,%rdx
call @read_str
cmp %r12,32(%rsp)
je @pseudo_not_quad
@pseudo_quad_loop
lea 32(%rsp),%rcx
call @skip_spaces
lea 32(%rsp),%rcx
mov (%rcx),%r12
call @read_constant
mov %rax,40(%rsp)
cmp %r12,32(%rsp)
je @pseudo_end
lea 40(%rsp),%rcx
mov $8,%edx
call @emit_code
lea 32(%rsp),%rcx
call @skip_spaces
mov 32(%rsp),%rax
cmpb $0x2c,(%rax)
jne @pseudo_end
incq 32(%rsp)
jmp @pseudo_quad_loop
@pseudo_not_quad

lea 32(%rsp),%rcx
mov $@pseudo_string,%rdx
call @read_str
cmp %r12,32(%rsp)
je @pseudo_not_string
lea 32(%rsp),%rcx
call @read_cstring
mov %rax,%rcx
mov %rax,%r12
mov (%rcx),%rdx
inc %rdx
add $8,%rcx
call @emit_code
mov %r12,%rcx
.dllcall "msvcrt.dll" "free"
jmp @pseudo_end
@pseudo_not_string

lea 32(%rsp),%rcx
mov $@pseudo_entry,%rdx
call @read_str
cmp %r12,32(%rsp)
je @pseudo_not_entry
mov @_$DATA+120,%rax
mov %rax,@_$DATA+40
mov @_$DATA+32,%rax
orb $1,8(%rax)
jmp @pseudo_end
@pseudo_not_entry

lea 32(%rsp),%rcx
mov $@pseudo_align,%rdx
call @read_str
cmp %r12,32(%rsp)
je @pseudo_not_align
mov @_$DATA+32,%rax
orb $1,8(%rax)
lea 32(%rsp),%rcx
call @skip_spaces
mov 32(%rsp),%rcx
mov (%rcx),%cl
sub $'1',%cl
cmp $7,%cl
jbe @align_noerr
mov $@err_badalign,%rcx
call @error
@align_noerr
inc %cl
mov $1,%eax
shl %cl,%eax
dec %eax
mov @_$DATA+120,%rcx
and %eax,%ecx
je @pseudo_end
sub %ecx,%eax
lea 1(%rax),%ecx
call @fill_code_zero
jmp @pseudo_end
@pseudo_not_align

lea 32(%rsp),%rcx
mov $@pseudo_datasize,%rdx
call @read_str
cmp %r12,32(%rsp)
je @pseudo_not_datasize
lea 32(%rsp),%rcx
call @skip_spaces
lea 32(%rsp),%rcx
mov (%rcx),%r12
call @read_number
cmp %r12,(%rcx)
jne @datasize_noerror
mov $@err_datasize,%rcx
call @error
@datasize_noerror
mov %rax,@_$DATA+56
jmp @pseudo_end
@pseudo_not_datasize

lea 32(%rsp),%rcx
mov $@pseudo_cui,%rdx
call @read_str
cmp %r12,32(%rsp)
je @pseudo_not_cui
movb $1,@_$DATA+25
jmp @pseudo_end
@pseudo_not_cui

mov $@err_badpseudoop,%rcx
call @error
@pseudo_end
add $48,%rsp
pop %r12
ret

@check_reg_rex
# %cl -- reg value
# %ch -- rex flag to add
# %al -- rex value
# %ah -- 0xff if one of %ah, %ch, %dh, %bh is used
cmp $8,%cl
jb @check_reg_rex_end
cmp $16,%cl
jb @check_reg_rex_add_rex
cmp $24,%cl
jb @check_reg_rex_add_norex
ja @check_reg_rex_add_rex2
jmp @check_reg_rex_end
@check_reg_rex_add_norex
mov $0xff,%ah
jmp @check_reg_rex_end
@check_reg_rex_add_rex
or %ch,%al
@check_reg_rex_add_rex2
or $0x40,%al
@check_reg_rex_end
ret

@assemble_write_address
# arg1 -- ins arg
# arg2 -- extra opcode
push %r12
sub $48,%rsp
mov %rcx,%r12
mov %dl,32(%rsp)
cmpq $0xffffffff80000000,8(%rcx)
jl @inserr
cmpq $0x7fffffff,8(%rcx)
jg @inserr
testb $1,30(%r12)
je @assemble_write_address_near
mov (%r12),%al
and $7,%al
cmp $5,%al
je @assemble_write_address_X2
cmpq $0,8(%r12)
je @assemble_write_address_X3
@assemble_write_address_X2
orb $0x40,32(%rsp)
@assemble_write_address_X3
cmpq $0xffffffffffffff80,8(%r12)
jl @assemble_write_address_X1
cmpq $0x7f,8(%r12)
jle @assemble_write_address_near
@assemble_write_address_X1
xorb $0xc0,32(%rsp)
@assemble_write_address_near
testb $2,30(%r12)
jne @assemble_write_address_index
testb $1,30(%r12)
je @assemble_write_address_noreg
cmpw $24,(%r12)
je @assemble_write_address_rip
mov (%r12),%al
and $7,%al
or %al,32(%rsp)
lea 32(%rsp),%rcx
mov $1,%edx
call @emit_code
mov (%r12),%al
and $7,%al
cmp $4,%al
jne @assemble_write_address_noindex_sp
movb $0x24,33(%rsp)
lea 33(%rsp),%rcx
mov $1,%edx
call @emit_code
@assemble_write_address_noindex_sp
testb $0xc0,32(%rsp)
je @assemble_write_address_end
mov $4,%edx
testb $0x40,32(%rsp)
je @assemble_write_address_noindex_far
mov $1,%edx
@assemble_write_address_noindex_far
lea 8(%r12),%rcx
call @emit_code
jmp @assemble_write_address_end
@assemble_write_address_noreg
orb $0x4,32(%rsp)
movb $0x25,33(%rsp)
lea 32(%rsp),%rcx
mov $2,%edx
call @emit_code
lea 8(%r12),%rcx
mov $4,%edx
call @emit_code
jmp @assemble_write_address_end
@assemble_write_address_rip
orb $5,32(%rsp)
lea 32(%rsp),%rcx
mov $1,%edx
call @emit_code
lea 8(%r12),%rcx
mov $4,%edx
call @emit_code
jmp @assemble_write_address_end
@assemble_write_address_index
cmpw $16,(%r12)
je @inserr
cmpw $16,4(%r12)
je @inserr
orb $4,32(%rsp)
lea 32(%rsp),%rcx
mov $1,%edx
call @emit_code
mov (%r12),%al
and $0x7,%al
cmp $5,%al
je @inserr
testb $1,30(%r12)
jne @assemble_write_address_index_has_base
mov $5,%al
@assemble_write_address_index_has_base
mov %al,33(%rsp)
mov 4(%r12),%al
and $7,%al
cmp $4,%al
je @inserr
shl $3,%al
or %al,33(%rsp)
mov 6(%r12),%al
shl $6,%al
or %al,33(%rsp)
lea 33(%rsp),%rcx
mov $1,%edx
call @emit_code
testb $0xc0,32(%rsp)
je @assemble_write_address_end
mov $4,%edx
testb $0x40,32(%rsp)
je @assemble_write_address_index_far
mov $1,%edx
@assemble_write_address_index_far
lea 8(%r12),%rcx
call @emit_code
@assemble_write_address_end
add $48,%rsp
pop %r12
ret

@assemble_write_ins
# arg1 -- ins format
# arg2 -- ins arg
push %r12
push %r13
sub $56,%rsp
mov %rcx,%r12
mov %rdx,%r13
@assemble_write_ins_prefix_loop
cmpb $':',(%r12)
je @assemble_write_ins_prefix_loop_end
lea 32(%rsp),%rcx
mov %r12,(%rcx)
call @read_number
mov 32(%rsp),%r12
inc %r12
mov %rax,32(%rsp)
lea 32(%rsp),%rcx
mov $1,%edx
call @emit_code
jmp @assemble_write_ins_prefix_loop
@assemble_write_ins_prefix_loop_end
inc %r12
mov $0,%ax
cmpb $'1',(%r12)
jne @assemble_write_rex
mov $0x48,%al
@assemble_write_rex
mov (%r13),%cl
mov $1,%ch
call @check_reg_rex
mov 2(%r13),%cl
mov $4,%ch
call @check_reg_rex
mov 4(%r13),%cl
mov $2,%ch
call @check_reg_rex
test %ah,%al
jne @inserr
test %al,%al
je @assemble_no_write_rex
lea 32(%rsp),%rcx
mov %al,(%rcx)
mov $1,%edx
call @emit_code
@assemble_no_write_rex
add $2,%r12
movb $0,32(%rsp)
@assemble_write_opcode_loop
cmpb $0,(%r12)
je @assemble_write_opcode_loop_end
cmpb $'0',(%r12)
jne @assemble_write_opcode_num
lea 40(%rsp),%rcx
mov %r12,(%rcx)
call @read_number
or %al,32(%rsp)
mov 40(%rsp),%r12
jmp @assemble_write_opcode_loop
@assemble_write_opcode_num
cmpb $'|',(%r12)
jne @assemble_write_opcode_X1
inc %r12
jmp @assemble_write_opcode_loop
@assemble_write_opcode_X1
cmpb $32,(%r12)
jne @assemble_write_opcode_X2
lea 32(%rsp),%rcx
mov $1,%edx
call @emit_code
movb $0,32(%rsp)
inc %r12
jmp @assemble_write_opcode_loop
@assemble_write_opcode_X2
cmpb $'R',(%r12)
jne @assemble_write_opcode_R
mov (%r13),%al
cmp $24,%al
je @inserr
and $0x7,%al
or %al,32(%rsp)
inc %r12
jmp @assemble_write_opcode_loop
@assemble_write_opcode_R
cmpb $'r',(%r12)
jne @assemble_write_opcode_r
mov 2(%r13),%al
cmp $24,%al
je @inserr
and $0x7,%al
or %al,32(%rsp)
inc %r12
jmp @assemble_write_opcode_loop
@assemble_write_opcode_r
cmpb $'s',(%r12)
jne @assemble_write_opcode_s
mov 2(%r13),%al
and $0x7,%al
shl $3,%al
or %al,32(%rsp)
inc %r12
jmp @assemble_write_opcode_loop
@assemble_write_opcode_s
cmpb $'A',(%r12)
jne @assemble_write_opcode_A
mov 32(%rsp),%dl
mov %r13,%rcx
call @assemble_write_address
movb $0,32(%rsp)
inc %r12
cmpb $32,(%r12)
jne @assemble_write_opcode_A_nospace
inc %r12
@assemble_write_opcode_A_nospace
jmp @assemble_write_opcode_loop
@assemble_write_opcode_A
cmpb $'i',(%r12)
jne @assemble_write_opcode_i
cmpq $0xff,16(%r13)
jg @assemble_write_opcode_err_cleanup
cmpq $0xffffffffffffff80,16(%r13)
jl @assemble_write_opcode_err_cleanup
lea 16(%r13),%rcx
mov $1,%edx
call @emit_code
add $2,%r12
jmp @assemble_write_opcode_loop
@assemble_write_opcode_i
cmpb $'I',(%r12)
jne @assemble_write_opcode_I
cmpq $0x7f,16(%r13)
jg @assemble_write_opcode_err_cleanup
cmpq $0xffffffffffffff80,16(%r13)
jl @assemble_write_opcode_err_cleanup
lea 16(%r13),%rcx
mov $1,%edx
call @emit_code
add $2,%r12
jmp @assemble_write_opcode_loop
@assemble_write_opcode_I
cmpb $'j',(%r12)
jne @assemble_write_opcode_j
cmpq $0xffff,16(%r13)
jg @assemble_write_opcode_err_cleanup
cmpq $0xffffffffffff8000,16(%r13)
jl @assemble_write_opcode_err_cleanup
lea 16(%r13),%rcx
mov $2,%edx
call @emit_code
add $2,%r12
jmp @assemble_write_opcode_loop
@assemble_write_opcode_j
cmpb $'k',(%r12)
jne @assemble_write_opcode_k
cmpl $0x0,20(%r13)
jg @assemble_write_opcode_err_cleanup
cmpq $0xffffffff80000000,16(%r13)
jl @assemble_write_opcode_err_cleanup
lea 16(%r13),%rcx
mov $4,%edx
call @emit_code
add $2,%r12
jmp @assemble_write_opcode_loop
@assemble_write_opcode_k
cmpb $'K',(%r12)
jne @assemble_write_opcode_K
cmpq $0x7fffffff,16(%r13)
jg @assemble_write_opcode_err_cleanup
cmpq $0xffffffff80000000,16(%r13)
jl @assemble_write_opcode_err_cleanup
lea 16(%r13),%rcx
mov $4,%edx
call @emit_code
add $2,%r12
jmp @assemble_write_opcode_loop
@assemble_write_opcode_K
cmpb $'l',(%r12)
jne @assemble_write_opcode_l
lea 16(%r13),%rcx
mov $8,%edx
call @emit_code
add $2,%r12
jmp @assemble_write_opcode_loop
@assemble_write_opcode_l
cmpb $'O',(%r12)
jne @assemble_write_opcode_O
mov 16(%r13),%rax
xor %edx,%edx
mov @_$DATA+32,%rcx
mov 24(%rcx),%rcx
test %rcx,%rcx
je @assemble_write_opcode_O_X1
mov (%rcx),%rdx
@assemble_write_opcode_O_X1
sub %rdx,%rax
sub @_$DATA+120,%rax
sub $0x400000+4,%rax
cmp $0xffffffff80000000,%rax
jl @assemble_write_opcode_err_cleanup
cmp $0x7fffffff,%rax
jg @assemble_write_opcode_err_cleanup
mov %rax,40(%rsp)
lea 40(%rsp),%rcx
mov $4,%edx
call @emit_code
add $2,%r12
jmp @assemble_write_opcode_loop
@assemble_write_opcode_O
cmpb $'o',(%r12)
jne @assemble_write_opcode_o
mov 16(%r13),%rax
xor %edx,%edx
mov @_$DATA+32,%rcx
mov 24(%rcx),%rcx
test %rcx,%rcx
je @assemble_write_opcode_o_X1
mov (%rcx),%rdx
@assemble_write_opcode_o_X1
sub %rdx,%rax
sub @_$DATA+120,%rax
sub $0x400000+1,%rax
cmp $0xffffffffffffff80,%rax
jl @assemble_write_opcode_err_cleanup
cmp $0x7f,%rax
jg @assemble_write_opcode_err_cleanup
mov %rax,40(%rsp)
lea 40(%rsp),%rcx
mov $1,%edx
call @emit_code
add $2,%r12
jmp @assemble_write_opcode_loop
@assemble_write_opcode_o

@assemble_write_opcode_loop_end
xor %eax,%eax
add $56,%rsp
pop %r13
pop %r12
ret
@assemble_write_opcode_err_cleanup
mov @_$DATA+32,%rax
mov 24(%rax),%rcx
movq $0,24(%rax)
.dllcall "msvcrt.dll" "free"
mov $1,%eax
add $56,%rsp
pop %r13
pop %r12
ret


@assemble_ins2
# arg1 -- pointer to string pointer
# arg2 -- ins format string
# return -- zero if success, nonzero otherwise
push %r12
push %r13
push %r14
sub $128,%rsp
movq $0,32(%rsp)
movq $0,40(%rsp)
movq $0,48(%rsp)
movq $0,56(%rsp)
mov %rcx,%r12
mov %rdx,%r13
mov (%rcx),%r14
@assemble_ins2_loop1
cmpb $':',(%r13)
je @assemble_ins2_loop1_end
lea 80(%rsp),%rcx
mov %r13,(%rcx)
mov $@ins_format,%rdx
call @string_array_get_index
mov 80(%rsp),%r13
cmp $0,%eax
jge @assemble_ins2_loop1_X1
mov %r12,%rcx
call @skip_spaces
mov (%r12),%rcx
mov (%rcx),%al
cmp %al,(%r13)
jne @assemble_ins2_err
inc %rcx
mov %rcx,(%r12)
inc %r13
jmp @assemble_ins2_loop1
@assemble_ins2_loop1_X1
cmp $0,%al
jne @assemble_ins2_loop1_RB1
mov %r12,%rcx
call @skip_spaces
mov %r12,%rcx
call @get_reg8
cmp $0,%eax
jl @assemble_ins2_err
mov %ax,32(%rsp)
jmp @assemble_ins2_loop1
@assemble_ins2_loop1_RB1
cmp $1,%al
jne @assemble_ins2_loop1_RW1
mov %r12,%rcx
call @skip_spaces
mov %r12,%rcx
call @get_reg16
cmp $0,%eax
jl @assemble_ins2_err
mov %ax,32(%rsp)
jmp @assemble_ins2_loop1
@assemble_ins2_loop1_RW1
cmp $2,%al
jne @assemble_ins2_loop1_RL1
mov %r12,%rcx
call @skip_spaces
mov %r12,%rcx
call @get_reg32
cmp $0,%eax
jl @assemble_ins2_err
mov %ax,32(%rsp)
jmp @assemble_ins2_loop1
@assemble_ins2_loop1_RL1
cmp $3,%al
jne @assemble_ins2_loop1_RQ1
mov %r12,%rcx
call @skip_spaces
mov %r12,%rcx
call @get_reg64
cmp $0,%eax
jl @assemble_ins2_err
mov %ax,32(%rsp)
jmp @assemble_ins2_loop1
@assemble_ins2_loop1_RQ1
cmp $4,%al
jne @assemble_ins2_loop1_RX1
mov %r12,%rcx
call @skip_spaces
mov %r12,%rcx
call @get_regxmm
cmp $0,%eax
jl @assemble_ins2_err
mov %ax,32(%rsp)
jmp @assemble_ins2_loop1
@assemble_ins2_loop1_RX1
cmp $5,%al
jne @assemble_ins2_loop1_RB2
mov %r12,%rcx
call @skip_spaces
mov %r12,%rcx
call @get_reg8
cmp $0,%eax
jl @assemble_ins2_err
mov %ax,34(%rsp)
jmp @assemble_ins2_loop1
@assemble_ins2_loop1_RB2
cmp $6,%al
jne @assemble_ins2_loop1_RW2
mov %r12,%rcx
call @skip_spaces
mov %r12,%rcx
call @get_reg16
cmp $0,%eax
jl @assemble_ins2_err
mov %ax,34(%rsp)
jmp @assemble_ins2_loop1
@assemble_ins2_loop1_RW2
cmp $7,%al
jne @assemble_ins2_loop1_RL2
mov %r12,%rcx
call @skip_spaces
mov %r12,%rcx
call @get_reg32
cmp $0,%eax
jl @assemble_ins2_err
mov %ax,34(%rsp)
jmp @assemble_ins2_loop1
@assemble_ins2_loop1_RL2
cmp $8,%al
jne @assemble_ins2_loop1_RQ2
mov %r12,%rcx
call @skip_spaces
mov %r12,%rcx
call @get_reg64
cmp $0,%eax
jl @assemble_ins2_err
mov %ax,34(%rsp)
jmp @assemble_ins2_loop1
@assemble_ins2_loop1_RQ2
cmp $9,%al
jne @assemble_ins2_loop1_RX2
mov %r12,%rcx
call @skip_spaces
mov %r12,%rcx
call @get_regxmm
cmp $0,%eax
jl @assemble_ins2_err
mov %ax,34(%rsp)
jmp @assemble_ins2_loop1
@assemble_ins2_loop1_RX2
cmp $10,%al
jne @assemble_ins2_loop1_ADDR
mov %r12,%rcx
call @skip_spaces
mov %r12,%rcx
lea 32(%rsp),%rdx
call @get_addr
jmp @assemble_ins2_loop1
@assemble_ins2_loop1_ADDR
cmp $12,%al
jne @assemble_ins2_loop1_CX
mov %r12,%rcx
call @skip_spaces
mov %r12,%rcx
call @get_reg8
cmp $1,%eax
jne @assemble_ins2_err
jmp @assemble_ins2_loop1
@assemble_ins2_loop1_CX
# IMM
mov %r12,%rcx
call @skip_spaces
mov %r12,%rcx
mov (%rcx),%rax
mov %rax,80(%rsp)
call @read_constant
mov (%r12),%rcx
cmp %rcx,80(%rsp)
je @assemble_ins2_err
mov %rax,48(%rsp)
jmp @assemble_ins2_loop1
@assemble_ins2_loop1_end
mov %r12,%rcx
call @skip_spaces
mov (%r12),%rcx
cmpb $0,(%rcx)
jne @assemble_ins2_err
inc %r13
mov %r13,%rcx
lea 32(%rsp),%rdx
call @assemble_write_ins
test %eax,%eax
jne @assemble_ins2_err
add $128,%rsp
pop %r14
pop %r13
pop %r12
ret

@assemble_ins2_err
mov %r14,(%r12)
mov $1,%eax
add $128,%rsp
pop %r14
pop %r13
pop %r12
ret

@assemble_ins1
# arg1 -- pointer to string pointer
# arg2 -- ins index
push %r12
push %r13
push %r14
push %r15
sub $40,%rsp
mov %rcx,%r12
mov %rdx,%r13
xor %ecx,%ecx
shl $2,%edx
@assemble_ins1_loop1
test %edx,%edx
je @assemble_ins1_loop1_end
sub $4,%edx
add @ins_offsets(%rdx),%ecx
jmp @assemble_ins1_loop1
@assemble_ins1_loop1_end
mov $@ins_operands,%rax
@assemble_ins1_loop2
test %ecx,%ecx
je @assemble_ins1_loop2_end
cmpb $0,(%rax)
jne @assemble_ins1_loop2_X1
dec %ecx
@assemble_ins1_loop2_X1
inc %rax
jmp @assemble_ins1_loop2
@assemble_ins1_loop2_end
mov %rax,%r15
shl $2,%r13
mov @ins_offsets(%r13),%r14d
@assemble_ins1_loop3
mov %r15,%rdx
mov %r12,%rcx
call @assemble_ins2
test %eax,%eax
je @assemble_ins1_success
mov %r15,%rcx
.dllcall "msvcrt.dll" "strlen"
add %rax,%r15
inc %r15
dec %r14d
jne @assemble_ins1_loop3
jmp @inserr
@assemble_ins1_success
add $40,%rsp
pop %r15
pop %r14
pop %r13
pop %r12
ret


@assemble_ins
push %r12
sub $48,%rsp
mov @_$DATA+32,%rax
mov 16(%rax),%r12
add $8,%r12
lea 32(%rsp),%rcx
mov %r12,(%rcx)
call @skip_spaces
lea 32(%rsp),%rcx
mov $@ins_list,%rdx
call @string_array_get_index
cmp $0,%eax
jl @inserr
lea 32(%rsp),%rcx
mov %eax,%edx
call @assemble_ins1
add $48,%rsp
pop %r12
ret

@assemble_line
push %r12
sub $48,%rsp
cmpb $0,@_$DATA+24
je @assemble_line_first_compile
mov @_$DATA+32,%rax
testb $1,8(%rax)
je @assemble_line_end
mov 24(%rax),%rcx
movq $0,24(%rax)
.dllcall "msvcrt.dll" "free"
@assemble_line_first_compile
mov @_$DATA+32,%rax
mov 16(%rax),%r12
add $8,%r12
lea 32(%rsp),%rcx
mov %r12,(%rcx)
call @skip_spaces
mov (%rcx),%r12
cmpb $0,(%r12)
je @assemble_line_end
cmpb $'#',(%r12)
je @assemble_line_end
cmpb $'.',(%r12)
jne @assemble_line_not_pseudo
call @handle_pseudo_op
jmp @assemble_line_end
@assemble_line_not_pseudo
cmpb $'@',(%r12)
jne @assemble_line_not_label
lea 1(%r12),%rcx
mov @_$DATA+120,%rdx
add $0x400000,%rdx
call @label_string_htab_set
jmp @assemble_line_end
@assemble_line_not_label
call @assemble_ins
@assemble_line_end
add $48,%rsp
pop %r12
ret

@assemble_all
push %r12
sub $32,%rsp
mov @_$DATA+16,%r12
movq $0x1000,@_$DATA+120
movb $0,@_$DATA+26
@assemble_all_loop
test %r12,%r12
je @assemble_all_end
mov %r12,@_$DATA+32
cmpq $0,16(%r12)
je @assemble_all_empty_line
call @assemble_line
@assemble_all_empty_line
mov 24(%r12),%rcx
test %rcx,%rcx
je @assemble_all_empty
mov (%rcx),%rcx
add %rcx,@_$DATA+120
@assemble_all_empty
mov (%r12),%r12
jmp @assemble_all_loop
@assemble_all_end
add $32,%rsp
pop %r12
ret

@string_hash
push %rcx
push %rdx
xor %eax,%eax
@string_hash_loop
cmpb $0,(%rcx)
je @string_hash_end
mov %eax,%edx
shl $1,%eax
shr $31,%edx
or %edx,%eax
mov (%rcx),%dl
movzbl %dl,%edx
add %edx,%eax
inc %rcx
jmp @string_hash_loop
@string_hash_end
and $511,%eax
pop %rdx
pop %rcx
ret

@label_string_hash
push %rcx
push %rdx
xor %eax,%eax
@label_string_hash_loop
xchg %rax,%rdx
mov (%rcx),%al
call @is_valid_id
xchg %rax,%rdx
test %edx,%edx
je @label_string_hash_end
mov %eax,%edx
shl $1,%eax
shr $31,%edx
or %edx,%eax
mov (%rcx),%dl
movzbl %dl,%edx
add %edx,%eax
inc %rcx
jmp @label_string_hash_loop
@label_string_hash_end
and $511,%eax
pop %rdx
pop %rcx
ret

# label_string_htab structure
# 0:8 -- next
# 8:8 -- string (pointer to string structure)
# 16:8 -- value
@label_string_htab_get
# arg1 -- string
# return -- htab node
push %r12
push %r13
sub $40,%rsp
mov %rcx,%r12
call @label_string_hash
shl $3,%rax
mov @_$DATA+8192(%rax),%r13
@label_string_htab_get_loop
test %r13,%r13
je @label_string_htab_get_end
mov 8(%r13),%rdx
add $8,%rdx
mov %r12,%rcx
@label_string_htab_get_cmp
mov (%rcx),%al
call @is_valid_id
test %eax,%eax
jne @label_string_htab_get_cmp_X1
mov (%rdx),%al
call @is_valid_id
jmp @label_string_htab_get_cmp_end
@label_string_htab_get_cmp_X1
mov (%rcx),%al
sub (%rdx),%al
jne @label_string_htab_get_cmp_end
inc %rcx
inc %rdx
jmp @label_string_htab_get_cmp
@label_string_htab_get_cmp_end
test %eax,%eax
je @label_string_htab_get_end
mov (%r13),%r13
jmp @label_string_htab_get_loop
@label_string_htab_get_end
mov %r13,%rax
test %rax,%rax
jne @label_string_htab_found
cmpb $0,@_$DATA+24
je @label_string_htab_found
mov $@err_undefined,%rcx
call @error
@label_string_htab_found
mov @_$DATA+32,%rcx
orb $1,8(%rcx)
add $40,%rsp
pop %r13
pop %r12
ret

@label_string_htab_set
# arg1 -- str
# arg2 -- value
push %r12
push %r13
push %r15
sub $32,%rsp
mov %rcx,%r12
mov %rdx,%r13
call @label_string_htab_get
test %rax,%rax
je @label_string_htab_new
cmp %r13,16(%rax)
je @label_string_htab_nochange
mov %r13,16(%rax)
movb $1,@_$DATA+26
@label_string_htab_nochange
cmpb $0,@_$DATA+24
jne @label_string_htab_end
mov $@err_redefined,%rcx
call @error
@label_string_htab_new
mov $32,%ecx
.dllcall "msvcrt.dll" "malloc"
test %rax,%rax
je @nomem
mov %rax,%r15
mov %r12,%rcx
call @dup_str
mov %rax,8(%r15)
mov %r13,16(%r15)
mov %r12,%rcx
call @label_string_hash
shl $3,%rax
mov @_$DATA+8192(%rax),%rcx
mov %rcx,(%r15)
mov %r15,@_$DATA+8192(%rax)
@label_string_htab_end
add $32,%rsp
pop %r15
pop %r13
pop %r12
ret

# dll_string_htab structure
# 0:8 -- next
# 8:8 -- string (pointer to string structure)
# 16:8 -- value 1
# 24:8 -- value 2
# 32:8 -- list next
@dll_string_htab_get
# arg1 -- string
# return -- htab node
push %r12
push %r13
sub $40,%rsp
mov %rcx,%r12
call @string_hash
shl $3,%rax
mov @_$DATA+4096(%rax),%r13
@dll_string_htab_get_loop
test %r13,%r13
je @dll_string_htab_get_end
mov 8(%r13),%rdx
add $8,%rdx
mov %r12,%rcx
.dllcall "msvcrt.dll" "strcmp"
test %eax,%eax
je @dll_string_htab_get_end
mov (%r13),%r13
jmp @dll_string_htab_get_loop
@dll_string_htab_get_end
mov %r13,%rax
add $40,%rsp
pop %r13
pop %r12
ret

@dll_string_htab_set
# arg1 -- str
# arg2 -- value 1
# arg3 -- value 2
# arg4 -- is_funname
push %r12
push %r13
push %r14
push %r15
push %r9
sub $32,%rsp
mov %rcx,%r12
mov %rdx,%r13
mov %r8,%r14
call @dll_string_htab_get
test %rax,%rax
je @dll_string_htab_new
mov %r13,16(%rax)
mov %r14,24(%rax)
jmp @dll_string_htab_end
@dll_string_htab_new
mov $48,%ecx
.dllcall "msvcrt.dll" "malloc"
test %rax,%rax
je @nomem
mov %rax,%r15
mov %r12,%rcx
call @dup_str
mov %rax,8(%r15)
mov %r13,16(%r15)
mov %r14,24(%r15)
mov %r12,%rcx
call @string_hash
shl $3,%rax
mov @_$DATA+4096(%rax),%rcx
mov %rcx,(%r15)
mov %r15,@_$DATA+4096(%rax)
cmpb $0,32(%rsp)
jne @dll_string_htab_funname
mov @_$DATA+104,%rax
mov %rax,32(%r15)
mov %r15,@_$DATA+104
jmp @dll_string_htab_end
@dll_string_htab_funname
mov @_$DATA+112,%rax
mov %rax,32(%r15)
mov %r15,@_$DATA+112
@dll_string_htab_end
add $40,%rsp
pop %r15
pop %r14
pop %r13
pop %r12
ret

@init_dlls
push %r12
push %r13
sub $40,%rsp
mov @_$DATA+16,%r12
xor %r13d,%r13d
@init_dlls_loop
test %r12,%r12
je @init_dlls_loop_end
mov 32(%r12),%rcx
test %rcx,%rcx
je @init_dlls_nodll
add $8,%rcx
call @dll_string_htab_get
test %rax,%rax
jne @init_dlls_initialized1
inc %r13
mov %r13,%rdx
xor %r8d,%r8d
mov 32(%r12),%rcx
add $8,%rcx
xor %r9d,%r9d
call @dll_string_htab_set
incl @_$DATA+28
mov 32(%r12),%rcx
add $8,%rcx
.dllcall "msvcrt.dll" "strlen"
add $3,%eax
add %eax,@_$DATA+52
@init_dlls_initialized1
mov 40(%r12),%rcx
add $8,%rcx
call @dll_string_htab_get
test %rax,%rax
jne @init_dlls_initialized2
mov 32(%r12),%rcx
add $8,%rcx
call @dll_string_htab_get
mov 24(%rax),%r8
incq 24(%rax)
inc %r13
mov %r13,%rdx
mov 40(%r12),%rcx
add $8,%rcx
mov $1,%r9d
call @dll_string_htab_set
incl @_$DATA+48
mov 40(%r12),%rcx
add $8,%rcx
.dllcall "msvcrt.dll" "strlen"
add $3,%eax
add %eax,@_$DATA+52
@init_dlls_initialized2
@init_dlls_nodll
mov (%r12),%r12
jmp @init_dlls_loop
@init_dlls_loop_end
add $40,%rsp
pop %r13
pop %r12
ret

@calculate_text_segment_size
xor %eax,%eax
mov @_$DATA+16,%rcx
@calculate_text_segment_size_loop
test %rcx,%rcx
je @calculate_text_segment_size_end
mov 24(%rcx),%rdx
test %rdx,%rdx
je @calculate_text_segment_size_empty
add (%rdx),%rax
@calculate_text_segment_size_empty
mov (%rcx),%rcx
jmp @calculate_text_segment_size_loop
@calculate_text_segment_size_end
add $511,%rax
and $0xfe00,%ax
mov %rax,@_$DATA+72
ret

@calculate_rdata_segment_size
mov @_$DATA+28,%eax
lea (%rax,%rax,8),%eax
mov @_$DATA+48,%ecx
shl $2,%ecx
add %ecx,%eax
shl $2,%eax
add $20+511,%eax
add @_$DATA+52,%eax
and $0xfe00,%ax
mov %rax,@_$DATA+80
ret

@calculate_sizes
pushq @_$DATA+96
call @calculate_text_segment_size
call @calculate_rdata_segment_size
mov @_$DATA+72,%rax
add $8191,%rax
and $0xf000,%ax
mov %rax,@_$DATA+88
add @_$DATA+80,%rax
add $4095,%rax
and $0xf000,%ax
mov %rax,@_$DATA+96
pop %rcx
cmp %rcx,%rax
je @calculate_sizes_end
movb $1,@_$DATA+26
@calculate_sizes_end
ret

@image_write_header
sub $40,%rsp
mov $@mzdosheader,%rcx
mov $128,%edx
call @write_image
mov $@peheader,%rcx
mov $@peheader_end-@peheader,%edx
call @write_image
call @pad_image
mov @_$DATA+64,%rcx
mov @_$DATA+88,%eax
sub $4096,%eax
mov %eax,164(%rcx)
mov @_$DATA+56,%eax
add $4095,%eax
and $0xf000,%ax
mov %eax,172(%rcx)
add @_$DATA+96,%eax
mov %eax,216(%rcx)
mov @_$DATA+40,%eax
mov %eax,176(%rcx)
movl $0x1000,180(%rcx)
cmpb $0,@_$DATA+25
je @image_write_header_gui
movw $3,228(%rcx)
@image_write_header_gui
mov @_$DATA+28,%eax
add @_$DATA+48,%eax
shl $4,%eax
add @_$DATA+88,%eax
mov %eax,280(%rcx)
mov @_$DATA+28,%eax
lea (%rax,%rax,4),%eax
shl $2,%eax
add $20,%eax
lea 142(%rcx),%rsi
movw $1,(%rsi)
mov %eax,284(%rcx)
add $400,%rcx
mov $0x747865742e,%rax
mov %rax,(%rcx)
mov @_$DATA+72,%eax
mov %eax,16(%rcx)
add $4095,%eax
and $0xf000,%ax
mov %eax,8(%rcx)
movl $0x1000,12(%rcx)
movl $0x200,20(%rcx)
movl $0x60000020,36(%rcx)
add $40,%rcx
mov @_$DATA+80,%eax
test %eax,%eax
je @image_write_header_nordata
incw (%rsi)
mov $0x61746164722e,%rax
mov %rax,(%rcx)
mov @_$DATA+80,%eax
mov %eax,16(%rcx)
add $4095,%eax
and $0xf000,%ax
mov %eax,8(%rcx)
mov @_$DATA+88,%eax
mov %eax,12(%rcx)
mov @_$DATA+72,%eax
add $0x200,%eax
mov %eax,20(%rcx)
movl $0xc0000040,36(%rcx)
add $40,%rcx
@image_write_header_nordata
mov @_$DATA+56,%eax
test %eax,%eax
je @image_write_header_nobss
incw (%rsi)
mov $0x7373622e,%eax
mov %eax,(%rcx)
mov @_$DATA+56,%eax
add $4095,%eax
and $0xf000,%ax
mov %eax,8(%rcx)
mov @_$DATA+96,%eax
mov %eax,12(%rcx)
movl $0xc0000080,36(%rcx)
@image_write_header_nobss

add $40,%rsp
ret

@get_dll_function_offset
# arg1 -- dllname
# arg2 -- funname
push %r13
push %r14
sub $40,%rsp
mov %rdx,%r13
xor %r14d,%r14d
call @dll_string_htab_get
@get_dll_function_offset_loop
cmpq $0,32(%rax)
je @get_dll_function_offset_loop_end
mov 32(%rax),%rax
add 24(%rax),%r14
inc %r14
jmp @get_dll_function_offset_loop
@get_dll_function_offset_loop_end
mov %r13,%rcx
call @dll_string_htab_get
add 24(%rax),%r14
mov %r14,%rax
shl $3,%rax
add $40,%rsp
pop %r14
pop %r13
ret

@get_dll_offset
# arg1 -- dllname
push %r14
sub $32,%rsp
xor %r14d,%r14d
call @dll_string_htab_get
@get_dll_offset_loop
cmpq $0,32(%rax)
je @get_dll_offset_loop_end
mov 32(%rax),%rax
add 24(%rax),%r14
inc %r14
jmp @get_dll_offset_loop
@get_dll_offset_loop_end
mov %r14,%rax
shl $3,%rax
add $32,%rsp
pop %r14
ret

@image_write_text
push %r12
sub $32,%rsp
mov @_$DATA+16,%r12
@image_write_text_loop
test %r12,%r12
je @image_write_text_end
mov 32(%r12),%rcx
test %rcx,%rcx
je @image_write_text_no_dll
add $8,%rcx
mov 40(%r12),%rdx
add $8,%rdx
call @get_dll_function_offset
add @_$DATA+88,%eax
add $0x400000,%eax
mov 24(%r12),%rcx
mov %eax,9(%rcx)
@image_write_text_no_dll
mov 24(%r12),%rsi
test %rsi,%rsi
je @image_write_null
mov (%rsi),%rdx
lea 8(%rsi),%rcx
call @write_image
@image_write_null
mov (%r12),%r12
jmp @image_write_text_loop
@image_write_text_end
call @pad_image
add $32,%rsp
pop %r12
ret

@image_fill_zeros
# arg1 -- size
push %r12
sub $32,%rsp
mov %rcx,%r12
@image_fill_zeros_loop
mov @_$DATA+64,%rcx
mov $0,%dl
call @string_append_ch
mov %rax,@_$DATA+64
dec %r12
jne @image_fill_zeros_loop
add $32,%rsp
pop %r12
ret

@image_write_rdata
push %r12
push %r13
push %r14
sub $32,%rsp
mov @_$DATA+64,%rax
mov (%rax),%r14
mov @_$DATA+28,%eax
lea (%rax,%rax,8),%eax
shl $2,%eax
mov @_$DATA+48,%ecx
shl $4,%ecx
add %eax,%ecx
add $20,%ecx
call @image_fill_zeros
mov @_$DATA+16,%r12
@image_write_rdata_loop1
test %r12,%r12
je @image_write_rdata_end1
mov 32(%r12),%rcx
mov 40(%r12),%rdx
test %rcx,%rcx
je @image_write_rdata_nodll
add $8,%rcx
add $8,%rdx
call @get_dll_function_offset
add %r14,%rax
mov @_$DATA+64,%rcx
mov (%rcx),%rdx
sub %r14,%rdx
add @_$DATA+88,%rdx
mov 8(%rcx,%rax,1),%rsi
test %rsi,%rsi
jne @image_write_rdata_dllwritten
mov %rdx,8(%rcx,%rax,1)
mov @_$DATA+28,%esi
add @_$DATA+48,%esi
shl $3,%esi
add %rsi,%rax
mov %rdx,8(%rcx,%rax,1)
mov $2,%ecx
call @image_fill_zeros
mov 40(%r12),%rcx
mov (%rcx),%rdx
add $8,%rcx
inc %rdx
call @write_image
@image_write_rdata_dllwritten

@image_write_rdata_nodll
mov (%r12),%r12
jmp @image_write_rdata_loop1
@image_write_rdata_end1
mov @_$DATA+104,%r12
xor %r13d,%r13d
@image_write_rdata_loop2
test %r12,%r12
je @image_write_rdata_end2
mov 8(%r12),%rcx
add $8,%rcx
call @get_dll_offset
mov @_$DATA+64,%rcx
mov (%rcx),%rdx
sub %r14,%rdx
add @_$DATA+88,%rdx
add $2,%rdx
add @_$DATA+88,%rax
mov @_$DATA+28,%esi
add @_$DATA+48,%esi
shl $4,%esi
lea 8(%rsi),%rdi
add @_$DATA+64,%rdi
add %r14,%rdi
add %r13,%rdi
mov %eax,16(%rdi)
mov %edx,12(%rdi)
shr $1,%esi
add %esi,%eax
mov %eax,(%rdi)
add $20,%r13d
mov $2,%ecx
call @image_fill_zeros
mov 8(%r12),%rcx
mov (%rcx),%rdx
add $8,%rcx
inc %rdx
call @write_image
mov 32(%r12),%r12
jmp @image_write_rdata_loop2
@image_write_rdata_end2
call @pad_image
add $32,%rsp
pop %r14
pop %r13
pop %r12
ret

@image_calculate_checksum
xor %eax,%eax
mov @_$DATA+64,%rcx
mov (%rcx),%rdx
add $8,%rcx
mov %edx,%esi
xor %edi,%edi
@image_checksum_loop
mov (%rcx),%r8w
movzwl %r8w,%r8d
add %r8,%rax
add %rdi,%rax
mov %rax,%rdi
shr $16,%edi
movzwl %ax,%eax
add $2,%rcx
sub $2,%esi
jne @image_checksum_loop
add %rdx,%rax
add %rdi,%rax
ret

.entry
sub $40,%rsp
mov $@msg_infile,%rcx
mov $@fmode_r,%rdx
call @request_file
mov %rax,@_$DATA+0
mov $@msg_outfile,%rcx
mov $@fmode_w,%rdx
call @request_file
mov %rax,@_$DATA+8

call @load_file

movq $0x1000,@_$DATA+40

call @assemble_all
call @init_dlls
call @calculate_sizes

movb $1,@_$DATA+24
@main_loop
movb $0,@_$DATA+26
call @assemble_all
call @calculate_sizes
cmpb $0,@_$DATA+26
jne @main_loop

call @image_write_header
call @image_write_text
call @image_write_rdata

call @image_calculate_checksum

mov @_$DATA+64,%rcx
mov %eax,224(%rcx)

mov @_$DATA+64,%rcx
mov (%rcx),%rdx
add $8,%rcx
mov $1,%r8d
mov @_$DATA+8,%r9
.dllcall "msvcrt.dll" "fwrite"
mov @_$DATA+0,%rcx
.dllcall "msvcrt.dll" "fclose"
mov @_$DATA+8,%rcx
.dllcall "msvcrt.dll" "fclose"
mov $@msg_success,%rcx
.dllcall "msvcrt.dll" "printf"
call @exit
.datasize 16384
# 0 -- fpi
# 8 -- fpo
# 16 -- line list head
# 24 -- mode
# 25 -- CUI
# 26 -- status
# 28 -- count DLLs
# 32 -- current line
# 40 -- entry
# 48 -- count DLL functions
# 52 -- total string length
# 56 -- data size
# 64 -- image buf
# 72 -- text size
# 80 -- rdata size
# 88 -- rdata addr
# 96 -- data addr
# 104 -- DLL name list
# 112 -- function name list
# 120 -- current_pc
# 4096 -- dll_string_htab
# 8192 -- label_string_htab
