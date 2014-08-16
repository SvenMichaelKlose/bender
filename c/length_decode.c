/* Bender – Copyright (c) 2014 Sven Michael Klose <pixel@copei.de> */

#include <stdio.h>

char operand_lengths[3][8] = {
    {1, 1, 0, 2, 1, 1, -1, 2},
    {1, 1, 1, 2, 1, 1, 2, 2},
    {1, 1, 0, 2, -1, 1, 0, 2}
};

char legal_addrmodes[3][8][8] = {
    {{0, 0, 0, 0, 0, 1, 1, 1},
     {0, 1, 0, 0, 1, 1, 1, 1},
     {0, 0, 0, 0, 0, 0, 0, 0},
     {0, 1, 1, 1, 1, 1, 1, 1},
     {0, 0, 0, 0, 0, 0, 0, 0},
     {0, 0, 0, 0, 1, 1, 0, 0},
     {0, 0, 0, 0, 0, 0, 0, 0},
     {0, 0, 0, 0, 1, 1, 0, 0}},
    {{1, 1, 1, 1, 1, 1, 1, 1},
     {1, 1, 1, 1, 1, 1, 1, 1},
     {1, 1, 1, 1, 1, 0, 1, 1},
     {1, 1, 1, 1, 1, 1, 1, 1},
     {1, 1, 1, 1, 1, 1, 1, 1},
     {1, 1, 1, 1, 1, 1, 1, 1},
     {1, 1, 1, 1, 1, 1, 1, 1},
     {1, 1, 1, 1, 1, 1, 1, 1}},
    {{0, 0, 0, 0, 0, 1, 0, 0},
     {1, 1, 1, 1, 1, 1, 1, 1},
     {1, 1, 1, 1, 0, 0, 0, 0},
     {1, 1, 1, 1, 1, 1, 1, 1},
     {0, 0, 0, 0, 0, 0, 0, 0},
     {1, 1, 1, 1, 1, 1, 1, 1},
     {0, 0, 0, 0, 0, 0, 0, 0},
     {1, 1, 1, 1, 1, 1, 1, 1}}
};

#define OPCODE_AA(opcode)   ((opcode >> 5) & 7)
#define OPCODE_BB(opcode)   ((opcode >> 2) & 7)
#define OPCODE_CC(opcode)   (opcode & 3)

int
get_operand_length (int opcode)
{
    return operand_lengths[OPCODE_CC(opcode)][OPCODE_BB(opcode)];
}

int
is_legal_addrmode (int opcode)
{
    return get_operand_length (opcode) != -1 && !legal_addrmodes[OPCODE_CC(opcode)][OPCODE_BB(opcode)][OPCODE_AA(opcode)];
}

int
length_decode (int opcode)
{
    int len;

    if (opcode == 0x20) /* JSR */
        return 2;
    if (opcode == 0x6c) /* JMP(indirect) */
        return 2;
    if (!(opcode & 0x9f))
        return 0;
    if (!(opcode & 0xe3) && !is_legal_addrmode (opcode))
        return 0;
    len = operand_lengths[OPCODE_CC(opcode)][OPCODE_BB(opcode)];
    if (len == -1)
        return 0;
    return len;
}
