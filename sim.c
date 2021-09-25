//Eu Jeong Hwang
//Lab2 Part2

#include <stdio.h>
#include "shell.h"
#include <stdlib.h>
#define OP_SPECIAL 0x00
#define OP_BRANCH 0x01
#define SUBOP_ADD 0x20
#define SUBOP_ADDU 0x21
#define OP_ADDI 0x08
#define OP_ADDIU 0x09
#define SUBOP_SYSCALL 0xc
/*part1*/
#define OP_ORI 0x0d
#define SUBOP_SUB 0x22
#define SUBOP_SUBU 0x23
#define SUBOP_SLL 0x00
#define SUBOP_SRL 0x02
#define SUBOP_SRA 0x03
#define SUBOP_MULTU 0x19
#define SUBOP_DIVU 0x1b
#define SUBOP_SLTU 0x2b
#define SUBOP_MFHI 0x10
/*part2*/
#define OP_J 0x02
#define OP_BEQ 0x04
#define OP_BNE 0x05
#define OP_BGTZ 0x07
#define OP_LUI 0x0F
#define OP_LB 0x20
#define OP_LBU 0x24
#define OP_LW 0x23
#define OP_SB 0x28
#define OP_SW 0x2B
/*bonus*/
#define OP_SLTI 0x0A
#define OP_SLTIU 0x0B
#define SUBOP_AND 0x24
#define SUBOP_OR 0x25
#define SUBOP_XOR 0x26
#define SUBOP_NOR 0x27
#define SUBOP_SLT 0x2A
#define SUBOP_MFLO 0x12
#define SUBOP_DIV 0x1a
#define SUBOP_MTHI 0x11
uint32_t dcd_op;     /* decoded opcode */
uint32_t dcd_rs;     /* decoded rs operand */
uint32_t dcd_rt;     /* decoded rt operand */
uint32_t dcd_rd;     /* decoded rd operand */
uint32_t dcd_shamt;  /* decoded shift amount */
uint32_t dcd_funct;  /* decoded function */
uint32_t dcd_imm;    /* decoded immediate value */
uint32_t dcd_target; /* decoded target address */
int dcd_se_imm;      /* decoded sign-extended immediate value */
uint32_t inst;       /* machine instruction */
int64_t temp;
uint32_t address;

uint32_t sign_extend_h2w(uint16_t c)
{
    return (c & 0x8000) ? (c | 0xffff8000) : c;
}

uint32_t sign_extend_b2w(uint8_t c)
{
    return (c & 0x80) ? (c | 0xffffff80) : c;
}
uint32_t zero_extend_b2w(uint8_t c)
{
    return ((uint32_t) c);
}
uint32_t zero_extend_h2w(uint16_t c)
{
    return ((uint32_t) c);
}
void fetch()
{
    /* fetch the 4 bytes of the current instruction */
    inst = mem_read_32(CURRENT_STATE.PC);
}

void decode()
{
    /* decoding an instruction */
    dcd_op = (inst >> 26) & 0x3F;
    dcd_rs = (inst >> 21) & 0x1F;
    dcd_rt = (inst >> 16) & 0x1F;
    dcd_rd = (inst >> 11) & 0x1F;
    dcd_shamt = (inst >> 6) & 0x1F;
    dcd_funct = (inst >> 0) & 0x3F;
    dcd_imm = (inst >> 0) & 0xFFFF;
    dcd_se_imm = sign_extend_h2w(dcd_imm);
    dcd_target = (inst >> 0) & ((1UL << 26) - 1);
}

void execute()
{
    NEXT_STATE.PC = CURRENT_STATE.PC + 4;
    CURRENT_STATE.REGS[0] = 0;
    switch(dcd_op){
        case OP_SPECIAL:
            switch (dcd_funct){
                case SUBOP_ADD:
                    NEXT_STATE.REGS[dcd_rd]=(int)CURRENT_STATE.REGS[dcd_rs]+(int)CURRENT_STATE.REGS[dcd_rt];
                    break;
                case SUBOP_ADDU:
                    NEXT_STATE.REGS[dcd_rd] = CURRENT_STATE.REGS[dcd_rs] + CURRENT_STATE.REGS[dcd_rt];
                    break;
                case SUBOP_SYSCALL:
                    if (CURRENT_STATE.REGS[2] == 10)
                        RUN_BIT = 0;
                    break;

                /*bonus*/
		//bonus1
                case SUBOP_AND:
                    NEXT_STATE.REGS[dcd_rd]= CURRENT_STATE.REGS[dcd_rs] & CURRENT_STATE.REGS[dcd_rt];
                    break;
                //bonus2
                case SUBOP_OR:
                    NEXT_STATE.REGS[dcd_rd] = CURRENT_STATE.REGS[dcd_rs] | CURRENT_STATE.REGS[dcd_rt];
		            break;
                //bonus3
                case SUBOP_XOR:
		     NEXT_STATE.REGS[dcd_rd] = CURRENT_STATE.REGS[dcd_rs] ^ CURRENT_STATE.REGS[dcd_rt];
		            break;
		//bonus4
                case SUBOP_NOR:
		     NEXT_STATE.REGS[dcd_rd] = ~(CURRENT_STATE.REGS[dcd_rs] | CURRENT_STATE.REGS[dcd_rt]);
		            break;
                //bonus5
                case SUBOP_SLT:
                    if ((int32_t)CURRENT_STATE.REGS[dcd_rs]<(int32_t)CURRENT_STATE.REGS[dcd_rt])
                        NEXT_STATE.REGS[dcd_rd] = 1;
                    else
                        NEXT_STATE.REGS[dcd_rd] = 0;
                    break;
                //bonus6
                case SUBOP_MFLO:
                    NEXT_STATE.REGS[dcd_rd] = CURRENT_STATE.LO;
                    break;
		//bonus7
                case SUBOP_DIV:
                    if (dcd_rt == 0){
                        fprintf(stderr, "can't be divided by 0\n");
                        exit(-1);
                //bonus8
                case SUBOP_MTHI:
                    NEXT_STATE.HI = CURRENT_STATE.REGS[dcd_rs];
                    break;
		}
             
NEXT_STATE.HI=((int32_t)CURRENT_STATE.REGS[dcd_rs] % (int32_t)CURRENT_STATE.REGS[dcd_rt]);
                    // lo = $s / $t
			        NEXT_STATE.LO=((int32_t)CURRENT_STATE.REGS[dcd_rs] / (int32_t)CURRENT_STATE.REGS[dcd_rt]);
                    break;
   
                /* Part 1*/
		//SUBOP_SLL
                case SUBOP_SLL:
                NEXT_STATE.REGS[dcd_rd]=CURRENT_STATE.REGS[dcd_rt] << dcd_shamt;
                break;
                //SUBOP_SUB
                case SUBOP_SUB:
                    NEXT_STATE.REGS[dcd_rd] = (int32_t)CURRENT_STATE.REGS[dcd_rs] - (int32_t)CURRENT_STATE.REGS[dcd_rt];
                    break;
                // SUBOP_SUBU
                case SUBOP_SUBU:
                    NEXT_STATE.REGS[dcd_rd] = CURRENT_STATE.REGS[dcd_rs]-CURRENT_STATE.REGS[dcd_rt];
                    break;
                // SUBOP_SRL
                case SUBOP_SRL:
                    NEXT_STATE.REGS[dcd_rd]=(uint32_t)CURRENT_STATE.REGS[dcd_rt] >> dcd_shamt;
                    break;
                // SUBOP_SRA
                case SUBOP_SRA:
                    NEXT_STATE.REGS[dcd_rd] = (int32_t)CURRENT_STATE.REGS[dcd_rt] >> dcd_shamt;
                    break;
		//SUBOP_MULTU
                case SUBOP_MULTU:
                    temp = (uint64_t)CURRENT_STATE.REGS[dcd_rs] * (uint64_t)CURRENT_STATE.REGS[dcd_rt];
                    NEXT_STATE.HI = temp >> 32;
                    NEXT_STATE.LO = temp&0x00000000FFFFFFFF;
                    break;
   		//SUBOP_DIVU
                case SUBOP_DIVU:
                    if (dcd_rt == 0){
                        fprintf(stderr, "can't divided by 0\n");
                        exit(-1);
                    }
NEXT_STATE.HI=((uint32_t)CURRENT_STATE.REGS[dcd_rs] % (uint32_t)CURRENT_STATE.REGS[dcd_rt]);
                			        				NEXT_STATE.LO=((uint32_t)CURRENT_STATE.REGS[dcd_rs] / (uint32_t)CURRENT_STATE.REGS[dcd_rt]);
                    break;
                //SUBOP_SLTU
                case SUBOP_SLTU:
                    if (CURRENT_STATE.REGS[dcd_rs] < CURRENT_STATE.REGS[dcd_rt]){
                        NEXT_STATE.REGS[dcd_rd] = 1;
                    } else{
                        NEXT_STATE.REGS[dcd_rd] = 0;
                    }
                    break;
              	//SUBOP_MFHI
                case SUBOP_MFHI:
                    NEXT_STATE.REGS[dcd_rd] = CURRENT_STATE.HI;
                    break;
            }
            break;

 	//OP_ADDI
        case OP_ADDI:
            NEXT_STATE.REGS[dcd_rt]=(int)CURRENT_STATE.REGS[dcd_rs]+dcd_se_imm;
            break;
	//OP_ADDIU
        case OP_ADDIU:
            NEXT_STATE.REGS[dcd_rt] = CURRENT_STATE.REGS[dcd_rs] + dcd_se_imm;
            break;
 	//OP_ORI
        case OP_ORI:
            NEXT_STATE.REGS[dcd_rt]=CURRENT_STATE.REGS[dcd_rs]|(dcd_imm&0x0000FFFF);
            break;

        /*Part 2*/
        //OP_J
        case OP_J:
            NEXT_STATE.PC = (dcd_target << 2) | (CURRENT_STATE.PC & 0xF0000000);
            break;
        //OP_BEQ
        case OP_BEQ:
            if (CURRENT_STATE.REGS[dcd_rs] == CURRENT_STATE.REGS[dcd_rt]){
                NEXT_STATE.PC = CURRENT_STATE.PC + (dcd_se_imm << 2);
            }
            break;
        //OP_BNE
        case OP_BNE:
            if(CURRENT_STATE.REGS[dcd_rs] != CURRENT_STATE.REGS[dcd_rt])
                NEXT_STATE.PC = CURRENT_STATE.PC + (dcd_se_imm << 2);
            break;
        // OP_BGTZ
        case OP_BGTZ:
            if((int32_t)CURRENT_STATE.REGS[dcd_rs] > 0)
                NEXT_STATE.PC = CURRENT_STATE.PC + (dcd_se_imm << 2);
            break;
        //OP_LUI
        case OP_LUI:
            NEXT_STATE.REGS[dcd_rt] = (dcd_imm << 16);
            break;
        //OP_LB
	//load byte
        case OP_LB:
            NEXT_STATE.REGS[dcd_rt] = sign_extend_b2w((mem_read_32(CURRENT_STATE.REGS[dcd_rs] + (dcd_se_imm)) & 0xFF));
            break;
        //OP_LBU
        case OP_LBU:
            NEXT_STATE.REGS[dcd_rt] = (uint32_t)(mem_read_32(CURRENT_STATE.REGS[dcd_rs] + dcd_se_imm) & 0xFF);
            break;
        //OP_LW
	//load word
        case OP_LW:
            NEXT_STATE.REGS[dcd_rt] = mem_read_32(CURRENT_STATE.REGS[dcd_rs] + dcd_se_imm);
            break;
        //OP_SB
        case OP_SB:{
            uint32_t address = dcd_se_imm + CURRENT_STATE.REGS[dcd_rs];
            mem_write_32((address), ((CURRENT_STATE.REGS[dcd_rt]) & 0xFF));
            break;
        }
        //OP_SW
        case OP_SW:
            address = dcd_se_imm + CURRENT_STATE.REGS[dcd_rs];
            mem_write_32(address, (int)CURRENT_STATE.REGS[dcd_rt]);      
            break;
      

        /*bonus*/
        //bonus9
        case OP_SLTI:
            if((int32_t)CURRENT_STATE.REGS[dcd_rs] < (int32_t)dcd_se_imm)
                NEXT_STATE.REGS[dcd_rt] = 1;
            else 
                NEXT_STATE.REGS[dcd_rt] = 0;
            break;
        //bonus10
        case OP_SLTIU:
            if(CURRENT_STATE.REGS[dcd_rs] < dcd_se_imm)
                NEXT_STATE.REGS[dcd_rt] = 1;
            else
                NEXT_STATE.REGS[dcd_rt] = 0;
            break;
}
        
        CURRENT_STATE.REGS[0] = 0;
}

void process_instruction()
{
    fetch();
    decode();
    execute();
    /* execute one instruction here. You should use CURRENT_STATE and modify
     * values in NEXT_STATE. You can call mem_read_32() and mem_write_32() to
     * access memory. */
}
