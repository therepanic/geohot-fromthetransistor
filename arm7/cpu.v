module cpu(
    input clk,
    input fetch_en,

    // instruction memory
    output instr_read_en,
    output [31:0] instr_read_addr,
    input [31:0] instr_read_instr,

    // register file
    output reg_write_en,
    output [3:0] reg_write_reg,
    output [31:0] reg_write_value,
    output reg_write_restore_from_SPSR,
    output reg_read_en,
    output [3:0] reg_read_reg,
    input [31:0] reg_read_value,
    output cpsr_read_en,
    input [31:0] cpsr_read_value,
    output cpsr_write_en,
    output [31:0] cpsr_write_value,
    output mode_read_en,
    input [31:0] mode_read_value,

    // fetch → decoder
    output decode_en,
    output [31:0] instr,
    
    // decoder → alu
    output alu_en,
    output alu_immediate,
    output [3:0] alu_opcode,
    output alu_s,
    output [3:0] alu_rn,
    output [3:0] alu_rd,
    output [11:0] alu_operand2,
    
    // decoder → branch
    output branch_en,
    output branch_cond,
    output branch_link,
    output [23:0] branch_offset,
    
    // decoder → sdt
    output sdt_en,
    output sdt_immediate,
    output sdt_pre,
    output sdt_up,
    output sdt_word,
    output sdt_write,
    output sdt_load,
    output [3:0] sdt_rn,
    output [3:0] sdt_rd,
    output [11:0] sdt_offset,

    // sdt → data memory
    output data_write_word_en,
    output data_write_byte_en,
    output data_read_word_en,
    output data_read_byte_en,
    output [31:0] data_write_word_address,
    output [31:0] data_write_byte_address,
    output [31:0] data_write_word_data,
    output [7:0] data_write_byte_data,
    output [31:0] data_read_word_address,
    output [31:0] data_read_byte_address,
    input [31:0] data_read_word_data,
    input [7:0] data_read_byte_data,

    // busy/finished
    input all_busy,
    output finished,

    input decoder_busy,
    input branch_busy,
    input sdt_busy
);
    wire all_busy;
    assign all_busy = alu_busy | decoder_busy | branch_busy | sdt_busy;

    fetch u_fetch(
        .clk(clk),
        .en(fetch_en),
        .decode_en(decode_en),
        .instr(instr),
        .instr_read_en(instr_read_en),
        .instr_read_addr(instr_read_addr),
        .instr_read_instr(instr_read_instr),
        .reg_read_en(reg_read_en),
        .reg_read_reg(reg_read_reg),
        .reg_read_value(reg_read_value),
        .reg_write_en(reg_write_en),
        .reg_write_reg(reg_write_reg),
        .reg_write_value(reg_write_value),
        .reg_write_restore_from_SPSR(reg_write_restore_from_SPSR),
        .all_busy(all_busy)
    );

    decoder u_decoder(
        .clk(clk),
        .decode_en(decode_en),
        .instr(instr),
        .cpsr_read_en(cpsr_read_en),
        .cpsr_read_value(cpsr_read_value),
        .branch_en(branch_en),
        .branch_cond(branch_cond),
        .branch_link(branch_link),
        .branch_offset(branch_offset),
        .alu_en(alu_en),
        .alu_immediate(alu_immediate),
        .alu_opcode(alu_opcode),
        .alu_s(alu_s),
        .alu_rn(alu_rn),
        .alu_rd(alu_rd),
        .alu_operand2(alu_operand2),
        .sdt_en(sdt_en),
        .sdt_immediate(sdt_immediate),
        .sdt_pre(sdt_pre),
        .sdt_up(sdt_up),
        .sdt_word(sdt_word),
        .sdt_write(sdt_write),
        .sdt_load(sdt_load),
        .sdt_rn(sdt_rn),
        .sdt_rd(sdt_rd),
        .sdt_offset(sdt_offset),
        .busy(decoder_busy)
    );

    alu u_alu(
        .clk(clk),
        .en(alu_en),
        .immediate(alu_immediate),
        .opcode(alu_opcode),
        .s(alu_s),
        .rn(alu_rn),
        .rd(alu_rd),
        .operand2(alu_operand2),
        .write_restore_from_SPSR(reg_write_restore_from_SPSR),
        .write_en(reg_write_en),
        .write_reg(reg_write_reg),
        .write_value(reg_write_value),
        .read_en(reg_read_en),
        .read_reg(reg_read_reg),
        .read_value(reg_read_value),
        .mode_read_en(mode_read_en),
        .mode_read_value(mode_read_value),
        .cpsr_read_en(cpsr_read_en),
        .cpsr_read_value(cpsr_read_value),
        .cpsr_write_en(cpsr_write_en),
        .cpsr_write_value(cpsr_write_value),
        .busy(alu_busy)
    );

    branch u_branch(
        .clk(clk),
        .en(branch_en),
        .cond(branch_cond),
        .link(branch_link),
        .offset(branch_offset),
        .write_restore_from_SPSR(reg_write_restore_from_SPSR),
        .write_en(reg_write_en),
        .write_reg(reg_write_reg),
        .write_value(reg_write_value),
        .read_en(reg_read_en),
        .read_reg(reg_read_reg),
        .read_value(reg_read_value),
        .busy(branch_busy)
    );

    sdt u_sdt(
        .clk(clk),
        .en(sdt_en),
        .immediate(sdt_immediate),
        .pre(sdt_pre),
        .up(sdt_up),
        .word(sdt_word),
        .write(sdt_write),
        .load(sdt_load),
        .rn(sdt_rn),
        .rd(sdt_rd),
        .offset(sdt_offset),
        .write_restore_from_SPSR(reg_write_restore_from_SPSR),
        .write_en(reg_write_en),
        .write_reg(reg_write_reg),
        .write_value(reg_write_value),
        .read_en(reg_read_en),
        .read_reg(reg_read_reg),
        .read_value(reg_read_value),
        .data_write_word_en(data_write_word_en),
        .data_write_byte_en(data_write_byte_en),
        .data_read_word_en(data_read_word_en),
        .data_read_byte_en(data_read_byte_en),
        .data_write_word_address(data_write_word_address),
        .data_write_byte_address(data_write_byte_address),
        .data_write_word_data(data_write_word_data),
        .data_write_byte_data(data_write_byte_data),
        .data_read_word_address(data_read_word_address),
        .data_read_byte_address(data_read_byte_address),
        .data_read_word_data(data_read_word_data),
        .data_read_byte_data(data_read_byte_data),
        .busy(sdt_busy)
    );

    instruction_memory u_imem(
        .clk(clk),
        .read_en(instr_read_en),
        .read_addr(instr_read_addr),
        .read_instr(instr_read_instr)
    );

    data_memory u_dmem(
        .clk(clk),
        .write_word_en(data_write_word_en),
        .write_byte_en(data_write_byte_en),
        .read_word_en(data_read_word_en),
        .read_byte_en(data_read_byte_en),
        .write_word_address(data_write_word_address),
        .write_byte_address(data_write_byte_address),
        .write_word_data(data_write_word_data),
        .write_byte_data(data_write_byte_data),
        .read_word_address(data_read_word_address),
        .read_byte_address(data_read_byte_address),
        .read_word_data(data_read_word_data),
        .read_byte_data(data_read_byte_data)
    );

endmodule
