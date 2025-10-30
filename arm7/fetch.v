module fetch(
    input clk,
    input en,

    output reg decode_en,
    output reg[31:0] instr,

    output reg instr_read_en,
    output reg[31:0] instr_read_addr,
    input[31:0] instr_read_instr,

    output reg reg_read_en,
    output reg[3:0] reg_read_reg,
    input[31:0] reg_read_value,

    output reg reg_write_en,
    output reg[3:0] reg_write_reg,
    output reg[31:0] reg_write_value,
    output reg reg_write_restore_from_SPSR,

    input all_busy
);

    reg state = 0;

    reg[31:0] cur_pc;
    reg new_pc = 0;

    reg[1:0] get_pc = 0;
    reg[1:0] get_instr = 0;
    reg[1:0] load_instr = 0;
    reg[2:0] update_pc = 0;

    always @(posedge clk) begin
        if (en || state) begin
            if (!state) begin
                state <= 1;
            end
            case (get_pc)
                0: begin
                    reg_read_en <= 1;
                    reg_read_reg <= 15;
                    get_pc <= get_pc + 1;
                end
                1: begin
                    reg_read_en <= 0;
                    get_pc <= get_pc + 1;
                end
                2: begin
                    case (get_instr)
                        0: begin
                            cur_pc <= reg_read_value;
                            instr_read_en <= 1;
                            instr_read_addr <= reg_read_value;
                            get_instr <= get_instr + 1;
                        end
                        1: begin
                            instr_read_en <= 0;
                            get_instr <= get_instr + 1;
                        end
                        2: begin
                            case (load_instr)
                                0: begin
                                    if (instr_read_instr == 0) begin
                                        reg_write_en <= 0;
                                        decode_en <= 0;
                                        get_pc <= 0;
                                        get_instr <= 0;
                                        load_instr <= 0;
                                        update_pc <= 0;
                                        state <= 0;
                                        // finish
                                        en <= 0;
                                    end else begin
                                        decode_en <= 1;
                                        instr <= instr_read_instr;
                                        load_instr <= load_instr + 1;
                                    end
                                end
                                1: begin
                                    load_instr <= load_instr + 1;
                                end
                                2: begin
                                    if (!all_busy) begin
                                        case (update_pc)
                                            0: begin
                                                reg_read_en <= 1;
                                                reg_read_reg <= 15;
                                                update_pc <= update_pc + 1;
                                            end
                                            1: begin
                                                reg_read_en <= 0;
                                                update_pc <= update_pc + 1;
                                            end
                                            2: begin
                                                if (reg_read_value != cur_pc) begin
                                                    new_pc <= 1;
                                                end else begin
                                                    new_pc <= 0;
                                                end
                                                cur_pc <= reg_read_value;
                                                update_pc <= update_pc + 1;
                                            end
                                            3: begin
                                                reg_write_en <= 1;
                                                reg_write_reg <= 15;
                                                reg_write_restore_from_SPSR <= 0;
                                                if (new_pc) begin
                                                    reg_write_value <= cur_pc;
                                                end else begin
                                                    reg_write_value <= cur_pc + 4;
                                                end
                                                update_pc <= update_pc + 1;
                                            end
                                            4: begin
                                                reg_write_en <= 0;
                                                decode_en <= 0;
                                                get_pc <= 0;
                                                get_instr <= 0;
                                                load_instr <= 0;
                                                update_pc <= 0;
                                                state <= 0;
                                            end
                                        endcase
                                    end
                                end
                            endcase
                        end
                    endcase
                end
            endcase
        end
    end

endmodule