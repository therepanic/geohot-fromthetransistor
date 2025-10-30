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

    input all_busy
);

    reg state = 0;

    reg[1:0] get_pc = 0;
    reg[1:0] get_instr = 0;
    reg[1:0] load_instr = 0;

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
                            instr_read_en <= 1;
                            instr_read_addr <= reg_read_value + 4;
                            get_instr <= get_instr + 1;
                        end
                        1: begin
                            instr_read_en <= 0;
                            get_instr <= get_instr + 1;
                        end
                        2: begin
                            case (load_instr)
                                0: begin
                                    decode_en <= 1;
                                    instr <= instr_read_instr;
                                    load_instr <= load_instr + 1;
                                end
                                1: begin
                                    load_instr <= load_instr + 1;
                                end
                                2: begin
                                    if (!all_busy) begin
                                        decode_en <= 0;
                                        get_pc <= 0;
                                        get_instr <= 0;
                                        load_instr <= 0;
                                        state <= 0;
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