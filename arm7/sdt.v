module sdt(
    input clk,
    input en,
    input immediate,
    input pre,
    input up,
    input word,
    input write,
    input load,
    input[3:0] rn,
    input[3:0] rd,
    input[11:0] offset,
    output reg write_restore_from_SPSR,
    output reg write_en,
    output reg[3:0] write_reg,
    output reg[31:0] write_value,
    output reg read_en,
    output reg[3:0] read_reg,
    input[31:0] read_value,
    output reg data_write_word_en,
    output reg data_write_byte_en,
    output reg data_read_word_en,
    output reg data_read_byte_en,
    output reg[31:0] data_write_word_address,
    output reg[31:0] data_write_byte_address,
    output reg[31:0] data_write_word_data,
    output reg[7:0] data_write_byte_data,
    output reg[31:0] data_read_word_address,
    output reg[31:0] data_read_byte_address,
    input[31:0] data_read_word_data,
    input[7:0] data_read_byte_data
);

    reg state = 0;

    reg cur_immediate;
    reg cur_pre;
    reg cur_up;
    reg cur_word;
    reg cur_write;
    reg cur_load;
    reg[3:0] cur_rn;
    reg[3:0] cur_rd;
    reg[11:0] cur_offset;

    reg[2:0] got_op1 = 0;
    reg[1:0] got_addr = 0;
    reg[2:0] got_load = 0;
    reg[2:0] got_write = 0;

    reg[31:0] temp_op1;
    reg signed[31:0] signed_offset;
    reg[31:0] addr;

    always @(posedge clk) begin
        if (en || state) begin
            if (!state) begin
                cur_immediate <= immediate;
                cur_pre <= pre;
                cur_up <= up;
                cur_word <= word;
                cur_write <= write;
                cur_load <= load;
                cur_rn <= rn;
                cur_rd <= rd;
                cur_offset <= offset;
                state <= 1;
            end
            case (got_op1)
                0: begin
                    read_en <= 1;
                    read_reg <= rn;
                    got_op1 <= got_op1 + 1;
                end
                1: begin
                    read_en <= 0;
                    got_op1 <= got_op1 + 1;
                end
                2: begin
                    temp_op1 <= read_value;
                    got_op1 <= got_op1 + 1;
                end
                3: begin
                    if (cur_immediate) begin
                        signed_offset <=  cur_up ?  $signed({{20{cur_offset[11]}},cur_offset})
                        : -$signed({{20{cur_offset[11]}},cur_offset});
                        got_op1 <= 6;
                    end else begin
                        // todo: shift for register, now we not support this feature
                        read_en <= 1;
                        read_reg <= cur_offset[3:0];
                        got_op1 <= got_op1 + 1;
                    end
                end
                4: begin
                    read_en <= 0;
                    got_op1 <= got_op1 + 1;
                end
                5: begin
                    signed_offset <= cur_up ?  read_value : -read_value;
                    got_op1 <= got_op1 + 1;
                end
                6: begin
                    case (got_addr)
                        0: begin
                            addr <= cur_pre ? temp_op1 + signed_offset : temp_op1;
                            got_addr <= got_addr + 1;
                        end
                        1: begin
                            case (got_load)
                                0: begin
                                    if (cur_load) begin
                                        if (cur_word) begin
                                            data_read_word_en <= 1;
                                            data_read_word_address <= addr;
                                        end else begin
                                            data_read_byte_en <= 1;
                                            data_read_byte_address <= addr;
                                        end  
                                    end else begin
                                        read_en <= 1;
                                        read_reg <= rd;
                                    end
                                    got_load <= got_load + 1;
                                end
                                1: begin
                                    if (cur_load) begin
                                        if (cur_word) begin
                                            data_read_word_en <= 0;
                                        end else begin
                                            data_read_byte_en <= 0;
                                        end
                                    end else begin
                                        read_en <= 0;
                                    end
                                    got_load <= got_load + 1;
                                end
                                2: begin
                                    if (cur_load) begin
                                        if (cur_word) begin
                                            write_en <= 1;
                                            write_reg <= rd;
                                            write_value <= data_read_word_data;
                                            write_restore_from_SPSR <= 0;
                                        end else begin
                                            write_en <= 1;
                                            write_reg <= rd;
                                            write_value <= data_read_byte_data;
                                            write_restore_from_SPSR <= 0;
                                        end
                                        got_load <= got_load + 1;
                                    end else begin
                                        if (cur_word) begin
                                            data_write_word_en <= 1;
                                            data_write_word_address <= addr;
                                            data_write_word_data <= read_value;
                                        end else begin
                                            data_write_byte_en <= 1;
                                            data_write_byte_address <= addr;
                                            data_write_byte_data <= read_value;
                                        end
                                        got_load <= got_load + 1;
                                    end
                                end
                                3: begin
                                    if (cur_load) begin
                                        write_en <= 0;
                                    end else begin
                                        if (cur_word) begin
                                            data_write_word_en <= 0;
                                        end else begin
                                            data_write_byte_en <= 0;
                                        end
                                    end
                                    got_load <= got_load + 1;
                                end
                                4: begin
                                    case (got_write)
                                        0: begin
                                            if (cur_write) begin
                                                if (cur_pre) begin
                                                    write_en <= 1;
                                                    write_restore_from_SPSR <= 0;
                                                    write_value <= addr;
                                                    write_reg <= cur_rn;
                                                end else begin
                                                    write_en <= 1;
                                                    write_restore_from_SPSR <= 0;
                                                    write_value <= temp_op1 + signed_offset;
                                                    write_reg <= cur_rn;
                                                end
                                            end
                                            got_write <= got_write + 1;
                                        end
                                        1: begin
                                            write_en <= 0;
                                            got_op1 <= 0;
                                            got_addr <= 0;
                                            got_load <= 0;
                                            got_write <= 0;
                                            state <= 0;
                                        end
                                    endcase
                                end
                            endcase
                        end
                    endcase
                end
            endcase
        end
    end

endmodule