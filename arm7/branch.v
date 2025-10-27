module branch(
    input clk,
    input en,
    input cond,
    input link,
    input[23:0] offset,
    output write_restore_from_SPSR,
    output reg write_en,
    output reg[3:0] write_reg,
    output reg[31:0] write_value,
    output reg read_en,
    output reg[3:0] read_reg,
    input[31:0] read_value
);
    reg state = 0;

    reg cur_cond;
    reg cur_link;
    reg[23:0] cur_offset;

    reg[1:0] temp_link = 2'b00;
    reg[1:0] temp = 2'b00;

    always @(posedge clk) begin
        if (en || temp_link != 2'b00 || temp != 2'b00) begin
            if (!state && en && temp_link == 2'b00 && temp == 2'b00) begin
                cur_cond <= cond;
                cur_link <= link;
                cur_offset <= offset;
                state <= 1;
            end
            if (state && cur_cond) begin
                if (cur_link && temp_link != 2'b11) begin
                    case (temp_link)
                        2'b00: begin
                            read_en <= 1;
                            read_reg <= 15;
                            temp_link <= temp_link + 1;
                        end
                        2'b01: begin
                            read_en <= 0;
                            temp_link <= temp_link + 1;
                        end
                        2'b10: begin
                            write_en <= 1;
                            write_restore_from_SPSR <= 0;
                            write_reg <= 14;
                            write_value <= read_value + 4;
                            temp_link <= temp_link + 1;
                        end
                    endcase
                end else begin
                    if (temp_link == 2'b11) begin
                        write_en <= 0;
                    end
                    case (temp)
                        2'b00: begin
                            read_en <= 1;
                            read_reg <= 15;
                            temp <= temp + 1;
                        end
                        2'b01: begin
                            read_en <= 0;
                            temp <= temp + 1;
                        end
                        2'b10: begin
                            write_en <= 1;
                            write_restore_from_SPSR <= 0;
                            write_reg <= 15;
                            write_value <= read_value + 8 + ({{6{cur_offset[23]}}, cur_offset, 2'b00});
                            temp <= temp + 1;
                        end
                        2'b11: begin
                            write_en <= 0;
                            temp <= 0;
                            temp_link <= 0;
                            state <= 0;
                        end
                    endcase
                end            
            end else if (state) begin
                case (temp)
                    2'b00: begin
                        read_en <= 1;
                        read_reg <= 15;
                        temp <= temp + 1;
                    end
                    2'b01: begin
                        read_en <= 0;
                        temp <= temp + 1;
                    end
                    2'b10: begin
                        write_en <= 1;
                        write_restore_from_SPSR <= 0;
                        write_reg <= 15;
                        write_value <= read_value + 4;
                        temp <= temp + 1;
                    end
                    2'b11: begin
                        write_en <= 0;
                        temp <= 0;
                        temp_link <= 0;
                        state <= 0;
                    end
                endcase
            end
        end
    end

endmodule
