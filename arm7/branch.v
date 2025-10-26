module branch(
    input clk,
    input en,
    input cond,
    input link,
    input[23:0] offset,
    output reg write_en,
    output reg[3:0] write_reg,
    output reg[31:0] write_value,
    output reg read_en,
    output reg[3:0] read_reg,
    input[31:0] read_value
);

    reg[1:0] tempLink = 2'b00;
    reg[1:0] temp = 2'b00;

    always @(posedge clk) begin
        if (en || tempLink != 2'b00 || temp != 2'b00) begin
            if (cond) begin
                if (link && tempLink != 2'b11) begin
                    case (tempLink)
                        2'b00:
                            read_en <= 1;
                            read_reg <= 15;
                            tempLink <= tempLink + 1;
                        2'b01:
                            read_en <= 0;
                            tempLink <= tempLink + 1;
                        2'b10:
                            write_en <= 1;
                            write_reg <= 14;
                            write_value <= read_value + 4;
                            tempLink <= tempLink + 1;
                    endcase
                end else begin
                    if (tempLink == 2'b11) begin
                        write_en <= 0;
                    end
                    case (temp)
                        2'b00:
                            read_en <= 1;
                            read_reg <= 15;
                            temp <= temp + 1;
                        2'b01:
                            read_en <= 0;
                            temp <= temp + 1;
                        2'b10:
                            write_en <= 1;
                            write_reg <= 15;
                            write_value <= read_value + 8 + ({{6{offset[23]}}, offset, 2'b00});
                            temp <= temp + 1;
                        2'b11:
                            write_en <= 0;
                            temp <= 0;
                            tempLink <= 0;
                    endcase
                end            
            end else begin
                case (temp)
                    2'b00:
                        read_en <= 1;
                        read_reg <= 15;
                        temp <= temp + 1;
                    2'b01:
                        read_en <= 0;
                        temp <= temp + 1;
                    2'b10:
                        write_en <= 1;
                        write_reg <= 15;
                        write_value <= read_value + 4;
                        temp <= temp + 1;
                    2'b11:
                        write_en <= 0;
                        temp <= 0;
                        tempLink <= 0;
                endcase
            end
        end
    end

endmodule
