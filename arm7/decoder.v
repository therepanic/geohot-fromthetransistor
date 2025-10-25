module decoder(
    input clk,
    input decode_en,
    input[31:0] instr
);
    reg[31:0] opcode;
    reg[2:0] type;
    always @(posedge clk) begin
        if (decode_en) begin
            opcode = instr[31:28];
            type = instr[27:25];
            case (type)
                3'b000, 3'b001:
                     //data processing
                3'b010, 3'b011:
                    //single data transfer
                3'b101:
                    //branch
            endcase
        end
    end
endmodule