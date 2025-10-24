module instruction_memory(
    input clk,
    input read_en,
    input[31:0] read_addr,
    output reg[31:0] read_instr
);
    reg[31:0] memory[0:1023];

    initial begin
        $readmemh("instr_firmware.hex", memory);
    end

    always @(posedge clk) begin
        if (read_en) begin
            read_instr <= memory[read_addr[11:2]];
        end
    end
endmodule
