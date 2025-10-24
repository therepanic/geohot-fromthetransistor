`timescale 1ns / 1ps

module test_instruction_memory();
    reg clk;
    reg read_en;
    reg [31:0] read_addr;
    wire [31:0] read_instr;

    instruction_memory im (
        .clk(clk),
        .read_en(read_en),
        .read_addr(read_addr),
        .read_instr(read_instr)
    );

    parameter CLK_PERIOD = 10;
    initial begin
        clk = 0;
        forever #(CLK_PERIOD / 2) clk = ~clk;
    end

    initial begin
        $display("--- Running tests for ARM7 instruction memory");

        read_en <= 1;
        read_addr = 0;

        repeat (4) begin
            @(posedge clk);
            read_en <= 0;
            @(posedge clk);

            $display("PC = %0d | instr = %h", read_addr, read_instr);
            read_addr <= read_addr + 4;
            read_en <= 1;
        end

        $display("--- Test finished ---");
        $finish;
    end

endmodule
