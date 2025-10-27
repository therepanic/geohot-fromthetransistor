`timescale 1ns / 1ps

module test_register_file();
    reg clk;
    reg write_en;
    reg [3:0] write_reg;
    reg [31:0] write_value;
    reg write_restore_from_SPSR;
    reg read_en;
    reg [3:0] read_reg;
    wire [31:0] read_value;
    reg cpsr_read_en;
    wire [31:0] cpsr_read_value;
    reg cpsr_write_en;
    reg [31:0] cpsr_write_value;
    reg mode_read_en;
    reg [31:0] mode_read_value;

    register_file uut(
        .clk(clk),
        .write_en(write_en),
        .write_reg(write_reg),
        .write_value(write_value),
        .write_restore_from_SPSR(write_restore_from_SPSR),
        .read_en(read_en),
        .read_reg(read_reg),
        .read_value(read_value),
        .cpsr_read_en(cpsr_read_en),
        .cpsr_read_value(cpsr_read_value),
        .cpsr_write_en(cpsr_write_en),
        .cpsr_write_value(cpsr_write_value)
        .mode_read_en(mode_read_en)
        .mode_read_value(mode_read_value)
    );

    parameter CLK_PERIOD = 10;

    initial begin
        clk = 0;
        forever #(CLK_PERIOD/2) clk = ~clk;
    end

    initial begin
        write_en = 0;
        read_en = 0;
        cpsr_read_en = 0;
        cpsr_write_en = 0;
        write_restore_from_SPSR = 0;
        write_reg = 0;
        write_value = 0;
        read_reg = 0;
        cpsr_write_value = 0;
        mode_read_en = 0;
        mode_read_value = 0;

        $display("--- Running tests for ARM7 register file");

        $display("T=%0t: TEST 1: Writing 0xDEADBEEF to R4", $time);
        write_en <= 1;
        write_reg <= 4;
        write_value <= 32'hDEADBEEF;
        @(posedge clk);
        write_en <= 0;
        @(posedge clk);

        $display("T=%0t: Reading R4 (expecting 0xDEADBEEF)", $time);
        read_en <= 1;
        read_reg <= 4;
        @(posedge clk);
        read_en <= 0;
        @(posedge clk);
        if (read_value == 32'hDEADBEEF)
            $display("T=%0t: R4 Read: OK (%h)", $time, read_value);
        else
            $display("T=%0t: R4 Read: ERROR. Expected 0xDEADBEEF, got %h", $time, read_value);

        $display("T=%0t: TEST 2: Writing 0x00001000 to SP (R13)", $time);
        write_en <= 1;
        write_reg <= 13;
        write_value <= 32'h00001000;
        @(posedge clk);
        write_en <= 0;
        @(posedge clk);

        $display("T=%0t: Reading SP (expecting 0x00001000)", $time);
        read_en <= 1;
        read_reg <= 13;
        @(posedge clk);
        read_en <= 0;
        @(posedge clk);
        if (read_value == 32'h00001000)
            $display("T=%0t: SP Read: OK (%h)", $time, read_value);
        else
            $display("T=%0t: SP Read: ERROR. Expected 0x00001000, got %h", $time, read_value);

        $display("T=%0t: TEST 3: Writing 0x00002000 to LR (R14)", $time);
        write_en <= 1;
        write_reg <= 14;
        write_value <= 32'h00002000;
        @(posedge clk);
        write_en <= 0;
        @(posedge clk);

        $display("T=%0t: Reading LR (expecting 0x00002000)", $time);
        read_en <= 1;
        read_reg <= 14;
        @(posedge clk);
        read_en <= 0;
        @(posedge clk);
        if (read_value == 32'h00002000)
            $display("T=%0t: LR Read: OK (%h)", $time, read_value);
        else
            $display("T=%0t: LR Read: ERROR. Expected 0x00002000, got %h", $time, read_value);

        $display("T=%0t: TEST 4: Writing 0x00003004 to PC (R15)", $time);
        write_en <= 1;
        write_reg <= 15;
        write_value <= 32'h00003004;
        write_restore_from_SPSR <= 0;
        @(posedge clk);
        write_en <= 0;
        @(posedge clk);

        $display("T=%0t: Reading PC (expecting 0x00003004)", $time);
        read_en <= 1;
        read_reg <= 15;
        @(posedge clk);
        read_en <= 0;
        @(posedge clk);
        if (read_value == 32'h00003004)
            $display("T=%0t: PC Read: OK (%h)", $time, read_value);
        else
            $display("T=%0t: PC Read: ERROR. Expected 0x00003004, got %h", $time, read_value);

        $display("T=%0t: TEST 5: Writing 0xCAFEBABE to CPSR", $time);
        cpsr_write_en <= 1;
        cpsr_write_value <= 32'hCAFEBABE;
        @(posedge clk);
        cpsr_write_en <= 0;
        @(posedge clk);

        $display("T=%0t: Reading CPSR (expecting 0xCAFEBABE)", $time);
        cpsr_read_en <= 1;
        @(posedge clk);
        cpsr_read_en <= 0;
        @(posedge clk);
        if (cpsr_read_value == 32'hCAFEBABE)
            $display("T=%0t: CPSR Read: OK (%h)", $time, cpsr_read_value);
        else
            $display("T=%0t: CPSR Read: ERROR. Expected 0xCAFEBABE, got %h", $time, cpsr_read_value);

        $display("--- Test finished ---");
        $finish;
    end

endmodule
