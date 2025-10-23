`timescale 1ns / 1ps
module test_data_memory();
    reg clk;
    reg write_word_en;
    reg write_byte_en;
    reg read_word_en;
    reg read_byte_en;
    reg [31:0] write_word_address;
    reg [31:0] write_byte_address;
    reg [31:0] write_word_data;
    reg [7:0] write_byte_data;
    reg [31:0] read_word_address;
    reg [31:0] read_byte_address;

    wire [31:0] read_word_data;
    wire [7:0] read_byte_data;

    data_memory dut (
        .clk(clk),
        .write_word_en(write_word_en),
        .write_byte_en(write_byte_en),
        .read_word_en(read_word_en),
        .read_byte_en(read_byte_en),
        .write_word_address(write_word_address),
        .write_byte_address(write_byte_address),
        .write_word_data(write_word_data),
        .write_byte_data(write_byte_data),
        .read_word_address(read_word_address),
        .read_byte_address(read_byte_address),
        .read_word_data(read_word_data),
        .read_byte_data(read_byte_data)
    );

    parameter CLK_PERIOD = 10;
    initial begin
        clk = 1'b0;
        forever #(CLK_PERIOD/2) clk = ~clk;
    end

    initial begin
        write_word_en = 0; write_byte_en = 0;
        read_word_en = 0; read_byte_en = 0;
        write_word_address = 0; write_byte_address = 0;
        write_word_data = 0; write_byte_data = 0;
        read_word_address = 0; read_byte_address = 0;
        $display("--- Running tests for ARM7 data memory");
        # (2 * CLK_PERIOD);
        
        $display("T=%0t: TEST 1: Writing the word 0xDEADBEEF to address 0x1000", $time);
        write_word_en = 1;
        write_word_address = 32'h1000;
        write_word_data = 32'hDEADBEEF;
        @(posedge clk);
        write_word_en = 0;

        $display("T=%0t: TEST 2: Reading a word from address 0x1000 (expecting 0xDEADBEEF)", $time);
        read_word_en = 1;
        read_word_address = 32'h1000;
        @(posedge clk);
        read_word_en = 0;
        @(posedge clk);

        if (read_word_data == 32'hDEADBEEF) begin
            $display("T=%0t: Word Read: OK. Received 0x%h", $time, read_word_data);
        end else begin
            $display("T=%0t: Word Read: ERROR. Expected 0xDEADBEEF, received 0x%h", $time, read_word_data);
        end

        # (2 * CLK_PERIOD);

        $display("T=%0t: TEST 3: Writing byte 0xAA to address 0x1001", $time);
        write_byte_en = 1;
        write_byte_address = 32'h1001;
        write_byte_data = 8'hAA;
        @(posedge clk);
        write_byte_en = 0;

        $display("T=%0t: Writing the second byte 0x55 to address 0x1003", $time);
        write_byte_en = 1;
        write_byte_address = 32'h1003;
        write_byte_data = 8'h55;
        @(posedge clk);
        write_byte_en = 0;
        
        # (2 * CLK_PERIOD);

        $display("T=%0t: TEST 4: Reading a byte from address 0x1001 (expecting 0xAA)", $time);
        read_byte_en = 1;
        read_byte_address = 32'h1001;
        @(posedge clk);
        read_byte_en = 0;
        @(posedge clk);

        if (read_byte_data == 8'hAA) begin
            $display("T=%0t: Byte Read 1: OK. Received 0x%h", $time, read_byte_data);
        end else begin
            $display("T=%0t: Byte Read 1: ERROR. Expected 0xAA, received 0x%h", $time, read_byte_data);
        end

        $display("T=%0t: Reading a byte from address 0x1003 (expecting 0x55)", $time);
        read_byte_en = 1;
        read_byte_address = 32'h1003;
        @(posedge clk);
        read_byte_en = 0;
        @(posedge clk);

        if (read_byte_data == 8'h55) begin
            $display("T=%0t: Byte Read 1: OK. Received 0x%h", $time, read_byte_data);
        end else begin
            $display("T=%0t: Byte Read 1: ERROR. Expected 0xAA, received 0x%h", $time, read_byte_data);
        end

        # (2 * CLK_PERIOD);

        $display("--- Test finished ---");
        $finish;
    end
endmodule