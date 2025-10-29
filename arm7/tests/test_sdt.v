`timescale 1ns / 1ps

module test_sdt();
reg clk;
parameter CLK_PERIOD = 10;
initial begin
    clk = 0;
    forever #(CLK_PERIOD/2) clk = ~clk;
end

reg en, immediate, pre, up, word, write, load;
reg [3:0] rn, rd;
reg [11:0] offset;

wire write_restore_from_SPSR;
wire write_en;
wire [3:0] write_reg;
wire [31:0] write_value;
wire read_en;
wire [3:0] read_reg;

wire data_write_word_en;
wire data_write_byte_en;
wire data_read_word_en;
wire data_read_byte_en;
wire [31:0] data_write_word_address;
wire [31:0] data_write_byte_address;
wire [31:0] data_write_word_data;
wire [7:0] data_write_byte_data;
wire [31:0] data_read_word_address;
wire [31:0] data_read_byte_address;
wire [31:0] data_read_word_data;
wire [7:0] data_read_byte_data;

reg tb_wr_en, tb_wr_restore_from_SPSR, tb_rd_en, tb_cpsr_wr_en, use_tb;
reg [3:0] tb_wr_reg, tb_rd_reg;
reg [31:0] tb_wr_val, tb_cpsr_wr_val;
wire [31:0] tb_rd_val;

wire rf_write_en = use_tb ? tb_wr_en : write_en;
wire [3:0] rf_wr = use_tb ? tb_wr_reg : write_reg;
wire [31:0] rf_wv = use_tb ? tb_wr_val : write_value;
wire rf_wr_restore = use_tb ? tb_wr_restore_from_SPSR : write_restore_from_SPSR;
wire rf_read_en = use_tb ? tb_rd_en : read_en;
wire [3:0] rf_rr = use_tb ? tb_rd_reg : read_reg;
wire rf_cpsr_write_en = use_tb ? tb_cpsr_wr_en : 1'b0;
wire [31:0] rf_cpsr_write_value = use_tb ? tb_cpsr_wr_val : 32'b0;
wire [31:0] mode_read_value;
register_file rf(
    .clk(clk),
    .write_en(rf_write_en),
    .write_reg(rf_wr),
    .write_value(rf_wv),
    .write_restore_from_SPSR(rf_wr_restore),
    .read_en(rf_read_en),
    .read_reg(rf_rr),
    .read_value(tb_rd_val),
    .cpsr_read_en(1'b0),
    .cpsr_read_value(),
    .cpsr_write_en(rf_cpsr_write_en),
    .cpsr_write_value(rf_cpsr_write_value),
    .mode_read_en(1'b0),
    .mode_read_value(mode_read_value)
);

sdt uut(
    .clk(clk),
    .en(en),
    .immediate(immediate),
    .pre(pre),
    .up(up),
    .word(word),
    .write(write),
    .load(load),
    .rn(rn),
    .rd(rd),
    .offset(offset),
    .write_restore_from_SPSR(write_restore_from_SPSR),
    .write_en(write_en),
    .write_reg(write_reg),
    .write_value(write_value),
    .read_en(read_en),
    .read_reg(read_reg),
    .read_value(tb_rd_val),
    .data_write_word_en(data_write_word_en),
    .data_write_byte_en(data_write_byte_en),
    .data_read_word_en(data_read_word_en),
    .data_read_byte_en(data_read_byte_en),
    .data_write_word_address(data_write_word_address),
    .data_write_byte_address(data_write_byte_address),
    .data_write_word_data(data_write_word_data),
    .data_write_byte_data(data_write_byte_data),
    .data_read_word_address(data_read_word_address),
    .data_read_byte_address(data_read_byte_address),
    .data_read_word_data(data_read_word_data),
    .data_read_byte_data(data_read_byte_data)
);

data_memory mem(
    .clk(clk),
    .write_word_en(data_write_word_en),
    .write_byte_en(data_write_byte_en),
    .read_word_en(data_read_word_en),
    .read_byte_en(data_read_byte_en),
    .write_word_address(data_write_word_address),
    .write_byte_address(data_write_byte_address),
    .write_word_data(data_write_word_data),
    .write_byte_data(data_write_byte_data),
    .read_word_address(data_read_word_address),
    .read_byte_address(data_read_byte_address),
    .read_word_data(data_read_word_data),
    .read_byte_data(data_read_byte_data)
);

initial begin
    uut.state = 0;
    uut.cur_immediate = 0;
    uut.cur_pre = 0;
    uut.cur_up = 0;
    uut.cur_word = 0;
    uut.cur_write = 0;
    uut.cur_load = 0;
    uut.cur_rn = 0;
    uut.cur_rd = 0;
    uut.cur_offset = 0;
    uut.got_op1 = 0;
    uut.got_addr = 0;
    uut.got_load = 0;
    uut.got_write = 0;
    uut.temp_op1 = 0;
    uut.signed_offset = 0;
    uut.addr = 0;
end

reg [31:0] val;

initial begin
    en = 0; immediate = 0; pre = 0; up = 0; word = 0; write = 0; load = 0;
    rn = 0; rd = 0; offset = 0;
    use_tb = 0;
    tb_wr_en = 0; tb_wr_reg = 0; tb_wr_val = 0; tb_wr_restore_from_SPSR = 0;
    tb_rd_en = 0; tb_rd_reg = 0;
    tb_cpsr_wr_en = 0; tb_cpsr_wr_val = 0;

    repeat (2) @(posedge clk);
    $display("--- Running tests for ARM7 sdt");

    // init registers
    use_tb <= 1;
    tb_wr_en <= 1; tb_wr_reg <= 1; tb_wr_val <= 32'd100; @(posedge clk); // R1 = 100 (base)
    tb_wr_en <= 0; @(posedge clk);
    tb_wr_en <= 1; tb_wr_reg <= 3; tb_wr_val <= 32'hDEADBEEF; @(posedge clk); // R3 = data for store
    tb_wr_en <= 0; @(posedge clk);
    tb_wr_en <= 1; tb_wr_reg <= 2; tb_wr_val <= 32'd200; @(posedge clk); // R2 = 200 (base for byte tests)
    tb_wr_en <= 0; @(posedge clk);
    tb_wr_en <= 1; tb_wr_reg <= 5; tb_wr_val <= 32'h000000AA; @(posedge clk); // R5 = 0xAA (byte data)
    tb_wr_en <= 0; @(posedge clk);
    tb_wr_en <= 1; tb_wr_reg <= 6; tb_wr_val <= 32'h00003000; @(posedge clk); // R6 = 0x3000
    tb_wr_en <= 0; @(posedge clk);
    tb_wr_en <= 1; tb_wr_reg <= 7; tb_wr_val <= 32'h00000077; @(posedge clk); // R7 = 0x77 (byte)
    tb_wr_en <= 0; @(posedge clk);
    use_tb <= 0; repeat (2) @(posedge clk);

    // TEST 1: store word R3 -> [R1 + 4], write-back R1 := R1 + 4
    en <= 1; immediate <= 1; pre <= 1; up <= 1; word <= 1; write <= 1; load <= 0;
    rn <= 4'd1; rd <= 4'd3; offset <= 12'h004;
    @(posedge clk); @(posedge clk); en <= 0;
    repeat (40) @(posedge clk);

    // check R1 == 100 + 4
    use_tb <= 1; tb_rd_en <= 1; tb_rd_reg <= 4'd1; @(posedge clk);
    tb_rd_en <= 0; @(posedge clk); val = tb_rd_val; use_tb <= 0;
    if (val == 32'd104) $display("STORE-WB OK RN=%h", val); else $display("STORE-WB ERR RN=%h (expected 104)", val);

    // load back word to R10
    en <= 1; immediate <= 1; pre <= 1; up <= 1; word <= 1; write <= 0; load <= 1;
    rn <= 4'd1; rd <= 4'd10; offset <= 12'h000; // because R1 was updated to addr already (we used pre=1)
    @(posedge clk); @(posedge clk); en <= 0;
    repeat (40) @(posedge clk);
    use_tb <= 1; tb_rd_en <= 1; tb_rd_reg <= 4'd10; @(posedge clk);
    tb_rd_en <= 0; @(posedge clk); val = tb_rd_val; use_tb <= 0;
    if (val == 32'hDEADBEEF) $display("LOAD-BACK OK R10=%h", val); else $display("LOAD-BACK ERR R10=%h (expected DEADBEEF)", val);

    // TEST 2: store byte R5 -> [R2 + 0], no write-back
    use_tb <= 1;
    tb_wr_en <= 1; tb_wr_reg <= 4'd2; tb_wr_val <= 32'd200; @(posedge clk); // ensure R2 = 200
    tb_wr_en <= 0; @(posedge clk);
    use_tb <= 0;
    en <= 1; immediate <= 1; pre <= 1; up <= 1; word <= 0; write <= 1; load <= 0;
    rn <= 4'd2; rd <= 4'd5; offset <= 12'h000;
    @(posedge clk); @(posedge clk); en <= 0;
    repeat (30) @(posedge clk);

    // load byte back to R11
    en <= 1; immediate <= 1; pre <= 1; up <= 1; word <= 0; write <= 0; load <= 1;
    rn <= 4'd2; rd <= 4'd11; offset <= 12'h000;
    @(posedge clk); @(posedge clk); en <= 0;
    repeat (30) @(posedge clk);
    use_tb <= 1; tb_rd_en <= 1; tb_rd_reg <= 4'd11; @(posedge clk);
    tb_rd_en <= 0; @(posedge clk); val = tb_rd_val; use_tb <= 0;
    if ((val & 32'hFF) == 8'hAA) $display("STORE-BYTE OK R11=%h", val & 32'hFF); else $display("STORE-BYTE ERR R11=%h (expected AA)", val & 32'hFF);

    // check R2 unchanged (should be 200)
    use_tb <= 1; tb_rd_en <= 1; tb_rd_reg <= 4'd2; @(posedge clk);
    tb_rd_en <= 0; @(posedge clk); val = tb_rd_val; use_tb <= 0;
    if (val == 32'd200) $display("NO-WB OK RN=%d", val); else $display("NO-WB ERR RN=%d (expected 200)", val);

    // TEST 3: post-index load byte with write-back
    // first store byte at [R6] (post uses base for access)
    en <= 1; immediate <= 1; pre <= 1; up <= 1; word <= 0; write <= 1; load <= 0;
    rn <= 4'd6; rd <= 4'd7; offset <= 12'h000; // store R7 -> [R6]
    @(posedge clk); @(posedge clk); en <= 0;
    repeat (30) @(posedge clk);

    // now post-index load from [R6], offset=4, write-back R6 = R6 + 4
    en <= 1; immediate <= 1; pre <= 0; up <= 1; word <= 0; write <= 1; load <= 1;
    rn <= 4'd6; rd <= 4'd8; offset <= 12'h004;
    @(posedge clk); @(posedge clk); en <= 0;
    repeat (40) @(posedge clk);

    // check RD (R8) got byte
    use_tb <= 1; tb_rd_en <= 1; tb_rd_reg <= 4'd8; @(posedge clk);
    tb_rd_en <= 0; @(posedge clk); val = tb_rd_val; use_tb <= 0;
    if ((val & 32'hFF) == 8'h77) $display("POST-LOAD OK R8=%h", val & 32'hFF); else $display("POST-LOAD ERR R8=%h (expected 77)", val & 32'hFF);

    // check R6 updated to base+4
    use_tb <= 1; tb_rd_en <= 1; tb_rd_reg <= 4'd6; @(posedge clk);
    tb_rd_en <= 0; @(posedge clk); val = tb_rd_val; use_tb <= 0;
    if (val == 32'h00003004) $display("POST-WB OK R6=%h", val); else $display("POST-WB ERR R6=%h (expected 3004)", val);

    $display("--- done ---");
    $finish;
end
endmodule
