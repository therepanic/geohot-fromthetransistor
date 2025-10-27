`timescale 1ns / 1ps

module test_branch();
reg clk;
parameter CLK_PERIOD = 10;
initial begin
    clk = 0;
    forever #(CLK_PERIOD/2) clk = ~clk;
end

reg en, cond, link;
reg[23:0] offset;
wire b_write_en;
wire[3:0] b_write_reg;
wire[31:0] b_write_value;
wire b_read_en;
wire[3:0] b_read_reg;
wire[31:0] b_read_value;

reg tb_wr_en;
reg[3:0] tb_wr_reg;
reg[31:0] tb_wr_val;
reg tb_rd_en;
reg[3:0] tb_rd_reg;
wire[31:0] tb_rd_val;


reg use_tb;
wire rf_write_en = use_tb ? tb_wr_en : b_write_en;
wire[3:0] rf_wr = use_tb ? tb_wr_reg : b_write_reg;
wire[31:0] rf_wv = use_tb ? tb_wr_val : b_write_value;
wire rf_read_en = use_tb ? tb_rd_en : b_read_en;
wire[3:0] rf_rr = use_tb ? tb_rd_reg : b_read_reg;

register_file rf(
    .clk(clk),
    .write_en(rf_write_en),
    .write_reg(rf_wr),
    .write_value(rf_wv),
    .write_restore_from_SPSR(1'b0),
    .read_en(rf_read_en),
    .read_reg(rf_rr),
    .read_value(tb_rd_val),
    .cpsr_read_en(1'b0),
    .cpsr_read_value(),
    .cpsr_write_en(1'b0),
    .cpsr_write_value(32'b0),
    .mode_read_en(1'b0),
    .mode_read_value(32'b0)
);

assign b_read_value = tb_rd_val;

branch uut(
    .clk(clk),
    .en(en),
    .cond(cond),
    .link(link),
    .offset(offset),
    .write_restore_from_SPSR(1'b0),
    .write_en(b_write_en),
    .write_reg(b_write_reg),
    .write_value(b_write_value),
    .read_en(b_read_en),
    .read_reg(b_read_reg),
    .read_value(b_read_value)
);

initial begin
    uut.state = 1'b0;
    uut.temp = 2'b00;
    uut.temp_link = 2'b00;
    uut.cur_cond = 1'b0;
    uut.cur_link = 1'b0;
    uut.cur_offset = 24'b0;
    uut.write_en = 1'b0;
    uut.read_en = 1'b0;
    uut.write_reg = 4'b0;
    uut.read_reg = 4'b0;
    uut.write_value = 32'b0;
end

reg [31:0] val;

initial begin
    en = 0; cond = 0; link = 0; offset = 0;
    use_tb = 0;
    tb_wr_en = 0; tb_wr_reg = 0; tb_wr_val = 0;
    tb_rd_en = 0; tb_rd_reg = 0;

    repeat (2) @(posedge clk);
    $display("--- branch tests ---");

    // Init PC = 0x1000
    use_tb <= 1;
    tb_wr_en <= 1; tb_wr_reg <= 4'd15; tb_wr_val <= 32'h00001000;
    @(posedge clk);
    tb_wr_en <= 0;
    @(posedge clk);
    use_tb <= 0;

    // 1) Not taken: PC += 4 -> 0x1004
    en <= 1; cond <= 0; link <= 0; offset <= 24'd0;
    @(posedge clk); @(posedge clk);
    en <= 0;
    repeat (12) @(posedge clk);
    use_tb <= 1;
    tb_rd_en <= 1; tb_rd_reg <= 4'd15;
    @(posedge clk);
    tb_rd_en <= 0; @(posedge clk);
    val = tb_rd_val;
    use_tb <= 0;
    if (val == 32'h00001004) begin
        $display("NOT-TAKEN OK PC=%h", val);
    end else begin
        $display("NOT-TAKEN ERR PC=%h", val);
    end

    // 2) Taken, no link, offset=3 -> PC = 0x1004 + 8 + (3<<2) = 0x1018
    en <= 1; cond <= 1; link <= 0; offset <= 24'd3;
    @(posedge clk); @(posedge clk);
    en <= 0;
    repeat (12) @(posedge clk);
    use_tb <= 1;
    tb_rd_en <= 1; tb_rd_reg <= 4'd15;
    @(posedge clk);
    tb_rd_en <= 0; @(posedge clk);
    val = tb_rd_val;
    use_tb <= 0;
    if (val == 32'h00001018) begin
        $display("TAKEN no-link OK PC=%h", val);
    end else begin
        $display("TAKEN no-link ERR PC=%h", val);
    end

    // 3) Set PC=0x2000
    use_tb <= 1;
    tb_wr_en <= 1; tb_wr_reg <= 4'd15; tb_wr_val <= 32'h00002000;
    @(posedge clk);
    tb_wr_en <= 0; @(posedge clk);
    use_tb <= 0;

    // BL with offset = -2 -> LR=0x2004, PC=0x2000
    en <= 1; cond <= 1; link <= 1; offset <= 24'hFFFFFE;
    @(posedge clk); @(posedge clk);
    en <= 0;
    repeat (12) @(posedge clk);

    // Check LR
    use_tb <= 1;
    tb_rd_en <= 1; tb_rd_reg <= 4'd14;
    @(posedge clk);
    tb_rd_en <= 0; @(posedge clk);
    val = tb_rd_val;
    if (val == 32'h00002004) begin
        $display("BL LR OK LR=%h", val);
    end else begin
        $display("BL LR ERR LR=%h", val);
    end

    // Check PC
    tb_rd_en <= 1; tb_rd_reg <= 4'd15;
    @(posedge clk);
    tb_rd_en <= 0; @(posedge clk);
    val = tb_rd_val;
    use_tb <= 0;
    if (val == 32'h00002000) begin
        $display("BL PC OK PC=%h", val);
    end else begin
        $display("BL PC ERR PC=%h", val);
    end

    $display("--- done ---");
    $finish;
end
endmodule