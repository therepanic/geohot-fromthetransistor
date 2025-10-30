`timescale 1ns / 1ps

module test_alu();
    reg clk;
    parameter CLK_PERIOD = 10;
    initial begin
        clk = 0;
        forever #(CLK_PERIOD/2) clk = ~clk;
    end

    reg en;
    reg immediate;
    reg [3:0] opcode;
    reg s;
    reg [3:0] rn;
    reg [3:0] rd;
    reg [11:0] operand2;

    wire alu_write_restore_from_SPSR;
    wire alu_write_en;
    wire [3:0] alu_write_reg;
    wire [31:0] alu_write_value;
    wire alu_read_en;
    wire [3:0] alu_read_reg;
    wire [31:0] alu_read_value;
    wire alu_mode_read_en;
    wire [31:0] alu_mode_read_value;
    wire alu_cpsr_read_en;
    wire [31:0] alu_cpsr_read_value;
    wire alu_cpsr_write_en;
    wire [31:0] alu_cpsr_write_value;
    wire busy;

    reg tb_wr_en;
    reg [3:0] tb_wr_reg;
    reg [31:0] tb_wr_val;
    reg tb_wr_restore_from_SPSR;
    reg tb_rd_en;
    reg [3:0] tb_rd_reg;
    wire [31:0] tb_rd_val;

    reg tb_cpsr_wr_en;
    reg [31:0] tb_cpsr_wr_val;

    reg use_tb;

    wire rf_write_en = use_tb ? tb_wr_en : alu_write_en;
    wire [3:0] rf_wr = use_tb ? tb_wr_reg : alu_write_reg;
    wire [31:0] rf_wv = use_tb ? tb_wr_val : alu_write_value;
    wire rf_wr_restore = use_tb ? tb_wr_restore_from_SPSR : alu_write_restore_from_SPSR;
    wire rf_read_en = use_tb ? tb_rd_en : alu_read_en;
    wire [3:0] rf_rr = use_tb ? tb_rd_reg : alu_read_reg;

    wire rf_cpsr_write_en = use_tb ? tb_cpsr_wr_en : alu_cpsr_write_en;
    wire [31:0] rf_cpsr_write_value = use_tb ? tb_cpsr_wr_val : alu_cpsr_write_value;

    register_file rf(
        .clk(clk),
        .write_en(rf_write_en),
        .write_reg(rf_wr),
        .write_value(rf_wv),
        .write_restore_from_SPSR(rf_wr_restore),
        .read_en(rf_read_en),
        .read_reg(rf_rr),
        .read_value(tb_rd_val),
        .cpsr_read_en(alu_cpsr_read_en),
        .cpsr_read_value(alu_cpsr_read_value),
        .cpsr_write_en(rf_cpsr_write_en),
        .cpsr_write_value(rf_cpsr_write_value),
        .mode_read_en(alu_mode_read_en),
        .mode_read_value(alu_mode_read_value)
    );

    assign alu_read_value = tb_rd_val;

    alu uut(
        .clk(clk),
        .en(en),
        .immediate(immediate),
        .opcode(opcode),
        .s(s),
        .rn(rn),
        .rd(rd),
        .operand2(operand2),
        .write_restore_from_SPSR(alu_write_restore_from_SPSR),
        .write_en(alu_write_en),
        .write_reg(alu_write_reg),
        .write_value(alu_write_value),
        .read_en(alu_read_en),
        .read_reg(alu_read_reg),
        .read_value(alu_read_value),
        .mode_read_en(alu_mode_read_en),
        .mode_read_value(alu_mode_read_value),
        .cpsr_read_en(alu_cpsr_read_en),
        .cpsr_read_value(alu_cpsr_read_value),
        .cpsr_write_en(alu_cpsr_write_en),
        .cpsr_write_value(alu_cpsr_write_value),
        .busy(busy)
    );

    reg [31:0] last_cpsr_written;
    always @(posedge clk) begin
        if (alu_cpsr_write_en) begin
            last_cpsr_written <= alu_cpsr_write_value;
        end
    end

    initial begin
        uut.state = 1'b0;
        uut.result_state = 3'b0;
        uut.got_mode = 2'b00;
        uut.got_op1 = 2'b00;
        uut.got_op2 = 2'b00;
        uut.got_cpsr = 2'b00;
        uut.write_cpsr_state = 2'b00;
        uut.cur_immediate = 0;
        uut.cur_opcode = 4'b0;
        uut.cur_s = 0;
        uut.cur_rn = 4'b0;
        uut.cur_rd = 4'b0;
        uut.cur_operand2 = 12'b0;
        uut.temp_mode = 3'b0;
        uut.temp_op1 = 32'b0;
        uut.temp_op2 = 32'b0;
        uut.temp_cpsr = 32'b0;
    end

    integer i;
    reg [31:0] val;
    

    initial begin
        // default signals
        en = 0; immediate = 0; opcode = 4'b0; s = 0; rn = 4'b0; rd = 4'b0; operand2 = 12'b0;
        use_tb = 0;
        tb_wr_en = 0; tb_wr_reg = 0; tb_wr_val = 0; tb_wr_restore_from_SPSR = 0;
        tb_rd_en = 0; tb_rd_reg = 0;
        tb_cpsr_wr_en = 0; tb_cpsr_wr_val = 32'b0;

        repeat (2) @(posedge clk);
    $display("--- Running tests for ARM7 alu");

        // prepare: set some registers and cpsr initial value
        // set r1 = 5
        use_tb <= 1;
        tb_wr_en <= 1; tb_wr_reg <= 4'd1; tb_wr_val <= 32'd5; tb_wr_restore_from_SPSR <= 1'b0;
        @(posedge clk);
        tb_wr_en <= 0;
        @(posedge clk);

        // set r2 = 7
        tb_wr_en <= 1; tb_wr_reg <= 4'd2; tb_wr_val <= 32'd7;
        @(posedge clk);
        tb_wr_en <= 0;
        @(posedge clk);

        // set r4 = 20, r5 = 3
        tb_wr_en <= 1; tb_wr_reg <= 4'd4; tb_wr_val <= 32'd20;
        @(posedge clk);
        tb_wr_en <= 0; @(posedge clk);
        tb_wr_en <= 1; tb_wr_reg <= 4'd5; tb_wr_val <= 32'd3;
        @(posedge clk);
        tb_wr_en <= 0; @(posedge clk);

        // set r6 = 32'hFF00_FF00, r7 = 32'h0F0F_0F0F
        tb_wr_en <= 1; tb_wr_reg <= 4'd6; tb_wr_val <= 32'hFF00FF00;
        @(posedge clk);
        tb_wr_en <= 0; @(posedge clk);
        tb_wr_en <= 1; tb_wr_reg <= 4'd7; tb_wr_val <= 32'h0F0F0F0F;
        @(posedge clk);
        tb_wr_en <= 0; @(posedge clk);

        // init CPSR: set C=1 (bit29), V=0 (bit28) for AND test
        tb_cpsr_wr_en <= 1; tb_cpsr_wr_val <= 32'b0;
        tb_cpsr_wr_val[29] <= 1'b1;
        tb_cpsr_wr_val[28] <= 1'b0;
        @(posedge clk);
        tb_cpsr_wr_en <= 0;
        @(posedge clk);

        use_tb <= 0;
        repeat (2) @(posedge clk);

        // 1) ADD: r0 = r1 + r2  (5 + 7 = 12)
        en <= 1; immediate <= 0; opcode <= 4'b0100; s <= 0;
        rn <= 4'd1; rd <= 4'd0; operand2 <= 12'h002;
        @(posedge clk); @(posedge clk);
        en <= 0;
        // let ALU finish
        repeat (40) @(posedge clk);

        // read r0
        use_tb <= 1;
        tb_rd_en <= 1; tb_rd_reg <= 4'd0;
        @(posedge clk);
        tb_rd_en <= 0; @(posedge clk);
        val = tb_rd_val;
        use_tb <= 0;
        if (val == 32'd12) $display("ADD OK r0=%d", val); else $display("ADD ERR r0=%d (expected 12)", val);

        // 2) SUB: r3 = r4 - r5  (20 - 3 = 17)
        en <= 1; immediate <= 0; opcode <= 4'b0010; s <= 0;
        rn <= 4'd4; rd <= 4'd3; operand2 <= 12'h005;
        @(posedge clk); @(posedge clk);
        en <= 0;
        repeat (40) @(posedge clk);

        // read r3
        use_tb <= 1;
        tb_rd_en <= 1; tb_rd_reg <= 4'd3;
        @(posedge clk);
        tb_rd_en <= 0; @(posedge clk);
        val = tb_rd_val;
        use_tb <= 0;
        if (val == 32'd17) $display("SUB OK r3=%d", val); else $display("SUB ERR r3=%d (expected 17)", val);

        // 3) AND: r8 = r6 & r7
        en <= 1; immediate <= 0; opcode <= 4'b0000; s <= 0;
        rn <= 4'd6; rd <= 4'd8; operand2 <= 12'h007; // reg 7
        @(posedge clk); @(posedge clk);
        en <= 0;
        repeat (20) @(posedge clk);

        // read r8
        use_tb <= 1;
        tb_rd_en <= 1; tb_rd_reg <= 4'd8;
        @(posedge clk);
        tb_rd_en <= 0; @(posedge clk);
        val = tb_rd_val;
        use_tb <= 0;
        if (val == (32'hFF00FF00 & 32'h0F0F0F0F)) $display("AND OK r8=%h", val); else $display("AND ERR r8=%h (expected %h)", val, (32'hFF00FF00 & 32'h0F0F0F0F));

        // 4) CMP: compare r4 (20) with r5 (3) -> sets flags: N=0 Z=0 C=1 V=0
        en <= 1; immediate <= 0; opcode <= 4'b1010; s <= 0; // CMP opcode
        rn <= 4'd4; rd <= 4'd0; operand2 <= 12'h005;
        @(posedge clk); @(posedge clk);
        en <= 0;
        repeat (30) @(posedge clk); // give enough cycles for cpsr write

        // check flags in last_cpsr_written
        // N = bit31, Z = bit30, C = bit29, V = bit28
        if (last_cpsr_written[31] == 1'b0 && last_cpsr_written[30] == 1'b0 && last_cpsr_written[29] == 1'b1 && last_cpsr_written[28] == 1'b0) begin
            $display("CMP OK CPSR N=%b Z=%b C=%b V=%b", last_cpsr_written[31], last_cpsr_written[30], last_cpsr_written[29], last_cpsr_written[28]);
        end else begin
            $display("CMP ERR CPSR N=%b Z=%b C=%b V=%b (expected 0 0 1 0)", last_cpsr_written[31], last_cpsr_written[30], last_cpsr_written[29], last_cpsr_written[28]);
        end

        // 5) MOV immediate: MOV r9, #0x123 -> r9 == 0x00000123
        en <= 1; immediate <= 1; opcode <= 4'b1101; s <= 0;
        rn <= 4'd0; rd <= 4'd9; operand2 <= 12'h123;
        @(posedge clk); @(posedge clk);
        en <= 0;
        repeat (20) @(posedge clk);

        // read r9
        use_tb <= 1;
        tb_rd_en <= 1; tb_rd_reg <= 4'd9;
        @(posedge clk);
        tb_rd_en <= 0; @(posedge clk);
        val = tb_rd_val;
        use_tb <= 0;
        if (val == 32'h00000123) $display("MOV OK r9=%h", val); else $display("MOV ERR r9=%h (expected 0x00000123)", val);

        $display("--- done ---");
        $finish;
    end

endmodule
