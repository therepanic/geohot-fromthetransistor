`timescale 1ns/1ps
module tb_uart();

reg clk = 0;

wire rdy;
wire tx_busy;
reg write_en = 0;

wire[7:0] rx_data;
reg[7:0] tx_data = 0;

wire loop;

reg rdy_clr = 0;

uart test_uart(
    .din(tx_data),
    .dout(rx_data),
    .write_en(write_en),
    .clk50(clk),
    .tx(loop),
    .rx(loop),
    .rdy(rdy),
    .rdy_clr(rdy_clr)
);

initial begin
	$dumpfile("uart.vcd");
	$dumpvars(0, tb_uart);
	write_en <= 1;
	#2 write_en <= 0;
end

always begin
    #1 clk = ~clk;
end

always @(posedge rdy) begin
    #2 rdy_clr <= 1;
    #2 rdy_clr <= 0;
    if (rx_data == tx_data) begin
        if (rx_data == 8'hff) begin
            $display("SUCCESS");
            $finish;
        end
        tx_data <= tx_data + 1;
        write_en <= 1;
        #2 write_en <= 0;
    end else begin
        $display("FAIL");
        $finish;
    end
end

endmodule