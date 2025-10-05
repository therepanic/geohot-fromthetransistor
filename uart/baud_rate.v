/*
 * Baud rate with a 50MHz clock, 115200 baud and 16x sample
 */
module baud_rate
#(parameter RX_MAX = 50000000 / (115200 * 16), parameter TX_MAX = 50000000 / 115200,
parameter RX_WIDTH = $clog2(RX_MAX), parameter TX_WIDTH = $clog2(TX_MAX))
(input clk50, output rx_clk, output tx_clk);

reg [RX_WIDTH - 1:0] rx = 0;
reg [TX_WIDTH - 1:0] tx = 0;

assign rx_clk = (rx == 0);
assign tx_clk = (tx == 0);

always @(posedge clk50) begin
    if (rx == RX_MAX - 1) begin
        rx <= 0;
    end else begin
        rx <= rx + 1;
    end
end

always @(posedge clk50) begin
    if (tx == TX_MAX - 1) begin
        tx <= 0;
    end else begin
        tx <= tx + 1;
    end
end

endmodule