module uart
(input[7:0] din, output[7:0] dout, input clk50, output tx, output tx_busy, input write_en,
input rx, output rdy, input rdy_clr);
    wire rx_clk, tx_clk;

    baud_rate uart_baud_rate(
        .clk50(clk50),
        .rx_clk(rx_clk),
        .tx_clk(tx_clk)
    );

    receiver uart_receiver(
        .rx(rx),
        .clk(rx_clk),
        .dout(dout),
        .rdy(rdy),
        .rdy_clr(rdy_clr)
    );

    transmitter uart_transmitter(
        .din(din),
        .write_en(write_en),
        .clk(tx_clk),
        .tx(tx),
        .tx_busy(tx_busy)
    );

endmodule