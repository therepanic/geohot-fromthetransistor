module blinking_led #(
    parameter MAX_COUNT = 10
)(
    input clk,
    output reg led
);

    reg[4:0] count;

    initial begin
        count = 0;
        led = 0;
    end

    always @(posedge clk)
        begin
            count <= count + 1;
            if (count > MAX_COUNT)
                begin
                   led <= ~led; 
                   count <= 0;
                end
        end

endmodule