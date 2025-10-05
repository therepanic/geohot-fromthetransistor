module tb_blinking_led;
    reg clk;
    wire led;

    blinking_led uut(
        .clk(clk),
        .led(led)
    );

    initial begin
        clk = 0;
        forever #1 clk = ~clk;
    end

    initial begin
        $monitor("Time: %0t | LED: %b", $time, led);
        #100 $finish;
    end
endmodule