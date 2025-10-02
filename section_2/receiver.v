module receiver
# (parameter START_STATE = 2'b00, parameter DATA_STATE = 2'b01, parameter STOP_STATE = 2'b11)
(input rx, input clk, output reg[7:0] dout, output reg rdy, input rdy_clr);

initial begin
    rdy <= 0;
    dout <= 0;
end

reg[3:0] sample = 0;
reg[3:0] bitpos = 0;
reg[7:0] temp_data = 0;
reg[1:0] state = START_STATE;

always @(posedge clk) begin
    if (rdy_clr)
        rdy <= 0;
    
    case (state)
        START_STATE: begin
            if (!rx || sample != 0) begin
                sample <= sample + 1;
            end
            if (sample == 15) begin
                state <= DATA_STATE;
                sample <= 0;
                bitpos <= 0;
                temp_data <= 0;
            end
        end
        DATA_STATE: begin
            sample <= sample + 1;
            if (sample == 8) begin
                temp_data[bitpos[2:0]] <= rx;
                bitpos <= bitpos + 1;
            end
            if (sample == 15 && bitpos == 8) begin
                state <= STOP_STATE;
            end
        end
        STOP_STATE: begin
            if (sample == 15) begin
                state <= START_STATE;
                dout <= temp_data;
                rdy <= 1;
                sample <= 0;
            end else begin
                sample <= sample + 1;
            end
        end
        default: begin
            state <= START_STATE;
        end
    endcase
end

endmodule