module transmitter
# (parameter IDLE_STATE = 2'b00, parameter START_STATE = 2'b01, parameter DATA_STATE = 2'b10,
parameter STOP_STATE = 2'b11)
(input[7:0] din, input write_en, input clk, output reg tx, output tx_busy);

initial begin
    tx = 1;
end

reg[7:0] data = 8'h0;
reg[2:0] bitpos = 0;
reg[1:0] state = IDLE_STATE;

assign tx_busy = (state != IDLE_STATE);

always @(posedge clk) begin
    case (state)
        IDLE_STATE: begin
            if (write_en) begin
                bitpos <= 0;
                data <= din;
                state <= START_STATE;    
            end           
        end
        START_STATE: begin
            tx <= 0;
            state <= DATA_STATE;
        end
        DATA_STATE : begin
             if (bitpos == 7) begin
                    state <= STOP_STATE;
                end else begin
                    bitpos <= bitpos + 1;
                end
            tx <= data[bitpos];
        end
        STOP_STATE: begin
            tx <= 1;
            state <= IDLE_STATE;
        end
        default: begin
            state <= IDLE_STATE;
            tx <= 1;
        end
    endcase
end

endmodule