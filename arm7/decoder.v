`define EQ 4'b0000
`define NE 4'b0001
`define CS 4'b0010
`define CC 4'b0011
`define MI 4'b0100
`define PL 4'b0101
`define VS 4'b0110
`define VC 4'b0111
`define HI 4'b1000
`define LS 4'b1001
`define GE 4'b1010
`define LT 4'b1011
`define GT 4'b1100
`define LE 4'b1101
`define AL 4'b1110

module decoder(
    input clk,
    input decode_en,
    input[31:0] instr,
    output reg cpsr_read_en,
    output reg[31:0] cpsr_read_value
);

    function check_condition;
        input [3:0] cond;
        input [31:0] cpsr_register;
        reg N, Z, C, V;
        begin
            N = cpsr_register[31];
            Z = cpsr_register[30];
            C = cpsr_register[29];
            V = cpsr_register[28];
            case (cond)
                `EQ: check_condition = Z;
                `NE: check_condition = ~Z;
                `CS: check_condition = C;
                `CC: check_condition = ~C;
                `MI: check_condition = N;
                `PL: check_condition = ~N;
                `VS: check_condition = V;
                `VC: check_condition = ~V;
                `HI: check_condition = C & ~Z;
                `LS: check_condition = ~C | Z;
                `GE: check_condition = (N == V);
                `LT: check_condition = (N != V);
                `GT: check_condition = ~Z & (N == V);
                `LE: check_condition = Z | (N != V);
                `AL: check_condition = 1'b1;
            endcase
        end
    endfunction

    reg[1:0] temp;

    reg[31:0] opcode;
    reg[2:0] type;
    always @(posedge clk) begin
        if (decode_en || temp != 2'b00) begin
            case (temp)
                2'b00:
                    cpsr_read_en <= 1;
                    temp <= temp + 1;
                2'b01:
                    cpsr_read_en <= 0;
                    temp <= temp + 1;
                2'b10:
                    opcode = instr[31:28];
                    if (check_condition(opcode, cpsr_read_value) == 1) begin
                        type = instr[27:25];
                        case (type)
                            3'b000, 3'b001:
                                //data processing
                            3'b010, 3'b011:
                                //single data transfer
                            3'b101:
                                //branch
                        endcase
                    end
                    temp <= 2'b00;
            endcase
        end
    end
endmodule