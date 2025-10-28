module alu(
    input clk,
    input en,
    input immediate,
    input[3:0] opcode,
    input s,
    input[3:0] rn,
    input[3:0] rd,
    input[11:0] operand2,
    output reg write_restore_from_SPSR,
    output reg write_en,
    output reg[3:0] write_reg,
    output reg[31:0] write_value,
    output reg read_en,
    output reg[3:0] read_reg,
    input[31:0] read_value,
    output reg mode_read_en,
    input[31:0] mode_read_value,
    output reg cpsr_read_en,
    input[31:0] cpsr_read_value,
    output reg cpsr_write_en,
    output reg[31:0] cpsr_write_value
);

    reg state = 0;
    reg[2:0] result_state = 0;
    reg[1:0] got_mode = 0;
    reg[1:0] got_op1 = 0;
    reg[1:0] got_op2 = 0;
    reg[1:0] got_cpsr = 0;
    reg[1:0] write_cpsr_state = 0;

    reg cur_immediate;
    reg[3:0] cur_opcode;
    reg cur_s;
    reg[3:0] cur_rn;
    reg[3:0] cur_rd;
    reg[11:0] cur_operand2;

    reg[2:0] temp_mode;
    reg[31:0] temp_op1;
    reg[31:0] temp_op2;
    reg[31:0] temp_cpsr;

    reg[31:0] result;
    reg[32:0] higher_result;

    reg setN = 0, setZ = 0, setC = 0, setV = 0;


    function shouldRestoreFromSPSR;
        input[3:0] rd;
        input s;
        input[2:0] mode;
        begin
            if (rd != 15 || !s) begin
                shouldRestoreFromSPSR = 0;
            end else begin
                shouldRestoreFromSPSR = (mode != 3'b000 && mode != 3'b001);
            end
        end
    endfunction

    always @(posedge clk) begin
        if (en || state) begin
            if (!state) begin
                setN <= 0; setZ <= 0; setC <= 0; setV <= 0;
                cur_immediate <= immediate;
                cur_opcode <= opcode;
                cur_s <= s;
                cur_rn <= rn;
                cur_rd <= rd;
                cur_operand2 <= operand2;
                state <= 1;
            end else begin
                // now getting mode
                case (got_mode)
                    0: begin
                        mode_read_en <= 1;
                        got_mode <= got_mode + 1;
                    end
                    1: begin
                        mode_read_en <= 0;
                        got_mode <= got_mode + 1;
                    end
                    2: begin
                        temp_mode <= mode_read_value;
                        got_mode <= got_mode + 1;
                    end
                    3: begin
                        // now geting op1
                        case (got_op1)
                            0: begin
                                if (cur_opcode == 4'b1101) begin
                                    temp_op1 <= 0;
                                    got_op1 <= 3;
                                end else begin
                                    read_en <= 1;
                                    read_reg <= cur_rn;
                                    got_op1 <= got_op1 + 1;
                                end
                            end
                            1: begin
                                read_en <= 0;
                                got_op1 <= got_op1 + 1;
                            end
                            2: begin
                                temp_op1 <= read_value;
                                got_op1 <= got_op1 + 1;
                            end
                            3: begin
                                // now geting op2. todo: we not supporting rotate for immediate and shift for registers now.
                                case (got_op2)
                                    0: begin
                                        if (immediate) begin
                                            temp_op2 <= cur_operand2;
                                            got_op2 <= 3;
                                        end else begin
                                            read_en <= 1;
                                            read_reg <= cur_operand2[3:0];
                                            got_op2 <= got_op2 + 1;
                                        end
                                    end
                                    1: begin
                                        read_en <= 0;
                                        got_op2 <= got_op2 + 1;
                                    end
                                    2: begin
                                        temp_op2 <= read_value;
                                        got_op2 <= got_op2 + 1;
                                    end
                                    3: begin
                                        // now handle cpsr
                                        case (got_cpsr)
                                            0: begin
                                                cpsr_read_en <= 1;
                                                got_cpsr <= got_cpsr + 1;
                                            end
                                            1: begin
                                                cpsr_read_en <= 0;
                                                got_cpsr <= got_cpsr + 1;
                                            end
                                            2: begin
                                                temp_cpsr <= cpsr_read_value;
                                                got_cpsr <= got_cpsr + 1;
                                            end
                                            3: begin
                                                // now handle opcode (we support now only ADD, SUB, AND, CMP ,MOV)
                                                case (cur_opcode)
                                                    4'b0100: begin
                                                        //ADD
                                                        case (result_state)
                                                            0: begin
                                                                higher_result <= {1'b0,temp_op1} + {1'b0,temp_op2};
                                                                result_state <= result_state + 1;
                                                            end
                                                            1: begin
                                                                result <= higher_result[31:0];
                                                                result_state <= result_state + 1;
                                                            end
                                                            2: begin
                                                                setC <= higher_result[32];
                                                                setV <= ((~(temp_op1[31] ^ temp_op2[31])) & (temp_op1[31] ^ result[31]));
                                                                result_state <= result_state + 1;
                                                            end
                                                            3: begin
                                                                write_en <= 1;
                                                                write_reg <= cur_rd;
                                                                write_value <= result;
                                                                write_restore_from_SPSR <= shouldRestoreFromSPSR(cur_rd, cur_s, temp_mode);
                                                                result_state <= result_state + 1;
                                                            end
                                                            4: begin
                                                                write_en <= 0;
                                                                result_state <= result_state + 1;
                                                            end
                                                        endcase
                                                    4'b0000: begin
                                                        //AND
                                                        case (result_state)
                                                            0: begin
                                                                setC <= temp_cpsr[29];
                                                                setV <= temp_cpsr[28];
                                                                result <= temp_op1 & temp_op2;
                                                                result_state <= result_state + 1;
                                                            end
                                                            1: begin
                                                                write_en <= 1;
                                                                write_reg <= cur_rd;
                                                                write_value <= result;
                                                                write_restore_from_SPSR <= shouldRestoreFromSPSR(cur_rd, cur_s, temp_mode);
                                                                result_state <= result_state + 1;
                                                            end
                                                            2: begin
                                                                write_en <= 0;
                                                                result_state <= 4;
                                                            end
                                                        endcase
                                                    end
                                                    4'b1010: begin
                                                        //CMP
                                                        case (result_state)
                                                            0: begin
                                                                higher_result <= {1'b0, temp_op1} - {1'b0, temp_op2};
                                                                result_state <= result_state + 1;
                                                            end
                                                            1: begin
                                                                result <= higher_result[31:0];
                                                                result_state <= result_state + 1;
                                                            end
                                                            2: begin
                                                                setC <= ~higher_result[32];
                                                                setV <= (temp_op1[31] ^ temp_op2[31]) & (temp_op1[31] ^ result[31]);
                                                                result_state <= 4;
                                                            end
                                                        endcase
                                                    end
                                                    4'b1101: begin
                                                        //MOV
                                                        case (result_state)
                                                            0: begin
                                                                setC <= temp_cpsr[29];
                                                                setV <= temp_cpsr[28];
                                                                result <= temp_op2;
                                                                result_state <= result_state + 1;
                                                            end
                                                            1: begin
                                                                write_en <= 1;
                                                                write_reg <= cur_rd;
                                                                write_value <= result;
                                                                write_restore_from_SPSR <= shouldRestoreFromSPSR(cur_rd, cur_s, temp_mode);
                                                                result_state <= result_state + 1;
                                                            end
                                                            2: begin
                                                                write_en <= 0;
                                                                result_state <= 4;
                                                            end
                                                        endcase
                                                    end
                                                    end
                                                endcase
                                                if (result_state == 4) begin
                                                    if (cur_s || cur_opcode == 4'b1010) begin
                                                        case (write_cpsr_state)
                                                            0: begin
                                                                setN <= result[31];
                                                                setZ <= (result == 32'b0);
                                                                write_cpsr_state <= write_cpsr_state + 1;
                                                            end
                                                            1: begin
                                                                temp_cpsr[31] <= setN;
                                                                temp_cpsr[30] <= setZ;
                                                                temp_cpsr[29] <= setC;
                                                                temp_cpsr[28] <= setV;
                                                                write_cpsr_state <= write_cpsr_state + 1;
                                                            end
                                                            2: begin
                                                                cpsr_write_en <= 1;
                                                                cpsr_write_value <= temp_cpsr;
                                                                write_cpsr_state <= write_cpsr_state + 1;
                                                            end
                                                            3: begin
                                                                // end
                                                                cpsr_write_en <= 0;
                                                                state <= 0;
                                                                result_state <= 0;
                                                                got_mode <= 0;
                                                                got_op1 <= 0;
                                                                got_op2 <= 0;
                                                                got_cpsr <= 0;
                                                                write_cpsr_state <= 0;
                                                            end
                                                        endcase
                                                    end
                                                end
                                            end
                                        endcase
                                    end
                                endcase
                            end
                        endcase

                    end
                endcase
            end
        end
    end

endmodule