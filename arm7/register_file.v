module register_file(
    input clk,
    input write_en,
    input[3:0] write_reg,
    input[31:0] write_value,
    input write_restore_from_SPSR,
    input read_en,
    input[3:0] read_reg,
    output reg[31:0] read_value,
    input cpsr_read_en,
    output reg[31:0] cpsr_read_value,
    input cpsr_write_en,
    input[31:0] cpsr_write_value,
    input mode_read_en,
    output reg[31:0] mode_read_value
);
    reg[31:0] GENERAL_registers[0:12];
    reg[31:0] FIQ_registers[0:4];
    // User/System -> FIQ -> IRQ -> SVC -> ABT -> UND
    reg[31:0] SP_registers[0:5];
    // User/System -> FIQ -> IRQ -> SVC -> ABT -> UND
    reg[31:0] LR_registers[0:5];
    // FIQ -> IRQ -> SVC -> ABT -> UND
    reg[31:0] SPSR_registers[0:4];
    reg[31:0] pc_register = 32'b0;
    reg[31:0] cpsr_register = 32'b0;
    // USR -> 000, SYS -> 001, FIQ -> 010, IRQ -> 011, SVC -> 100, ABT -> 101, UND -> 110
    reg[2:0] mode = 000;

    integer i;

    initial begin
        for (i = 0; i <= 12; i = i + 1)
            GENERAL_registers[i] = 32'b0;

        for (i = 0; i <= 4; i = i + 1)
            FIQ_registers[i] = 32'b0;

        for (i = 0; i <= 5; i = i + 1)
            SP_registers[i] = 32'b0;

        for (i = 0; i <= 5; i = i + 1)
            LR_registers[i] = 32'b0;

        for (i = 0; i <= 4; i = i + 1)
            SPSR_registers[i] = 32'b0;
    end

    function [2:0] bank_idx;
        input [2:0] mode;
        begin
            case(mode)
                3'b000, 3'b001: bank_idx = 0; // USR/SYS
                3'b010: bank_idx = 1; // FIQ
                3'b011: bank_idx = 2; // IRQ
                3'b100: bank_idx = 3; // SVC
                3'b101: bank_idx = 4; // ABT
                3'b110: bank_idx = 5; // UND
                default: bank_idx = 0;
            endcase
        end
    endfunction

    function [31:0] select_SPSR;
        input [2:0] mode;
        begin
            case(mode)
                3'b010: select_SPSR = SPSR_registers[0];
                3'b011: select_SPSR = SPSR_registers[1];
                3'b100: select_SPSR = SPSR_registers[2];
                3'b101: select_SPSR = SPSR_registers[3];
                3'b110: select_SPSR = SPSR_registers[4];
                default: select_SPSR = 32'b0;
            endcase
        end
    endfunction

    always @(posedge clk) begin
        if (write_en) begin
            if (mode == 3'b010 && write_reg >= 8 && write_reg <= 12) begin
                // if mode == FIQ
                FIQ_registers[write_reg - 8] <= write_value;
            end else begin
                if (write_reg == 13) begin
                    SP_registers[bank_idx(mode)] <= write_value;
                end else if (write_reg == 14) begin
                    LR_registers[bank_idx(mode)] <= write_value;
                end else if (write_reg == 15) begin
                    //note: we not support thumb now
                    if (write_restore_from_SPSR == 1) begin
                        cpsr_register <= select_SPSR(mode);
                    end
                    // for sure
                    pc_register <= write_value & ~3; // USR -> 000, SYS -> 001, FIQ -> 010, IRQ -> 011, SVC -> 100, ABT -> 101, UND -> 110
                end else begin
                    GENERAL_registers[write_reg] <= write_value;
                end
            end
        end
        if (read_en) begin
            if (mode == 3'b010 && read_reg >= 8 && read_reg <= 12) begin
                read_value <= FIQ_registers[read_reg - 8];
            end else begin
                if (read_reg == 13) begin
                    read_value <= SP_registers[bank_idx(mode)];
                end else if (read_reg == 14) begin
                    read_value <= LR_registers[bank_idx(mode)];
                end else if (read_reg == 15) begin
                    read_value <= pc_register;
                end else begin
                    read_value <= GENERAL_registers[read_reg];
                end

            end
        end
        if (cpsr_read_en) begin
            cpsr_read_value <= cpsr_register;
        end
        if (cpsr_write_en) begin
            cpsr_register <= cpsr_write_value;
        end
        if (mode_read_en) begin
            mode_read_value <= mode;
        end
    end

endmodule