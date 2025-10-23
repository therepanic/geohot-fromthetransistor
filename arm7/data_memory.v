// very simple sdata implementation with 1mb~ memory

module data_memory(
    input clk,
    input write_word_en,
    input write_byte_en,
    input read_word_en,
    input read_byte_en,
    input[31:0] write_word_address,
    input[31:0] write_byte_address,
    input[31:0] write_word_data,
    input[7:0] write_byte_data,
    input[31:0] read_word_address,
    input[31:0] read_byte_address,
    output reg [31:0] read_word_data,
    output reg [7:0] read_byte_data
);

    reg[31:0] memory[0:262143];
    reg [17:0] write_word_index;
    reg [1:0] write_byte_offset;
    reg [31:0] write_temp_word;

    reg [17:0] read_word_index;
    reg [1:0] read_byte_offset;
    reg [31:0] read_temp_word;

    always @(posedge clk) begin
        if (write_word_en) begin
            memory[write_word_address[31:2]] <= write_word_data;
        end
        if (write_byte_en) begin
            write_word_index = write_byte_address[31:2];
            write_byte_offset = write_byte_address[1:0];
            write_temp_word = memory[write_word_index];
            case (write_byte_offset)
                2'b00: write_temp_word[7:0] = write_byte_data;
                2'b01: write_temp_word[15:8] = write_byte_data;
                2'b10: write_temp_word[23:16] = write_byte_data;
                2'b11: write_temp_word[31:24] = write_byte_data;
            endcase
            memory[write_word_index] <= write_temp_word;
        end
        if (read_word_en) begin
            read_word_data <= memory[read_word_address[31:2]];
        end
        if (read_byte_en) begin
            read_word_index = read_byte_address[31:2];
            read_byte_offset = read_byte_address[1:0];
            read_temp_word = memory[read_word_index];
            case (read_byte_offset)
                2'b00: read_byte_data <= read_temp_word[7:0];
                2'b01: read_byte_data <= read_temp_word[15:8];
                2'b10: read_byte_data <= read_temp_word[23:16];
                2'b11: read_byte_data <= read_temp_word[31:24];
            endcase
        end
    end

endmodule