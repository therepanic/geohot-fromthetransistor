package com.therepanic;

import com.therepanic.parser.Parser;
import com.therepanic.parser.arm7.ARM7BranchParser;

import java.io.DataOutputStream;
import java.io.IOException;
import java.io.UncheckedIOException;
import java.nio.file.Files;
import java.nio.file.Path;
import java.util.*;

public class ARM7Compiler implements Compiler {

    private static final Set<String> COND_CODES = Set.of(
            "EQ", "NE", "CS", "CC",
            "MI", "PL", "VS", "VC",
            "HI", "LS", "GE", "LT",
            "GT", "LE"
    );

    @Override
    public void compile(Path fromAsm, Path toBin) {
        Map<String, Integer> labelAddressMap = new HashMap<>();
        List<String> lines;
        try {
            lines = Files.readAllLines(fromAsm);
        } catch (IOException e) {
            throw new UncheckedIOException(e);
        }
        parseLabels(lines, labelAddressMap);
        List<Instruction> instructions = parseInstructions(lines, labelAddressMap);
        saveAsBinary(instructions, toBin);
    }

    private void saveAsBinary(List<Instruction> instructions, Path toBin) {
        try (DataOutputStream out = new DataOutputStream(Files.newOutputStream(toBin))) {
            for (Instruction instruction : instructions) {
                out.writeInt(instruction.getBit());
            }
        } catch (IOException e) {
            throw new UncheckedIOException(e);
        }
    }

    private void parseLabels(List<String> lines, Map<String, Integer> labelAddressMap) {
        int addr = 0b0000;
        for (String line : lines) {
            //clear comments
            line = line.split("\\;")[0];
            line = line.trim();
            if (line.endsWith(":")) {
                int colonIt = line.indexOf(':');
                String label = line.substring(0, colonIt);
                labelAddressMap.put(label.toUpperCase(), addr);
            } else if (!line.isEmpty()) {
                addr += 4;
            }
        }
    }

    private List<Instruction> parseInstructions(List<String> lines, Map<String, Integer> labelAddressMap) {
        List<Instruction> instructions = new ArrayList<>();
        int addr = 0;
        for (String line : lines) {
            //clear comments
            line = line.split("\\;")[0];
            line = line.trim();
            if (!line.endsWith(":")) {
                List<String> tokens = Arrays.stream(line.split(" "))
                        .map(t -> t.replace(",", "").trim())
                        .filter(t -> !t.isEmpty())
                        .map(String::toUpperCase)
                        .toList();
                if (!tokens.isEmpty()) {
                    String fullOpcode = tokens.getFirst();
                    String baseOpcode = fullOpcode;
                    for (String cond : COND_CODES) {
                        if (fullOpcode.endsWith(cond)) {
                            baseOpcode = fullOpcode.substring(0, fullOpcode.length() - cond.length());
                            break;
                        }
                    }
                    Parser parser =  ParserRegistry.createForOpcode(baseOpcode);
                    if (parser instanceof ARM7BranchParser abp) {
                        abp.setLabelAddressMap(labelAddressMap);
                        abp.setCurAddress(addr);
                    }
                    Instruction instruction = parser.parse(tokens);
                    instructions.add(instruction);
                    addr += 4;
                }
            }
        }
        return instructions;
    }

}
