package com.therepanic;

import java.nio.file.Path;

public interface Compiler {

    void compile(Path fromAsm, Path toBin);

}
