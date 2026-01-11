package com.therepanic.cu;

public interface ControlUnit {

    int fetch();

    //it's a two-pipeline, not a three-pipeline, but it's not very easy to release at a high level.
    void decodeAndExecute(int instruction);

}
