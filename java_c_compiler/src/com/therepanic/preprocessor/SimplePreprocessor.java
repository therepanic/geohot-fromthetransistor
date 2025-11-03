package com.therepanic.preprocessor;

public class SimplePreprocessor implements Preprocessor {

    @Override
    public String process(String source) {
        source = source.replaceAll("//.*", "");
        source = source.replaceAll("/\\*.*?\\*/", "");
        source = source.replaceAll("\\s+", " ");
        return source.trim();
    }

}
