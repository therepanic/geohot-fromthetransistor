package com.therepanic.registerFile;

public class ARM7RegisterFileProfile {

    private int sp;

    private int lr;

    public ARM7RegisterFileProfile(int sp, int lr) {
        this.sp = sp;
        this.lr = lr;
    }

    public int getSp() {
        return sp;
    }

    public void setSp(int sp) {
        this.sp = sp;
    }

    public int getLr() {
        return lr;
    }

    public void setLr(int lr) {
        this.lr = lr;
    }

}
