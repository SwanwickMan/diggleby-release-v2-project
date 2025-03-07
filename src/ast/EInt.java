package ast;

public class EInt extends Expr {

    private int value;

    public EInt(int value) {
        this.value = value;
    }

    public int getValue() {
        return value;
    }

    @Override
    public String toString() {
        return String.valueOf(value);
    }
    

}
