package ast;

public class EBool extends Expr {

    private boolean value;
    public boolean getValue() { return value; }

    public EBool(boolean value) {
        this.value = value;
    }

    public String toString() {
        if (value) {
            return "true";
        } else {
            return "false";
        }
    }

}
