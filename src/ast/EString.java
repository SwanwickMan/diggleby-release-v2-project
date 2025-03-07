package ast;

public class EString extends Expr {

    private String value;

    public EString(String value) {
        this.value = value;
    }
    public String getValue() { return value; }

    public String toString() {
        return String.format("\"%s\"", value);
    }
}
