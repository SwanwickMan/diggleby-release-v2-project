package ast;

public class TyString extends Type {

    private TyString() {
    }

    private static TyString type = new TyString();

    public static TyString type() {
        return type;
    }

    @Override
    public String toString() {
        return "String";
    }
}
