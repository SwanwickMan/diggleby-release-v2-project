package ast;

public class TyBool extends Type {

    private TyBool() {
    }

    private static TyBool type = new TyBool();

    public static TyBool type() {
        return type;
    }

    @Override
    public String toString() {
        return "Bool";
    }
}
