package ast;

public class TyInt extends Type {

    private TyInt() {
    }

    private static TyInt type = new TyInt();

    public static TyInt type() {
        return type;
    }

    @Override
    public String toString() {
        return "Int";
    }

}
