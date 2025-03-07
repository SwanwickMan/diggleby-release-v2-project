package ast;

public class TyUnit extends Type {

    private TyUnit() {
    }

    private static TyUnit type = new TyUnit();

    public static TyUnit type() {
        return type;
    }

    @Override
    public String toString() {
        return "Unit";
    }

}
