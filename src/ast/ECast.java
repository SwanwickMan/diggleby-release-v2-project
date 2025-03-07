package ast;

public class ECast extends Expr {

    private Expr expr;
    private Type type;
    public ECast(Expr expr, Type type) {
        this.expr = expr;
        this.type = type;
    }
    public Expr getExpr() {
        return expr;
    }
    public Type getType() {
        return type;
    }

    @Override
    public String toString() {
        return String.format("(%s : %s)", expr, type);
    }

    

}
