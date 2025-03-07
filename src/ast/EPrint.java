package ast;

public class EPrint extends Expr {

    private Expr expr;

    public EPrint(Expr expr) {
        this.expr = expr;
    }

    public Expr getExpr() {
        return expr;
    }

    public String toString() {
        return String.format("print(%s)", expr);
    }

}
