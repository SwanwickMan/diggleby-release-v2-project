package ast;

public class ENot extends Expr {
    private Expr expr;

    public ENot(Expr expr) {
        this.expr = expr;
    }

    public Expr getExpr() {
        return expr;
    }

    @Override
    public String toString() {
        return "!" + expr;
    }
    

}
