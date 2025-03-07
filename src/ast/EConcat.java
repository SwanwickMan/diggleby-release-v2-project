package ast;
import java.util.ArrayList;
import java.util.List;

public class EConcat extends Expr {
    private List<Expr> exprs;

    public EConcat(List<Expr> exprs) {
        this.exprs = exprs;
    }

    public List<Expr> getExprs() {
        return exprs;
    }

    @Override
    public String toString() {
        List<String> exprStrings = new ArrayList<>();
        for (Expr e : exprs) {
            exprStrings.add(e.toString());
        }
        String exprsStr = String.join(", ", exprStrings);
        return String.format("concat(%s)", exprsStr);
    }


}
