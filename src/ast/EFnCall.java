package ast;

import java.util.ArrayList;
import java.util.List;

public class EFnCall extends Expr {
    private String fnName;
    private List<Expr> args;
    
    public EFnCall(String fnName, List<Expr> args) {
        this.fnName = fnName;
        this.args = args;
    }

    public String getFnName() {
        return fnName;
    }

    public List<Expr> getArgs() {
        return args;
    }

    @Override
    public String toString() {
        List<String> argStrs = new ArrayList<>();
        for (Expr e : args) {
            argStrs.add(e.toString());
        }
        String argsStr = String.join(", ", argStrs);
        return String.format("%s(%s)", fnName, argsStr);
    }

}
