package ast;

import java.util.ArrayList;
import java.util.List;

public class Program {

    private List<FnDef> functionDefs;
    private Expr mainExpr;

    public Program(List<FnDef> functionDefs, Expr mainExpr) {
        this.functionDefs = functionDefs;
        this.mainExpr = mainExpr;
    }
    public List<FnDef> getFunctionDefs() {
        return functionDefs;
    }
    public Expr getMainExpr() {
        return mainExpr;
    }

    @Override
    public String toString() {
        List<String> defStrs = new ArrayList<>();
        for (FnDef def : functionDefs) {
            defStrs.add(def.toString());
        }
        String defsStr = String.join("\n\n", defStrs);
        return String.format("%s\n%s", defsStr, mainExpr);
    }


}
