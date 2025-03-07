import java.util.Map;

import ast.*;

public class Typecheck {

    private void checkType(Type expected, Type actual) {
        if (!actual.equals(expected)) {
            throw new TypeErrorException(String.format("Type mismatch: Expected %s but got %s", expected, actual));
        }
    }

    public Type typecheckExpr(Map<String, FnDef> fnDefs, Environment<Type> tyEnv, Expr e) {
        throw new RuntimeException("Fill in 'typecheckExpr' in Typecheck.java");
    }

    public void typecheckDef(Map<String, FnDef> defs, FnDef def) {
        throw new RuntimeException("Fill in 'typecheckDef' in Typecheck.java");
    }

    public Type typecheckProgram(Program p) {
        throw new RuntimeException("Fill in 'typecheckProgram' in Typecheck.java");
    }

}
