import ast.*;

import java.util.HashMap;
import java.util.Map;
import java.util.Scanner;

public class Interp {
    private Scanner reader = new Scanner(System.in);

    public Interp() {

    }

    private boolean unwrapBool(Value v) {
        if (v instanceof VBool) {
            return ((VBool) v).getValue();
        } else {
            throw new RuntimeException("Runtime type error: Expected a bool but got value " + v);
        }
    }

    private int unwrapInt(Value v) {
        if (v instanceof VInt) {
            return ((VInt) v).getValue();
        } else {
            throw new RuntimeException("Runtime type error: Expected an int but got value " + v);
        }
    }

    private String unwrapString(Value v) {
        if (v instanceof VString) {
            return ((VString) v).getValue();
        } else {
            throw new RuntimeException("Runtime type error: Expected a string but got value " + v);
        }
    }

    // Given a value and a type, performs a cast (according to the rules in the spec) if possible,
    // raising a CastException otherwise.
    private Value cast(Value vFrom, Type ty) throws CastException {
        throw new RuntimeException("Fill in 'cast' in Interp.java");
    }

    // Given a map from function names to function definitions, and a value environment,
    // return the value resulting from evaluating expression e
    public Value interpExpr(Map<String, FnDef> fnDefs, Environment<Value> valEnv, Expr e) {
        throw new RuntimeException("Fill in 'interpExpr' in Interp.java");
    }

    // Runs a program by constructing a map from function names to function definitions,
    // and evaluating the main expression under an empty value environment
    public Value interpProgram(Program prog) {
        // Step 1: Construct a mapping from function names to function definitions
        Map<String, FnDef> functionMap = new HashMap<>();
        for (FnDef fn : prog.getFunctionDefs()) {
            functionMap.put(fn.getFnName(), fn);
        }

        // Step 2: Evaluate the main expression under an empty environment
        return interpExpr(functionMap, new Environment<>(), prog.getMainExpr());
    }

}
