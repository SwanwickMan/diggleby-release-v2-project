import ast.*;

import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Scanner;
import java.util.stream.Collectors;

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
        return switch (ty) {
            case TyString ignored -> switch (vFrom) {
                case VInt vi -> new VString(Integer.toString(vi.getValue()));
                case VBool vb -> new VString(vb.getValue() ? "true" : "false");
                case VString vs -> vs;
                case VUnit ignoredToo -> new VString("()");
                default -> throw new CastException("Bad cast: " + vFrom + " to String");
            };

            case TyInt ignored -> switch (vFrom) {
                case VString vs when vs.getValue().matches("-?\\d+") -> new VInt(Integer.parseInt(vs.getValue()));
                case VBool vb -> new VInt(vb.getValue() ? 1 : 0);
                case VInt vi -> vi;
                default -> throw new CastException("Bad cast: " + vFrom + " to Int");
            };

            case TyUnit ignored -> switch (vFrom) {
                case VUnit vu -> vu;
                default -> throw new CastException("Bad cast: " + vFrom + " to Unit");
            };

            case TyBool ignored -> switch (vFrom) {
                case VBool vb -> vb;
                case VString vs when vs.getValue().equals("true") -> new VBool(true);
                case VString vs when vs.getValue().equals("false") -> new VBool(false);
                default -> throw new CastException("Invalid cast from " + vFrom + " to Bool");
            };

            default -> throw new CastException("Bad cast: " + vFrom + " to " + ty);
        };
    }

    // Given a map from function names to function definitions, and a value environment,
    // return the value resulting from evaluating expression e
    public Value interpExpr(Map<String, FnDef> fnDefs, Environment<Value> valEnv, Expr e) {
        return switch (e) {
            case EInt ei -> new VInt(ei.getValue());
            case EBool eb -> new VBool(eb.getValue());
            case EString es -> new VString(es.getValue());
            case EUnit ignored -> new VUnit();
            case EVar ev -> valEnv.lookup(ev.getVar());

            case EBinOp binOp -> evalBinOp(binOp.getOp(),
                    interpExpr(fnDefs, valEnv, binOp.getE1()),
                    interpExpr(fnDefs, valEnv, binOp.getE2()));

            case ENot en -> new VBool(!unwrapBool(interpExpr(fnDefs, valEnv, en.getExpr())));
            case ELet el -> {
                Value boundVal = interpExpr(fnDefs, valEnv, el.getSubject());
                yield interpExpr(fnDefs, valEnv.extend(el.getBinder(), boundVal), el.getContinuation());
            }
            case ECond ec -> unwrapBool(interpExpr(fnDefs, valEnv, ec.getTest()))
                    ? interpExpr(fnDefs, valEnv, ec.getThenBranch())
                    : interpExpr(fnDefs, valEnv, ec.getElseBranch());
            case EFnCall efc -> {
                FnDef fn = fnDefs.get(efc.getFnName());
                if (fn == null) throw new RuntimeException("Function not defined: " + efc.getFnName());
                List<Value> argValues = efc.getArgs().stream()
                        .map(arg -> interpExpr(fnDefs, valEnv, arg))
                        .toList();
                Environment<Value> newEnv = valEnv;
                for (int i = 0; i < fn.getParams().size(); i++) {
                    newEnv = newEnv.extend(fn.getParams().get(i).param, argValues.get(i));
                }
                yield interpExpr(fnDefs, newEnv, fn.getBody());
            }
            case EConcat ec -> {
                String result = ec.getExprs().stream()
                        .map(arg -> unwrapString(interpExpr(fnDefs, valEnv, arg)))
                        .collect(Collectors.joining());
                yield new VString(result);
            }
            case EPrint ep -> {
                System.out.println(unwrapString(interpExpr(fnDefs, valEnv, ep.getExpr())));
                yield new VUnit();
            }
            case EGetInput ignored -> new VString(reader.nextLine());
            case ECast ec -> {
                Value val = interpExpr(fnDefs, valEnv, ec.getExpr());
                try {
                    yield cast(val, ec.getType());
                } catch (CastException ex) {
                    throw new RuntimeException("Invalid cast: " + ex.getMessage());
                }
            }
            default -> throw new RuntimeException("Unrecognized expression type: " + e);
        };
    }

    private Value evalBinOp(EBinOp.Op op, Value left, Value right) {
        return switch (op) {
            case ADD -> new VInt(unwrapInt(left) + unwrapInt(right));
            case SUB -> new VInt(unwrapInt(left) - unwrapInt(right));
            case MUL -> new VInt(unwrapInt(left) * unwrapInt(right));
            case DIV -> {
                int r = unwrapInt(right);
                if (r == 0) throw new RuntimeException("Division by zero");
                yield new VInt(unwrapInt(left) / r);
            }
            case MOD -> {
                int r = unwrapInt(right);
                if (r == 0) throw new RuntimeException("Modulo by zero");
                yield new VInt(unwrapInt(left) % r);
            }
            case GT  -> new VBool(unwrapInt(left) > unwrapInt(right));
            case LT  -> new VBool(unwrapInt(left) < unwrapInt(right));
            case EQ  -> new VBool(left.equals(right)); // Works for all types
            case AND -> new VBool(unwrapBool(left) && unwrapBool(right));
            case OR  -> new VBool(unwrapBool(left) || unwrapBool(right));
        };
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
