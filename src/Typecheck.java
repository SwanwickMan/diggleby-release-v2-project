import java.util.HashMap;
import java.util.Map;

import ast.*;

public class Typecheck {

    private void checkType(Type expected, Type actual) {
        if (!actual.equals(expected)) {
            throw new TypeErrorException(String.format("Type mismatch: Expected %s but got %s", expected, actual));
        }
    }

    public void typecheckDef(Map<String, FnDef> defs, FnDef def) {
        Environment<Type> paramEnv = new Environment<>();
        for (AnnotatedParam param : def.getParams()) {
            paramEnv = paramEnv.extend(param.param, param.type);
        }
        Type bodyType = typecheckExpr(defs, paramEnv, def.getBody());
        checkType(def.getReturnType(), bodyType);
    }

    public Type typecheckProgram(Program p) {
        Map<String, FnDef> functionMap = new HashMap<>();
        for (FnDef fn : p.getFunctionDefs()) {
            functionMap.put(fn.getFnName(), fn);
        }
        for (FnDef fn : p.getFunctionDefs()) {
            typecheckDef(functionMap, fn);
        }
        return typecheckExpr(functionMap, new Environment<>(), p.getMainExpr());
    }



    public Type typecheckExpr(Map<String, FnDef> fnDefs, Environment<Type> tyEnv, Expr e) {
        return switch (e) {
            case EInt ignored -> TyInt.type();
            case EBool ignored -> TyBool.type();
            case EString ignored -> TyString.type();
            case EUnit ignored -> TyUnit.type();
            case EVar ev -> tyEnv.lookup(ev.getVar());
            case EBinOp binOp -> {
                Type leftType = typecheckExpr(fnDefs, tyEnv, binOp.getE1());
                Type rightType = typecheckExpr(fnDefs, tyEnv, binOp.getE2());
                EBinOp.Op op = binOp.getOp();
                yield switch (op) {
                    case ADD, SUB, MUL, DIV, MOD -> {
                        checkType(TyInt.type(), leftType);
                        checkType(TyInt.type(), rightType);
                        yield TyInt.type();
                    }
                    case GT, LT -> {
                        checkType(TyInt.type(), leftType);
                        checkType(TyInt.type(), rightType);
                        yield TyBool.type();
                    }
                    case EQ -> {
                        checkType(leftType, rightType);
                        yield TyBool.type();
                    }
                    case AND, OR -> {
                        checkType(TyBool.type(), leftType);
                        checkType(TyBool.type(), rightType);
                        yield TyBool.type();
                    }
                };
            }
            case ENot en -> {
                Type exprType = typecheckExpr(fnDefs, tyEnv, en.getExpr());
                checkType(TyBool.type(), exprType);
                yield TyBool.type();
            }
            case ELet el -> {
                Type boundType = typecheckExpr(fnDefs, tyEnv, el.getSubject());
                Environment<Type> newEnv = tyEnv.extend(el.getBinder(), boundType);
                yield typecheckExpr(fnDefs, newEnv, el.getContinuation());
            }
            case ECond ec -> {
                Type condType = typecheckExpr(fnDefs, tyEnv, ec.getTest());
                checkType(TyBool.type(), condType);
                Type thenType = typecheckExpr(fnDefs, tyEnv, ec.getThenBranch());
                Type elseType = typecheckExpr(fnDefs, tyEnv, ec.getElseBranch());
                checkType(thenType, elseType);
                yield thenType;
            }
            case EFnCall efc -> {
                FnDef fn = fnDefs.get(efc.getFnName());
                if (fn == null) {
                    throw new TypeErrorException("Undefined function: " + efc.getFnName());
                }
                if (fn.getParams().size() != efc.getArgs().size()) {
                    throw new TypeErrorException("Incorrect number of arguments for function: " + efc.getFnName());
                }
                for (int i = 0; i < fn.getParams().size(); i++) {
                    Type expected = fn.getParams().get(i).type;
                    Type actual = typecheckExpr(fnDefs, tyEnv, efc.getArgs().get(i));
                    checkType(expected, actual);
                }
                yield fn.getReturnType();
            }
            case EConcat ec -> {
                for (Expr expr : ec.getExprs()) {
                    checkType(TyString.type(), typecheckExpr(fnDefs, tyEnv, expr));
                }
                yield TyString.type();
            }
            case EPrint ep -> {
                checkType(TyString.type(), typecheckExpr(fnDefs, tyEnv, ep.getExpr()));
                yield TyUnit.type();
            }
            case EGetInput ignored -> TyString.type();
            case ECast ec -> {
                typecheckExpr(fnDefs, tyEnv, ec.getExpr());
                yield ec.getType();
            }
            default -> throw new TypeErrorException("Bad type not recognised: " + e);
        };
    }
}
