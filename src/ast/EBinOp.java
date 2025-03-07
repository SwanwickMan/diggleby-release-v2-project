package ast;

public class EBinOp extends Expr {

    public enum Op {
        ADD, SUB, MUL, DIV, MOD, GT, LT,
        AND, OR, EQ
    }

    private Expr e1;
    private Op op;
    private Expr e2;
    
    public EBinOp(Expr e1, Op op, Expr e2) {
        this.e1 = e1;
        this.op = op;
        this.e2 = e2;
    }

    public Expr getE1() {
        return e1;
    }

    public Op getOp() {
        return op;
    }
    
    public Expr getE2() {
        return e2;
    }

    private String opToString(Op op) {
        switch (op) {
            case ADD: return "+";
            case SUB: return "-";
            case MUL: return "*";
            case DIV: return "/";
            case MOD: return "%";
            case GT: return ">";
            case LT: return "<";
            case AND: return "&&";
            case OR: return "||";
            case EQ: return "==";
            default: throw new RuntimeException("invalid op");
        }
    }

    @Override
    public String toString() {
        return String.format("%s %s %s", e1, opToString(op), e2);
    }

    

    
}
