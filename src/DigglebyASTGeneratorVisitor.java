import antlr.*;
import antlr.DigglebyParser.*;
import ast.*;
import ast.EBinOp.Op;

import java.util.ArrayList;
import java.util.List;

import org.antlr.v4.runtime.tree.ParseTree;

public class DigglebyASTGeneratorVisitor {

    private Type stringToType(String s) {
        if (s.equals("String")) {
            return TyString.type();
        } else if (s.equals("Bool")) { 
            return TyBool.type();
        } else if (s.equals("Int")) {
            return TyInt.type();
        } else if (s.equals("Unit")) {
            return TyUnit.type();
        } else {
            throw new RuntimeException("Unexpected type string in stringToType during AST generation: " + s);
        }
    }

    public class ExprVisitor extends DigglebyBaseVisitor<Expr> {

        private Expr binOp(Op op, Expr e1, Expr e2) {
            return new EBinOp(e1, op, e2);
        }

        private List<Expr> visitExprs(Expr_listContext exprs) {
            if (exprs == null || exprs.expr() == null) {
                return new ArrayList<>();
            } 
            return exprs.expr().stream().map(e -> visit(e)).toList();
        }

        @Override
        public Expr visitBoolOp(BoolOpContext ctx) {
            Op op;
            if (ctx.AND() != null) {
                op = Op.AND;
            } else if (ctx.OR() != null) {
                op = Op.OR;
            } else {
                throw new RuntimeException("bad bool op");
            }
            return binOp(op, visit(ctx.base_expr(0)), visit(ctx.base_expr(1)));
        }

        @Override
        public Expr visitCast(CastContext ctx) {
            Expr e = visit(ctx.expr());
            Type ty = stringToType(ctx.type().getText());
            return new ECast(e, ty);
        }

        @Override
        public Expr visitConcat(ConcatContext ctx) {
            return new EConcat(visitExprs(ctx.expr_list()));
        }

        @Override
        public Expr visitFalse(FalseContext ctx) {
            return new EBool(false);
        }

        @Override
        public Expr visitFnCall(FnCallContext ctx) {
            String fnName = ctx.IDENT().getText();
            return new EFnCall(fnName, visitExprs(ctx.expr_list()));
        }
        @Override
        public Expr visitGetInput(GetInputContext ctx) {
            return new EGetInput();
        }

        @Override
        public Expr visitIdent(IdentContext ctx) {
            return new EVar(ctx.IDENT().getText());
        }

        @Override
        public Expr visitIfThenElse(IfThenElseContext ctx) {
            Expr test = visit(ctx.expr(0));
            Expr thenBranch = visit(ctx.expr(1));
            Expr elseBranch = visit(ctx.expr(2));
            return new ECond(test, thenBranch, elseBranch);
        }

        @Override
        public Expr visitInt(IntContext ctx) {
            return new EInt(Integer.valueOf(ctx.INT().getText()));
        }

        @Override
        public Expr visitLet(LetContext ctx) {
            return new ELet(ctx.IDENT().getText(), visit(ctx.base_expr()), visit(ctx.expr()));
        }

        @Override
        public Expr visitMulDivOp(MulDivOpContext ctx) {
            Op op;
            if (ctx.MUL() != null) {
                op = Op.MUL;
            } else if (ctx.DIV() != null) {
                op = Op.DIV;
            } else if (ctx.MOD() != null) {
                op = Op.MOD;
            } else {
                throw new RuntimeException("invalid mul/div/mod op");
            }
            return binOp(op, visit(ctx.base_expr(0)), visit(ctx.base_expr(1)));
        }

        @Override
        public Expr visitNot(NotContext ctx) {
            return new ENot(visit(ctx.fact()));
        }

        @Override
        public Expr visitParens(ParensContext ctx) {
            return visit(ctx.expr());
        }

        @Override
        public Expr visitPlusMinusOp(PlusMinusOpContext ctx) {
            Op op;
            if (ctx.PLUS() != null) {
                op = Op.ADD;
            } else if (ctx.MINUS() != null) {
                op = Op.SUB;
            } else {
                throw new RuntimeException("invalid plus/minus op");
            }
            return binOp(op, visit(ctx.base_expr(0)), visit(ctx.base_expr(1)));
        }

        @Override
        public Expr visitPrint(PrintContext ctx) {
            return new EPrint(visit(ctx.expr()));
        }

        @Override
        public Expr visitRelOp(RelOpContext ctx) {
            Op op;
            if (ctx.LT() != null) {
                op = Op.LT;
            } else if (ctx.GT() != null) {
                op = Op.GT;
            } else if (ctx.EQQ() != null) {
                op = Op.EQ;
            } else {
                throw new RuntimeException("invalid rel op");
            }
            return binOp(op, visit(ctx.base_expr(0)), visit(ctx.base_expr(1)));
        }

        @Override
        public Expr visitStr(StrContext ctx) {
            return new EString(ctx.STR().getText().replaceAll("^\"|\"$", ""));
        }

        @Override
        public Expr visitTrue(TrueContext ctx) {
            return new EBool(true);
        }

        @Override
        public Expr visitUnit(UnitContext ctx) {
            return new EUnit();
        }

    }

    public class FnDefVisitor extends DigglebyBaseVisitor<FnDef> {
        ExprVisitor exprVis = new ExprVisitor();
        
        @Override
        public FnDef visitFnDef(FnDefContext ctx) {
            // Final occurrence of type gets us the return type
            Type returnType = stringToType(ctx.type().getText());

            List<AnnotatedParam> params = new ArrayList<>();
            Param_listContext paramList = ctx.param_list();
            Expr body = exprVis.visit(ctx.expr());
            if (paramList == null) {
                return new FnDef(ctx.IDENT().getText(), params, returnType, body);
            }

            for (int i = 0; i < paramList.IDENT().size(); i++) {
                Type ty = stringToType(paramList.type(i).getText());
                String param = paramList.IDENT(i).getText();
                params.add(new AnnotatedParam(param, ty));
            }
           return new FnDef(ctx.IDENT().getText(), params, returnType, body);
        }
        
    }

    public class ProgramVisitor extends DigglebyBaseVisitor<Program> {

        @Override
        public Program visitProgram(ProgramContext ctx) {
            ExprVisitor exprVis = new ExprVisitor();
            FnDefVisitor fnDefVis = new FnDefVisitor();
            List<FnDef> defs = ctx.def()
                                  .stream()
                                  .map(def -> fnDefVis.visit(def))
                                  .toList();
            Expr body = exprVis.visit(ctx.expr());
            return new Program(defs, body);
        }
    }

    public Program visitProgram(ParseTree tree) {
        return new ProgramVisitor().visit(tree);
    }

}
