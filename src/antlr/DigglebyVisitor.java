// Generated from Diggleby.g4 by ANTLR 4.9.1

package antlr;

import org.antlr.v4.runtime.tree.ParseTreeVisitor;

/**
 * This interface defines a complete generic visitor for a parse tree produced
 * by {@link DigglebyParser}.
 *
 * @param <T> The return type of the visit operation. Use {@link Void} for
 * operations with no return type.
 */
public interface DigglebyVisitor<T> extends ParseTreeVisitor<T> {
	/**
	 * Visit a parse tree produced by {@link DigglebyParser#type}.
	 * @param ctx the parse tree
	 * @return the visitor result
	 */
	T visitType(DigglebyParser.TypeContext ctx);
	/**
	 * Visit a parse tree produced by {@link DigglebyParser#expr_list}.
	 * @param ctx the parse tree
	 * @return the visitor result
	 */
	T visitExpr_list(DigglebyParser.Expr_listContext ctx);
	/**
	 * Visit a parse tree produced by the {@code Let}
	 * labeled alternative in {@link DigglebyParser#expr}.
	 * @param ctx the parse tree
	 * @return the visitor result
	 */
	T visitLet(DigglebyParser.LetContext ctx);
	/**
	 * Visit a parse tree produced by the {@code BaseExpr}
	 * labeled alternative in {@link DigglebyParser#expr}.
	 * @param ctx the parse tree
	 * @return the visitor result
	 */
	T visitBaseExpr(DigglebyParser.BaseExprContext ctx);
	/**
	 * Visit a parse tree produced by the {@code Concat}
	 * labeled alternative in {@link DigglebyParser#base_expr}.
	 * @param ctx the parse tree
	 * @return the visitor result
	 */
	T visitConcat(DigglebyParser.ConcatContext ctx);
	/**
	 * Visit a parse tree produced by the {@code Cast}
	 * labeled alternative in {@link DigglebyParser#base_expr}.
	 * @param ctx the parse tree
	 * @return the visitor result
	 */
	T visitCast(DigglebyParser.CastContext ctx);
	/**
	 * Visit a parse tree produced by the {@code MulDivOp}
	 * labeled alternative in {@link DigglebyParser#base_expr}.
	 * @param ctx the parse tree
	 * @return the visitor result
	 */
	T visitMulDivOp(DigglebyParser.MulDivOpContext ctx);
	/**
	 * Visit a parse tree produced by the {@code Print}
	 * labeled alternative in {@link DigglebyParser#base_expr}.
	 * @param ctx the parse tree
	 * @return the visitor result
	 */
	T visitPrint(DigglebyParser.PrintContext ctx);
	/**
	 * Visit a parse tree produced by the {@code Not}
	 * labeled alternative in {@link DigglebyParser#base_expr}.
	 * @param ctx the parse tree
	 * @return the visitor result
	 */
	T visitNot(DigglebyParser.NotContext ctx);
	/**
	 * Visit a parse tree produced by the {@code PlusMinusOp}
	 * labeled alternative in {@link DigglebyParser#base_expr}.
	 * @param ctx the parse tree
	 * @return the visitor result
	 */
	T visitPlusMinusOp(DigglebyParser.PlusMinusOpContext ctx);
	/**
	 * Visit a parse tree produced by the {@code GetInput}
	 * labeled alternative in {@link DigglebyParser#base_expr}.
	 * @param ctx the parse tree
	 * @return the visitor result
	 */
	T visitGetInput(DigglebyParser.GetInputContext ctx);
	/**
	 * Visit a parse tree produced by the {@code FnCall}
	 * labeled alternative in {@link DigglebyParser#base_expr}.
	 * @param ctx the parse tree
	 * @return the visitor result
	 */
	T visitFnCall(DigglebyParser.FnCallContext ctx);
	/**
	 * Visit a parse tree produced by the {@code Fac}
	 * labeled alternative in {@link DigglebyParser#base_expr}.
	 * @param ctx the parse tree
	 * @return the visitor result
	 */
	T visitFac(DigglebyParser.FacContext ctx);
	/**
	 * Visit a parse tree produced by the {@code RelOp}
	 * labeled alternative in {@link DigglebyParser#base_expr}.
	 * @param ctx the parse tree
	 * @return the visitor result
	 */
	T visitRelOp(DigglebyParser.RelOpContext ctx);
	/**
	 * Visit a parse tree produced by the {@code IfThenElse}
	 * labeled alternative in {@link DigglebyParser#base_expr}.
	 * @param ctx the parse tree
	 * @return the visitor result
	 */
	T visitIfThenElse(DigglebyParser.IfThenElseContext ctx);
	/**
	 * Visit a parse tree produced by the {@code BoolOp}
	 * labeled alternative in {@link DigglebyParser#base_expr}.
	 * @param ctx the parse tree
	 * @return the visitor result
	 */
	T visitBoolOp(DigglebyParser.BoolOpContext ctx);
	/**
	 * Visit a parse tree produced by the {@code Unit}
	 * labeled alternative in {@link DigglebyParser#fact}.
	 * @param ctx the parse tree
	 * @return the visitor result
	 */
	T visitUnit(DigglebyParser.UnitContext ctx);
	/**
	 * Visit a parse tree produced by the {@code True}
	 * labeled alternative in {@link DigglebyParser#fact}.
	 * @param ctx the parse tree
	 * @return the visitor result
	 */
	T visitTrue(DigglebyParser.TrueContext ctx);
	/**
	 * Visit a parse tree produced by the {@code False}
	 * labeled alternative in {@link DigglebyParser#fact}.
	 * @param ctx the parse tree
	 * @return the visitor result
	 */
	T visitFalse(DigglebyParser.FalseContext ctx);
	/**
	 * Visit a parse tree produced by the {@code Int}
	 * labeled alternative in {@link DigglebyParser#fact}.
	 * @param ctx the parse tree
	 * @return the visitor result
	 */
	T visitInt(DigglebyParser.IntContext ctx);
	/**
	 * Visit a parse tree produced by the {@code Str}
	 * labeled alternative in {@link DigglebyParser#fact}.
	 * @param ctx the parse tree
	 * @return the visitor result
	 */
	T visitStr(DigglebyParser.StrContext ctx);
	/**
	 * Visit a parse tree produced by the {@code Ident}
	 * labeled alternative in {@link DigglebyParser#fact}.
	 * @param ctx the parse tree
	 * @return the visitor result
	 */
	T visitIdent(DigglebyParser.IdentContext ctx);
	/**
	 * Visit a parse tree produced by the {@code Parens}
	 * labeled alternative in {@link DigglebyParser#fact}.
	 * @param ctx the parse tree
	 * @return the visitor result
	 */
	T visitParens(DigglebyParser.ParensContext ctx);
	/**
	 * Visit a parse tree produced by {@link DigglebyParser#param_list}.
	 * @param ctx the parse tree
	 * @return the visitor result
	 */
	T visitParam_list(DigglebyParser.Param_listContext ctx);
	/**
	 * Visit a parse tree produced by the {@code FnDef}
	 * labeled alternative in {@link DigglebyParser#def}.
	 * @param ctx the parse tree
	 * @return the visitor result
	 */
	T visitFnDef(DigglebyParser.FnDefContext ctx);
	/**
	 * Visit a parse tree produced by the {@code Program}
	 * labeled alternative in {@link DigglebyParser#prog}.
	 * @param ctx the parse tree
	 * @return the visitor result
	 */
	T visitProgram(DigglebyParser.ProgramContext ctx);
}