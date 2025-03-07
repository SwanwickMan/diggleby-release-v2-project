// Generated from Diggleby.g4 by ANTLR 4.9.1

package antlr;

import org.antlr.v4.runtime.atn.*;
import org.antlr.v4.runtime.dfa.DFA;
import org.antlr.v4.runtime.*;
import org.antlr.v4.runtime.misc.*;
import org.antlr.v4.runtime.tree.*;
import java.util.List;
import java.util.Iterator;
import java.util.ArrayList;

@SuppressWarnings({"all", "warnings", "unchecked", "unused", "cast"})
public class DigglebyParser extends Parser {
	static { RuntimeMetaData.checkVersion("4.9.1", RuntimeMetaData.VERSION); }

	protected static final DFA[] _decisionToDFA;
	protected static final PredictionContextCache _sharedContextCache =
		new PredictionContextCache();
	public static final int
		T__0=1, T__1=2, T__2=3, T__3=4, WS=5, COMMA=6, DEF=7, TRUE=8, FALSE=9, 
		INT=10, LET=11, IN=12, PLUS=13, MINUS=14, MUL=15, DIV=16, MOD=17, GT=18, 
		LT=19, EQQ=20, EQ=21, AND=22, OR=23, XOR=24, NOT=25, CONCAT=26, IF=27, 
		THEN=28, ELSE=29, PRINT=30, GETINPUT=31, COLON=32, LPAREN=33, RPAREN=34, 
		LBRACE=35, RBRACE=36, STR=37, IDENT=38;
	public static final int
		RULE_type = 0, RULE_expr_list = 1, RULE_expr = 2, RULE_base_expr = 3, 
		RULE_fact = 4, RULE_param_list = 5, RULE_def = 6, RULE_prog = 7;
	private static String[] makeRuleNames() {
		return new String[] {
			"type", "expr_list", "expr", "base_expr", "fact", "param_list", "def", 
			"prog"
		};
	}
	public static final String[] ruleNames = makeRuleNames();

	private static String[] makeLiteralNames() {
		return new String[] {
			null, "'Bool'", "'Int'", "'String'", "'Unit'", null, "','", "'def'", 
			"'true'", "'false'", null, "'let'", "'in'", "'+'", "'-'", "'*'", "'/'", 
			"'%'", "'>'", "'<'", "'=='", "'='", "'&&'", "'||'", "'^'", "'!'", "'concat'", 
			"'if'", "'then'", "'else'", "'print'", "'getInput'", "':'", "'('", "')'", 
			"'{'", "'}'"
		};
	}
	private static final String[] _LITERAL_NAMES = makeLiteralNames();
	private static String[] makeSymbolicNames() {
		return new String[] {
			null, null, null, null, null, "WS", "COMMA", "DEF", "TRUE", "FALSE", 
			"INT", "LET", "IN", "PLUS", "MINUS", "MUL", "DIV", "MOD", "GT", "LT", 
			"EQQ", "EQ", "AND", "OR", "XOR", "NOT", "CONCAT", "IF", "THEN", "ELSE", 
			"PRINT", "GETINPUT", "COLON", "LPAREN", "RPAREN", "LBRACE", "RBRACE", 
			"STR", "IDENT"
		};
	}
	private static final String[] _SYMBOLIC_NAMES = makeSymbolicNames();
	public static final Vocabulary VOCABULARY = new VocabularyImpl(_LITERAL_NAMES, _SYMBOLIC_NAMES);

	/**
	 * @deprecated Use {@link #VOCABULARY} instead.
	 */
	@Deprecated
	public static final String[] tokenNames;
	static {
		tokenNames = new String[_SYMBOLIC_NAMES.length];
		for (int i = 0; i < tokenNames.length; i++) {
			tokenNames[i] = VOCABULARY.getLiteralName(i);
			if (tokenNames[i] == null) {
				tokenNames[i] = VOCABULARY.getSymbolicName(i);
			}

			if (tokenNames[i] == null) {
				tokenNames[i] = "<INVALID>";
			}
		}
	}

	@Override
	@Deprecated
	public String[] getTokenNames() {
		return tokenNames;
	}

	@Override

	public Vocabulary getVocabulary() {
		return VOCABULARY;
	}

	@Override
	public String getGrammarFileName() { return "Diggleby.g4"; }

	@Override
	public String[] getRuleNames() { return ruleNames; }

	@Override
	public String getSerializedATN() { return _serializedATN; }

	@Override
	public ATN getATN() { return _ATN; }

	public DigglebyParser(TokenStream input) {
		super(input);
		_interp = new ParserATNSimulator(this,_ATN,_decisionToDFA,_sharedContextCache);
	}

	public static class TypeContext extends ParserRuleContext {
		public TypeContext(ParserRuleContext parent, int invokingState) {
			super(parent, invokingState);
		}
		@Override public int getRuleIndex() { return RULE_type; }
		@Override
		public <T> T accept(ParseTreeVisitor<? extends T> visitor) {
			if ( visitor instanceof DigglebyVisitor ) return ((DigglebyVisitor<? extends T>)visitor).visitType(this);
			else return visitor.visitChildren(this);
		}
	}

	public final TypeContext type() throws RecognitionException {
		TypeContext _localctx = new TypeContext(_ctx, getState());
		enterRule(_localctx, 0, RULE_type);
		int _la;
		try {
			enterOuterAlt(_localctx, 1);
			{
			setState(16);
			_la = _input.LA(1);
			if ( !((((_la) & ~0x3f) == 0 && ((1L << _la) & ((1L << T__0) | (1L << T__1) | (1L << T__2) | (1L << T__3))) != 0)) ) {
			_errHandler.recoverInline(this);
			}
			else {
				if ( _input.LA(1)==Token.EOF ) matchedEOF = true;
				_errHandler.reportMatch(this);
				consume();
			}
			}
		}
		catch (RecognitionException re) {
			_localctx.exception = re;
			_errHandler.reportError(this, re);
			_errHandler.recover(this, re);
		}
		finally {
			exitRule();
		}
		return _localctx;
	}

	public static class Expr_listContext extends ParserRuleContext {
		public List<ExprContext> expr() {
			return getRuleContexts(ExprContext.class);
		}
		public ExprContext expr(int i) {
			return getRuleContext(ExprContext.class,i);
		}
		public List<TerminalNode> COMMA() { return getTokens(DigglebyParser.COMMA); }
		public TerminalNode COMMA(int i) {
			return getToken(DigglebyParser.COMMA, i);
		}
		public Expr_listContext(ParserRuleContext parent, int invokingState) {
			super(parent, invokingState);
		}
		@Override public int getRuleIndex() { return RULE_expr_list; }
		@Override
		public <T> T accept(ParseTreeVisitor<? extends T> visitor) {
			if ( visitor instanceof DigglebyVisitor ) return ((DigglebyVisitor<? extends T>)visitor).visitExpr_list(this);
			else return visitor.visitChildren(this);
		}
	}

	public final Expr_listContext expr_list() throws RecognitionException {
		Expr_listContext _localctx = new Expr_listContext(_ctx, getState());
		enterRule(_localctx, 2, RULE_expr_list);
		int _la;
		try {
			setState(26);
			_errHandler.sync(this);
			switch ( getInterpreter().adaptivePredict(_input,1,_ctx) ) {
			case 1:
				enterOuterAlt(_localctx, 1);
				{
				setState(18);
				expr();
				setState(21); 
				_errHandler.sync(this);
				_la = _input.LA(1);
				do {
					{
					{
					setState(19);
					match(COMMA);
					setState(20);
					expr();
					}
					}
					setState(23); 
					_errHandler.sync(this);
					_la = _input.LA(1);
				} while ( _la==COMMA );
				}
				break;
			case 2:
				enterOuterAlt(_localctx, 2);
				{
				setState(25);
				expr();
				}
				break;
			}
		}
		catch (RecognitionException re) {
			_localctx.exception = re;
			_errHandler.reportError(this, re);
			_errHandler.recover(this, re);
		}
		finally {
			exitRule();
		}
		return _localctx;
	}

	public static class ExprContext extends ParserRuleContext {
		public ExprContext(ParserRuleContext parent, int invokingState) {
			super(parent, invokingState);
		}
		@Override public int getRuleIndex() { return RULE_expr; }
	 
		public ExprContext() { }
		public void copyFrom(ExprContext ctx) {
			super.copyFrom(ctx);
		}
	}
	public static class BaseExprContext extends ExprContext {
		public Base_exprContext base_expr() {
			return getRuleContext(Base_exprContext.class,0);
		}
		public BaseExprContext(ExprContext ctx) { copyFrom(ctx); }
		@Override
		public <T> T accept(ParseTreeVisitor<? extends T> visitor) {
			if ( visitor instanceof DigglebyVisitor ) return ((DigglebyVisitor<? extends T>)visitor).visitBaseExpr(this);
			else return visitor.visitChildren(this);
		}
	}
	public static class LetContext extends ExprContext {
		public TerminalNode LET() { return getToken(DigglebyParser.LET, 0); }
		public TerminalNode IDENT() { return getToken(DigglebyParser.IDENT, 0); }
		public TerminalNode EQ() { return getToken(DigglebyParser.EQ, 0); }
		public Base_exprContext base_expr() {
			return getRuleContext(Base_exprContext.class,0);
		}
		public TerminalNode IN() { return getToken(DigglebyParser.IN, 0); }
		public ExprContext expr() {
			return getRuleContext(ExprContext.class,0);
		}
		public LetContext(ExprContext ctx) { copyFrom(ctx); }
		@Override
		public <T> T accept(ParseTreeVisitor<? extends T> visitor) {
			if ( visitor instanceof DigglebyVisitor ) return ((DigglebyVisitor<? extends T>)visitor).visitLet(this);
			else return visitor.visitChildren(this);
		}
	}

	public final ExprContext expr() throws RecognitionException {
		ExprContext _localctx = new ExprContext(_ctx, getState());
		enterRule(_localctx, 4, RULE_expr);
		try {
			setState(36);
			_errHandler.sync(this);
			switch (_input.LA(1)) {
			case LET:
				_localctx = new LetContext(_localctx);
				enterOuterAlt(_localctx, 1);
				{
				setState(28);
				match(LET);
				setState(29);
				match(IDENT);
				setState(30);
				match(EQ);
				setState(31);
				base_expr(0);
				setState(32);
				match(IN);
				setState(33);
				expr();
				}
				break;
			case TRUE:
			case FALSE:
			case INT:
			case NOT:
			case CONCAT:
			case IF:
			case PRINT:
			case GETINPUT:
			case LPAREN:
			case STR:
			case IDENT:
				_localctx = new BaseExprContext(_localctx);
				enterOuterAlt(_localctx, 2);
				{
				setState(35);
				base_expr(0);
				}
				break;
			default:
				throw new NoViableAltException(this);
			}
		}
		catch (RecognitionException re) {
			_localctx.exception = re;
			_errHandler.reportError(this, re);
			_errHandler.recover(this, re);
		}
		finally {
			exitRule();
		}
		return _localctx;
	}

	public static class Base_exprContext extends ParserRuleContext {
		public Base_exprContext(ParserRuleContext parent, int invokingState) {
			super(parent, invokingState);
		}
		@Override public int getRuleIndex() { return RULE_base_expr; }
	 
		public Base_exprContext() { }
		public void copyFrom(Base_exprContext ctx) {
			super.copyFrom(ctx);
		}
	}
	public static class ConcatContext extends Base_exprContext {
		public TerminalNode CONCAT() { return getToken(DigglebyParser.CONCAT, 0); }
		public TerminalNode LPAREN() { return getToken(DigglebyParser.LPAREN, 0); }
		public TerminalNode RPAREN() { return getToken(DigglebyParser.RPAREN, 0); }
		public Expr_listContext expr_list() {
			return getRuleContext(Expr_listContext.class,0);
		}
		public ConcatContext(Base_exprContext ctx) { copyFrom(ctx); }
		@Override
		public <T> T accept(ParseTreeVisitor<? extends T> visitor) {
			if ( visitor instanceof DigglebyVisitor ) return ((DigglebyVisitor<? extends T>)visitor).visitConcat(this);
			else return visitor.visitChildren(this);
		}
	}
	public static class CastContext extends Base_exprContext {
		public TerminalNode LPAREN() { return getToken(DigglebyParser.LPAREN, 0); }
		public ExprContext expr() {
			return getRuleContext(ExprContext.class,0);
		}
		public TerminalNode COLON() { return getToken(DigglebyParser.COLON, 0); }
		public TypeContext type() {
			return getRuleContext(TypeContext.class,0);
		}
		public TerminalNode RPAREN() { return getToken(DigglebyParser.RPAREN, 0); }
		public CastContext(Base_exprContext ctx) { copyFrom(ctx); }
		@Override
		public <T> T accept(ParseTreeVisitor<? extends T> visitor) {
			if ( visitor instanceof DigglebyVisitor ) return ((DigglebyVisitor<? extends T>)visitor).visitCast(this);
			else return visitor.visitChildren(this);
		}
	}
	public static class MulDivOpContext extends Base_exprContext {
		public Token op;
		public List<Base_exprContext> base_expr() {
			return getRuleContexts(Base_exprContext.class);
		}
		public Base_exprContext base_expr(int i) {
			return getRuleContext(Base_exprContext.class,i);
		}
		public TerminalNode MUL() { return getToken(DigglebyParser.MUL, 0); }
		public TerminalNode DIV() { return getToken(DigglebyParser.DIV, 0); }
		public TerminalNode MOD() { return getToken(DigglebyParser.MOD, 0); }
		public MulDivOpContext(Base_exprContext ctx) { copyFrom(ctx); }
		@Override
		public <T> T accept(ParseTreeVisitor<? extends T> visitor) {
			if ( visitor instanceof DigglebyVisitor ) return ((DigglebyVisitor<? extends T>)visitor).visitMulDivOp(this);
			else return visitor.visitChildren(this);
		}
	}
	public static class PrintContext extends Base_exprContext {
		public TerminalNode PRINT() { return getToken(DigglebyParser.PRINT, 0); }
		public TerminalNode LPAREN() { return getToken(DigglebyParser.LPAREN, 0); }
		public ExprContext expr() {
			return getRuleContext(ExprContext.class,0);
		}
		public TerminalNode RPAREN() { return getToken(DigglebyParser.RPAREN, 0); }
		public PrintContext(Base_exprContext ctx) { copyFrom(ctx); }
		@Override
		public <T> T accept(ParseTreeVisitor<? extends T> visitor) {
			if ( visitor instanceof DigglebyVisitor ) return ((DigglebyVisitor<? extends T>)visitor).visitPrint(this);
			else return visitor.visitChildren(this);
		}
	}
	public static class NotContext extends Base_exprContext {
		public TerminalNode NOT() { return getToken(DigglebyParser.NOT, 0); }
		public FactContext fact() {
			return getRuleContext(FactContext.class,0);
		}
		public NotContext(Base_exprContext ctx) { copyFrom(ctx); }
		@Override
		public <T> T accept(ParseTreeVisitor<? extends T> visitor) {
			if ( visitor instanceof DigglebyVisitor ) return ((DigglebyVisitor<? extends T>)visitor).visitNot(this);
			else return visitor.visitChildren(this);
		}
	}
	public static class PlusMinusOpContext extends Base_exprContext {
		public Token op;
		public List<Base_exprContext> base_expr() {
			return getRuleContexts(Base_exprContext.class);
		}
		public Base_exprContext base_expr(int i) {
			return getRuleContext(Base_exprContext.class,i);
		}
		public TerminalNode PLUS() { return getToken(DigglebyParser.PLUS, 0); }
		public TerminalNode MINUS() { return getToken(DigglebyParser.MINUS, 0); }
		public PlusMinusOpContext(Base_exprContext ctx) { copyFrom(ctx); }
		@Override
		public <T> T accept(ParseTreeVisitor<? extends T> visitor) {
			if ( visitor instanceof DigglebyVisitor ) return ((DigglebyVisitor<? extends T>)visitor).visitPlusMinusOp(this);
			else return visitor.visitChildren(this);
		}
	}
	public static class GetInputContext extends Base_exprContext {
		public TerminalNode GETINPUT() { return getToken(DigglebyParser.GETINPUT, 0); }
		public TerminalNode LPAREN() { return getToken(DigglebyParser.LPAREN, 0); }
		public TerminalNode RPAREN() { return getToken(DigglebyParser.RPAREN, 0); }
		public GetInputContext(Base_exprContext ctx) { copyFrom(ctx); }
		@Override
		public <T> T accept(ParseTreeVisitor<? extends T> visitor) {
			if ( visitor instanceof DigglebyVisitor ) return ((DigglebyVisitor<? extends T>)visitor).visitGetInput(this);
			else return visitor.visitChildren(this);
		}
	}
	public static class FnCallContext extends Base_exprContext {
		public TerminalNode IDENT() { return getToken(DigglebyParser.IDENT, 0); }
		public TerminalNode LPAREN() { return getToken(DigglebyParser.LPAREN, 0); }
		public TerminalNode RPAREN() { return getToken(DigglebyParser.RPAREN, 0); }
		public Expr_listContext expr_list() {
			return getRuleContext(Expr_listContext.class,0);
		}
		public FnCallContext(Base_exprContext ctx) { copyFrom(ctx); }
		@Override
		public <T> T accept(ParseTreeVisitor<? extends T> visitor) {
			if ( visitor instanceof DigglebyVisitor ) return ((DigglebyVisitor<? extends T>)visitor).visitFnCall(this);
			else return visitor.visitChildren(this);
		}
	}
	public static class FacContext extends Base_exprContext {
		public FactContext fact() {
			return getRuleContext(FactContext.class,0);
		}
		public FacContext(Base_exprContext ctx) { copyFrom(ctx); }
		@Override
		public <T> T accept(ParseTreeVisitor<? extends T> visitor) {
			if ( visitor instanceof DigglebyVisitor ) return ((DigglebyVisitor<? extends T>)visitor).visitFac(this);
			else return visitor.visitChildren(this);
		}
	}
	public static class RelOpContext extends Base_exprContext {
		public Token op;
		public List<Base_exprContext> base_expr() {
			return getRuleContexts(Base_exprContext.class);
		}
		public Base_exprContext base_expr(int i) {
			return getRuleContext(Base_exprContext.class,i);
		}
		public TerminalNode LT() { return getToken(DigglebyParser.LT, 0); }
		public TerminalNode GT() { return getToken(DigglebyParser.GT, 0); }
		public TerminalNode EQQ() { return getToken(DigglebyParser.EQQ, 0); }
		public RelOpContext(Base_exprContext ctx) { copyFrom(ctx); }
		@Override
		public <T> T accept(ParseTreeVisitor<? extends T> visitor) {
			if ( visitor instanceof DigglebyVisitor ) return ((DigglebyVisitor<? extends T>)visitor).visitRelOp(this);
			else return visitor.visitChildren(this);
		}
	}
	public static class IfThenElseContext extends Base_exprContext {
		public TerminalNode IF() { return getToken(DigglebyParser.IF, 0); }
		public TerminalNode LPAREN() { return getToken(DigglebyParser.LPAREN, 0); }
		public List<ExprContext> expr() {
			return getRuleContexts(ExprContext.class);
		}
		public ExprContext expr(int i) {
			return getRuleContext(ExprContext.class,i);
		}
		public TerminalNode RPAREN() { return getToken(DigglebyParser.RPAREN, 0); }
		public List<TerminalNode> LBRACE() { return getTokens(DigglebyParser.LBRACE); }
		public TerminalNode LBRACE(int i) {
			return getToken(DigglebyParser.LBRACE, i);
		}
		public List<TerminalNode> RBRACE() { return getTokens(DigglebyParser.RBRACE); }
		public TerminalNode RBRACE(int i) {
			return getToken(DigglebyParser.RBRACE, i);
		}
		public TerminalNode ELSE() { return getToken(DigglebyParser.ELSE, 0); }
		public IfThenElseContext(Base_exprContext ctx) { copyFrom(ctx); }
		@Override
		public <T> T accept(ParseTreeVisitor<? extends T> visitor) {
			if ( visitor instanceof DigglebyVisitor ) return ((DigglebyVisitor<? extends T>)visitor).visitIfThenElse(this);
			else return visitor.visitChildren(this);
		}
	}
	public static class BoolOpContext extends Base_exprContext {
		public Token op;
		public List<Base_exprContext> base_expr() {
			return getRuleContexts(Base_exprContext.class);
		}
		public Base_exprContext base_expr(int i) {
			return getRuleContext(Base_exprContext.class,i);
		}
		public TerminalNode AND() { return getToken(DigglebyParser.AND, 0); }
		public TerminalNode OR() { return getToken(DigglebyParser.OR, 0); }
		public TerminalNode XOR() { return getToken(DigglebyParser.XOR, 0); }
		public BoolOpContext(Base_exprContext ctx) { copyFrom(ctx); }
		@Override
		public <T> T accept(ParseTreeVisitor<? extends T> visitor) {
			if ( visitor instanceof DigglebyVisitor ) return ((DigglebyVisitor<? extends T>)visitor).visitBoolOp(this);
			else return visitor.visitChildren(this);
		}
	}

	public final Base_exprContext base_expr() throws RecognitionException {
		return base_expr(0);
	}

	private Base_exprContext base_expr(int _p) throws RecognitionException {
		ParserRuleContext _parentctx = _ctx;
		int _parentState = getState();
		Base_exprContext _localctx = new Base_exprContext(_ctx, _parentState);
		Base_exprContext _prevctx = _localctx;
		int _startState = 6;
		enterRecursionRule(_localctx, 6, RULE_base_expr, _p);
		int _la;
		try {
			int _alt;
			enterOuterAlt(_localctx, 1);
			{
			setState(80);
			_errHandler.sync(this);
			switch ( getInterpreter().adaptivePredict(_input,5,_ctx) ) {
			case 1:
				{
				_localctx = new NotContext(_localctx);
				_ctx = _localctx;
				_prevctx = _localctx;

				setState(39);
				match(NOT);
				setState(40);
				fact();
				}
				break;
			case 2:
				{
				_localctx = new ConcatContext(_localctx);
				_ctx = _localctx;
				_prevctx = _localctx;
				setState(41);
				match(CONCAT);
				setState(42);
				match(LPAREN);
				setState(44);
				_errHandler.sync(this);
				_la = _input.LA(1);
				if ((((_la) & ~0x3f) == 0 && ((1L << _la) & ((1L << TRUE) | (1L << FALSE) | (1L << INT) | (1L << LET) | (1L << NOT) | (1L << CONCAT) | (1L << IF) | (1L << PRINT) | (1L << GETINPUT) | (1L << LPAREN) | (1L << STR) | (1L << IDENT))) != 0)) {
					{
					setState(43);
					expr_list();
					}
				}

				setState(46);
				match(RPAREN);
				}
				break;
			case 3:
				{
				_localctx = new PrintContext(_localctx);
				_ctx = _localctx;
				_prevctx = _localctx;
				setState(47);
				match(PRINT);
				setState(48);
				match(LPAREN);
				setState(49);
				expr();
				setState(50);
				match(RPAREN);
				}
				break;
			case 4:
				{
				_localctx = new GetInputContext(_localctx);
				_ctx = _localctx;
				_prevctx = _localctx;
				setState(52);
				match(GETINPUT);
				setState(53);
				match(LPAREN);
				setState(54);
				match(RPAREN);
				}
				break;
			case 5:
				{
				_localctx = new FnCallContext(_localctx);
				_ctx = _localctx;
				_prevctx = _localctx;
				setState(55);
				match(IDENT);
				setState(56);
				match(LPAREN);
				setState(58);
				_errHandler.sync(this);
				_la = _input.LA(1);
				if ((((_la) & ~0x3f) == 0 && ((1L << _la) & ((1L << TRUE) | (1L << FALSE) | (1L << INT) | (1L << LET) | (1L << NOT) | (1L << CONCAT) | (1L << IF) | (1L << PRINT) | (1L << GETINPUT) | (1L << LPAREN) | (1L << STR) | (1L << IDENT))) != 0)) {
					{
					setState(57);
					expr_list();
					}
				}

				setState(60);
				match(RPAREN);
				}
				break;
			case 6:
				{
				_localctx = new IfThenElseContext(_localctx);
				_ctx = _localctx;
				_prevctx = _localctx;
				setState(61);
				match(IF);
				setState(62);
				match(LPAREN);
				setState(63);
				expr();
				setState(64);
				match(RPAREN);
				setState(65);
				match(LBRACE);
				setState(66);
				expr();
				setState(67);
				match(RBRACE);
				setState(68);
				match(ELSE);
				setState(69);
				match(LBRACE);
				setState(70);
				expr();
				setState(71);
				match(RBRACE);
				}
				break;
			case 7:
				{
				_localctx = new CastContext(_localctx);
				_ctx = _localctx;
				_prevctx = _localctx;
				setState(73);
				match(LPAREN);
				setState(74);
				expr();
				setState(75);
				match(COLON);
				setState(76);
				type();
				setState(77);
				match(RPAREN);
				}
				break;
			case 8:
				{
				_localctx = new FacContext(_localctx);
				_ctx = _localctx;
				_prevctx = _localctx;
				setState(79);
				fact();
				}
				break;
			}
			_ctx.stop = _input.LT(-1);
			setState(96);
			_errHandler.sync(this);
			_alt = getInterpreter().adaptivePredict(_input,7,_ctx);
			while ( _alt!=2 && _alt!=org.antlr.v4.runtime.atn.ATN.INVALID_ALT_NUMBER ) {
				if ( _alt==1 ) {
					if ( _parseListeners!=null ) triggerExitRuleEvent();
					_prevctx = _localctx;
					{
					setState(94);
					_errHandler.sync(this);
					switch ( getInterpreter().adaptivePredict(_input,6,_ctx) ) {
					case 1:
						{
						_localctx = new RelOpContext(new Base_exprContext(_parentctx, _parentState));
						pushNewRecursionContext(_localctx, _startState, RULE_base_expr);
						setState(82);
						if (!(precpred(_ctx, 12))) throw new FailedPredicateException(this, "precpred(_ctx, 12)");
						setState(83);
						((RelOpContext)_localctx).op = _input.LT(1);
						_la = _input.LA(1);
						if ( !((((_la) & ~0x3f) == 0 && ((1L << _la) & ((1L << GT) | (1L << LT) | (1L << EQQ))) != 0)) ) {
							((RelOpContext)_localctx).op = (Token)_errHandler.recoverInline(this);
						}
						else {
							if ( _input.LA(1)==Token.EOF ) matchedEOF = true;
							_errHandler.reportMatch(this);
							consume();
						}
						setState(84);
						base_expr(13);
						}
						break;
					case 2:
						{
						_localctx = new BoolOpContext(new Base_exprContext(_parentctx, _parentState));
						pushNewRecursionContext(_localctx, _startState, RULE_base_expr);
						setState(85);
						if (!(precpred(_ctx, 11))) throw new FailedPredicateException(this, "precpred(_ctx, 11)");
						setState(86);
						((BoolOpContext)_localctx).op = _input.LT(1);
						_la = _input.LA(1);
						if ( !((((_la) & ~0x3f) == 0 && ((1L << _la) & ((1L << AND) | (1L << OR) | (1L << XOR))) != 0)) ) {
							((BoolOpContext)_localctx).op = (Token)_errHandler.recoverInline(this);
						}
						else {
							if ( _input.LA(1)==Token.EOF ) matchedEOF = true;
							_errHandler.reportMatch(this);
							consume();
						}
						setState(87);
						base_expr(12);
						}
						break;
					case 3:
						{
						_localctx = new MulDivOpContext(new Base_exprContext(_parentctx, _parentState));
						pushNewRecursionContext(_localctx, _startState, RULE_base_expr);
						setState(88);
						if (!(precpred(_ctx, 10))) throw new FailedPredicateException(this, "precpred(_ctx, 10)");
						setState(89);
						((MulDivOpContext)_localctx).op = _input.LT(1);
						_la = _input.LA(1);
						if ( !((((_la) & ~0x3f) == 0 && ((1L << _la) & ((1L << MUL) | (1L << DIV) | (1L << MOD))) != 0)) ) {
							((MulDivOpContext)_localctx).op = (Token)_errHandler.recoverInline(this);
						}
						else {
							if ( _input.LA(1)==Token.EOF ) matchedEOF = true;
							_errHandler.reportMatch(this);
							consume();
						}
						setState(90);
						base_expr(11);
						}
						break;
					case 4:
						{
						_localctx = new PlusMinusOpContext(new Base_exprContext(_parentctx, _parentState));
						pushNewRecursionContext(_localctx, _startState, RULE_base_expr);
						setState(91);
						if (!(precpred(_ctx, 9))) throw new FailedPredicateException(this, "precpred(_ctx, 9)");
						setState(92);
						((PlusMinusOpContext)_localctx).op = _input.LT(1);
						_la = _input.LA(1);
						if ( !(_la==PLUS || _la==MINUS) ) {
							((PlusMinusOpContext)_localctx).op = (Token)_errHandler.recoverInline(this);
						}
						else {
							if ( _input.LA(1)==Token.EOF ) matchedEOF = true;
							_errHandler.reportMatch(this);
							consume();
						}
						setState(93);
						base_expr(10);
						}
						break;
					}
					} 
				}
				setState(98);
				_errHandler.sync(this);
				_alt = getInterpreter().adaptivePredict(_input,7,_ctx);
			}
			}
		}
		catch (RecognitionException re) {
			_localctx.exception = re;
			_errHandler.reportError(this, re);
			_errHandler.recover(this, re);
		}
		finally {
			unrollRecursionContexts(_parentctx);
		}
		return _localctx;
	}

	public static class FactContext extends ParserRuleContext {
		public FactContext(ParserRuleContext parent, int invokingState) {
			super(parent, invokingState);
		}
		@Override public int getRuleIndex() { return RULE_fact; }
	 
		public FactContext() { }
		public void copyFrom(FactContext ctx) {
			super.copyFrom(ctx);
		}
	}
	public static class StrContext extends FactContext {
		public TerminalNode STR() { return getToken(DigglebyParser.STR, 0); }
		public StrContext(FactContext ctx) { copyFrom(ctx); }
		@Override
		public <T> T accept(ParseTreeVisitor<? extends T> visitor) {
			if ( visitor instanceof DigglebyVisitor ) return ((DigglebyVisitor<? extends T>)visitor).visitStr(this);
			else return visitor.visitChildren(this);
		}
	}
	public static class IdentContext extends FactContext {
		public TerminalNode IDENT() { return getToken(DigglebyParser.IDENT, 0); }
		public IdentContext(FactContext ctx) { copyFrom(ctx); }
		@Override
		public <T> T accept(ParseTreeVisitor<? extends T> visitor) {
			if ( visitor instanceof DigglebyVisitor ) return ((DigglebyVisitor<? extends T>)visitor).visitIdent(this);
			else return visitor.visitChildren(this);
		}
	}
	public static class ParensContext extends FactContext {
		public TerminalNode LPAREN() { return getToken(DigglebyParser.LPAREN, 0); }
		public ExprContext expr() {
			return getRuleContext(ExprContext.class,0);
		}
		public TerminalNode RPAREN() { return getToken(DigglebyParser.RPAREN, 0); }
		public ParensContext(FactContext ctx) { copyFrom(ctx); }
		@Override
		public <T> T accept(ParseTreeVisitor<? extends T> visitor) {
			if ( visitor instanceof DigglebyVisitor ) return ((DigglebyVisitor<? extends T>)visitor).visitParens(this);
			else return visitor.visitChildren(this);
		}
	}
	public static class TrueContext extends FactContext {
		public TerminalNode TRUE() { return getToken(DigglebyParser.TRUE, 0); }
		public TrueContext(FactContext ctx) { copyFrom(ctx); }
		@Override
		public <T> T accept(ParseTreeVisitor<? extends T> visitor) {
			if ( visitor instanceof DigglebyVisitor ) return ((DigglebyVisitor<? extends T>)visitor).visitTrue(this);
			else return visitor.visitChildren(this);
		}
	}
	public static class FalseContext extends FactContext {
		public TerminalNode FALSE() { return getToken(DigglebyParser.FALSE, 0); }
		public FalseContext(FactContext ctx) { copyFrom(ctx); }
		@Override
		public <T> T accept(ParseTreeVisitor<? extends T> visitor) {
			if ( visitor instanceof DigglebyVisitor ) return ((DigglebyVisitor<? extends T>)visitor).visitFalse(this);
			else return visitor.visitChildren(this);
		}
	}
	public static class UnitContext extends FactContext {
		public TerminalNode LPAREN() { return getToken(DigglebyParser.LPAREN, 0); }
		public TerminalNode RPAREN() { return getToken(DigglebyParser.RPAREN, 0); }
		public UnitContext(FactContext ctx) { copyFrom(ctx); }
		@Override
		public <T> T accept(ParseTreeVisitor<? extends T> visitor) {
			if ( visitor instanceof DigglebyVisitor ) return ((DigglebyVisitor<? extends T>)visitor).visitUnit(this);
			else return visitor.visitChildren(this);
		}
	}
	public static class IntContext extends FactContext {
		public TerminalNode INT() { return getToken(DigglebyParser.INT, 0); }
		public IntContext(FactContext ctx) { copyFrom(ctx); }
		@Override
		public <T> T accept(ParseTreeVisitor<? extends T> visitor) {
			if ( visitor instanceof DigglebyVisitor ) return ((DigglebyVisitor<? extends T>)visitor).visitInt(this);
			else return visitor.visitChildren(this);
		}
	}

	public final FactContext fact() throws RecognitionException {
		FactContext _localctx = new FactContext(_ctx, getState());
		enterRule(_localctx, 8, RULE_fact);
		try {
			setState(110);
			_errHandler.sync(this);
			switch ( getInterpreter().adaptivePredict(_input,8,_ctx) ) {
			case 1:
				_localctx = new UnitContext(_localctx);
				enterOuterAlt(_localctx, 1);
				{
				setState(99);
				match(LPAREN);
				setState(100);
				match(RPAREN);
				}
				break;
			case 2:
				_localctx = new TrueContext(_localctx);
				enterOuterAlt(_localctx, 2);
				{
				setState(101);
				match(TRUE);
				}
				break;
			case 3:
				_localctx = new FalseContext(_localctx);
				enterOuterAlt(_localctx, 3);
				{
				setState(102);
				match(FALSE);
				}
				break;
			case 4:
				_localctx = new IntContext(_localctx);
				enterOuterAlt(_localctx, 4);
				{
				setState(103);
				match(INT);
				}
				break;
			case 5:
				_localctx = new StrContext(_localctx);
				enterOuterAlt(_localctx, 5);
				{
				setState(104);
				match(STR);
				}
				break;
			case 6:
				_localctx = new IdentContext(_localctx);
				enterOuterAlt(_localctx, 6);
				{
				setState(105);
				match(IDENT);
				}
				break;
			case 7:
				_localctx = new ParensContext(_localctx);
				enterOuterAlt(_localctx, 7);
				{
				setState(106);
				match(LPAREN);
				setState(107);
				expr();
				setState(108);
				match(RPAREN);
				}
				break;
			}
		}
		catch (RecognitionException re) {
			_localctx.exception = re;
			_errHandler.reportError(this, re);
			_errHandler.recover(this, re);
		}
		finally {
			exitRule();
		}
		return _localctx;
	}

	public static class Param_listContext extends ParserRuleContext {
		public List<TerminalNode> IDENT() { return getTokens(DigglebyParser.IDENT); }
		public TerminalNode IDENT(int i) {
			return getToken(DigglebyParser.IDENT, i);
		}
		public List<TerminalNode> COLON() { return getTokens(DigglebyParser.COLON); }
		public TerminalNode COLON(int i) {
			return getToken(DigglebyParser.COLON, i);
		}
		public List<TypeContext> type() {
			return getRuleContexts(TypeContext.class);
		}
		public TypeContext type(int i) {
			return getRuleContext(TypeContext.class,i);
		}
		public List<TerminalNode> COMMA() { return getTokens(DigglebyParser.COMMA); }
		public TerminalNode COMMA(int i) {
			return getToken(DigglebyParser.COMMA, i);
		}
		public Param_listContext(ParserRuleContext parent, int invokingState) {
			super(parent, invokingState);
		}
		@Override public int getRuleIndex() { return RULE_param_list; }
		@Override
		public <T> T accept(ParseTreeVisitor<? extends T> visitor) {
			if ( visitor instanceof DigglebyVisitor ) return ((DigglebyVisitor<? extends T>)visitor).visitParam_list(this);
			else return visitor.visitChildren(this);
		}
	}

	public final Param_listContext param_list() throws RecognitionException {
		Param_listContext _localctx = new Param_listContext(_ctx, getState());
		enterRule(_localctx, 10, RULE_param_list);
		int _la;
		try {
			setState(126);
			_errHandler.sync(this);
			switch ( getInterpreter().adaptivePredict(_input,10,_ctx) ) {
			case 1:
				enterOuterAlt(_localctx, 1);
				{
				setState(112);
				match(IDENT);
				setState(113);
				match(COLON);
				setState(114);
				type();
				setState(119); 
				_errHandler.sync(this);
				_la = _input.LA(1);
				do {
					{
					{
					setState(115);
					match(COMMA);
					setState(116);
					match(IDENT);
					setState(117);
					match(COLON);
					setState(118);
					type();
					}
					}
					setState(121); 
					_errHandler.sync(this);
					_la = _input.LA(1);
				} while ( _la==COMMA );
				}
				break;
			case 2:
				enterOuterAlt(_localctx, 2);
				{
				setState(123);
				match(IDENT);
				setState(124);
				match(COLON);
				setState(125);
				type();
				}
				break;
			}
		}
		catch (RecognitionException re) {
			_localctx.exception = re;
			_errHandler.reportError(this, re);
			_errHandler.recover(this, re);
		}
		finally {
			exitRule();
		}
		return _localctx;
	}

	public static class DefContext extends ParserRuleContext {
		public DefContext(ParserRuleContext parent, int invokingState) {
			super(parent, invokingState);
		}
		@Override public int getRuleIndex() { return RULE_def; }
	 
		public DefContext() { }
		public void copyFrom(DefContext ctx) {
			super.copyFrom(ctx);
		}
	}
	public static class FnDefContext extends DefContext {
		public TerminalNode DEF() { return getToken(DigglebyParser.DEF, 0); }
		public TerminalNode IDENT() { return getToken(DigglebyParser.IDENT, 0); }
		public TerminalNode LPAREN() { return getToken(DigglebyParser.LPAREN, 0); }
		public TerminalNode RPAREN() { return getToken(DigglebyParser.RPAREN, 0); }
		public TerminalNode COLON() { return getToken(DigglebyParser.COLON, 0); }
		public TypeContext type() {
			return getRuleContext(TypeContext.class,0);
		}
		public TerminalNode LBRACE() { return getToken(DigglebyParser.LBRACE, 0); }
		public ExprContext expr() {
			return getRuleContext(ExprContext.class,0);
		}
		public TerminalNode RBRACE() { return getToken(DigglebyParser.RBRACE, 0); }
		public Param_listContext param_list() {
			return getRuleContext(Param_listContext.class,0);
		}
		public FnDefContext(DefContext ctx) { copyFrom(ctx); }
		@Override
		public <T> T accept(ParseTreeVisitor<? extends T> visitor) {
			if ( visitor instanceof DigglebyVisitor ) return ((DigglebyVisitor<? extends T>)visitor).visitFnDef(this);
			else return visitor.visitChildren(this);
		}
	}

	public final DefContext def() throws RecognitionException {
		DefContext _localctx = new DefContext(_ctx, getState());
		enterRule(_localctx, 12, RULE_def);
		int _la;
		try {
			_localctx = new FnDefContext(_localctx);
			enterOuterAlt(_localctx, 1);
			{
			setState(128);
			match(DEF);
			setState(129);
			match(IDENT);
			setState(130);
			match(LPAREN);
			setState(132);
			_errHandler.sync(this);
			_la = _input.LA(1);
			if (_la==IDENT) {
				{
				setState(131);
				param_list();
				}
			}

			setState(134);
			match(RPAREN);
			setState(135);
			match(COLON);
			setState(136);
			type();
			setState(137);
			match(LBRACE);
			setState(138);
			expr();
			setState(139);
			match(RBRACE);
			}
		}
		catch (RecognitionException re) {
			_localctx.exception = re;
			_errHandler.reportError(this, re);
			_errHandler.recover(this, re);
		}
		finally {
			exitRule();
		}
		return _localctx;
	}

	public static class ProgContext extends ParserRuleContext {
		public ProgContext(ParserRuleContext parent, int invokingState) {
			super(parent, invokingState);
		}
		@Override public int getRuleIndex() { return RULE_prog; }
	 
		public ProgContext() { }
		public void copyFrom(ProgContext ctx) {
			super.copyFrom(ctx);
		}
	}
	public static class ProgramContext extends ProgContext {
		public ExprContext expr() {
			return getRuleContext(ExprContext.class,0);
		}
		public List<DefContext> def() {
			return getRuleContexts(DefContext.class);
		}
		public DefContext def(int i) {
			return getRuleContext(DefContext.class,i);
		}
		public ProgramContext(ProgContext ctx) { copyFrom(ctx); }
		@Override
		public <T> T accept(ParseTreeVisitor<? extends T> visitor) {
			if ( visitor instanceof DigglebyVisitor ) return ((DigglebyVisitor<? extends T>)visitor).visitProgram(this);
			else return visitor.visitChildren(this);
		}
	}

	public final ProgContext prog() throws RecognitionException {
		ProgContext _localctx = new ProgContext(_ctx, getState());
		enterRule(_localctx, 14, RULE_prog);
		int _la;
		try {
			_localctx = new ProgramContext(_localctx);
			enterOuterAlt(_localctx, 1);
			{
			setState(144);
			_errHandler.sync(this);
			_la = _input.LA(1);
			while (_la==DEF) {
				{
				{
				setState(141);
				def();
				}
				}
				setState(146);
				_errHandler.sync(this);
				_la = _input.LA(1);
			}
			setState(147);
			expr();
			}
		}
		catch (RecognitionException re) {
			_localctx.exception = re;
			_errHandler.reportError(this, re);
			_errHandler.recover(this, re);
		}
		finally {
			exitRule();
		}
		return _localctx;
	}

	public boolean sempred(RuleContext _localctx, int ruleIndex, int predIndex) {
		switch (ruleIndex) {
		case 3:
			return base_expr_sempred((Base_exprContext)_localctx, predIndex);
		}
		return true;
	}
	private boolean base_expr_sempred(Base_exprContext _localctx, int predIndex) {
		switch (predIndex) {
		case 0:
			return precpred(_ctx, 12);
		case 1:
			return precpred(_ctx, 11);
		case 2:
			return precpred(_ctx, 10);
		case 3:
			return precpred(_ctx, 9);
		}
		return true;
	}

	public static final String _serializedATN =
		"\3\u608b\ua72a\u8133\ub9ed\u417c\u3be7\u7786\u5964\3(\u0098\4\2\t\2\4"+
		"\3\t\3\4\4\t\4\4\5\t\5\4\6\t\6\4\7\t\7\4\b\t\b\4\t\t\t\3\2\3\2\3\3\3\3"+
		"\3\3\6\3\30\n\3\r\3\16\3\31\3\3\5\3\35\n\3\3\4\3\4\3\4\3\4\3\4\3\4\3\4"+
		"\3\4\5\4\'\n\4\3\5\3\5\3\5\3\5\3\5\3\5\5\5/\n\5\3\5\3\5\3\5\3\5\3\5\3"+
		"\5\3\5\3\5\3\5\3\5\3\5\3\5\5\5=\n\5\3\5\3\5\3\5\3\5\3\5\3\5\3\5\3\5\3"+
		"\5\3\5\3\5\3\5\3\5\3\5\3\5\3\5\3\5\3\5\3\5\3\5\5\5S\n\5\3\5\3\5\3\5\3"+
		"\5\3\5\3\5\3\5\3\5\3\5\3\5\3\5\3\5\7\5a\n\5\f\5\16\5d\13\5\3\6\3\6\3\6"+
		"\3\6\3\6\3\6\3\6\3\6\3\6\3\6\3\6\5\6q\n\6\3\7\3\7\3\7\3\7\3\7\3\7\3\7"+
		"\6\7z\n\7\r\7\16\7{\3\7\3\7\3\7\5\7\u0081\n\7\3\b\3\b\3\b\3\b\5\b\u0087"+
		"\n\b\3\b\3\b\3\b\3\b\3\b\3\b\3\b\3\t\7\t\u0091\n\t\f\t\16\t\u0094\13\t"+
		"\3\t\3\t\3\t\2\3\b\n\2\4\6\b\n\f\16\20\2\7\3\2\3\6\3\2\24\26\3\2\30\32"+
		"\3\2\21\23\3\2\17\20\2\u00a9\2\22\3\2\2\2\4\34\3\2\2\2\6&\3\2\2\2\bR\3"+
		"\2\2\2\np\3\2\2\2\f\u0080\3\2\2\2\16\u0082\3\2\2\2\20\u0092\3\2\2\2\22"+
		"\23\t\2\2\2\23\3\3\2\2\2\24\27\5\6\4\2\25\26\7\b\2\2\26\30\5\6\4\2\27"+
		"\25\3\2\2\2\30\31\3\2\2\2\31\27\3\2\2\2\31\32\3\2\2\2\32\35\3\2\2\2\33"+
		"\35\5\6\4\2\34\24\3\2\2\2\34\33\3\2\2\2\35\5\3\2\2\2\36\37\7\r\2\2\37"+
		" \7(\2\2 !\7\27\2\2!\"\5\b\5\2\"#\7\16\2\2#$\5\6\4\2$\'\3\2\2\2%\'\5\b"+
		"\5\2&\36\3\2\2\2&%\3\2\2\2\'\7\3\2\2\2()\b\5\1\2)*\7\33\2\2*S\5\n\6\2"+
		"+,\7\34\2\2,.\7#\2\2-/\5\4\3\2.-\3\2\2\2./\3\2\2\2/\60\3\2\2\2\60S\7$"+
		"\2\2\61\62\7 \2\2\62\63\7#\2\2\63\64\5\6\4\2\64\65\7$\2\2\65S\3\2\2\2"+
		"\66\67\7!\2\2\678\7#\2\28S\7$\2\29:\7(\2\2:<\7#\2\2;=\5\4\3\2<;\3\2\2"+
		"\2<=\3\2\2\2=>\3\2\2\2>S\7$\2\2?@\7\35\2\2@A\7#\2\2AB\5\6\4\2BC\7$\2\2"+
		"CD\7%\2\2DE\5\6\4\2EF\7&\2\2FG\7\37\2\2GH\7%\2\2HI\5\6\4\2IJ\7&\2\2JS"+
		"\3\2\2\2KL\7#\2\2LM\5\6\4\2MN\7\"\2\2NO\5\2\2\2OP\7$\2\2PS\3\2\2\2QS\5"+
		"\n\6\2R(\3\2\2\2R+\3\2\2\2R\61\3\2\2\2R\66\3\2\2\2R9\3\2\2\2R?\3\2\2\2"+
		"RK\3\2\2\2RQ\3\2\2\2Sb\3\2\2\2TU\f\16\2\2UV\t\3\2\2Va\5\b\5\17WX\f\r\2"+
		"\2XY\t\4\2\2Ya\5\b\5\16Z[\f\f\2\2[\\\t\5\2\2\\a\5\b\5\r]^\f\13\2\2^_\t"+
		"\6\2\2_a\5\b\5\f`T\3\2\2\2`W\3\2\2\2`Z\3\2\2\2`]\3\2\2\2ad\3\2\2\2b`\3"+
		"\2\2\2bc\3\2\2\2c\t\3\2\2\2db\3\2\2\2ef\7#\2\2fq\7$\2\2gq\7\n\2\2hq\7"+
		"\13\2\2iq\7\f\2\2jq\7\'\2\2kq\7(\2\2lm\7#\2\2mn\5\6\4\2no\7$\2\2oq\3\2"+
		"\2\2pe\3\2\2\2pg\3\2\2\2ph\3\2\2\2pi\3\2\2\2pj\3\2\2\2pk\3\2\2\2pl\3\2"+
		"\2\2q\13\3\2\2\2rs\7(\2\2st\7\"\2\2ty\5\2\2\2uv\7\b\2\2vw\7(\2\2wx\7\""+
		"\2\2xz\5\2\2\2yu\3\2\2\2z{\3\2\2\2{y\3\2\2\2{|\3\2\2\2|\u0081\3\2\2\2"+
		"}~\7(\2\2~\177\7\"\2\2\177\u0081\5\2\2\2\u0080r\3\2\2\2\u0080}\3\2\2\2"+
		"\u0081\r\3\2\2\2\u0082\u0083\7\t\2\2\u0083\u0084\7(\2\2\u0084\u0086\7"+
		"#\2\2\u0085\u0087\5\f\7\2\u0086\u0085\3\2\2\2\u0086\u0087\3\2\2\2\u0087"+
		"\u0088\3\2\2\2\u0088\u0089\7$\2\2\u0089\u008a\7\"\2\2\u008a\u008b\5\2"+
		"\2\2\u008b\u008c\7%\2\2\u008c\u008d\5\6\4\2\u008d\u008e\7&\2\2\u008e\17"+
		"\3\2\2\2\u008f\u0091\5\16\b\2\u0090\u008f\3\2\2\2\u0091\u0094\3\2\2\2"+
		"\u0092\u0090\3\2\2\2\u0092\u0093\3\2\2\2\u0093\u0095\3\2\2\2\u0094\u0092"+
		"\3\2\2\2\u0095\u0096\5\6\4\2\u0096\21\3\2\2\2\17\31\34&.<R`bp{\u0080\u0086"+
		"\u0092";
	public static final ATN _ATN =
		new ATNDeserializer().deserialize(_serializedATN.toCharArray());
	static {
		_decisionToDFA = new DFA[_ATN.getNumberOfDecisions()];
		for (int i = 0; i < _ATN.getNumberOfDecisions(); i++) {
			_decisionToDFA[i] = new DFA(_ATN.getDecisionState(i), i);
		}
	}
}