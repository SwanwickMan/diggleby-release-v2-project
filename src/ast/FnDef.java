package ast;

import java.util.ArrayList;
import java.util.List;

public class FnDef {

    private String fnName;
    private List<AnnotatedParam> params;
    private Type returnType;
    private Expr body;
    public FnDef(String fnName, List<AnnotatedParam> params, Type returnType, Expr body) {
        this.fnName = fnName;
        this.params = params;
        this.returnType = returnType;
        this.body = body;
    }
    public String getFnName() {
        return fnName;
    }
    public List<AnnotatedParam> getParams() {
        return params;
    }

    public Expr getBody() {
        return body;
    }

    public Type getReturnType() {
        return returnType;
    }

    private String paramToString(AnnotatedParam paramPair) {
        return paramPair.param + ": " + paramPair.type.toString();
    }

    public String toString() {
        List<String> paramStrs = new ArrayList<>();
        for (AnnotatedParam p : params) {
            paramStrs.add(paramToString(p));
        }
        String paramsStr = String.join(", ", paramStrs);
        return String.format("def %s(%s): %s { %s }", fnName, paramsStr, returnType, body);
    }

}
