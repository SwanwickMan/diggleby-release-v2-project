package ast;

public class ELet extends Expr {

    private String binder;
    private Expr subject;
    private Expr continuation;

    public ELet(String binder, Expr subject, Expr continuation) {
        this.binder = binder;
        this.subject = subject;
        this.continuation = continuation;
    }

    public String getBinder() {
        return binder;
    }

    public Expr getSubject() {
        return subject;
    }

    public Expr getContinuation() {
        return continuation;
    }

    @Override
    public String toString() {
        return String.format("let %s = %s in %s", binder, subject, continuation);
    }

    
}
