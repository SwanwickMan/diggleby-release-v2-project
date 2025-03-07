package ast;

public class ECond extends Expr {
    private Expr test;
    private Expr thenBranch;
    private Expr elseBranch;
    public ECond(Expr test, Expr thenBranch, Expr elseBranch) {
        this.test = test;
        this.thenBranch = thenBranch;
        this.elseBranch = elseBranch;
    }
    public Expr getTest() {
        return test;
    }
    public Expr getThenBranch() {
        return thenBranch;
    }
    public Expr getElseBranch() {
        return elseBranch;
    }
    @Override
    public String toString() {
        return String.format("if ( %s ) { %s } else { %s }", test, thenBranch, elseBranch);
    }

    

}
