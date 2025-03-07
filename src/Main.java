import org.antlr.v4.runtime.*;
import org.antlr.v4.runtime.tree.*;
import ast.*;
import antlr.*;


import java.nio.file.Files;
import java.nio.file.Paths;

public class Main {

    private static Program loadAndParse(String path) throws Exception {
        String code = Files.readString(Paths.get(path));
        CharStream input = CharStreams.fromString(code);
        DigglebyLexer lexer = new DigglebyLexer(input);
        CommonTokenStream tokens = new CommonTokenStream(lexer);
        DigglebyParser parser = new DigglebyParser(tokens);
        ParseTree tree = parser.prog();
        //System.out.println(tree.toStringTree(parser));
        return new DigglebyASTGeneratorVisitor().visitProgram(tree);
    }
    
    private static void typecheck(Program prog) {
        Type type = (new Typecheck()).typecheckProgram(prog);
        System.out.println("Type: " + type);
    }

    private static void interp(Program prog) {
        Value result = (new Interp()).interpProgram(prog);
        System.out.println("Result: " + result);
    }

    public static void main(String[] args) throws Exception {
        if (args.length == 1) {
            String inputFile = args[0];
            Program p = loadAndParse(inputFile);
            typecheck(p);
            interp(p);
        } else if (args.length == 2 && args[0].equals("-typecheck")) {
            String inputFile = args[1];
            Program p = loadAndParse(inputFile);
            typecheck(p);
        } else if (args.length == 2 && args[0].equals("-interp")) {
            String inputFile = args[1];
            Program p = loadAndParse(inputFile);
            interp(p);
        } else {
            System.out.println("Usage: ./run.(sh|bat) [-typecheck | -interp] <filename>");
        }
    }
}
