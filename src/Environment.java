import java.util.LinkedHashMap;
import java.util.Map;
import java.util.Map.Entry;

/*
 * An immutable map from strings to another type. We will use this for both
 * type environments and runtime value environments.
 */
public class Environment<T> {

    // Internal (mutable) map
    private Map<String, T> env;

    public Environment() {
        this.env = new LinkedHashMap<>();
    }

    private void setEnv(Map<String, T> env) {
        this.env = env;
    }

    public T lookup(String var) throws RuntimeException {
        if (env.containsKey(var)) {
            return env.get(var);
        } else {
            throw new RuntimeException("Variable lookup failed for variable " + var + ".");
        }
    }

    public Environment<T> extend(String var, T t) {
        Map<String, T> newEnv = new LinkedHashMap<>();
        for (Entry<String, T> entry : env.entrySet()) {
            newEnv.put(entry.getKey(), entry.getValue());
        }
        newEnv.put(var, t);
        Environment<T> newEnvironment = new Environment<>();
        newEnvironment.setEnv(newEnv);
        return newEnvironment;
    }

    public void dump() {
        for (Entry<String, T> e : env.entrySet()) {
            System.out.println(e.getKey() + ": " + e.getValue());
        }
    }

}
