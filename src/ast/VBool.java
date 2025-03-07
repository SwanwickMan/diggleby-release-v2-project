package ast;

public class VBool extends Value {

    private boolean value;
    public boolean getValue() { return value; }

    public VBool(boolean value) {
        this.value = value;
    }

    public String toString() {
        if (value) {
            return "true";
        } else {
            return "false";
        }
    }

    @Override
    public int hashCode() {
        final int prime = 31;
        int result = 1;
        result = prime * result + (value ? 1231 : 1237);
        return result;
    }

    @Override
    public boolean equals(Object obj) {
        if (this == obj)
            return true;
        if (obj == null)
            return false;
        if (getClass() != obj.getClass())
            return false;
        VBool other = (VBool) obj;
        if (value != other.value)
            return false;
        return true;
    }

    
}
