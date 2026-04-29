import java.math.BigDecimal;

public class FloatingPointAddition {
    public static void main(String[] args) {
        double a = 0.1;
        double b = 0.2;
        System.out.println(a + b);        // 0.30000000000000004
        System.out.println(a + b == 0.3); // false

        BigDecimal x = new BigDecimal("0.1");
        BigDecimal y = new BigDecimal("0.2");
        System.out.println(x.add(y));                                // 0.3
        System.out.println(x.add(y).compareTo(new BigDecimal("0.3")) == 0); // true
    }
}
