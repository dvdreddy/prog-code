package code.util;

public class Factorial {
    private static final int LIMIT = 1000002000;
    private static final long MOD = 1000000007L;

    private static final long[] FACTORIAL = new long[LIMIT];

    static {
        long res = 1; FACTORIAL[0] = 1L;
        long startTime = System.currentTimeMillis();
        for (int i = 1; i < LIMIT; i++) {
            res = (res * (long) i) % MOD;
            FACTORIAL[i] = res;
            /*if ((i & 1048575) == 0) {
                System.out.println("Done " + i);
            }*/
        }
        long endTime = System.currentTimeMillis();
        System.out.println("Elapsed time : " + (endTime - startTime) + "ms");
    }

    public static long factorial(int n) {
        return FACTORIAL[n];
    }
}
