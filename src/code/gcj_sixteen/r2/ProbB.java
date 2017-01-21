package code.gcj_sixteen.r2;

import code.util.InputReader;
import java.io.FileInputStream;
import java.io.PrintWriter;
import java.util.Arrays;

public class ProbB {

    public static String resultOne(InputReader reader) {
        int n = reader.nextInt();
        int kx = reader.nextInt();
        double[] probs = new double[n];
        for (int i = 0; i < n; i++) {
            probs[i] = reader.nextDouble();
        }

        Arrays.sort(probs);

        double curx = 0.0;
        for (int i = 0; i + kx <= n; i++) {
            curx = Math.max(curx,resultOneSub(probs, kx, i));
        }

        for (int i = 0; i <= kx; i++) {
            double[] newProbs = new double[kx];
            for (int j = 0; j < i; j++) {
                newProbs[j] = probs[j];
            }
            int last = n - 1;
            for (int j = i; j < kx; j++) {
                newProbs[j] = probs[last];
                last--;
            }
            curx = Math.max(curx, resultOneSub(newProbs,kx,0));
        }

        return String.format("%.8f", curx);
    }

    public static double resultOneSub(double[] probs, int kx, int strt) {
        double[][] dp = new double[kx + 1][kx + 1];

        dp[0][0] = 1.0;

        for (int i = 0; i < kx; i++) {
            for (int k = 0; k <= i; k++) {
                double curx =  dp[i][k];
                double curxP = probs[strt + i];
                dp[i + 1][k] += curx * (1 - curxP);
                dp[i + 1][k + 1] += curx * curxP;
            }
        }

        return dp[kx][kx/2];
    }


    public static String result(InputReader reader) {
        int t = reader.nextInt();
        StringBuilder builder = new StringBuilder();
        for (int i = 1; i <= t; i++) {
            builder.append("Case #");
            builder.append(i);
            builder.append(": ");
            builder.append(resultOne(reader));
            builder.append("\n");
        }
        return builder.toString();
    }


    public static void main(String[] args) throws Exception {
        InputReader reader = null;
        if (args.length == 0) {
            reader = new InputReader(System.in);
        } else if (args.length == 2) {
            reader = new InputReader(new FileInputStream(args[0]));
        } else {
            throw new RuntimeException("Error in args");
        }


        String res = result(reader);
        if (args.length == 0) {
            System.out.println(res);
        } else {
            PrintWriter out = new PrintWriter(args[1]);
            out.println(out);
            out.close();
        }
    }
}
