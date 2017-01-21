package code.util;

import java.util.*;

public class Main {
    public static void main(String[] args) {
        Scanner in = new Scanner(System.in);
        int T = in.nextInt();
        for(int t = 1; t <= T; t++) {
            int n = in.nextInt();
            int r = in.nextInt();
            int p = in.nextInt();
            int s = in.nextInt();
            System.out.println("Case #" + t + ": " + solve(n, r, p, s));
        }
    }

    private static List<String> listFrom(String s) {
        List<String> ret = new ArrayList<String>();
        ret.add(s);
        return ret;
    }

    static String solve(int n, int r, int p, int s) {
        String a = solve(listFrom("PR"), n, r, p, s);
        String b = solve(listFrom("PS"), n, r, p, s);
        String c = solve(listFrom("RS"), n, r, p, s);
        if (a == null && b == null && c == null) return "IMPOSSIBLE";
        String best = a;
        if (best == null || b != null && best.compareTo(b) > 0) {
            best = b;
        }
        if (best == null || c != null && best.compareTo(c) > 0) {
            best = c;
        }
        return best;
    }

    static String solve(List<String> nextRound, int n, int r, int p, int s) {
        if ((1 << n - 1) == nextRound.size()) {
            String ret = generate(nextRound, 0, nextRound.size());
            for(char c : ret.toCharArray()) {
                if (c == 'R') --r;
                if (c == 'P') --p;
                if (c == 'S') --s;
            }
            if (r != 0 || p != 0 || s != 0) return null;
            return ret;
        }
        List<String> thisRound = new ArrayList<String>();
        for(String match : nextRound) {
            thisRound.add(prevMatch(match.charAt(0)));
            thisRound.add(prevMatch(match.charAt(1)));
        }
        return solve(thisRound, n, r, p, s);
    }

    private static String generate(List<String> round, int st, int end) {
        if (end - st == 1) {
            if (round.get(st).charAt(0) > round.get(st).charAt(1))
                return "" + round.get(st).charAt(1) + round.get(st).charAt(0);
            else return round.get(st);
        }

        String first = generate(round, st, st + (end - st) / 2);
        String second = generate(round, st + (end - st) / 2, end);
        if (first.compareTo(second) < 0) return first + second;
        return second + first;
    }

    private static String prevMatch(char winner) {
        if (winner == 'P') return "PR";
        if (winner == 'R') return "RS";
        if (winner == 'S') return "SP";
        throw new RuntimeException();
    }
}
