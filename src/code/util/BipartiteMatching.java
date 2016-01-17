package code.util;

import java.util.Arrays;
import java.util.List;

public class BipartiteMatching {

    final List<List<Long>> w;
    final int[] mr;
    final int[] mc;
    final boolean[] seen;

    boolean ran = false;
    int ct = 0;

    public BipartiteMatching(List<List<Long>> matrix) {
        this.w = matrix;
        this.mr = new int[w.size()];
        this.mc = new int[w.get(0).size()];
        this.seen = new boolean[w.get(0).size()];
        // Initialization
        for (int i = 0; i < mr.length; i++) {
            mr[i] = -1;
        }
        for (int i = 0; i < mc.length; i++) {
            mc[i] = -1; seen[i] = false;
        }
    }

    public int run() {
        if (!ran ) {
            for (int i = 0; i < w.size(); i++) {
                for (int j = 0; j < mc.length; j++) {
                    seen[j] = false;
                }
                if (findMatch(i))
                    ct++;
            }
            ran = true;
        }
        return ct;
    }


    private boolean findMatch(int i) {
        for (int j = 0; j < w.get(i).size(); j++) {
            if ((w.get(i).get(j) != 0) && !seen[j]) {
                seen[j] = true;
                if (mc[j] < 0 || findMatch(mc[j])) {
                    mr[i] = j;
                    mc[j] = i;
                    return true;
                }
            }
        }
        return false;
    }
}
