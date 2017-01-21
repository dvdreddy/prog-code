 package code.gcj_sixteen.distributed;

import java.util.HashMap;
import java.util.Map;

public class MajoritySolution {

    static class Vote {
        long count = 0;

        Vote() {
        }

        public void inc() {
            count++;
        }

        public void inc(long val) {
            count += val;
        }
    }


    public static void main(String[] args) {
        int currentNodeId = message.MyNodeId();


        if (currentNodeId == 0) {
            HashMap<Long, Vote> votes = new HashMap<Long, Vote>() ;
            int allNodes = message.NumberOfNodes();
            long n = majority.GetN();
            long step = n / (allNodes - 1);
            long strt = 0;
            for (int i = 1; i < allNodes; i++) {
                message.PutLL(i, strt);
                long end = strt + step;
                if (i + 1 == allNodes) {
                    end = n;
                }
                message.PutLL(i, end);
                message.Send(i);
                strt += step;
            }

            for (int i = 1; i < allNodes; i++) {
                message.Receive(i);
                int count = message.GetInt(i);
                for (int k = 0; k < count; k++) {
                    long cand = message.GetLL(i);
                    long voteCount = message.GetLL(i);

                    Vote x = votes.get(cand);
                    if (x == null) {
                        x = new Vote();
                        votes.put(cand, x);
                    }
                    x.inc(voteCount);
                }
            }

            long win = -1;
            for (Map.Entry<Long, Vote> mp  : votes.entrySet()) {
                System.err.println("Vote " + mp.getKey() + " " + mp.getValue().count);
                if (mp.getValue().count >= (n + 2) / 2) {
                    win = mp.getKey();
                    break;
                }
            }

            if (win == -1) {
                System.out.println("NO WINNER");
            } else {
                System.out.print(win);
            }

        } else {
            message.Receive(0);
            long start = message.GetLL(0);
            long stop = message.GetLL(0);

            HashMap<Long, Vote> votes = new HashMap<Long, Vote>() ;
            System.err.println("Start " + start + " : " + stop);
            for (long i = start; i < stop; i++) {
                long getVote = majority.GetVote(i);
                Vote x = votes.get(getVote);
                if (x == null) {
                    x = new Vote();
                    votes.put(getVote, x);
                }

                x.inc();
            }

            message.PutInt(0, votes.size());
            for (Map.Entry<Long, Vote> mp  : votes.entrySet()) {
                message.PutLL(0, mp.getKey());
                message.PutLL(0, mp.getValue().count);
            }
            message.Send(0);
        }
    }
}
