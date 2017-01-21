package code.gcj_sixteen.distributed;


import java.util.Arrays;

public class RPSSolution {
    static int MASTER_NODE = 7;
    static long DONE = -1;


    public static long getWinner(long id1, char pref1, long id2, char pref2) {
        if (pref1 == pref2) {
            return id1;
        }

        char[] prefs = new char[2];
        prefs[0] = pref1;
        prefs[1] = pref2;
        Arrays.sort(prefs);

        if(prefs[0] == 'R' && prefs[1] == 'S') {
            return (pref1 == 'R') ? id1 : id2;
        } else if (prefs[0] == 'P' && prefs[1] == 'R') {
            return (pref1 == 'P') ? id1 : id2;
        } else if (prefs[0] == 'P' && prefs[1] == 'S') {
            return (pref1 == 'S') ? id1 : id2;
        } else {
            System.err.println("Unknown condition in the else statement" );
            return -1;
        }
    }


    public static void main(String[] args) {
        int N = (int) rps.GetN();
        long totalCands = 1 << N;

        long nodes = message.NumberOfNodes();
        int my_id = message.MyNodeId();

        long step = totalCands / nodes;
        long start = step * my_id;
        long end = (my_id == (nodes - 1)) ? totalCands : step * (my_id + 1);

        long[] myCandidates = new long[(int) (end - start) + 5];

        // System.err.println(" My ids " + start + " " + end);

        int curId = 0;
        for (long i = start; i < end; i++) {
            myCandidates[curId] = i;
            curId++;
        }

        int remaining = curId;
        long currentRound = N;
        long bitStep = 0;
        boolean predRecieved = false;

        while (currentRound > 0) {
            if (remaining > 0) {
                int newIds = 0;
                int startId = 0;
                if ((myCandidates[0] >> bitStep) % 2 != 0) {
                    // System.err.println("Sending this to my pred" + myCandidates[0] + " " + (my_id - 1));
                    // Send this to my predecessor
                    message.PutLL(my_id - 1, myCandidates[0]);
                    message.Send(my_id - 1);
                    startId++;
                }

                for (int i = startId; i < remaining; i += 2) {

                    long candidate1 = myCandidates[i];
                    long candidate2 = myCandidates[i + 1];

                    if (i == (remaining - 1)) {
                        // Wait for my this to my successor
                        message.Receive(my_id + 1);
                        // System.err.println("Recieving stuff from " + (my_id + 1));
                        candidate2 = message.GetLL(my_id + 1);

                        if (candidate2 == DONE) {
                            myCandidates[0] = candidate1;
                            newIds = 1;
                            predRecieved = true;
                            break;
                        }
                    }

                    // System.err.println("Candidates are  " + candidate1 + " "  + candidate2);
                    char pref1 = rps.GetFavoriteMove(candidate1);
                    char pref2 = rps.GetFavoriteMove(candidate2);

                    long winner = getWinner(candidate1, pref1, candidate2, pref2);
                    myCandidates[newIds] = winner;
                    newIds++;
                }

                remaining = newIds;
                // System.err.println("Current round " + currentRound +
                // " Remaining cands " +  Arrays.toString(myCandidates) + " " + remaining);

            } else {
                break;
            }
            currentRound--;
            bitStep++;
        }

        if (predRecieved) {
            System.err.println("Done Recieving from pred");
        } else if (my_id == (nodes - 1)) {
            message.PutLL(my_id - 1, DONE);
            message.Send(my_id - 1);
        } else {

            // For Parrying through messages
            while (true) {
                message.Receive(my_id + 1);
                long val = message.GetLL(my_id + 1);
                if (val == DONE) {
                    System.err.println("Recieved Done from " + (my_id + 1));
                    if (my_id != 0) {
                        message.PutLL(my_id - 1, DONE);
                        message.Send(my_id - 1);
                    }
                    break;
                } else {
                    // System.err.println("Passing through message from " + (my_id + 1) + " Val " + val);
                    message.PutLL(my_id - 1, val);
                    message.Send(my_id - 1);
                }
            }
        }


        if (remaining > 0) {
            if (remaining > 1) {
                System.err.println("Something wrong with the code");
                System.err.println("Number of remaining" + remaining);
            }

            System.out.println(myCandidates[0]);
        } else if (remaining == 0) {
            System.err.println("All My cands are out");
        }
    }
}

