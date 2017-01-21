package code.gcj_sixteen.distributed;

public class OopsSolution {
    static int MASTER_NODE = 7;
    static int DONE = -1;

    public static void main(String[] args) {
        long N = oops.GetN();
        long nodes = message.NumberOfNodes();
        long my_id = message.MyNodeId();
        long best_so_far = Long.MIN_VALUE;
        long worst_so_far = Long.MAX_VALUE;
        for (long j = 0; j < N; ++j) {
            if (j % nodes == my_id) {
                long candidate = oops.GetNumber(j);
                if (candidate > best_so_far) {
                    best_so_far = candidate;
                }
                if (candidate < worst_so_far) {
                    worst_so_far = candidate;
                }
            }
        }

        message.PutLL(MASTER_NODE, best_so_far);
        message.PutLL(MASTER_NODE, worst_so_far);
        message.PutInt(MASTER_NODE, DONE);
        message.Send(MASTER_NODE);

        if (my_id == MASTER_NODE) {
            long global_best_so_far = Long.MIN_VALUE;
            long global_worst_so_far = Long.MAX_VALUE;
            for (int node = 0; node < nodes; ++node) {
                long received_candidate = 0;
                message.Receive(node);
                received_candidate = message.GetLL(node);
                if (received_candidate > global_best_so_far) {
                    global_best_so_far = received_candidate;
                }

                received_candidate = message.GetLL(node);
                if (received_candidate < global_worst_so_far) {
                    global_worst_so_far = received_candidate;
                }

                int doneMessage = message.GetInt(node);
                if (doneMessage != DONE) {
                    System.err.println("Unknown Behavior");
                }
            }
            System.out.println(global_best_so_far - global_worst_so_far);
        }
    }
}

