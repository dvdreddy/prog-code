package code.gcj_sixteen.distributed;


import java.util.HashSet;
import java.util.Set;
import java.util.TreeSet;

 public class Main2 {
     static int MASTER_NODE = 0;
     static long DONE = -1;


     public static int hash(long value) {
         int a = (int)(value ^ (value >>> 32));
         a ^= (a << 13);
         a ^= (a >>> 17);
         a ^= (a << 5);
         return a;
     }

     public static void main(String[] args) {

         int nodes = message.NumberOfNodes();
         int my_id = message.MyNodeId();

         TreeSet<Long>[] sets = new TreeSet[nodes];
         Set<Long>[] dupSets = new Set[nodes];

         for (int i = 0; i < nodes; i++) {
             sets[i] = new TreeSet<Long>();
             dupSets[i]  = new HashSet<Long>();
         }


         int N = (int) winning_move.GetNumPlayers();

         for (int i = 0; i < N; i++) {
             if (i % nodes == my_id) {
                 Long val = winning_move.GetSubmission(i);
                 int hash = hash(val);
                 int targetNode = hash % nodes;
                 if (dupSets[targetNode].contains(val)) {
                     // nothing to do here
                 } else if (sets[targetNode].contains(val)) {
                     sets[targetNode].remove(val);
                     dupSets[targetNode].add(val);
                 } else {
                     sets[targetNode].add(val);
                 }
             }
         }


         TreeSet<Long> myFinalSet = new TreeSet<Long>();
         Set<Long> myDupSet = new HashSet<Long>();

         for (int i = 0; i < nodes; i++) {
             if (i == my_id) {
                 myFinalSet = sets[i];
                 myDupSet = dupSets[i];
             } else {

                 message.PutInt(i, sets[i].size());
                 for (Long val : sets[i]) {
                     message.PutLL(i, val);
                 }
                 message.Send(i);
             }
         }


         sets = null; dupSets = null; // Free memory
         // Recieve
         for (int i = 0; i < nodes; i++) {
             if (i != my_id) {
                 message.Receive(i);
                 int size = message.GetInt(i);
                 for (int j = 0; j < size; j++) {
                     long newVal = message.GetLL(i);
                     if (myDupSet.contains(newVal)) {
                         // nothing to do here
                     } else if (myFinalSet.contains(newVal)) {
                         myFinalSet.remove(newVal);
                         myDupSet.add(newVal);
                     } else {
                         myFinalSet.add(newVal);
                     }
                 }
             }
         }



         // Send to 0
         if (myFinalSet.size() > 0) {
             message.PutLL(MASTER_NODE, myFinalSet.first());
         } else {
             message.PutLL(MASTER_NODE, DONE);
         }
         message.Send(MASTER_NODE);



         if (my_id == MASTER_NODE) {

             long finalResult = Long.MAX_VALUE;

             for (int i = 0; i < nodes; i++) {
                 message.Receive(i);

                 long val = message.GetLL(i);
                 if (val != DONE) {
                     finalResult = Math.min(finalResult, val);
                 }
             }

             finalResult = (finalResult == Long.MAX_VALUE) ?
                     0 : finalResult;

             System.out.println(finalResult);
         }




     }
}

