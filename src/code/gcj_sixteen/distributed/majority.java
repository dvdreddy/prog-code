package code.gcj_sixteen.distributed;

// Sample input 1, in Java.
public class majority {
  public majority() {
  }

  public static long GetN() {
    return 5L;
  }

  public static long GetVote(long i) {
    switch ((int)i) {
      case 0: return 7L;
      case 1: return 2L;
      case 2: return 7L;
      case 3: return 7L;
      case 4: return 5L;
      default: throw new IllegalArgumentException("Invalid argument");
    }
  }
}
