#lang mini-java

class Main {
  public static void main(String[] args) {
    System.out.println(new StateMachineRunner().doTheThing());
  }
}

#2dstate-machine
╔══════════╦══════════════════════════════════╦══════════════════════════════════╗
║ Receiver ║              wait_0              ║              wait_1              ║
╠══════════╬══════════════════════════════════╬══════════════════════════════════╣
║   zero   ║       System.out.println(0);     ║       System.out.println(1);     ║
║          ║              wait_1              ║              wait_1              ║
╠══════════╬══════════════════════════════════╬══════════════════════════════════╣
║   one    ║       System.out.println(2);     ║       System.out.println(3);     ║
║          ║              wait_0              ║              wait_0              ║
╚══════════╩══════════════════════════════════╩══════════════════════════════════╝

class StateMachineRunner {
    public int doTheThing() {
        Receiver r;
        r = new Receiver();
        System.out.println(r.one());
        System.out.println(r.zero());
        System.out.println(r.zero());
        System.out.println(r.one());
        return 0;
    }
}
