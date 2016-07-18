#lang mini-java

class Main {
    public static void main(String[] args) {
        System.out.println(new Thing().do_the_thing());
    }
}



#2dstate-machine
╔══════════╦═══════════════════════════╦═══════════════════════════╗
║ Machine  ║              a            ║            b              ║
╠══════════╬═══════════════════════════╬═══════════════════════════╣
║ change   ║  System.out.println(0); b ║  System.out.println(1); a ║
╠══════════╬═══════════════════════════╬═══════════════════════════╣
║ same     ║  System.out.println(2); a ║  System.out.println(3); b ║
╚══════════╩═══════════════════════════╩═══════════════════════════╝

class Thing {
    public int do_the_thing() {
        Machine m;
        m = new Machine();
        System.out.println(m.same());
        System.out.println(m.change());
        System.out.println(m.same());
        System.out.println(m.change());
        return 0;
    }
}

