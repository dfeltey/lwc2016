#lang mini-java

class Main {
 public static void main(String [] args) {
  System.out.println((new Runner()).run(10));
 }
}

#2dstate-machine
╔═══════════════╦════════════════════════╦════════════════════════╗
║ ParityMachine ║          even          ║          odd           ║ 
╠═══════════════╬════════════════════════╬════════════════════════╣
║    add_one    ║          odd           ║          even          ║
╠═══════════════╬════════════════════════╬════════════════════════╣
║     reset     ║          even          ║          even          ║
╠═══════════════╬════════════════════════╬════════════════════════╣
║     print     ║ System.out.println(0); ║ System.out.println(1); ║
║               ║          even          ║          odd           ║
╚═══════════════╩════════════════════════╩════════════════════════╝

class Parity extends ParityMachine {
 public int print_parity(int n) {
  int dumb;
  int start;
  start = 0;
  while (start < n) {
   dumb = this.add_one();
   start = start + 1;
  }
  dumb = this.print();
  dumb = this.reset();
  return 42;
 }
}

class Runner {
 Parity check;
 public int run(int n) {
  int current;
  int dumb;
  check = new Parity();
  current = 0;
  while (current < n) {
   dumb = check.print_parity(current);
   current = current + 1;
  }
  return 84;
 }
}


