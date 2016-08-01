#lang mini-java

class Main {
  public static void main(String [] args) {
    {
    System.out.println((new Breaker()).uses_break(13));
    System.out.println((new Breaker()).uses_break(31));
    }
  }
}

class Breaker {          
   public int uses_break(int n) {
   int count;
   count = 0;
   while (count < n) {
     System.out.println(count);                 
     if (count == 17) {
        break;
     }
     else {
      count = count + 1;
    }
   }
   return count;
  } 
}