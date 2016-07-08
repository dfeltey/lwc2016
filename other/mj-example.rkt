#lang mini-java

/* example mini-java program */

class Factorial{
    public static void main(String[] a){
        System.out.println(new Fac().ComputeFac(10));
    }
}

#2d
╔══════════╦══════════╦══════════╗
║    0     ║    1 2   ║   3 4    ║
╠══════════╬══════════╩══════════╣
║    5     ║         6           ║
╚══════════╩═════════════════════╝

class Fac {
    public int ComputeFac(int num){
        int num_aux ;
        if (num < 1)
            num_aux = 1 ;
        else
            num_aux = num * (this.ComputeFac(num-1)) ;
        return num_aux ;
    }
}

