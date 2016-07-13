 #lang mini-java

/* example mini-java program */

class Factorial{
    public static void main(String[] a){
        System.out.println(new Fac().ComputeFac(10));
    }
}

#2d
╔══════════╦══════════╦══════════╗
║    a     ║    c     ║   d      ║
╠══════════╬══════════╩══════════╣
║    b     ║         a = 5; f    ║
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