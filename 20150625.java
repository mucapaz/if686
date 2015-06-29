import java.util.*;
 
public class ArvoreBusca {
       
       
       
        public static void main(String[] args) throws Exception{
               
                No ini = new No(0);
               
                ArrayList<T> ar = new ArrayList<T>();
                       
                for(int x=0;x<50;x++){
                        ar.add(new T(ini));
                }      
                for(int x=0;x<50;x++){
                        ar.get(x).run();
                }
                for(int x=0;x<50;x++){
                        ar.get(x).join();
                }
               
                ini.imprime();
               
        }
}
 
class T extends Thread{
                No ini;
               
                T(No ini){																														
                        this.ini = ini;
                }
                               
                public void run(){
                        Random r = new Random();
                        for(int x=0;x<10;x++){
                                ini.insert(r.nextInt()%10000 + 10000);
                        }
                }
}
 
 
class No{
        int v;
        No l,r;        
       
        No(int v){
                this.v = v;
        }
        //synchronized
        synchronized  void insert(int n){
                System.out.println(n + " " + v);
                if(n <= v){
                        if(l == null){
                                l = new No(n);
                        }else{
                                l.insert(n);
                        }                      
                }else{
                        if(r == null){
                                r = new No(n);
                        }else{
                                r.insert(n);
                        }              
                }              
        }
       
        void imprime(){
               
                if(l != null)l.imprime();
                System.out.println(v);
                if(r != null)r.imprime();
        }
}

