import java.util.*;
import java.util.concurrent.locks.*;
 
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
               
               	ArrayList<Integer> br = new ArrayList<Integer>();
               	
                ini.inOrder(br);
               	System.out.println("Size = " + br.size());
               	boolean ok = true;
               	for(int x=0;x<br.size()-1;x++){
               		if(br.get(x) > br.get(x+1)) {
               			ok = false;
               		}
               	}
               	if(ok)System.out.println("Beleza");
               	else System.out.println("Merda\n");
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
       	private final Lock lock = new ReentrantLock();
       	
        No(int v){
                this.v = v;
                //this.lock = new ReentrantLock();
        }
        //synchronized
        synchronized  void insert(int n){
        		
        		boolean bool = lock.tryLock();
        		
        		try{
					while(!bool){
						lock.unlock();
				
						bool = lock.tryLock();
					}
			
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
                }finally{
                	lock.unlock();
                }
                             
        }
       
        void inOrder(ArrayList<Integer> br){
        	if(l != null) l.inOrder(br);
        	System.out.println(v);
        	br.add(v);
        	if(r != null) r.inOrder(br);
        }
        
}


/////////////////////////////
