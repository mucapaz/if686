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


import java.util.*;
import java.util.concurrent.locks.*;
 
public class Main {
	
	public static void main(String[] args){
		
		Vetor vec = new Vetor(100);
		Random rand = new Random();
		int r,v1,v2;
		for(int x=0;x<1000;x++){
			r = Math.abs(rand.nextInt())%3;
			v1 = Math.abs(rand.nextInt())%100;
			v2 = Math.abs(rand.nextInt())%100;
			
			try{
			
				if(r == 0){ // read
					System.out.println("Read "+ v1 + " " + vec.read(v1));
				}else if(r == 1){ //write
					System.out.println("Write at " + v1 + " " + v2);
					vec.write(v1,v2);
				}else{//swap
					System.out.println("Swap " +v1 + " "+v2);
					vec.swap(v2,v2);
				}	
			}catch(Exception e){
				e.printStackTrace();
			}	
		}
	}
}

class Int{
	int v;
	private Lock l;
	
	Int(){
		v = 0;
		this.l = new ReentrantLock();
	}
	
	Int(int v){
		this.v = v;
		this.l = new ReentrantLock();
	}
	
	public void setV(int v){
		this.v = v;
	}		
	
	public int getV(){
		return this.v;
	}
	
	public boolean lock(){
		return l.tryLock();
	}
	
	public void unlock(){
		l.unlock();
	}
	
}

class Vetor{
	
	Int ar[];
	Vetor(int t){
		ar = new Int[t];
		for(int x=0;x<t;x++)ar[x] = new Int();
	}
	
	public int read(int i) throws Exception{
		if(!inside(i)) throw new Exception("Erro no index.");
		synchronized(ar[i]){
			return ar[i].getV();
		}
	}
	
	public void write(int i, int v) throws Exception{
		if(!inside(i)) throw new Exception("Erro no index.");
		synchronized(ar[i]){
			ar[i].setV(v);
		}
	}
	
	public void swap(int i, int j) throws Exception{
		if((!inside(i)) || (!(inside(j)))) throw new Exception("Erro no index.");
		boolean l1 = ar[i].lock(), l2 = ar[j].lock();
		
		while(!(l1 && l2)){
			ar[i].unlock();
			ar[j].unlock();
		
			l1 = ar[i].lock();
			l2 = ar[j].lock();
		}
		
		int temp = ar[i].getV();
		ar[i].setV(ar[j].getV());
		ar[j].setV(temp);
		
		ar[i].lock();
		ar[j].lock();
	}
	
	private boolean inside(int i){
		return (i>=0) && (i<ar.length);
	}
}



















