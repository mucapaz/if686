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
       
        void insert(int n){
        		
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
	
	public static void main(String[] args) throws Exception{
		
		Vetor vec = new Vetor(100);
		
		ArrayList<ThreadWrapper> vt = new ArrayList<ThreadWrapper>();
		
		for(int x=0;x<100;x++){
			vt.add(new ThreadWrapper(vec));
		}
		
		for(int x=0;x<100;x++){
			vt.get(x).run();
		}	
		for(int x=0;x<100;x++){
			vt.get(x).join();
		}
	}
}

class ThreadWrapper extends Thread{
	
	Vetor vec;
	
	ThreadWrapper(Vetor vec){
		this.vec = vec;
	}	
	public void run(){
		Random rand = new Random();
		int r,v1,v2;
		for(int x=0;x<20;x++){
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

// Trabalho 13


import java.util.*;
import java.util.concurrent.*;
import java.util.concurrent.atomic.AtomicInteger;

public class Main{
	public static void main(String[] args) throws Exception{
		AtomicInteger ai = new AtomicInteger(0);
		ArrayList<T> ar = new ArrayList<T>();	
			
		for(int x=0;x<100;x++){
			ar.add(new T(ai));
		}	
		
		for(int x=0;x<100;x++){
			ar.get(x).run();
		}	
		
		for(int x=0;x<100;x++){
			ar.get(x).join();
		}	
		
	}	
}

class T extends Thread{
	ArrayList<Integer> ar;
	AtomicInteger ai;
	
	T(AtomicInteger ai){
		this.ai = ai;
		ar = new ArrayList<Integer>();
	}
	
	public void run(){
		for(int x=0;x<10000;x++){
			ar.add(ai.incrementAndGet());
		}		
	}
}


/*
Liveness - quando existe a certeza que em algum momento o programa chega em um estado desejavel. Processos que devem executar seram executados. Mensagens enviadas por processos seram recebidas.

Safety - é a propriedade de que em um ambiente de diversas threads temos que apenas uma thread por vez tem o poder de manipular algum dado de memória compartilhada.

Para o exercicio das árvores temos safety a parti do momento que deixamos só uma thread por vez usar um nó e também temos liveness porque todos processos que devem ser executados são processados.

*/








































