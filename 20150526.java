//Exercicios em sala

// e1

package thread;

import java.util.Scanner;
import java.util.Vector;

public class PrintNumberThread extends Thread{
	
	int l1,l2;

	public PrintNumberThread(int l1, int l2){
		this.l1 = l1;
		this.l2 = l2;

	}
	public void run(){
		for(int x=l1;x<l2;x++){
			System.out.println(x);
		}
	}

	public static void main(String args[]){
		Scanner in = new Scanner(System.in);

		System.out.println("Inteiro e range");

		int i = in.nextInt();
		int j = in.nextInt();

		Vector<PrintNumberThread> vec = new Vector<PrintNumberThread>();

		for(int x=0; x<= i; x+=i/j){
			vec.add(new PrintNumberThread(x, x + i/j ) );


		}

		try {
			for(int x=0;x<vec.size();x++){
				vec.elementAt(x).start();				
			}
			for(int x=0;x<vec.size();x++){
				vec.elementAt(x).join();				
			}
		} catch (InterruptedException e) {
			// TODO Auto-generated catch block
			e.printStackTrace();
		}

	}
}

//e2 


package exercicio;

import java.util.Vector;

public class E2Thread extends Thread{
	
	int i,j;
	
	public E2Thread (int i, int j){
		this.i = i;
		this.j = j;
	}
	
	public void loopPrint(){
		for(int x=i;x<=j;x++){
			System.out.println(x);
		}		
	}
	
	public void run (){
		loopPrint();		
	}
	
	public static void main(String args[]) throws InterruptedException{
		
		Vector<E2Thread> vec = new Vector<E2Thread>();
		
		for(int x=0;x<10;x++){
			vec.add(new E2Thread(0, 100000));			
		}
		
		for(int x=0;x<10;x++){
			vec.elementAt(x).start();
		}
		
		for(int x=0;x<10;x++){
			vec.elementAt(x).join();			
		}
		
	}
}


// e3 


package exercicio;

import java.util.Vector;

public class E2Thread extends Thread{
	
	static int i = 0 ,j = 1000000;
	
	public E2Thread (){
		
	}
	
	public void loopPrint(){
		while(i < j){
			System.out.println(i);
			i++;
		}
	}
	
	public void run (){
		loopPrint();		
	}
	
	public static void main(String args[]) throws InterruptedException{
		
		Vector<E2Thread> vec = new Vector<E2Thread>();
		
		for(int x=0;x<10;x++){
			vec.add(new E2Thread());			
		}
		
		for(int x=0;x<10;x++){
			vec.elementAt(x).start();
		}
		
		for(int x=0;x<10;x++){
			vec.elementAt(x).join();			
		}
		
	}
}


//e4 

package exercicio;

import java.util.Vector;

public class E2Thread extends Thread{
	
	int i = 0 ;
	public static ThreadLocal<Integer> tlocal = new ThreadLocal<Integer>();
	
	public E2Thread (int i){
		this.i = i;
	}
	
	
	public void run (){
		
		tlocal.set(i);
		new Teste().metodo();
		tlocal.remove();
	}
	
	public static void main(String args[]) throws InterruptedException{
		
		Vector<E2Thread> vec = new Vector<E2Thread>();
		
		for(int x=0;x<10;x++){
			vec.add(new E2Thread(x*10000));			
		}
		
		for(int x=0;x<10;x++){
			vec.elementAt(x).start();
		}
		
		for(int x=0;x<10;x++){
			vec.elementAt(x).join();			
		}
		
	}
}

class Teste{
	
	public void metodo(){
			int i =  E2Thread.tlocal.get();
			for(int x=0;x<i;x++){
				System.out.println(x);
			}
			
	}
	
	
}


//e5

package exercicio;

import java.util.Vector;

public class EThread extends Thread{
	
	public static Vector<Integer> vec = new Vector<Integer>();
	int i;
	public EThread(int i){
		this.i = i;		
	}
	
	public boolean isPrime(){
		if(i == 0 || i == 1) return false;
		for(int x=2;x<i;x++){
			if(i%x == 0) return false;
		}
		return true;
	}
	
	public void run(){
		if(isPrime()) vec.add(i);
	}
	
	 public static void main(String[] args) throws InterruptedException{
		Vector<EThread> v = new Vector<EThread>();
		
		for(int x=0;x<10000;x++){
			v.add(new EThread(x));
		}
		
		for(int x=0;x<10000;x++){
			v.elementAt(x).start();
		}
		
		for(int x=0;x<10000;x++){
			v.elementAt(x).join();
		}
		
		for(int x=0;x<vec.size();x++)System.out.println(vec.elementAt(x));
		
	}
	
	
}


//e6 

package exercicio;

import java.util.Vector;

public class EThread extends Thread{
	
	public static Vector<Integer> vec = new Vector<Integer>();
	int i;
	public EThread(int i){
		this.i = i;		
	}
	
	public boolean isPrime(){
		if(i == 0 || i == 1) return false;
		for(int x=2;x<i;x++){
			if(i%x == 0) return false;
			
		}
		return true;
	}
	
	public void run(){
		if(isPrime()) vec.add(i);
	}
	
	 public static void main(String[] args) throws InterruptedException{
		Vector<EThread> v = new Vector<EThread>();
		
		for(int x=0;x<10000;x++){
			v.add(new EThread(x));
		}
		
		for(int x=0;x<10000;x++){
			v.elementAt(x).start();
			v.elementAt(x).sleep(1);
		}
		
		for(int x=0;x<10000;x++){
			v.elementAt(x).join();
		}
		
		for(int x=0;x<vec.size();x++)System.out.println(vec.elementAt(x));
		
	}
	
	
}



