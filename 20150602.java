// Com synchronized
package exercicio;

import java.util.ArrayList;
import java.util.Scanner;
import java.util.Vector;

public class E1{
	
	public static FilaSegura fila;

	public static void main(String []args) throws InterruptedException{
		fila = new FilaSegura();
		fila.inserir(2);
		ArrayList<E1Thread> filaThread = new ArrayList<E1Thread>();
		
		Scanner in = new Scanner(System.in);
		System.out.println("Diga o numero de threads");
		int n = in.nextInt();
		
		for(int x=0;x<n;x++)filaThread.add(new E1Thread(x));
		
		for(int x=0;x<n;x++)filaThread.get(x).start();
		
		for(int x=0;x<n;x++)filaThread.get(x).join();
		
	}
	
}

class E1Thread extends Thread{
	
	int id;
	
	public E1Thread(int id){
		this.id = id;		
	}
	
	public void run(){
		for(int x=0;x<1000;x++){
			System.out.println("Thread "+ id+ " tentando inserir" + x);
			E1.fila.inserir(x);			
		}
		
		for(int x=0;x<1000;x++){
			try {
				System.out.println("Thread "+ id+ " removeu " + E1.fila.remover());
			} catch (Exception e) {
				// TODO Auto-generated catch block
				e.printStackTrace();
			}
		}
	}
}


class FilaSegura{
	
	Vector<Integer> al;
	
	public FilaSegura(){
		al = new Vector<Integer>();
	}
	
	public void inserir(int i){
		synchronized(this){
			al.add(i);	
		}
	}
	
	public int remover() throws Exception{
		synchronized(this){
			if(al.isEmpty()){
				throw new Exception();
			}else{
				return al.remove(0);		
			}
		}
	}
	
}
