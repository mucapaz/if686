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
