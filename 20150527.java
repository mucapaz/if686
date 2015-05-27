// Trabalho 12
package exercicio;

import java.io.FileNotFoundException;
import java.io.FileReader;
import java.util.Scanner;
import java.util.Vector;

public class T12 {

	static int e[][][] = new int[3][][];
	public static ThreadLocal<int [][][]> tl = new ThreadLocal<int [][][]>();
	
	public static void main(String []args) throws InterruptedException, FileNotFoundException{
		
		Scanner in  = new Scanner(new FileReader("in"));
		tl.set(e);
		int n = in.nextInt();
		int n1 = in.nextInt(), m1 = in.nextInt();
		int n2 = in.nextInt(), m2 = in.nextInt();
		
		e[0] = new int[n1][m1];
		e[1] = new int[n2][m2];
		e[2] = new int[n1][m2];
		
		for(int x=0;x<n1;x++)for(int y=0;y<m1;y++) e[0][x][y] = in.nextInt();
		for(int x=0;x<n2;x++)for(int y=0;y<m2;y++) e[1][x][y] = in.nextInt();
		
		
		
		Vector<MulThread> vec = new Vector<MulThread>();
		for(int x=0;x<n;x++) vec.add(new MulThread());
		
		int c1 = 0, c2 =0, tam = (n1*m2)/n;
		
		for(int x=0;x<n1;x++){
			for(int y=0;y<m2;y++){
				if(c1 == n - 1){
					vec.elementAt(c1).add(new Triple(x,y,m1));
				}else{
					c2++;
					if(c2 == tam){
						vec.elementAt(c1).add(new Triple(x, y,m1));
						c2 = 0;						
						c1++;
					}else{
						vec.elementAt(c1).add(new Triple(x,y,m1));						
					}
				}
			}
		}
		
		for(int x=0;x<n;x++) vec.elementAt(x).start();
		for(int x=0;x<n;x++) vec.elementAt(x).join();
		
		tl.remove();
		
		for(int x=0;x<n1;x++){
			for(int y=0;y<m2;y++){
				System.out.print(e[2][x][y] + "-");
			}
			System.out.println();
		}
		
	}
}

class MulThread extends Thread{
	
	Vector<Triple> vec;
	int e[][][];
	
	public MulThread(){
		vec = new Vector<Triple>();
	}
	
	public void add(Triple p){
		vec.add(p);
	}

	public void play(int l){
		int x = vec.elementAt(l).x, y = vec.elementAt(l).y, i = vec.elementAt(l).i;
		int sum = 0;
		for(int k=0;k<3;k++){
			sum+= 
					e[0][x][k]*
					e[1][k][y];
		}
		e[2][x][y] = sum;
	}
		
	public void run(){
		e = T12.e;
		//e = T12.tl.get();
		
		for(int k=0;k<vec.size();k++){
			play(k);
		}		
	}
	
}

class Triple{
	public int x,y,i;
	public Triple(int x, int y, int i){
		this.x = x;
		this.y = y;
		this.i = i;
	}
}










