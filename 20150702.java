//trabalho 14

import java.util.*;
import java.util.concurrent.*;
import java.lang.*;

public class Main {
	public static void main(String[] args) throws Exception{
		Bathroom bath = new Bathroom(10, 10000);
		bath.start();
		ArrayList<Person> al = new ArrayList<Person>();
		Random rand = new Random();
			
		for(int x=0;x<10000;x++){
			if(rand.nextInt() > 0)al.add(new Person("Male", bath,x));
			else al.add(new Person("Female", bath,x));
			
			//System.out.println("Criado " + x + " " + al.get(x).genre );	
				
			al.get(x).start();
			
		}
		
		for(int x=0;x<3;x++){
			al.get(x).join();
		}
	
		//System.out.println("-------------");	
		bath.join();	
	}	
}

class Person extends Thread {
	public String genre;
	public int id;
	//boolean done;
	Bathroom bath;

	Person(String genre, Bathroom bath, int id) {
		this.genre = genre;
		this.bath = bath;
		this.id = id;
		//this.done = false;
	}
	public void run(){
		synchronized(this) {
			bath.push(this);
			try{
				//System.out.println("Entrou em wait "  + id);
				wait(); // time to do something in the bathroom
				//System.out.println("Fez o trabalho "  + id);
			}catch(Exception e){
				e.printStackTrace();
				return;
			}
			bath.remove(this);
			//System.out.println("Removeu "  +id);
		}
	}
}

class Bathroom extends Thread {
	Queue < Person > queue;
	ArrayList < Person > inside;
	String genre;
	Random rand;
	int limit, total;
	Bathroom(int limit, int total) {
		this.queue = new LinkedList < Person > ();
		this.inside = new ArrayList < Person > ();
		this.genre = "";
		this.rand = new Random();
		this.limit = limit;
		this.total = total;
	}

	public void run() {
		while(true) {
			//System.out.println(total);
			if(total == 0) return;
			if(!schedule()){
				System.out.println("Deu MERDA");
				return;
			}	
		}
	}
	
	public synchronized boolean schedule(){
		String ge = "";
		boolean ok = true;
		for(Person pe : inside){
			if(ge == "")ge = pe.genre;
			else{
				if(pe.genre != ge){
					ok = false;
				}
			}
		}
		if(!ok) return false;
		
		Person person;
		while (inside.size() > 0) {
			if (Math.abs(rand.nextInt()) % 3 == 0) {
				break;
			} else {
				person = inside.remove(0);
				synchronized(person){
					person.notifyAll();
				}
			}
		}
		while (queue.size() > 0) {
			if (inside.size() > 0) {
				if (inside.get(0).genre == queue.peek().genre) {
					if(inside.size() < limit){
						inside.add(queue.remove());
					}else{
						break;
					}
				}else{
						break;
				}
			} else {
				inside.add(queue.remove());
				break;
			}
		}
		return true;
	}
	public synchronized void push(Person p) {
		synchronized(p){
			queue.add(p);
		}
	}
	public synchronized void remove(Person p) {
		synchronized(p){
			inside.remove(p);
			total--;
		}
	}

}

//// com lock e condition

import java.util.*;
import java.util.concurrent.*;
import java.util.concurrent.locks.*;
import java.lang.*;

public class Main {
	public static void main(String[] args) throws Exception{
		Bathroom bath = new Bathroom(10, 10000);
		bath.start();
		ArrayList<Person> al = new ArrayList<Person>();
		Random rand = new Random();
			
		for(int x=0;x<10000;x++){
			if(rand.nextInt() > 0)al.add(new Person("Male", bath,x));
			else al.add(new Person("Female", bath,x));
			
			//System.out.println("Criado " + x + " " + al.get(x).genre );	
			al.get(x).start();
		}
		
		for(int x=0;x<3;x++){
			al.get(x).join();
		}
		//System.out.println("-------------");	
		bath.join();	
	}	
}

class Person extends Thread {
	public String genre;
	public int id;
	public Lock lock = new ReentrantLock();
	Condition cond = lock.newCondition();
	//boolean done;
	Bathroom bath;

	Person(String genre, Bathroom bath, int id) {
		this.genre = genre;
		this.bath = bath;
		this.id = id;
		//this.done = false;
	}
	public void run(){
		lock.lock();
		bath.push(this);
		try{
			//System.out.println("Entrou em wait "  + id);
			cond.await(); // time to do something in the bathroom
			//System.out.println("Fez o trabalho "  + id);
		}catch(Exception e){
			e.printStackTrace();
			return;
		}
		bath.remove(this);
		//System.out.println("Removeu "  +id);
		lock.unlock();
	}
}

class Bathroom extends Thread {
	Queue < Person > queue;
	ArrayList < Person > inside;
	String genre;
	Random rand;
	int limit, total;
	Lock lock = new ReentrantLock();
	
	Bathroom(int limit, int total) {
		this.queue = new LinkedList < Person > ();
		this.inside = new ArrayList < Person > ();
		this.genre = "";
		this.rand = new Random();
		this.limit = limit;
		this.total = total;
	}

	public void run() {
		while(true) {
			//System.out.println(total);
			if(total == 0) return;
			if(!schedule()){
				System.out.println("Deu MERDA");
				return;
			}	
		}
	}
	
	public boolean schedule(){
		lock.lock();
		String ge = "";
		boolean ok = true;
		for(Person pe : inside){
			if(ge == "")ge = pe.genre;
			else{
				if(pe.genre != ge){
					ok = false;
				}
			}
		}
		if(!ok) return false;
		
		Person person;
		while (inside.size() > 0) {
			if (Math.abs(rand.nextInt()) % 3 == 0) {
				break;
			} else {
				person = inside.remove(0);
				person.lock.lock();
				person.cond.signalAll();
				person.lock.unlock();
			}
		}
		while (queue.size() > 0) {
			if (inside.size() > 0) {
				if (inside.get(0).genre == queue.peek().genre) {
					if(inside.size() < limit){
						inside.add(queue.remove());
					}else{
						break;
					}
				}else{
						break;
				}
			} else {
				inside.add(queue.remove());
				break;
			}
		}
		lock.unlock();
		return true;
	}
	
	
	public void push(Person p) {
		lock.lock();
		p.lock.lock();
		queue.add(p);
		p.lock.unlock();
		lock.unlock();
	}
	public void remove(Person p) {
		lock.lock();
		p.lock.lock();
		inside.remove(p);
		total--;
		p.lock.unlock();
		lock.unlock();
	}

}
