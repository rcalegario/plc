import java.util.ArrayList;

public class ListaThread {
	
	private ArrayList<Integer> list;
	private boolean acesso = false;

	public ListaThread(){
		this.list = new ArrayList<Integer>();
	}
	
	public boolean getAcesso(){
		return this.acesso;
	}
	
	public void insert(int n){
		//synchronized(this){
		this.acesso = true;	
		list.add(n);
		this.acesso = false;
		//}
	}
	
	public void remove(){
		//synchronized(this){
		this.acesso = true;	
		list.remove(0);
		this.acesso = false;
		//}
	}
	
}
