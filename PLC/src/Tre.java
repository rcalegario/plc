public class Tre extends Thread{
		
		ListaThread list;
		int n;
		
		public Tre(ListaThread list, int n){
			this.list = list;
			this.n = n;
		}
		
		public void run(){
			boolean fez = false;
			while(!fez){
				if(!list.getAcesso()){
					list.insert(n);
					System.out.println("Thread " + this.n + " adicionou");
					fez = true;
				}else{
					System.out.println("Thread " + this.n + " não adicionou");
					try {
						Thread.sleep(10);
					} catch (InterruptedException e) {
						e.printStackTrace();
					}
				}
			}
			fez = false;
			
			while(!fez){
				if(!list.getAcesso()){
					list.remove();
					System.out.println("Thread " + this.n + " removeu");
					fez = true;
				}else{
					System.out.println("Thread " + this.n + " não removeu");
					try {
						Thread.sleep(10);
					} catch (InterruptedException e) {
						e.printStackTrace();
					}
				}
			}
			
		}
	}
