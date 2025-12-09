import java.math.BigInteger;

public class Main {
	
	
	public static void main (String[] args) throws java.lang.Exception
	{
		BigInteger sum = BigInteger.ZERO;
		BigInteger mod = BigInteger.TEN.pow(10);
		
		for (BigInteger i = BigInteger.ONE; i.intValue() <= 1000; i = i.add(BigInteger.ONE)) {
			sum = sum.add(i.modPow(i, mod)).mod(mod);
		}
		
		System.out.println(sum);
	}
}